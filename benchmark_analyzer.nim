import std / [strformat, sequtils, tables, strutils, logging, os, macros, stats, strscans, times, options]
import std / [terminal, colors] # for color printing text
import ggplotnim, mpfit

import std / json # for easier printing of data to log files

from seqmath import median


const DateCol = "Date"
const ProgramCol = "Program"

# Constants for the CSV file of the trace sizes (also used for the DF for plotting!)
const TraceName = "Name"
const MainTrace = "Main trace"
const PermTrace = "Permutation trace"
const MPTrace = "Total trace size"

# Constants for the CSV file containing the raw data
const TestNameCol = "Test Name" ## column containing name of the bench in the raw files
const IterCol = "Iteration" ## column containing the iteration index
const RTimeCol = "Real Time" ## column containing the real time the program ran
const UTimeCol = "User Time" ## column containing the real time the program ran
const MemoryCol = "Memory (MB)" ## column containing used memory by the program

const StdSuffix = " [σ]"

## XXX: Make the log file dependent on the input file to map to that?
const LogFile {.strdefine.} = "./logs/benchmark_analyzer.log"

macro getBody(fn: typed): untyped =
  let fnImpl = fn.getImpl
  result = newLit(fnImpl.body.repr)

template red(msg: untyped): untyped =
  ## Writes the given text in red using ANSI color codes
  ansiForegroundColorCode(colRed) & msg & ansiResetCode
template green(msg: untyped): untyped =
  ## Writes the given text in green using ANSI color codes
  ansiForegroundColorCode(colGreen) & msg & ansiResetCode

template warnRed(msg: varargs[untyped]): untyped =
  warn(red msg.join())
template warnGreen(msg: varargs[untyped]): untyped =
  warn(green msg.join())



type
  ## Helper to store the fit results of a single VM version + trace
  FitResult = object
    df: DataFrame
    ps: seq[float] ## final fit parameters
    res: MpResult ## full fit results

  ## a distinct class label for VM version & the trace
  Class = tuple[vm, trace: string]

  ## Implemented fit functions
  FitFunction = enum
    ffLogPlusLin = "logLin"
    ffLinear = "linear"

  LogVerbosity = enum
    lvQuiet,   ## No log (not implemented yet)
    lvDefault, ## Regular log file
    lvVerbose  ## Extra verbose log, e.g. contains calculated metrics etc.

  ## Context object storing the user settings
  Config = object
    fname: string ## Path to the CSV file containing benchmark results
    plotPath: string = "/tmp"
    logPath: string = "./logs"
    log10: bool = false
    fitFunc: FitFunction = ffLogPlusLin
    raw: bool = false ## Indicates input is raw data
    logVerbosity = lvDefault  ##
    traceSizes: string = "resources/trace_sizes.csv" ## Path to the file containing trace sizes for each benchmark

    # Parameters of the data files
    testCol: string = TestNameCol
    iterCol: string = IterCol
    rTimeCol: string = RTimeCol
    uTimeCol: string = UTimeCol
    memCol: string = MemoryCol

    # Stats config
    outlierThr: float = 3.0 ## threshold in standard deviations a point needs to be away from mean to count as an outlier
                            ## i.e. `3.0` implies a point needs to be 3σ away from the mean.

    # Comparison config
    compare: string ## Another input file. If given, will produce a report comparing performance to the main `fname` input
    perfDiffThr: float ## Threshold in performance difference between the two files at which we add them to the report

  Model = object
    fn: FuncProto[float]
    body: string ## stringified version of the function body

  ## Statistics about a specific metric. This can be 'Real Time', 'User Time', 'Memory', ...
  MetricStats = object
    raw: seq[float]   ## the raw data values
    min, max: float
    mean, median: float
    stdS: float       ## *sample* standard deviation
    outlierThr: float ## threshold in standard deviations a point needs to be away from mean to count as an outlier
                      ## i.e. `3.0` implies a point needs to be 3σ away from the mean. Set via `Config` field of same name.
    thrMin: float ## Lower threshold for outlier detection
    thrMax: float ## Upper threshold for outlier detection
    outliers: int     ## number of outliers
    # whatever else we might want to add?

  ## A set of *known* benchmarks. Their string value corresponds to the substring that must be contained
  ## in the `TestName` column string.
  ## For some tests the string might be a string to be used with `strscans`
  BenchmarkKind = enum
    bkUnknown = ""
    bkRecursiveFibonacci = "Fib$i Rec"
    bkIterFibonacci = "Fib$i Iter"
    bkSha = "SHA$i"
    bkKeccak = "Keccak$i"

  ProgrammingLanguage = enum
    plC = "C"
    plRust = "Rust"

  ## stores information for a single bench?
  Benchmark = object
    name: string ## name of the benchmark program
    date: string ## Formatted date the benchmark ran
    commit: string ## Valida commit on which the benchmark was run
    isSerial: bool ## Whether the bench is a serial bench
    lang: ProgrammingLanguage ## Language the bench is written in
    bench: BenchmarkKind ## Set of *known* benchmark kinds. If it is known we can merge multiple different
                             ## benchmarks of the same kind with different sizes
    size:      int ## Some benchmarks include size information in their name, e.g. `Fib6`, `Keccak500` etc
    mainTrace: int ## Size of the main trace in field elements
    permTrace: int ## Size of the permutation trace in binomial extension field elements of degree 5
    totalTrace: int ## Size of the main trace + permutation trace in field elements (`main + 5 * perm`)
    rTimes: MetricStats
    uTimes: MetricStats
    mem: MetricStats

  BenchTable = Table[string, Benchmark] # maps each benchmark (based on the `TestName` column value) to the `Benchmark` data

var cLog = newConsoleLogger(fmtStr = "[$time] - $levelname: ")
createDir(LogFile.parentDir) # create `logs` dir if it does not exist
var fLog = newFileLogger(LogFile, fmtStr = "[$time] - $levelname: ")
addHandler(cLog)
addHandler(fLog)

# Config can be adjusted from `bench_analyzer.config` file or CL arguments
proc initConfig(): Config = Config() # just init with default values

## To determine if an input CSV file contains raw data
proc isRawFile(s: string): bool = s.endsWith("_raw.csv")

proc logPlusLin(p: seq[float], x: float): float =
  result = p[0] * x * ln(x) + p[1] * x + p[2]

proc linear(p: seq[float], x: float): float =
  result = p[0] * x + p[1]

proc getFitFunction(fitFn: FitFunction): Model =
  case fitFn
  of ffLogPlusLin: Model(fn: logPlusLin, body: getBody(logPlusLin))
  of ffLinear: Model(fn: linear, body: getBody(linear))

template separator(): string = repeat('-', 30)

proc fitData(m: Model, xs, ys, ey: seq[float]): FitResult =
  ## Perform a fit of `fitFn` given the data.
  let params = @[1.0, 1.0, 1.0]
  let (pRes, res) = fit(m.fn, params,
                        x = xs,
                        y = ys,
                        ey = ey) # error is sample standard deviation
  let resText = pretty(pRes, res)
  info("Fit result for fit function:")
  info(&"\t`{m.body}`")
  info(separator() & "\n" & resText)
  info(separator())

  # Compute values for the function based on fit results
  let xs = linspace(xs.min, xs.max, 1000)
  let ys = xs.mapIt(m.fn(pRes, it))
  result = FitResult(df: toDf({"xs" : xs, "ys" : ys}),
                     ps: pRes,
                     res: res)

proc sanitize(s: string): string =
  ## sanitizes the string to be sane for a file name.
  result = s.multiReplace((" ", "_"), (":", "_"), ("(", "_"), (")", "_"),
                          ("/", "_"), ("+", ""))

proc individualPlots(cfg: Config, df: DataFrame, suffix: string = "") =
  ## Creates the individual plots of the different traces
  ## split by the different VM versions.

  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  proc plotAgainst(yCol, ySuf: string) =
    ggplot(df, aes(MainTrace, yCol, color = DateCol)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_trace_{ySuf}.pdf")
    ggplot(df, aes(PermTrace, yCol, color = DateCol)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/permutation_trace_{ySuf}.pdf")
    ggplot(df, aes(MPTrace, yCol, color = DateCol)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_permutation_trace_{ySuf}.pdf")

  plotAgainst(UTimeCol, "user_time" & suffix)
  plotAgainst(RTimeCol, "real_time" & suffix)
  plotAgainst(MemoryCol, "memory" & suffix)

proc facetPlots(cfg: Config, df: DataFrame, suffix: string = "") =
  ## Create facet plots of the different plots
  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  # 1. facet by type for one trace. One pane space, user time, real time
  #block:
  #  let df = df.gather([UTimeCol, RTimeCol, MemoryCol], "Type", "Value")
  #  ggplot(df, aes(MainTrace, "Value", color = DateCol)) +
  #    facet_wrap("Type", scales = "free") +
  #    geom_point() +
  #    xScale + yScale +
  #    ggsave(&"{cfg.plotPath}/main_trace_facet.pdf")
  #  ggplot(df, aes(PermTrace, "Value", color = DateCol)) +
  #    facet_wrap("Type", scales = "free") +
  #    geom_point() +
  #    xScale + yScale +
  #    ggsave(&"{cfg.plotPath}/perm_trace_facet.pdf")
  #  ggplot(df, aes(MPTrace, "Value", color = DateCol)) +
  #    facet_wrap("Type", scales = "free") +
  #    geom_point() +
  #    xScale + yScale +
  #    ggsave(&"{cfg.plotPath}/main_perm_trace_facet.pdf")

  # 2. Facet of all traces against single time / space variable
  block:
    let df = df.gather([MainTrace, PermTrace, MPTrace], "Type", "Value")
    proc plotAgainst(yCol, ySuf: string) =
      let σCol = yCol & StdSuffix
      ggplot(df, aes("Value", yCol, color = DateCol)) +
        facet_wrap("Type", scales = "free") +
        facetMargin(0.5) +
        margin(right = 2.0, bottom = 1.5) +
        xlab(rotate = -30.0, alignTo = "right") +
        geom_point() +
        geom_errorbar(aes = aes(yMin = f{idx(yCol) - idx(σCol)}, yMax = f{idx(yCol) + idx(σCol)})) +
        legendPosition(0.5, 0.2) +
        xScale + yScale +
        ggsave(&"{cfg.plotPath}/traces_{ySuf}_facet.pdf")

    plotAgainst(UTimeCol, "user_time" & suffix)
    plotAgainst(RTimeCol, "real_time" & suffix)
    plotAgainst(MemoryCol, "space" & suffix)

proc assembleDf(tab: Table[Class, FitResult]): DataFrame =
  ## Assembles all individual fitting result DFs to a single DF
  result = newDataFrame()
  for (tup, fitRes) in pairs(tab):
    let (ver, tr) = tup
    var df = fitRes.df
    df[DateCol] = ver
    df["Type"] = tr
    result.add df

proc getErrorColumn(s: string): string = s & StdSuffix

proc plotFitAllTraces(cfg: Config, df: DataFrame, yCol: string, suffix: string = "") =
  ## Fits all traces for the given y column `yCol` and creates a plot for
  ## each fit individually, which includes the fit results as an annotation.
  ##
  ## Finally produces a combined facet plot of all traces against the
  ## `y` column and split by the different VM versions.
  let df = df.gather([MainTrace, PermTrace, MPTrace], "Type", "Value")
    .mutate(f{"yMin" ~ idx(yCol) - idx(getErrorColumn(yCol))},
            f{"yMax" ~ idx(yCol) + idx(getErrorColumn(yCol))})

  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  var fitTab = initTable[Class, FitResult]()
  for (tup, subDf) in groups(df.group_by([DateCol, "Type"])):
    ## Fit each trace & VM version
    let ver = tup[0][1].toStr # corresponds to `DateCol`
    let tr = tup[1][1].toStr  # corresponds to `"Type"`
    let class = (vm: ver, trace: tr)

    let fn = getFitFunction(cfg.fitFunc)
    info(&"Fitting data for: {ver} -- {tr} against {yCol}")
    let fitRes = fn.fitData(subDf["Value", float].toSeq1D,
                            subDf[yCol, float].toSeq1D,
                            subDf[getErrorColumn(yCol), float].toSeq1D)
    fitTab[class] = fitRes

    let verStr = ver.sanitize()
    let trStr = tr.sanitize()

    # Individual plot of this fit & data
    ggplot(subDf, aes("Value", yCol)) +
      geom_point() +
      geom_errorbar(aes = aes(x = "Value", yMin = "yMin", yMax = "yMax")) +
      geom_line(data = fitRes.df, aes = aes("xs", "ys")) +
      annotate(left = 0.05, bottom = 0.45, text = pretty(fitRes.ps, fitRes.res)) +
      xlab(tr) +
      xScale + yScale +
      ggtitle(&"{DateCol}: {ver}") +
      ggsave(&"{cfg.plotPath}/{trStr}_{verStr}_with_fit{suffix}.pdf")

  let dfFits = assembleDf(fitTab)
  # Cmobined plot of all the fits
  ggplot(df, aes("Value", yCol, color = DateCol)) +
    geom_point() +
    geom_errorbar(aes = aes(x = "Value", yMin = "yMin", yMax = "yMax")) +
    geom_line(data = dfFits, aes = aes("xs", "ys", color = DateCol)) +
    facet_wrap("Type", scales = "free") +
    facetMargin(0.525) +
    margin(right = 0.001, bottom = 1.5) +
    xlab(rotate = -30.0, alignTo = "right") +
    legendPosition(0.55, 0.2) +
    xScale + yScale +
    xlab("Trace size") +
    ggsave(&"{cfg.plotPath}/all_traces_{yCol.sanitize()}_with_fit{suffix}.pdf")


func aggregate(cfg: Config, s: seq[float]): MetricStats =
  var rs: RunningStat
  rs.push s

  # compute threshold values (lower and upper)
  let mean = rs.mean
  let stdS = rs.standardDeviationS
  let thrMin = mean - stdS * cfg.outlierThr
  let thrMax = mean + stdS * cfg.outlierThr

  let outliersLow = s.filterIt(it < thrMin).len
  let outliersHih = s.filterIt(it > thrMax).len

  result = MetricStats(raw: s,
                       min: rs.min, max: rs.max,
                       mean: mean, median: s.median,
                       stdS: stdS,
                       outlierThr: cfg.outlierThr,
                       thrMin: thrMin, thrMax: thrMax,
                       outliers: outliersLow + outliersHih)

func removeSuffix(x, suf: string): string =
  # out-of-place version of `removeSuffix`
  result = x
  result.removeSuffix(suf)

proc parseDateCommit(fname: string): (Time, string) =
  ## NOTE: The commit is matched via `$+.`. This means for raw input
  ## files it will include the `_raw` portion. We need to remove that
  const validaStr = "valida-$i-$i-$i-$i-$i-$+.csv"
  # bizarrely, if we leave out the tuple type here the compiler deduces all
  # elements to be of type `Hash`?!
  let (success,
       month, day, year,
       hour, minute,
       hash): (bool, int, int, int, int, int, string) = scanTuple(fname.extractFilename, validaStr)
  if not success:
    raiseAssert "Could not parse date and commit hash from input file: " & $fname
  result = (initDateTime(day.Monthdayrange, month.Month, year,
                         hour.HourRange, minute.MinuteRange, 00, 00, utc()).toTime(),
            hash.removeSuffix("_raw"))


import ./utils
proc extractInfoFromName(name: string, date: Time, commit: string): Benchmark =
  let isSerial = ("Serial" in name) or not ("Parallel" in name)
  ## NOTE: Assumption (!) the programming language is in "field" index 1, i.e. `Valida <ProgLang> ...`
  let fields = name.split()
  let lang = parseEnum[ProgrammingLanguage](fields[1])

  # try to match the benchmark kind
  proc getBenchKind(name: string): (BenchmarkKind, int) =
    staticFor el, strVal, BenchmarkKind:
      const str = "$*" & strVal
      when el != bkUnknown: # so that we return `bkUnknown` by default
        let (success, _, x) = scanTuple(name, str) # `_` to skip things matched by `$*`
        if success:
          return (el, x)
  let (bench, size) = getBenchKind(name)

  result = Benchmark(name: name,
                     isSerial: isSerial,
                     lang: lang,
                     bench: bench,
                     size: size,
                     date: date.format("YYYY-MM-dd HH:mm"),
                     commit: commit)

proc processRawData(cfg: Config, fname: string): BenchTable =
  let rawDf = readCsv(fname)

  # 1. parse date and commit
  let (date, commit) = parseDateCommit(fname)

  info &"Processing raw data of commit: {commit} benched on {date}"

  # iterate over groups of all tests
  for (tup, subDf) in groups(rawDf.group_by(cfg.testCol)):
    # 1. compute aggregate statistics
    let name = tup[0][1].toStr
    info &"Computing aggregate data for: {name}"
    let rTimes = cfg.aggregate subDf[cfg.rTimeCol, float].toSeq1D
    let uTimes = cfg.aggregate subDf[cfg.uTimeCol, float].toSeq1D
    let mem    = cfg.aggregate subDf[cfg.memCol, float].toSeq1D

    var bench = extractInfoFromName(name, date, commit)
    bench.rTimes = rTimes
    bench.uTimes = uTimes
    bench.mem = mem

    result[name] = bench

    if cfg.logVerbosity == lvVerbose:
      info(&"Aggregate metric for:\n{(% bench).pretty()}")

    # check for outliers and warn if any found
    proc warnOutliers(name, metric: string, outliers: int) =
      if outliers > 0:
        warn(&"Outliers detected in {name} for metric {metric}: {outliers}")

    warnOutliers(name, cfg.rTimeCol, rTimes.outliers)
    warnOutliers(name, cfg.uTimeCol, uTimes.outliers)
    warnOutliers(name, cfg.memCol, mem.outliers)

proc parseTraceSizes(benchTab: var BenchTable, traceSizes: string) =
  ## Parses the CSV file containing trace sizes and stores the
  ## sizes in the `benchTab` for each program.
  let df = readCsv(traceSizes)
  if df.len != benchTab.len:
    raise newException(ValueError, "The CSV file containing the trace sizes: " & $traceSizes &
      "contains " & $df.len & " programs, but the data file contains: " & $benchTab.len)

  # iterate over all rows of the DF and fill trace sizes
  for row in df:
    let name       = row[TraceName].toStr
    let mainTrace  = row[MainTrace].toInt
    let permTrace  = row[PermTrace].toInt
    let totalTrace = row[MPTrace].toInt

    if name notin benchTab:
      # NOTE: if the trace information has _more_ programs we could just skip it. But as this
      # should not really happen, we prefer to error
      raise newException(KeyError, "The CSV file containing the trace sizes: " & $traceSizes &
        "contains a benchmark " & name & ", which is not contained in the data file.")

    # update the traces in the bench tab
    var bench = benchTab[name]
    bench.mainTrace  = mainTrace
    bench.permTrace  = permTrace
    bench.totalTrace = totalTrace
    benchTab[name] = bench

proc benchToDf(name: string, bench: Benchmark): DataFrame =
  ## Returns a single row DF containing the data of the given benchmark
  ## Of the `MetricStats` currently only the mean and sample standard
  ## deviation are included
  const fieldMap = {
    "name" : ProgramCol,
    "date" : DateCol,
    "mainTrace" : MainTrace,
    "permTrace" : PermTrace,
    "totalTrace" : MPTrace,
    "rTimes" : RTimeCol,
    "uTimes" : UTimeCol,
    "mem" : MemoryCol
  }.toTable
  result = newDataFrame()
  for field, val in fieldPairs(bench):
    var col = if field in fieldMap: fieldMap[field] else: field
    when typeof(val) is enum: # for enums use the string representation
      result[col] = @[$val]
    elif typeof(val) is MetricStats: # for stats of a metric need more than one field
      # 1. the main value (mean)
      result[col] = @[val.mean]
      # 2. sample standard deviation
      result[getErrorColumn(col)] = @[val.stdS]
    else: # else use as is
      result[col] = @[val]

proc assembleDf(benchTab: BenchTable): DataFrame =
  ## Assembles a DataFrame from the `BenchTable`.
  var dfs = newSeq[DataFrame]()
  for (name, bench) in pairs(benchTab):
    # create a mini DF
    dfs.add benchToDf(name, bench)
  result = assignStack(dfs)

proc producePlots(cfg: Config, df: DataFrame) =
  ## Produces plots from the benchmark results of the raw data files.
  # now filter to everything we _can_ sensibly plot, i.e. filter out
  # by benchmark programs.
  # Only recursive and iterative Fibonacci use useful at the moment
  let dfP = df.filter(f{`bench` in [$bkIterFibonacci, $bkRecursiveFibonacci]})
  # split by serial and parallel versions
  for (tup, subDf) in groups(dfP.group_by("isSerial")):

    let suffix = "_isSerial_" & tup[0][1].toStr

    cfg.individualPlots(subDf, suffix)

    # 2. Facet plots
    cfg.facetPlots(df, suffix)

    # 3. Individual plots and facet plots of all traces
    cfg.plotFitAllTraces(df, UTimeCol, suffix)
    cfg.plotFitAllTraces(df, RTimeCol, suffix)
    cfg.plotFitAllTraces(df, MemoryCol, suffix)

proc produceComparison(cfg: Config, bench1, bench2: BenchTable) =
  ## Produces a comparison of the two input data files.
  ## Highlights performance regressions and improvements.
  raiseAssert "Not implemented yet."


proc main(cfg: Config) =
  if cfg.raw:
    var benchTab = cfg.processRawData(cfg.fname)
    benchTab.parseTraceSizes(cfg.traceSizes)

    # For the plots we need to assemble a DF from the `benchTab`
    var df = assembleDf(benchTab)

    echo benchTab
    if cfg.compare.len > 0:
      # also read the comparison file
      let benchCTab = cfg.processRawData(cfg.fname)

      # produce a performance comparison report. We'll highlight performance *regressions* as well as
      # improvements.
      cfg.produceComparison(benchTab, benchCTab)

      # to also plot these, add them to the DF. They will differ by their `DateCol`
      df.add assembleDf(benchCTab)

    cfg.producePlots(df)
  else:

    # 0. read the CSV file
    let df = readCsv(cfg.fname)

    # 1. Start with individual plots
    cfg.individualPlots(df)

    # 2. Facet plots
    cfg.facetPlots(df)

    # 3. Individual plots and facet plots of all traces
    cfg.plotFitAllTraces(df, UTimeCol)
    cfg.plotFitAllTraces(df, RTimeCol)
    cfg.plotFitAllTraces(df, MemoryCol)

when isMainModule:
  import cligen
  const ConfigPath = "bench_analyzer.config" # can be used to overwrite default config
  include mergeCfgEnvLocal

  const dfl* = initConfig() # set defaults!=default for type
  var app = initFromCL(dfl, cmdName = "benchmark_analyzer", help = {
    "fname" : "Input CSV file with benchmarking results.",
    "fitFunc" : "Function used for fitting.",
    "log10" : "If true all plots will be log10.",
    "logPath" : "Path where the log file is written to. CURRENTLY IGNORED.",
    "plotPath" : "Path wheer the plot files are written to.",
    "raw" : "Indicates if the input is raw data or aggregate.",
    "traceSizes" : "Path to the CSV file containing trace sizes for each benchmark program.",
    })
  app.main # Only --help/--version/parse errors cause early exit
