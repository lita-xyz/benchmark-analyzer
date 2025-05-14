import std / [strformat, sequtils, tables, strutils, logging, os, macros, stats, strscans, times, options, sets, algorithm]
import std / [terminal, colors] # for color printing text
import ggplotnim, mpfit, orgtables

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

# check for `NO_COLOR` environment variable once upon startup.
# Following https://no-color.org/ we disable color if the var
# is defined and has _any_ value.
let noColor = getEnv("NO_COLOR").len > 0

template color(col, msg: untyped): untyped =
  if not noColor:
    ansiForegroundColorCode(col) & msg & ansiResetCode
  else:
    msg

template red(msg: untyped): untyped =
  ## Writes the given text in red using ANSI color codes
  color(colRed, msg)
template green(msg: untyped): untyped =
  ## Writes the given text in green using ANSI color codes
  color(colGreen, msg)
template orange(msg: untyped): untyped =
  ## Writes the given text in orange using ANSI color codes
  color(colOrange, msg)
template cyan(msg: untyped): untyped =
  ## Writes the given text in orange using ANSI color codes
  color(colCyan, msg)
template pink(msg: untyped): untyped =
  ## Writes the given text in pink using ANSI color codes
  color(colDeepPink, msg)

template warnRed(msg: varargs[untyped]): untyped =
  warn(red msg.join())
template warnGreen(msg: varargs[untyped]): untyped =
  warn(green msg.join())

template print(fn, color: untyped, msg: varargs[untyped]): untyped =
  fn color(msg.join())

template smallSep(fn: untyped): untyped =
  fn repeat("-", 80)

template bigSep(fn: untyped): untyped =
  fn ""
  fn cyan(repeat("=", 80))
  fn ""

template bigSep(fn, color: untyped): untyped =
  fn ""
  fn color(repeat("=", 80))
  fn ""


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
    ffSquare = "square"
    ffExp = "exp"

  LogFields = enum
    lfProcessing, lfWarnings, lfPerformance, lfFitting, lfDetailedMetrics

  LogVerbosity = enum
    lvManual, ## use if want to manually pass `logFields`
    lvNoFits, ## log everything but fit data
    lvVerbose, ## log (almost) everything
    lvDebug ## log everything including detailed metric stats

  ## Context object storing the user settings
  Config = object
    fname: string ## Path to the CSV file containing benchmark results
    plotPath: string = "/tmp"
    logPath: string = "./logs"
    log10: bool = false
    fitFunc: FitFunction = ffLogPlusLin
    raw: bool = false ## Indicates input is raw data
    logVerbosity: LogVerbosity = lvVerbose
    logFields: set[LogFields] ## Which data to log, by default all but detailed metrics
    traceSizes: seq[string] = @["resources/trace_sizes.csv"] ## Path to the file containing trace sizes for each benchmark. [main file, comparison file]

    # Parameters of the data files
    testCol: string = TestNameCol
    iterCol: string = IterCol
    rTimeCol: string = RTimeCol
    uTimeCol: string = UTimeCol
    memCol: string = MemoryCol

    # Stats config
    outlierThr: float = 3.0 ## threshold in standard deviations a point needs to be away from mean to count as an outlier
                            ## i.e. `3.0` implies a point needs to be 3σ away from the mean.
    # XXX: set numbers less strict. this is for testing!
    regressionThr: float = 1.01
    optimizationThr: float = 0.99

    # Comparison config
    compare: string ## Another input file. If given, will produce a report comparing performance to the main `fname` input

    # Fitting related
    maxFuncEvals: int = 1000 ## Maximum number of function evaluations to perform before considering fit a failure

  Model = object
    fn: FuncProto[float]
    numParams: int ## Number of parameters the function takes
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

proc square(p: seq[float], x: float): float =
  result = p[0] * x * x + p[1] * x + p[2]

proc exp(p: seq[float], x: float): float =
  result = p[0] * exp(p[1] * x) + p[2]

proc getFitFunction(fitFn: FitFunction): Model =
  case fitFn
  of ffLogPlusLin: Model(fn: logPlusLin, body: getBody(logPlusLin), numParams: 3)
  of ffLinear: Model(fn: linear, body: getBody(linear), numParams: 2)
  of ffSquare: Model(fn: square, body: getBody(square), numParams: 3)
  of ffExp: Model(fn: exp, body: getBody(exp), numParams: 3)

proc fitConfig(cfg: Config): MpConfig =
  ## Default configuration we use for fitting. Mostly default, but we restrict
  ## the maximum number of iterations so that a run cannot loop infinitely anymore.
  result = MpConfig(maxfev: cfg.maxFuncEvals)

proc fitData(cfg: Config, m: Model, xs, ys, ey: seq[float],
             class: tuple[vm, trace: string], yCol: string): FitResult =
  ## Perform a fit of `fitFn` given the data.
  let params = (0 ..< m.numParams).toSeq().mapIt(1.0) # set all parameters to 1 initally
  let (pRes, res) = fit(m.fn, params,
                        x = xs,
                        y = ys,
                        ey = ey, # error is sample standard deviation
                        config = some(cfg.fitConfig()))
  let resText = pretty(pRes, res)

  if lfWarnings in cfg.logFields and res.status == MP_MAXITER:
    warnRed(&"Fit did not converge before reaching max function evaluations in {class} of {yCol}!")

  if lfFitting in cfg.logFields:
    info("Fit result for fit function:")
    info(&"\t`{m.body}`")
    smallSep(info)
    info(resText)
    smallSep(info)

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

proc getErrorColumn(s: string): string = s & StdSuffix

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
      let df = df.mutate(f{"yMin" ~ idx(yCol) - idx(getErrorColumn(yCol))},
                         f{"yMax" ~ idx(yCol) + idx(getErrorColumn(yCol))})

      ggplot(df, aes("Value", yCol, color = DateCol)) +
        facet_wrap("Type", scales = "free") +
        facetMargin(0.5) +
        margin(right = 2.0, bottom = 1.5) +
        xlab(rotate = -30.0, alignTo = "right") +
        geom_point() +
        geom_errorbar(aes = aes(yMin = "yMin", yMax = "yMax")) +
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
    if lfFitting in cfg.logFields:
      bigSep(info)
      info(&"Fitting data for: {ver} -- {tr} against {yCol}")
    let fitRes = cfg.fitData(fn, subDf["Value", float].toSeq1D,
                             subDf[yCol, float].toSeq1D,
                             subDf[getErrorColumn(yCol), float].toSeq1D,
                             class, yCol)
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
      ggsave(&"{cfg.plotPath}/{trStr}_{verStr}_{yCol}_with_fit{suffix}.pdf")

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

  if lfProcessing in cfg.logFields:
    bigSep(notice)
    notice.print(pink, &"Start of raw data processing for {fname}")
    bigSep(notice)

    notice(&"Processing raw data of commit: {commit} benched on {date}")

  # iterate over groups of all tests
  for (tup, subDf) in groups(rawDf.group_by(cfg.testCol)):
    # 1. compute aggregate statistics
    let name = tup[0][1].toStr
    if lfProcessing in cfg.logFields:
      notice(&"Computing aggregate data for: {name}")
    let rTimes = cfg.aggregate subDf[cfg.rTimeCol, float].toSeq1D
    let uTimes = cfg.aggregate subDf[cfg.uTimeCol, float].toSeq1D
    let mem    = cfg.aggregate subDf[cfg.memCol, float].toSeq1D

    var bench = extractInfoFromName(name, date, commit)
    bench.rTimes = rTimes
    bench.uTimes = uTimes
    bench.mem = mem

    result[name] = bench

    if lfDetailedMetrics in cfg.logFields:
      notice(&"Aggregate metric for:\n{(% bench).pretty()}")

      # check for outliers and warn if any found
      proc warnOutliers(name, metric: string, outliers: int) =
        if outliers > 0:
          warnRed(&"Outliers detected in {name} for metric {metric}: {outliers}")

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

proc benchToDf(cfg: Config, name: string, bench: Benchmark): DataFrame =
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
      if val.raw.len > 1:
        result[getErrorColumn(col)] = @[val.stdS]
      else:
        if lfWarnings in cfg.logFields:
          warnRed(&"{name} only has one sample. Setting errors to 1")
        result[getErrorColumn(col)] = @[1.0]
    else: # else use as is
      result[col] = @[val]

proc assembleDf(cfg: Config, benchTab: BenchTable): DataFrame =
  ## Assembles a DataFrame from the `BenchTable`.
  var dfs = newSeq[DataFrame]()
  for (name, bench) in pairs(benchTab):
    # create a mini DF
    dfs.add cfg.benchToDf(name, bench)
  result = assignStack(dfs)

proc producePlots(cfg: Config, df: DataFrame) =
  ## Produces plots from the benchmark results of the raw data files.
  # now filter to everything we _can_ sensibly plot, i.e. filter out
  # by benchmark programs.
  # Only recursive and iterative Fibonacci use useful at the moment

  if lfFitting in cfg.logFields:
    bigSep(info)
    info.print(pink, "Start of numerical analysis of benchmarks against trace size")
    bigSep(info)

  let dfP = df.filter(f{`bench` in [$bkIterFibonacci, $bkRecursiveFibonacci]})
  # split by serial and parallel versions
  for (tup, subDf) in groups(dfP.group_by("isSerial")):
    let suffix = "_isSerial_" & tup[0][1].toStr

    ## XXX: for individual plots split by date / commit!
    cfg.individualPlots(subDf, suffix)

    # 2. Facet plots
    cfg.facetPlots(subDf, suffix)

    # 3. Individual plots and facet plots of all traces
    cfg.plotFitAllTraces(subDf, UTimeCol, suffix)
    cfg.plotFitAllTraces(subDf, RTimeCol, suffix)
    cfg.plotFitAllTraces(subDf, MemoryCol, suffix)

# First, add this import to benchmark_analyzer.nim
# Define a structure to hold comparison data
type
  ComparisonEntry = tuple
    benchmark: string    # Benchmark name
    mtric: string        # Metric type (RTime, UTime, Memory)
    newVal: float        # Main file value (usually the new value)
    oldVal: float        # Comparison file value (usually the old value)
    diff: float          # Raw difference
    ratio: float         # Ratio of oldVal/newVal
    percChange: float      # Percentage change
    isRegression: bool   # Whether it's a regression

# Add this function to produce the comparison table
proc generateComparisonTable(entries: seq[ComparisonEntry]): string =
  # Sort entries: regressions first, then by percentage difference magnitude
  var sortedEntries = entries.sortedByIt(it.ratio).reversed

  # Generate the table
  result = "** Performance Comparison Summary\n\n"
  result.add toOrgHeader(ComparisonEntry)
  for entry in sortedEntries:
    result.add toOrgLine(entry, 6)

proc formatPerformanceChange(oldVal, newVal: float): string =
  let ratio = newVal / oldVal
  # For time/memory metrics, lower is better
  if ratio < 1.0:  # Improvement
    if ratio <= 0.1:  # More than 10x better
      result = fmt"{1/ratio:.1f}x better"
    elif ratio <= 0.5:  # 2-10x better
      result = fmt"{1/ratio:.1f}x better"
    else:  # Less than 2x better
      result = fmt"{(1.0-ratio)*100:.1f}% better"
  else:  # Regression
    if ratio >= 10.0:  # More than 10x worse
      result = fmt"{ratio:.1f}x worse"
    elif ratio >= 2.0:  # 2-10x worse
      result = fmt"{ratio:.1f}x worse"
    else:  # Less than 2x worse
      result = fmt"{(ratio-1.0)*100:.1f}% worse"

proc produceComparison(cfg: Config, bench1, bench2: BenchTable) =
  ## Produces a comparison of the two input data files.
  ## Highlights performance regressions and improvements.

  # 1. given that we have the bench table, iterate over all benches
  # and compare each benchmark by:
  # - median
  # - mean
  # - outliers
  # - σ
  # 2. for the moment (!) error if they don't agree in all the
  # benchmarks. This is to make sure we decide on how to handle
  # this case in the future. Could just log a warning?

  # NOTE: We don't just exclude the whole logic if `lfPerformance` is not in
  # the fields we log, because some warnings need to be printed if `lfWarnings`
  # is given. Unlikely to use one or the other, but might be useful.

  # get a random element for `date` and `commit` fields
  let b1 = bench1[bench1.keys().toSeq()[0]]
  let b2 = bench2[bench2.keys().toSeq()[0]]

  if lfPerformance in cfg.logFields:
    bigSep(notice)
    notice.print(pink, "Start of performance regression / improvement analysis")
    notice.print(pink, &"Between {b1.date} - {b1.commit} (main) and {b2.date} - {b2.commit} (comparison)")
    bigSep(notice)

  if bench1.len != bench2.len and lfWarnings in cfg.logFields:
    warnRed(&"There are {bench1.len} benchmarks in the main input file and " &
      &"{bench2.len} in the comparison file!")

  # Collection to store comparison entries for the table
  var comparisonEntries: seq[ComparisonEntry]

  for bench in keys(bench1):
    if bench notin bench2 and lfWarnings in cfg.logFields:
      warn(&"The benchmark {bench} does *not* exist in the comparison file!")
    let b1 = bench1[bench]
    let b2 = bench2[bench]

    # log what we are comparing
    if lfPerformance in cfg.logFields:
      smallSep(notice)
      notice.print(orange, &"Comparison of {bench}")

    # just some basic sanity checks that we are actually looking at the
    # same benchmark
    template assertField(field: untyped{ident}): untyped =
      let b1f = b1.field
      let b2f = b2.field
      doAssert b1f == b2f, astToStr(field) & " does not agree between the two benchmarks! " &
        "Main file input: & " & $b1f & ", comparison file: " & $b2f
    assertField(isSerial)
    assertField(lang)
    assertField(bench)
    assertField(size)

    # now compare the trace sizes. At the moment this can't really happen,
    # because we don't allow for separate trace inputs, but that will change
    # soon!
    # NOTE: Due to using a very generic approach to reduce code duplication
    # the warning messages may read a bit weird for certain metrics.
    template warnIfDifferent(field: untyped): untyped =
      if b1.field != b2.field:
        # NOTE: can't use `&` macro in second line. Cannot access the field
        # inside the `{}` using the `field` argument here...
        warnRed(astToStr(field) & " differs between the two benchmarks! " &
          "Main file input: " & $b1.field & ", comparison file: " & $b2.field)
    if lfWarnings in cfg.logFields:
      warnIfDifferent(mainTrace)
      warnIfDifferent(permTrace)
      warnIfDifferent(totalTrace)

    template compareMetricField(ms1, ms2, metricField, metric: untyped): untyped =
      let m1 = ms1.metricField
      let m2 = ms2.metricField
      let mf = $astToStr(metricField)

      let ratio = m2.float / m1.float
      let perc = formatPerformanceChange(m2.float, m1.float)
      let percChange = (ratio - 1) * 100

      # Add to our comparison entries if the difference is significant enough
      var tup = (
          benchmark: bench,
          mtric: metric & "." & mf,
          newVal: m1.float,
          oldVal: m2.float,
          diff: abs(m1 - m2).float,
          ratio: ratio,
          percChange: percChange,
          isRegression: false,
      )

      when typeof(m1) is float:
        if m1 > m2 * cfg.regressionThr: # regression detected!
          warnRed(&"Metric: `" & mf & "` of `" & metric & "`: New is " & perc & "!")
          tup.isRegression = true
          comparisonEntries.add tup
        elif m1 < m2 * cfg.optimizationThr: # optimization detected!
          warnGreen(&"Metric: `" & mf & "` of `" & metric & "`: New is " & perc & "!")
          comparisonEntries.add tup
      elif typeof(m1) is int:
        # compare only the number
        if m1 < m2: # less of it
          warnRed("Metric: `" & mf & "` of `" & metric & "`: Less in new. New: " & $m1 & ", old: " & $m2)
          comparisonEntries.add tup
        elif m1 > m2: # more of it
          warnGreen("Metric: `" & mf & "` of `" & metric & "`: More in new. New: " & $m1 & ", old: " & $m2)
          tup.isRegression = true
          comparisonEntries.add tup
      else:
        error("Unexpected input type for field " & astToStr(metricField) & " of type: " & $typeof(m1))

    template compareMetric(field: untyped): untyped =
      compareMetricField(b1.field, b2.field, mean, astToStr(field))
      compareMetricField(b1.field, b2.field, median, astToStr(field))
      ## XXX: optionally show sample standard deviation?
      # compareMetricField(b1.field, b2.field, stdS, astToStr(field))
      compareMetricField(b1.field, b2.field, outliers, astToStr(field))

    if lfPerformance in cfg.logFields:
      compareMetric(rTimes)
      compareMetric(uTimes)
      compareMetric(mem)

  # Generate and output the performance comparison table
  if comparisonEntries.len > 0:
    bigSep(notice)
    notice.print(cyan, "Performance Comparison Summary Table")
    let table = generateComparisonTable(comparisonEntries).formatOrgTable()
    notice("\n" & table)
  else:
    notice("No significant performance differences found.")

  # check for benches that exist in `bench2` but not in `bench1`
  let onlyIn2 = bench2.keys.toSeq.toSet - bench1.keys.toSeq.toSet
  if onlyIn2.len > 0 and lfWarnings in cfg.logFields:
    bigSep(notice)
    warn.print(pink, "List of benchmarks only contained in secondary input file.")
    bigSep(notice)
  for el in onlyIn2:
    warn(&"The benchmark {el} *only* exists in the comparison file!")

proc main(cfg: Config) =
  if cfg.raw:
    var benchTab = cfg.processRawData(cfg.fname)
    doAssert cfg.traceSizes.len >= 1, "At least one path to a CSV file with trace sizes must be supplied."
    benchTab.parseTraceSizes(cfg.traceSizes[0])

    # For the plots we need to assemble a DF from the `benchTab`
    var df = cfg.assembleDf(benchTab)

    if cfg.compare.len > 0:
      # also read the comparison file
      var benchCTab = cfg.processRawData(cfg.compare)
      let trS = if cfg.traceSizes.len > 1: cfg.traceSizes[1] else: cfg.traceSizes[0]
      benchCTab.parseTraceSizes(trS)
      # produce a performance comparison report. We'll highlight performance *regressions* as well as
      # improvements.
      cfg.produceComparison(benchTab, benchCTab)

      # to also plot these, add them to the DF. They will differ by their `DateCol`
      df.add cfg.assembleDf(benchCTab)

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

  proc setLogFields(cfg: var Config) =
    ## Set the fields we log based on the `logVerbosity`, if given
    case cfg.logVerbosity:
    of lvManual: return
    of lvNoFits: cfg.logFields = {lfProcessing, lfWarnings, lfPerformance}
    of lvVerbose: cfg.logFields = {lfProcessing, lfWarnings, lfPerformance, lfFitting}
    of lvDebug: cfg.logFields = {lfProcessing, lfWarnings, lfPerformance, lfFitting, lfDetailedMetrics}

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
    "traceSizes" : """Paths to the CSV files containing trace sizes for each benchmark program.
First argument is for the main input and the second for the comparison input (if any). If only one
argument given, will use the first input for both.
    "maxFuncEvals" : "Maximum number of function evaluations to perform in fits before considering it a failure.",
""",
    })
  app.setLogFields()
  app.main # Only --help/--version/parse errors cause early exit
