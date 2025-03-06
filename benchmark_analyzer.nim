import std / [strformat, sequtils, tables, strutils, logging, os, macros, stats, strscans, times, options]
import ggplotnim, mpfit

import std / json # for easier printing of data to log files

from seqmath import median

const VMver = "VM version"
const Program = "Program"
const MainTrace = "Main trace size (field elements)"
const PermTrace = "Permutation trace size (extension field elements)"
const MPTrace = "Main + permutation trace size (field elements)"
#const UTime = "User time (s)"
#const RTime = "Real time (s)"
const Space = "Max space (kB)"


const TestName = "Test Name" ## column containing name of the bench in the raw files
const Iter = "Iteration" ## column containing the iteration index
const RTime = "Real Time" ## column containing the real time the program ran
const UTime = "User Time" ## column containing the real time the program ran
const Memory = "Memory (MB)" ## column containing used memory by the program

## XXX: Make the log file dependent on the input file to map to that?
const LogFile {.strdefine.} = "./logs/benchmark_analyzer.log"

macro getBody(fn: typed): untyped =
  let fnImpl = fn.getImpl
  result = newLit(fnImpl.body.repr)

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

    # Parameters of the data files
    testCol: string = TestName
    iterCol: string = Iter
    rTimeCol: string = RTime
    uTimeCol: string = UTime
    memCol: string = Memory

    # Stats config
    outlierThr: float = 3.0 ## threshold in standard deviations a point needs to be away from mean to count as an outlier
                            ## i.e. `3.0` implies a point needs to be 3σ away from the mean.

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
    isSerial: bool ## Whether the bench is a serial bench
    lang: ProgrammingLanguage ## Language the bench is written in
    bench: BenchmarkKind ## Set of *known* benchmark kinds. If it is known we can merge multiple different
                             ## benchmarks of the same kind with different sizes
    size:      int ## Some benchmarks include size information in their name, e.g. `Fib6`, `Keccak500` etc
    traceSize: int ## Size of the main trace + permutation trace of this program
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

proc logPlusLin(p: seq[float], x: float): float =
  result = p[0] * x * ln(x) + p[1] * x + p[2]

proc linear(p: seq[float], x: float): float =
  result = p[0] * x + p[1]

proc getFitFunction(fitFn: FitFunction): Model =
  case fitFn
  of ffLogPlusLin: Model(fn: logPlusLin, body: getBody(logPlusLin))
  of ffLinear: Model(fn: linear, body: getBody(linear))

proc calcError(x: float): float =
  ## TODO: choose sensible errors! Need some data for different
  ## runs to do so.
  result = x * 0.03

proc calcErrors(ys: seq[float]): seq[float] =
  ## TODO: choose sensible errors! Need some data for different
  ## runs to do so.
  result = ys.mapIt(calcError it)

template separator(): string = repeat('-', 30)

proc fitData(m: Model, xs, ys: seq[float]): FitResult =
  ## Perform a fit of `fitFn` given the data.
  let params = @[1.0, 1.0, 1.0]
  let (pRes, res) = fit(m.fn, params,
                        x = xs,
                        y = ys,
                        ey = calcErrors(ys)) # for now just static 3% errors
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


proc individualPlots(cfg: Config, df: DataFrame) =
  ## Creates the individual plots of the different traces
  ## split by the different VM versions.

  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  proc plotAgainst(yCol, ySuf: string) =
    ggplot(df, aes(MainTrace, yCol, color = VMver)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_trace_{ySuf}.pdf")
    ggplot(df, aes(PermTrace, yCol, color = VMver)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/permutation_trace_{ySuf}.pdf")
    ggplot(df, aes(MPTrace, yCol, color = VMver)) +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_permutation_trace_{ySuf}.pdf")

  plotAgainst(UTime, "user_time")
  plotAgainst(RTime, "real_time")
  plotAgainst(Space, "space")

proc facetPlots(cfg: Config, df: DataFrame) =
  ## Create facet plots of the different plots
  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  # 1. facet by type for one trace. One pane space, user time, real time
  block:
    let df = df.gather([UTime, RTime, Space], "Type", "Value")
    ggplot(df, aes(MainTrace, "Value", color = VMver)) +
      facet_wrap("Type", scales = "free") +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_trace_facet.pdf")
    ggplot(df, aes(PermTrace, "Value", color = VMver)) +
      facet_wrap("Type", scales = "free") +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/perm_trace_facet.pdf")
    ggplot(df, aes(MPTrace, "Value", color = VMver)) +
      facet_wrap("Type", scales = "free") +
      geom_point() +
      xScale + yScale +
      ggsave(&"{cfg.plotPath}/main_perm_trace_facet.pdf")

  # 2. Facet of all traces against single time / space variable
  block:
    let df = df.gather([MainTrace, PermTrace, MPTrace], "Type", "Value")
    proc plotAgainst(yCol, ySuf: string) =
      ggplot(df, aes("Value", yCol, color = VMver)) +
        facet_wrap("Type", scales = "free") +
        facetMargin(0.5) +
        margin(right = 2.0, bottom = 1.5) +
        xlab(rotate = -30.0, alignTo = "right") +
        geom_point() +
        legendPosition(0.5, 0.2) +
        xScale + yScale +
        ggsave(&"{cfg.plotPath}/traces_{ySuf}_facet.pdf")

    plotAgainst(UTime, "user_time")
    plotAgainst(RTime, "real_time")
    plotAgainst(Space, "space")


proc assembleDf(tab: Table[Class, FitResult]): DataFrame =
  ## Assembles all individual fitting result DFs to a single DF
  result = newDataFrame()
  for (tup, fitRes) in pairs(tab):
    let (ver, tr) = tup
    var df = fitRes.df
    df[VMVer] = ver
    df["Type"] = tr
    result.add df

proc plotFitAllTraces(cfg: Config, df: DataFrame, yCol: string) =
  ## Fits all traces for the given y column `yCol` and creates a plot for
  ## each fit individually, which includes the fit results as an annotation.
  ##
  ## Finally produces a combined facet plot of all traces against the
  ## `y` column and split by the different VM versions.
  let df = df.gather([MainTrace, PermTrace, MPTrace], "Type", "Value")
    .mutate(f{"yMin" ~ idx(yCol) - calcError(idx(yCol))}, # calc error, will become smarter in the future
            f{"yMax" ~ idx(yCol) + calcError(idx(yCol))})

  let xScale = if cfg.log10: scale_x_log10()
               else: scale_x_continuous()
  let yScale = if cfg.log10: scale_y_log10()
               else: scale_y_continuous()

  var fitTab = initTable[Class, FitResult]()
  for (tup, subDf) in groups(df.group_by([VMVer, "Type"])):
    ## Fit each trace & VM version
    let ver = tup[0][1].toStr # corresponds to `VMVer`
    let tr = tup[1][1].toStr  # corresponds to `"Type"`
    let class = (vm: ver, trace: tr)

    let fn = getFitFunction(cfg.fitFunc)
    info(&"Fitting data for: {ver} -- {tr} against {yCol}")
    let fitRes = fn.fitData(subDf["Value", float].toSeq1D,
                            subDf[yCol, float].toSeq1D)
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
      ggtitle(&"{VMVer}: {ver}") +
      ggsave(&"{cfg.plotPath}/{trStr}_{verStr}_with_fit.pdf")

  let dfFits = assembleDf(fitTab)
  # Cmobined plot of all the fits
  ggplot(df, aes("Value", yCol, color = VMVer)) +
    geom_point() +
    geom_errorbar(aes = aes(x = "Value", yMin = "yMin", yMax = "yMax")) +
    geom_line(data = dfFits, aes = aes("xs", "ys", color = VMVer)) +
    facet_wrap("Type", scales = "free") +
    facetMargin(0.525) +
    margin(right = 0.001, bottom = 1.5) +
    xlab(rotate = -30.0, alignTo = "right") +
    legendPosition(0.55, 0.2) +
    xScale + yScale +
    xlab("Trace size") +
    ggsave(&"{cfg.plotPath}/all_traces_{yCol.sanitize()}_with_fit.pdf")


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
proc extractInfoFromName(name: string): Benchmark =
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
                     size: size)

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

    var bench = extractInfoFromName(name)
    bench.rTimes = rTimes
    bench.uTimes = uTimes
    bench.mem = mem

    result[name] = bench

    if cfg.logVerbosity == lvVerbose:
      info(&"Aggregate metric for:\n{(% bench).pretty()}")

proc main(cfg: Config) =

  if cfg.raw:
    let benchTab = cfg.processRawData(cfg.fname)

    echo benchTab
  else:

    # 0. read the CSV file
    let df = readCsv(cfg.fname)

    # 1. Start with individual plots
    cfg.individualPlots(df)

    # 2. Facet plots
    cfg.facetPlots(df)

    # 3. Individual plots and facet plots of all traces
    cfg.plotFitAllTraces(df, UTime)
    cfg.plotFitAllTraces(df, RTime)
    cfg.plotFitAllTraces(df, Space)

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
    })
  app.main # Only --help/--version/parse errors cause early exit
