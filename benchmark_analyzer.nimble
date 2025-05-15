# Package

version       = "0.1.6"
author        = "Vindaar"
description   = "Benchmark analysis for Lita / Valida"
license       = "Proprietary"
bin           = @["benchmark_analyzer"]


# Dependencies

requires "nim >= 2.2"
requires "ggplotnim == 0.7.4"
requires "mpfit == 0.2.1"
requires "cligen"
requires "https://github.com/Vindaar/seqmath >= 0.2.2"
requires "orgtables >= 0.1.1"
