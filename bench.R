

remotes::install_version("CDCPLACES", "1.1.5")
library(CDCPLACES)
library(microbenchmark)

old_benchmark <- bench::bench_time(
get_places("census", state = "MI", measure = "SLEEP")
)


old_benchmark2 <- bench::bench_time(
  get_places("census", state = c("MI", "NY", "OH", "CA"))
)

remotes::install_version("CDCPLACES", "1.1.6")
library(CDCPLACES)
library(bench)

new_benchmark <- bench::bench_time(
  get_places("census", state = "MI", measure = "SLEEP")
)

new_benchmark2 <- bench::bench_time(
  get_places("census", state = c("MI", "NY", "OH", "CA"))
)
