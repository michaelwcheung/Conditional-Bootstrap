# method to be run locally for the purposes of testing parallel processing
libraries <- c("DT", "tidyverse", "WeightIt", "sandwich", "lmtest", "boot", "plotly", "promises", "future", "foreach", "doFuture", "progressr", "data.table", "parallel", "future.apply", "stringr", "dplyr", "microbenchmark")
lapply(libraries, require, character.only = TRUE)

# source files
file_names <- c(
  "../helper-functions.R",
  "create_ems.R",
  "boot_fff.R",
  "boot_flff.R",
  "boot_fflf.R",
  "boot_fffl.R",
  "boot_lll.R",
  "boot_flll.R",
  "boot_lfll.R",
  "boot_llfl.R"
)
lapply(file_names, source)

# load in data/parameters
data <- read_csv("~/benmarhnia-lab/data_droughts_malnutrition_101822.csv")
y <- pull(data, stunted)
z <- pull(data, drought)
X <- select(data, -c(stunted, drought))
P <- select(X, education_none, mass_media, rural_residence)
quantiles <- c(0, 50, 100)
B <- 1
estimand <- "rd"
estimation_method <- "lr"
test <- "BH"
alpha <- 0.05

if (estimand == "or") {
    family <- "binomial"
} else if (estimand == "rd") {
    family <- "gaussian"
} else {}

if (estimation_method == "iptw" | estimation_method == "dr") {
    propensity <- ipweights(data, X, z)
} else if (estimation_method == "lr") {
    propensity <- rep(1, length(z))
} else {}

# create ems and clean P
create_ems_output <- create_ems(P)
P <- create_ems_output$P
ems <- create_ems_output$ems

fut_globals <- list(
    data = data, y = y, z = z, X = X, P = P, ems = ems,
    quantiles = quantiles, estimand = estimand, test = test, estimation_method = estimation_method, alpha = alpha, family = family, propensity = propensity,
    bse = bse, cochrans_q_het = cochrans_q_het, ipweights = ipweights, generate_adj_X = generate_adj_X, estimate_ate = estimate_ate, estimate_cate = estimate_cate, dr_estimate = dr_estimate, aiptw = aiptw
)

# bootstrap benchmarking
# microbenchmark(
#   boot_fff(fut_globals),
#   boot_flff(fut_globals),
#   boot_fflf(fut_globals),
#   boot_fffl(fut_globals),
#   boot_lll(fut_globals),
#   boot_flll(fut_globals),
#   boot_lfll(fut_globals),
#   boot_llfl(fut_globals),
#   times = 3
# )

# bootstrap workers benchmarking 
# MBAir has 4 cores; max use 3
# sink(file = "parallel_boots_workers_bootstrap_benchmark.txt")
microbenchmark(
  boot_lll(fut_globals, 1),
  boot_flll(fut_globals, 2, 1),
  boot_flll(fut_globals, 3, 1),
  # boot_flll(fut_globals, 4, 1),
  boot_lll(fut_globals, 10),
  boot_flll(fut_globals, 2, 10),
  boot_flll(fut_globals, 3, 10),
  # boot_flll(fut_globals, 4, 10),
  boot_lll(fut_globals, 100), 
  boot_flll(fut_globals, 2, 100),
  boot_flll(fut_globals, 3, 100),
  # boot_flll(fut_globals, 4, 100),
  # boot_flll(fut_globals, 8),
  times = 10
)
# sink(file = NULL)
