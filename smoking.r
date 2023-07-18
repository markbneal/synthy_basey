# replicate smoking study
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
library(data.table)
library(dplyr)

## Test setup of cmdstan ####
# From https://mc-stan.org/cmdstanr/articles/cmdstanr.html
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

# install_cmdstan(cores = 2)
# CmdStan path set to: /home/nealm@dexcel.co.nz/.cmdstan/cmdstan-2.32.2

cmdstan_path()
cmdstan_version()
file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()

mod$exe_file()
# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit$summary()
fit$summary(variables = c("theta", "lp__"), "mean", "sd")
# use a formula to summarize arbitrary functions, e.g. Pr(theta <= 0.5)
fit$summary("theta", pr_lt_half = ~ mean(. <= 0.5))


# default is a 3-D draws_array object from the posterior package
# iterations x chains x variables
draws_arr <- fit$draws() # or format="array"
str(draws_arr)

## Return to example ####

load("synth/smoking.rda")
load("smoking.rda")

# stan_file <- "synth_penalized.stan"
stan_file <- "synth_horseshoe_b_tau_x.stan"
mod_synth <- cmdstan_model(stan_file)

smoking <- data.table(smoking)
smoking_y <- dcast(smoking, year ~ state, value.var = "cigsale")
summaries <- smoking %>%
  group_by(state) %>%
  summarize(retprice = mean(retprice, na.rm = TRUE),
            lnincome = mean(lnincome, na.rm = TRUE),
            age15to24 = mean(age15to24, na.rm = TRUE),
            beer = mean(beer, na.rm = TRUE))

sm1975 <- data.frame(smoking[year == 1975, "cigsale"],
                     smoking[year == 1975, "state"])
sm1980 <- data.frame(smoking[year == 1980, "cigsale"],
                     smoking[year == 1980, "state"])
sm1988 <- data.frame(smoking[year == 1988, "cigsale"],
                     smoking[year == 1988, "state"])
sm1975 <- rename(sm1975, cigsale1975 = cigsale)
sm1980 <- rename(sm1980, cigsale1980 = cigsale)
sm1988 <- rename(sm1988, cigsale1988 = cigsale)

summaries <- inner_join(summaries, sm1975, by = "state")
summaries <- inner_join(summaries, sm1980, by = "state")
summaries <- inner_join(summaries, sm1988, by = "state")

predictors_target <- t(as.matrix(summaries[summaries$state == "California", 2:8]))
predictors_control <- t(as.matrix(summaries[summaries$state != "California", 2:8]))
X_pred <- cbind(predictors_target, predictors_control)

target <- "California"
target_index <- which(names(smoking_y) == target)
other_index <- which(names(smoking_y) %in% names(smoking_y)[c(-1, -4)])

which(smoking_y$year == 1988)


X_pred[3, ] <- log(X_pred[3, ] / (1 - X_pred[3, ]))

stan_data <- list(
  T = nrow(smoking_y),
  J = ncol(smoking_y) - 1,
  L = 8,
  P = nrow(X_pred),
  X = X_pred,
  Y = transpose(smoking_y[ , c(..target_index, ..other_index)]),
  trt_times = nrow(smoking_y) - which(smoking_y$year == 1988)
)

fit_synth <- mod_synth$sample(data = stan_data,
                              seed = 123123,
                              iter_warmup = 500,
                              iter_sampling = 500,
                              init = 0.1,
                              chains = 4,
                              parallel_chains = 4,
                              max_treedepth = 13,
                              adapt_delta = 0.8
)
