if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
}
cmdstanr::install_cmdstan(cores = 2, quiet = TRUE)
