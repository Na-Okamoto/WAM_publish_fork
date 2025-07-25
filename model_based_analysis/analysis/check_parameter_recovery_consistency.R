library(tidyverse)
library(rio)

#' Compare parameter recovery results from two directories
#'
#' @param old_dir directory containing results from the old script
#' @param new_dir directory containing results from the new script
#' @param tolerance numeric tolerance passed to `all.equal`
#' @return logical indicating if all files matched
compare_parameter_recovery <- function(old_dir, new_dir, tolerance = 1e-8) {
  old_files <- list.files(old_dir, pattern = "_parameter_recovery_iter_\\d+\\.rds$", full.names = TRUE)
  if (length(old_files) == 0) {
    stop(sprintf("No parameter recovery files found in %s", old_dir))
  }
  all_match <- TRUE
  for (old_file in old_files) {
    fname <- basename(old_file)
    new_file <- file.path(new_dir, fname)
    if (!file.exists(new_file)) {
      message("New result missing: ", fname)
      all_match <- FALSE
      next
    }
    old_data <- import(old_file) %>% arrange(PlayerID, iteration)
    new_data <- import(new_file) %>% arrange(PlayerID, iteration)
    if (isTRUE(all.equal(old_data, new_data, tolerance = tolerance))) {
      message(fname, " matched")
    } else {
      message(fname, " differs")
      all_match <- FALSE
    }
  }
  if (all_match) {
    message("All files match")
  } else {
    message("Some files did not match")
  }
  invisible(all_match)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2) {
    stop("Usage: Rscript check_parameter_recovery_consistency.R <old_dir> <new_dir>")
  }
  compare_parameter_recovery(args[1], args[2])
}
