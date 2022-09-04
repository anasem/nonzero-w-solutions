# Source dependencies 
message("Sourcing dependencies")

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(lmodel2)
library(lubridate)
library(latex2exp)
library(patchwork)

# Options ----------------------------------------------------------------------
data_dir        <- "../data"
project_dir     <- "."

plot_output_dir <- file.path(data_dir, "figures")


# External scripts -------------------------------------------------------------

source(file.path(project_dir, "02-theme.R"))

results_file <- file.path(data_dir, 'rds/nonzero_w_sim_results.rds')

