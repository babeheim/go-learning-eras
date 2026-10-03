
library(kaya)        # github.com/babeheim/kaya
library(cmdstanr)
library(posterior)
library(tictoc)
library(yaml)
library(igraph)
library(viridis)
library(dplyr)
library(stringdist)
library(philentropy) # for JSD, Jensen-Shannon Divergence

stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))

machine_name <- Sys.info()[["nodename"]]
project_seed <- 2025

init_project <- FALSE     # wipes figures/, rds/, data/

set.seed(project_seed)
options(warnPartialMatchDollar=TRUE)

files <- list.files("R/functions", full.names = TRUE)
for (i in seq_along(files)) source(files[i])

n_chains <- 4
n_iter <- 1000
adapt_delta <- 0.95
