
rm(list = ls())

source("project_support.R")

tic("plot openings")
source("R_scripts/plot_openings.R")
toc(log = TRUE)

tic("plot opening trees")
source("R_scripts/plot_opening_trees.R")
toc(log = TRUE)

tic("plot database coverage")
source("R_scripts/plot_database_coverage.R")
toc(log = TRUE)

tic("calculate game distances")
source("R_scripts/calc_game_distances.R")
toc(log = TRUE)

tic("calculate match networks")
source("R_scripts/calc_match_networks.R")
toc(log = TRUE)

tic("analyze opening diversity")
source("R_scripts/analyze_opening_diversity.R")
toc(log = TRUE)

tic("analyze speed evolution")
source("R_scripts/analyze_speed_evolution.R")
toc(log = TRUE)
