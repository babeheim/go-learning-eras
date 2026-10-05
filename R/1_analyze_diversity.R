
tic("plot openings")
source("R/plot_openings.R")
toc(log = TRUE)

tic("plot opening trees")
source("R/plot_opening_trees.R")
toc(log = TRUE)

tic("plot database coverage")
source("R/plot_database_coverage.R")
toc(log = TRUE)

tic("analyze opening diversity")
source("R/analyze_opening_diversity.R")
toc(log = TRUE)

tic("analyze opening diversity")
source("R/analyze_opening_diversity_CN.R")
toc(log = TRUE)

tic("analyze opening diversity")
source("R/analyze_opening_diversity_JP.R")
toc(log = TRUE)

tic("analyze opening diversity")
source("R/analyze_opening_diversity_KR.R")
toc(log = TRUE)

tic("calc game distances")
source("R/calc_game_distances.R")
toc(log = TRUE)

tic("analyze speed evolution")
source("R/analyze_speed_evolution.R")
toc(log = TRUE)

tic("calc match networks")
source("R/calc_match_networks.R")
toc(log = TRUE)
