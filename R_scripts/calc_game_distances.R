
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")

calcs <- c()

n_moves <- 50 # analysis is on patterns in the first 50
n_games_era <- 1000
calcs$nGamesMDSEra <- format(n_games_era, big.mark = ",", trim = TRUE)
calcs$nGamesMDSTotal <- format(n_games_era * nrow(eras), big.mark = ",", trim = TRUE)
calcs$nMovesMDSDepth <- n_moves

if (!file.exists("data/distance_mds.RDS")) {

  set.seed(project_seed)
  hits <- integer()
  for (i in 1:nrow(eras)) {
    tar <- which(games$era == eras$name[i])
    add <- sample(tar, n_games_era)
    hits <- c(hits, add)
  }

  # subsample the games
  games <- games[hits,]

  game_moves <- extract_game_moves(games$opening, n_moves = n_moves, cumulative = TRUE, unicode = TRUE)
  game_moves2 <- extract_game_moves(games$opening, n_moves = n_moves, cumulative = TRUE, unicode = FALSE)
  # n_games x n_moves

  distance_matrix <- stringdistmatrix(game_moves[, n_moves], method = "lv")
  # other options: osa (optimal string alignment) or dl (Damerau-Levenshtein)
  mds_result <- cmdscale(distance_matrix, k=2)
  mds <- data.frame(x = as.numeric(scale(mds_result[,1])), y = as.numeric(scale(mds_result[,2])))
  mds$hash_id <- games$hash_id
  saveRDS(mds, file = "data/distance_mds.RDS")

}

write_yaml(calcs, "figures/calcs_game_distances.yaml")
writeLines(prep_latex_variables(calcs), "figures/calcsGameDistances.tex")
