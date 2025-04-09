
source("project_support.R")

move12s <- read.csv("data/move12s.csv")


dat <- list(
  list(
    pattern = "qddc", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc])"
  ),
  list(
    pattern = "qddd", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dd])"
  ),
  # list(
  #   pattern = "qcdd", # x, rare
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qc];W[dd])"
  # ),
  list(
    pattern = "pddd", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dd])"
  ),
  # list(
  #   pattern = "qdpp", # x,
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[pp])"
  # ),
  list(
    pattern = "pddc", # x,
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dc])"
  ),
  # list(
  #   pattern = "pdcc", # x,
  #   sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[cc])"
  # ),
  list(
    pattern = "qdoc", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[oc])"
  ),
  list(
    pattern = "qddp", # x
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dp])"
  ),
  # list(
  #   pattern = "qcdp", # x
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qc];W[dp])"
  #),
  list(
    pattern = "pdcq", # x
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[cq])"
  ),
  list(
    pattern = "pddp", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dp])"
  )
) |> bind_rows() |> as.data.frame()



stopifnot(!any(duplicated(dat$pattern)))

dat$name <- paste0(substr(dat$pattern, 1, 2), ",", substr(dat$pattern, 3, 4))

dat$board_col <- move12s$col[match(dat$pattern, move12s$pattern)]

board_size <- 19
stone_size <- 1.7
line.color <- gray(0.1, 0.5)

png("figures/common_openings.png", res = 300, units = "in", height = 4.6, width = 8)

par(mfrow = c(2, 4))

for (i in 1:nrow(dat)) {

  game <- simplify_game(parse_sgf(dat$sgf[i], FALSE))
  game_moves <- game$moves
  par(mar=c(0.1, 0.1, 2, 0.1))
  plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
    ylim = -c((board_size + 1), 0), axes = FALSE,
    xaxt="n", yaxt="n", xlab="", ylab="", main = dat$name[i])
  polygon(c(0, 0, (board_size + 1), (board_size + 1)),
    -c(0, (board_size + 1), (board_size + 1), 0), col = col_alpha(dat$board_col[i]))
  for (j in 1:board_size) {
    lines(c(j, j), -c(1, board_size), col = line.color)
    lines(c(1, board_size), -c(j, j), col = line.color)
  }
  starpoints <- c(4, 10, 16)
  for (sx in starpoints) {
    for (sy in starpoints) {
      points(sx, -sy, pch = 20, cex = 0.7, col = line.color)
    }
  }
  points(game_moves$column, -game_moves$row,
    bg=game_moves$color, pch = 21, cex = stone_size)

}

dev.off()
