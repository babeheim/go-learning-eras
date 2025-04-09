
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")
move12s <- read.csv("data/move12s.csv")

calcs <- c()

n_moves <- 50 # analysis is on patterns in the first 50
n_games_era <- 1000
calcs$nGamesMDSEra <- format(n_games_era, big.mark = ",", trim = TRUE)
calcs$nGamesMDSTotal <- format(n_games_era * nrow(eras), big.mark = ",", trim = TRUE)
calcs$nMovesMDSDepth <- n_moves

set.seed(project_seed)
hits <- integer()
for (i in 1:nrow(eras)) {
  tar <- which(games$era == eras$name[i])
  add <- sample(tar, n_games_era)
  hits <- c(hits, add)
}

# subsample the games
games <- games[hits,]

game_moves <- extract_game_moves(games, n_moves = n_moves, cumulative = TRUE, unicode = TRUE)
game_moves2 <- extract_game_moves(games, n_moves = n_moves, cumulative = TRUE, unicode = FALSE)
# n_games x n_moves

if (!file.exists("data/distance_mds.RDS")) {

  distance_matrix <- stringdistmatrix(game_moves[, n_moves], method = "lv")
  # other options: osa (optimal string alignment) or dl (Damerau-Levenshtein)
  mds_result <- cmdscale(distance_matrix, k=2)
  mds <- data.frame(x = as.numeric(scale(mds_result[,1])), y = as.numeric(scale(mds_result[,2])))
  saveRDS(mds, file = "data/distance_mds.RDS")

}

mds <- readRDS("data/distance_mds.RDS")
mds$era <- games$era
mds$move1 <- game_moves2[,1]
mds$move12 <- game_moves2[,2]

mds$move12_col <- move12s$col[match(mds$move12, move12s$pattern)]

mds$move12_col[is.na(mds$move12_col)] <- "yellow"

mds$opening <- games$opening
mds$hash_id <- games$hash_id

mds$move14 <- substr(mds$opening, 1, 12)

mds |>
  group_by(move14) |>
  summarize(
    n = n()
  ) |> 
  arrange(desc(n)) |>
  filter(n >= 10) -> move14s

move14s$col <- sample(magma(nrow(move14s), begin = 0.3, end = 0.8))

mds$move14_col <- move14s$col[match(mds$move14, move14s$move14)]

# # 3212 don't work?
# i <- sample(1:nrow(mds), 1)
# dists <- sqrt((mds[i,]$x - mds$x)^2 + (mds[i,]$y - mds$y)^2)
# dists[which(dists == 0)] <- 1000
# j <- which.min(dists)
# par(mfrow = c(1, 3))
# plot(dists, col = mds$move12_col, ylim = c(0, 3))
# plot(dists, col = mds$move12_col, ylim = c(0, 0.1))
# abline(v = j)
# abline(h = dists[j])

# plot(mds$x, mds$y, col = gray(0.5, 0.3))
# points(mds$x[c(i, j)], mds$y[c(i, j)], col = "red", pch = 20)

# games[c(i, j), c("hash_id", "year", "opening")]

# game_moves3 <- extract_game_moves(games[c(i, j),], n_moves = n_moves, cumulative = TRUE, unicode = TRUE)
# x <- game_moves3[,n_moves]
# stringdist(x[1], x[2], method = "lv")

# 1ef42e6c777255fc9ca and 9617b87c7ad8362fe13 - diferent opeings but similar follwo-ups

# 55048dd40491f2abe19 and e214ce4bcbea9206be0, very low edit distance even tho lots of variation in oepning

# ccf582c896f3b70f00d and 02f6f4464184cb4fb08

# 606bd532995f9675d58...huh? distiance fo 50 with closest neighbor huh

# cdc2b14e4400fc46380 and 3e4df00d5e89f0cbccd
# qd;dc;pq;nd;pf;dp;de;ce;cf;cd;df;ed;cj;jq;mq;qn;fq;qq;qp;pp;qr;rq;op;po;or;rr;rs;rp;dn;eq;fp;eo;ip;en;dm;ko;in;em;dl;er;iq;co;jc;hc;oo;km;pm;ql;il;kk
# qd;dc;pq;cq;de;od;oc;nc;pc;ce;cf;cd;df;ed;dj;dl;nd;po;qm;qq;mq;pp;pr;np;mp;mo;lo;mn;pj;ln;ko;kq;nq;iq;kn;km;jm;kl;jl;kk;hp;gr;ip;jp;jo;fp;md;qk;pk;qj


xlims <- c(-1.8, 1.8)
ylims <- c(-2, 2.8)

png("figures/move12_distance_mds.png", res = 300, units = "in", height = 7, width = 7)

alpha <- 0.3

par(mfrow = c(1, 1))

par(mar = c(4, 4, 0, 0))

plot(mds$x, mds$y, col = NA, frame.plot = FALSE, xlab = "MDS 1", ylab = "MDS 2",
  xlim = xlims, ylim = ylims)

moves <- c("pddd", "pddp", "pddc", "pdcq", "qddc", "qddd", "qddp", "qcdp", "qcdd", "qdoc")
# not shown: qdpp, qdpp

for (i in 1:length(moves)) {
  move <- moves[i] # xx
  movename <- paste0(substr(move, 1, 2), ",", substr(move, 3, 4))
  tar <- which(mds$move12 == move)
  points(mds$x[tar], mds$y[tar], col = col_alpha(mds$move12_col[tar], alpha), pch = 16)
  shadowtext(mean(mds$x[tar]), mean(mds$y[tar]), labels = movename, col = "black", bg = "white")
}

dev.off()



png("figures/move14_distance_mds.png", res = 300, units = "in", height = 7, width = 7)

alpha <- 0.3

par(mfrow = c(1, 1))

plot(mds$x, mds$y, frame.plot = FALSE, xlab = "MDS 1", ylab = "MDS 2",
xlim = xlims, ylim = ylims, col = col_alpha(mds$move14_col, 0.5), pch = 16)

dev.off()

# the lower cloud of the pd;dd space is pd;dd;qp;dq or pd;dd;pp;dq or pd;dd;qp;dp
# also: pd;dd;pq;dp


png("figures/move12_distance_mds_eras.png", res = 300, units = "in", height = 15, width = 10)

past_alpha <- 0.7
alpha <- 0.7

par(mfrow = c(3, 2))

par(mar = c(4, 4, 2, 1))  # bottom, left, top, right

for (i in 1:nrow(eras)) {
  if (i == 1) {
    tar <- which(mds$era == eras$name[i])
    plot(mds$x[tar], mds$y[tar], col = col_alpha(mds$move12_col[tar], alpha),
    pch = 16, frame.plot = FALSE, xlab = "MDS 1", ylab = "MDS 2",
    xlim = xlims, ylim = ylims, main = eras$label[i])
  } else {
   
    tar <- which(mds$era == eras$name[i-1])
    plot(mds$x[tar], mds$y[tar], col = col_alpha(mds$move12_col[tar], past_alpha),
    pch = 1, frame.plot = FALSE, xlab = "MDS 1", ylab = "MDS 2",
    xlim = xlims, ylim = ylims, main = eras$label[i])
    tar <- which(mds$era == eras$name[i])

    points(mds$x[tar], mds$y[tar], col = col_alpha(mds$move12_col[tar], alpha), pch = 16)

  }

}

dev.off()

write_yaml(calcs, "figures/calcs_game_distances.yaml")
writeLines(prep_latex_variables(calcs), "figures/calcsGameDistances.tex")
