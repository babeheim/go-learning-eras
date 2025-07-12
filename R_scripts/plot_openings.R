

source("project_support.R")

move12s <- read.csv("data/move12s.csv")
games <- read.csv("data/games.csv")

# not so simple...

cfact <- data.frame(
  year = 1920:2024,
  komi_mu = NA,
  komi_me = NA,
  komi_mo = NA,
  komi_q1 = NA,
  komi_q3 = NA
)

for (i in 1:nrow(cfact)) {
  tar <- which(games$year == cfact$year[i])
  if (length(tar) > 0) {
    cfact$komi_mu[i] <- mean(games[tar,]$komi, na.rm = TRUE)
    cfact$komi_me[i] <- median(games[tar,]$komi, na.rm = TRUE)
    cfact$komi_mo[i] <- as.numeric(names(rev(sort(table(games[tar,]$komi))))[1])
    cfact$komi_q1[i] <- quantile(games[tar,]$komi, 0.25, na.rm = TRUE)
    cfact$komi_q3[i] <- quantile(games[tar,]$komi, 0.75, na.rm = TRUE)
  }
}

png("figures/komi_year.png", res = 300, units = "in", height = 5, width = 7)
plot(NULL, xlim = c(1920, 2024), ylim = c(0, 8), xlab = "year", ylab = "komi (handicap to White)")
abline(h = 0:8, col = gray(0.8, 0.9))
points(games$year, games$komi, pch = 20, col = gray(0.8, 0.4))
points(cfact$year, cfact$komi_mu, type = "l")
points(cfact$year, cfact$komi_me, type = "l", col = "red")
dev.off()





png("figures/opening_examples.png", res = 300, units = "in", height = 7, width = 7)

par(mfrow = c(2, 2))

board_size <- 19
stone_size <- 2.5
number_size <- 0.9
coord_size <- 0.8
board_col <- "#f6d494"
line.color <- gray(0.1, 0.8)
line_width <- 0.8

# shushaku opening
sgf <- "(;GM[1]FF[4]CA[UTF-8]AP[Sabaki:0.52.2]KM[6.5]SZ[19]DT[2025-04-02];B[qd];W[dc];B[pq];W[oc];B[cp];W[qo];B[pe])"

game <- kaya:::simplify_game(kaya:::parse_sgf(sgf, FALSE))
game_moves <- game$moves

# par(mai = c(0, 0, 0, 0))
par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
  ylim = -c((board_size + 1), 0), axes = FALSE,
  xaxt="n", yaxt="n", xlab="", ylab="")
polygon(c(0, 0, (board_size + 1), (board_size + 1)),
  -c(0, (board_size + 1), (board_size + 1), 0), col = board_col)
for (j in 1:board_size) {
  lines(c(j, j), -c(1, board_size), col = line.color, lwd = line_width)
  lines(c(1, board_size), -c(j, j), col = line.color, lwd = line_width)
}
starpoints <- c(4, 10, 16)
for (sx in starpoints) {
  for (sy in starpoints) {
    points(sx, -sy, pch = 20, cex = 0.7, col = line.color)
  }
}
points(game_moves$column, -game_moves$row,
  bg=game_moves$color, pch = 21, cex = stone_size)
for (i in 1:nrow(game_moves)) {
  text(game_moves$column[i], -game_moves$row[i], i,
    col = ifelse(i %% 2 == 0, "black", "white"), cex = number_size)
}

text(1:19, rep(-0.5, 19), labels = setdiff(LETTERS[1:20], "I"), cex = coord_size)
text(rep(0.5, 19), -c(1:19), labels = 19:1, cex = coord_size)

sgf_string <- paste0(game_moves$coord_sgf, collapse = ";")
hits <- grep(sgf_string, games$opening)
next_moves_raw <- substr(games$opening[hits], nchar(sgf_string)+2, nchar(sgf_string)+3)

next_move_tab <- rev(sort(table(next_moves_raw)))

next_moves <- data.frame(
  sgf_coord = names(next_move_tab),
  count = as.numeric(next_move_tab)
)

next_moves$column <- match(substr(next_moves$sgf_coord, 1, 1), letters[1:19])
next_moves$row <- match(substr(next_moves$sgf_coord, 2, 2), letters[1:19]) 

next_moves$freq <- next_moves$count/sum(next_moves$count)
cap <- ceiling(exp(sum(log(1/next_moves$freq) * next_moves$freq)))
next_moves <- next_moves[1:cap,]
next_moves <- next_moves[which(next_moves$freq > 0.11),]
next_moves$perc <- round(next_moves$freq * 100)
next_moves$perc <- paste0(next_moves$perc, "%")

for (i in 1:nrow(next_moves)) {
  shadowtext(next_moves$column[i], -next_moves$row[i], labels = next_moves$perc[i], col = gray(0.3))
}



# "Move 8 is bad"
sgf <- "(;GM[1]FF[4]CA[UTF-8]AP[Sabaki:0.52.2]KM[6.5]SZ[19]DT[2025-04-02];B[pd];W[dd];B[pp];W[dp];B[pj];W[nc];B[pf])"

game <- kaya:::simplify_game(kaya:::parse_sgf(sgf, FALSE))
game_moves <- game$moves

par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
  ylim = -c((board_size + 1), 0), axes = FALSE,
  xaxt="n", yaxt="n", xlab="", ylab="")
polygon(c(0, 0, (board_size + 1), (board_size + 1)),
  -c(0, (board_size + 1), (board_size + 1), 0), col = board_col)
for (j in 1:board_size) {
  lines(c(j, j), -c(1, board_size), col = line.color, lwd = line_width)
  lines(c(1, board_size), -c(j, j), col = line.color, lwd = line_width)
}
starpoints <- c(4, 10, 16)
for (sx in starpoints) {
  for (sy in starpoints) {
    points(sx, -sy, pch = 20, cex = 0.7, col = line.color)
  }
}
points(game_moves$column, -game_moves$row,
  bg=game_moves$color, pch = 21, cex = stone_size)
for (i in 1:nrow(game_moves)) {
  text(game_moves$column[i], -game_moves$row[i], i,
    col = ifelse(i %% 2 == 0, "black", "white"), cex = number_size)
}

text(1:19, rep(-0.5, 19), labels = setdiff(LETTERS[1:20], "I"), cex = coord_size)
text(rep(0.5, 19), -c(1:19), labels = 19:1, cex = coord_size)

sgf_string <- paste0(game_moves$coord_sgf, collapse = ";")
hits <- grep(sgf_string, games$opening)
next_moves_raw <- substr(games$opening[hits], nchar(sgf_string)+2, nchar(sgf_string)+3)

next_move_tab <- rev(sort(table(next_moves_raw)))

next_moves <- data.frame(
  sgf_coord = names(next_move_tab),
  count = as.numeric(next_move_tab)
)

next_moves$column <- match(substr(next_moves$sgf_coord, 1, 1), letters[1:19])
next_moves$row <- match(substr(next_moves$sgf_coord, 2, 2), letters[1:19]) 

next_moves$freq <- next_moves$count/sum(next_moves$count)
cap <- ceiling(exp(sum(log(1/next_moves$freq) * next_moves$freq)))
next_moves <- next_moves[1:cap,]
next_moves <- next_moves[which(next_moves$freq > 0.11),]
next_moves$perc <- round(next_moves$freq * 100)
next_moves$perc <- paste0(next_moves$perc, "%")

for (i in 1:nrow(next_moves)) {
  # points(next_moves$column[i], -next_moves$row[i])
  shadowtext(next_moves$column[i], -next_moves$row[i], labels = next_moves$perc[i], col = gray(0.3))
}









# "Low Chinese Opening"

sgf <- "(;GM[1]FF[4]CA[UTF-8]AP[Sabaki:0.52.2]KM[6.5]SZ[19]DT[2025-04-02];B[pd];W[dd];B[pq];W[dp];B[qk])"

game <- kaya:::simplify_game(kaya:::parse_sgf(sgf, FALSE))
game_moves <- game$moves

par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
  ylim = -c((board_size + 1), 0), axes = FALSE,
  xaxt="n", yaxt="n", xlab="", ylab="")
polygon(c(0, 0, (board_size + 1), (board_size + 1)),
  -c(0, (board_size + 1), (board_size + 1), 0), col = board_col)
for (j in 1:board_size) {
  lines(c(j, j), -c(1, board_size), col = line.color, lwd = line_width)
  lines(c(1, board_size), -c(j, j), col = line.color, lwd = line_width)
}
starpoints <- c(4, 10, 16)
for (sx in starpoints) {
  for (sy in starpoints) {
    points(sx, -sy, pch = 20, cex = 0.7, col = line.color)
  }
}
points(game_moves$column, -game_moves$row,
  bg=game_moves$color, pch = 21, cex = stone_size)
for (i in 1:nrow(game_moves)) {
  text(game_moves$column[i], -game_moves$row[i], i,
    col = ifelse(i %% 2 == 0, "black", "white"), cex = number_size)
}

text(1:19, rep(-0.5, 19), labels = setdiff(LETTERS[1:20], "I"), cex = coord_size)
text(rep(0.5, 19), -c(1:19), labels = 19:1, cex = coord_size)


sgf_string <- paste0(game_moves$coord_sgf, collapse = ";")
hits <- grep(sgf_string, games$opening)
next_moves_raw <- substr(games$opening[hits], nchar(sgf_string)+2, nchar(sgf_string)+3)

next_move_tab <- rev(sort(table(next_moves_raw)))

next_moves <- data.frame(
  sgf_coord = names(next_move_tab),
  count = as.numeric(next_move_tab)
)

next_moves$column <- match(substr(next_moves$sgf_coord, 1, 1), letters[1:19])
next_moves$row <- match(substr(next_moves$sgf_coord, 2, 2), letters[1:19]) 

next_moves$freq <- next_moves$count/sum(next_moves$count)
cap <- ceiling(exp(sum(log(1/next_moves$freq) * next_moves$freq)))
next_moves <- next_moves[1:cap,]
next_moves <- next_moves[which(next_moves$freq > 0.11),]
next_moves$perc <- round(next_moves$freq * 100)
next_moves$perc <- paste0(next_moves$perc, "%")

for (i in 1:nrow(next_moves)) {
  shadowtext(next_moves$column[i], -next_moves$row[i], labels = next_moves$perc[i], col = gray(0.3))
}








sgf <- "(;GM[1]FF[4]CA[UTF-8]AP[Sabaki:0.52.2]KM[6.5]SZ[19]DT[2025-04-02];B[qc];W[cd];B[dp];W[pq];B[jj])"

sgf <- '(;FF[3]OH[2-(B)-2]AP[MultiGo:4.2.1]SZ[19]GC[Played over 14 sessions]EV["Game with the Meijin" sponsored by Yomiuri Shinbun to commemorate 20,000th issue]DT[1933-10-16~1934-01-29]PC[Kajibashi Ryokan, Kyobashi, Tokyo]PB[Go Seigen]BR[5d]PW[Honinbo Shusai]WR[9d]KM[0]TM[24h]RE[W+2]US[GoGoD95];B[qc];W[cd];B[dp];W[pq];B[jj];W[pd];B[qd];W[pe];B[ob];W[qn];B[jp];W[lq];B[dj];W[ed];B[lo];W[nq];B[kq];W[lp];B[ko];W[ch];B[gj];W[fq];B[fo];W[iq];B[gp];W[cq];B[cp];W[dq];B[nd];W[ph];B[pj];W[ne];B[me];W[mf];B[nf];W[ng];B[oe];W[of];B[ne];W[nj];B[om];W[mg];B[ke];W[hc];B[qm];W[rm];B[rn];W[rl];B[ro];W[pm];B[ql];W[qk];B[pl];W[pn];B[pk];W[qj];B[nl];W[pi];B[qp];W[on];B[nn];W[no];B[mo];W[op];B[qr];W[qq];B[rq];W[pr];B[rr];W[mn];B[nm];W[lm];B[ll];W[kp];B[mp];W[mq];B[ip];W[jq];B[kk];W[cn];B[ep];W[cj];B[ck];W[dk];B[cl];W[dl];B[ci];W[bj];B[bi];W[di];B[ej];W[bh];B[dm];W[ei];B[fj];W[cm];B[ak];W[ai];B[el];W[bp];B[rf];W[co];B[qg];W[rh];B[jc];W[gq];B[eq];W[en];B[fn];W[er];B[fr];W[dr];B[hq];W[hr];B[hp];W[gr];B[fc];W[ic];B[dc];W[cc];B[gd];W[he];B[ge];W[je];B[hd];W[id];B[hf];W[kd];B[le];W[kc];B[og];W[oh];B[pg];W[em];B[ek];W[fm];B[gm];W[mb];B[cb];W[ec];B[eb];W[dd];B[db];W[gb];B[bc];W[bd];B[bb];W[nb];B[oa];W[fb];B[ea];W[gh];B[jf];W[ie];B[ff];W[hi];B[hg];W[eo];B[fp];W[hl];B[il];W[jh];B[fh];W[fi];B[hj];W[if];B[ih];W[ii];B[ig];W[jg];B[ji];W[fg];B[kf];W[ij];B[ik];W[hh];B[gf];W[mk];B[df];W[ml];B[sm];W[sk];B[mm];W[dg];B[li];W[ol];B[ln];W[nk];B[km];W[fl];B[gl];W[oj];B[qh];W[ri];B[cf];W[ef];B[gi];W[eh];B[ee];W[eg];B[de];W[bf];B[bq];W[br];B[ps];W[os];B[qs];W[oq];B[bo];W[aq];B[an];W[bn];B[am];W[ao];B[rg];W[mi];B[qi];W[lh];B[kh];W[lj];B[rj];W[rk];B[sh];W[sj];B[bg];W[ae];B[cg];W[ag];B[fe];W[gc];B[fd];W[ce];B[kg];W[mc];B[ki];W[fk];B[ld];W[lc];B[gk];W[bk];B[bl];W[na];B[nc];W[gg];B[fa];W[ga];B[ac];W[ad];B[lg];W[mh];B[qo];W[lk])'

game <- kaya:::simplify_game(kaya:::parse_sgf(sgf, FALSE))
game_moves <- game$moves

game_moves <- game_moves[1:50,]


par(mar = c(0.0, 0.0, 0.0, 0.0))

plot(1, 1, col=NULL, xlim=c(0, (board_size + 1)),
  ylim = -c((board_size + 1), 0), axes = FALSE,
  xaxt="n", yaxt="n", xlab="", ylab="")
polygon(c(0, 0, (board_size + 1), (board_size + 1)),
  -c(0, (board_size + 1), (board_size + 1), 0), col = board_col)
for (j in 1:board_size) {
  lines(c(j, j), -c(1, board_size), col = line.color, lwd = line_width)
  lines(c(1, board_size), -c(j, j), col = line.color, lwd = line_width)
}
starpoints <- c(4, 10, 16)
for (sx in starpoints) {
  for (sy in starpoints) {
    points(sx, -sy, pch = 20, cex = 0.7, col = line.color)
  }
}
points(game_moves$column, -game_moves$row,
  bg=game_moves$color, pch = 21, cex = stone_size)
for (i in 1:nrow(game_moves)) {
  text(game_moves$column[i], -game_moves$row[i], i,
    col = ifelse(i %% 2 == 0, "black", "white"), cex = number_size)
}

text(1:19, rep(-0.5, 19), labels = setdiff(LETTERS[1:20], "I"), cex = coord_size)
text(rep(0.5, 19), -c(1:19), labels = 19:1, cex = coord_size)

dev.off()





dat <- list(
  list(
    pattern = "pd;dd;pq;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dd];B[pq])"
  ),
  list(
    pattern = "pd;dd;qp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dd];B[qp])"
  ),
  list(
    pattern = "pd;dd;pp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dd];B[pp])"
  ),
  list(
    pattern = "pd;dp;dd;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dp];B[dd])"
  ),
  list(
    pattern = "pd;dp;cd;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dp];B[cd])"
  ),
  list(
    pattern = "pd;dp;dc;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dp];B[dc])"
  ),
  list(
    pattern = "pd;dc;pp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dc];B[pp])"
  ),
  list(
    pattern = "pd;dc;pq;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dc];B[pq])"
  ),
  list(
    pattern = "pd;dc;dp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dc];B[dp])"
  ),
  list(
    pattern = "qd;dc;pq;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc];B[pq])"
  ),
  list(
    pattern = "qd;dc;cp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc];B[cp])"
  ),
  list(
    pattern = "qd;dc;pp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc];B[pp])"
  ),
  list(
    pattern = "qd;dc;dp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc];B[dp])"
  ),
  list(
    pattern = "qd;dd;pq;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dd];B[pq])"
  ),
  list(
    pattern = "qd;dd;pp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dd];B[pp])"
  ),
  list(
    pattern = "qd;dd;od;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dd];B[od])"
  ),
  list(
    pattern = "qd;dp;pq;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dp];B[pq])"
  ),
  list(
    pattern = "qd;dp;cd;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dp];B[cd])"
  )
) |> bind_rows() |> as.data.frame()



stopifnot(!any(duplicated(dat$pattern)))

dat$name <- sgf_to_korschelt(dat$pattern)

dat$board_col <- "white"

board_size <- 19
stone_size <- 1.1
line.color <- gray(0.75, 1.0)

png("figures/common_move123s.png", res = 300, units = "in", height = 4.9, width = 8)

par(mfrow = c(3,6))

for (i in 1:nrow(dat)) {

  game <- kaya:::simplify_game(kaya:::parse_sgf(dat$sgf[i], FALSE))
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







dat <- list(
  list(
    pattern = "qd;dc;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dc])"
  ),
  list(
    pattern = "qd;dd;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dd])"
  ),
  # list(
  #   pattern = "qc;dd;", # x, rare
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qc];W[dd])"
  # ),
  list(
    pattern = "pd;dd;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dd])"
  ),
  # list(
  #   pattern = "qd;pp;", # x,
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[pp])"
  # ),
  list(
    pattern = "pd;dc;", # x,
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dc])"
  ),
  # list(
  #   pattern = "pd;cc;", # x,
  #   sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[cc])"
  # ),
  list(
    pattern = "qd;oc;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[oc])"
  ),
  list(
    pattern = "qd;dp;", # x
    sgf = "(;FF[4]GM[1]SZ[19];B[qd];W[dp])"
  ),
  # list(
  #   pattern = "qc;dp;", # x
  #   sgf = "(;FF[4]GM[1]SZ[19];B[qc];W[dp])"
  #),
  list(
    pattern = "pd;cq;", # x
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[cq])"
  ),
  list(
    pattern = "pd;dp;", # xx
    sgf = "(;FF[4]GM[1]SZ[19];B[pd];W[dp])"
  )
) |> bind_rows() |> as.data.frame()



stopifnot(!any(duplicated(dat$pattern)))

dat$name <- sgf_to_korschelt(dat$pattern)

dat$board_col <- move12s$col[match(dat$pattern, move12s$pattern)]

board_size <- 19
stone_size <- 1.7
line.color <- gray(0.1, 0.5)

png("figures/common_move12s.png", res = 300, units = "in", height = 4.6, width = 8)

par(mfrow = c(2, 4))

for (i in 1:nrow(dat)) {

  game <- kaya:::simplify_game(kaya:::parse_sgf(dat$sgf[i], FALSE))
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


