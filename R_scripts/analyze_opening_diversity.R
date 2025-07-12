 
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")
move12s <- read.csv("data/move12s.csv")

n_moves <- 26

game_moves <- extract_game_moves(games$opening, n_moves = n_moves, cumulative = TRUE)

stopifnot(nrow(games) == 118348)

cat("calc opening diversity stats\n")

calcs <- list()

games$move12 <- substr(games$opening, 1, 6)
games$move14 <- substr(games$opening, 1, 12)

calcs$openingRichness <- length(unique(games$move12))
calcs$openingDiversity <- sprintf("%2.1f", exp(entropy(games$move12)))
calcs$openingDiversityRounded <- sprintf("%2.0f", exp(entropy(games$move12)))

props <- cumsum(rev(sort(table(games$move12))) / nrow(games))
calcs$nOpeningsNinety <- length(which(props < 0.9))
calcs$nOpeningsNinetyNine <- length(which(props < 0.99))



cat("define period lengths from 1600 to 2024\n")

games$period <- NA
tar <- which(games$year < 1850)
games$period[tar] <- games$year[tar] - games$year[tar] %% 50 + 25
tar <- which(games$year >= 1850 & games$year <= 1950)
games$period[tar] <- games$year[tar] - games$year[tar] %% 10 + 5
tar <- which(games$year > 1950)
games$period[tar] <- games$year[tar]

games |>
  group_by(period) |>
  summarize(
    n_games = n()
  ) |>
  rename(name = period) -> periods




cat("define moves12s and move stack order\n")

moves <- data.frame(
  name = names(rev(sort(table(games$move12)))),
  count = as.numeric(rev(sort(table(games$move12))))
)

# determine order to stack the plot polygons

moves$count_old <- NA
moves$count_new <- NA
for (i in 1:nrow(moves)) {
  tar <- which(games$era %in% c("early modern", "imperial"))
  moves$count_old[i] <- sum(games$move12[tar] == moves$name[i])
  tar <- which(games$era %in% c("cold war", "international", "internet", "superhuman ai"))
  moves$count_new[i] <- sum(games$move12[tar] == moves$name[i])
}

moves <- arrange(moves, desc(count_old))

tar <- which(moves$name == "qd;dd;") # R16,D16
o <- move_to_start(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "qd;dc;") # R16,D17
o <- move_to_start(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "pd;cq;")
o <- move_to_end(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "qd;dp;")
o <- move_to_end(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "pd;dd;")
o <- move_to_end(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "pd;dc;")
o <- move_to_end(1:nrow(moves), tar)
moves <- moves[o,]

tar <- which(moves$name == "pd;dp;")
o <- move_to_end(1:nrow(moves), tar)
moves <- moves[o,]

mean(games$move12 %in% moves$name) # 98.6% of all games

calcs$nMovesStacked <- nrow(moves)

# filter games to just those with the moves we are focusing on
games <- filter(games, games$move12 %in% moves$name)

stopifnot(nrow(games) == 118348)



cat("calculate period-specific move12 frequencies\n")

period_moves <- expand.grid(
  period = periods$name,
  move = moves$name
)
period_moves$n_games <- NA
for (i in 1:nrow(period_moves)) {
  tar <- which(games$move12 == period_moves$move[i] & games$period == period_moves$period[i])
  period_moves$n_games[i] <- length(tar)
}
period_moves$move <- as.character(period_moves$move)
period_moves$period <- as.numeric(as.character(period_moves$period))
period_moves$n_games_period <- periods$n_games[match(period_moves$period, periods$name)]
period_moves$prop_games <- period_moves$n_games / period_moves$n_games_period
period_moves$ybp <- 2024 + 1 - period_moves$period

period_moves$col <- move12s$col[match(period_moves$move, move12s$pattern)]

period_moves$col[is.na(period_moves$col)] <- gray(0.5, 0.7)



cat("calculate period-specific diversities and divergences (by bootstrap)\n")

n_bootstrap_iters <- 100
bootstrap_sample_size <- 100

calcs$openingDiversitySampleSize <- bootstrap_sample_size
calcs$openingDiversityBootstrapIterations <- n_bootstrap_iters

# for each bootstrap iteration, we calculate the diversity of each period

for (iter in 1:n_bootstrap_iters) {

  hits <- c()
  for (i in 1:nrow(periods)) {
    tar <- which(games$period == periods$name[i])
    hits <- c(hits, sample(tar, bootstrap_sample_size, replace = TRUE))
  }

  games_sub <- games[sort(hits),]
  periods_sub <- periods

  # calculate diversity within that period
  periods_sub$move12_diversity <- NA
  periods_sub$n_players_total <- NA
  periods_sub$n_players_effective <- NA
  for (i in 1:nrow(periods_sub)) {
    tar <- which(games_sub$period == periods_sub$name[i])
    periods_sub$move12_diversity[i] <- exp(entropy(games_sub$move12[tar]))
    players_sub <- c(games_sub$player_id_black[tar], games_sub$player_id_black[tar])
    periods_sub$n_players_total[i] <- length(unique(players_sub))
    periods_sub$n_players_effective[i] <- exp(entropy(players_sub))
  }

  # calculate JSD between the focal period and last period
  periods_sub$move12_divergence <- NA
  for (i in 2:nrow(periods_sub)) {
    tar_now <- which(games_sub$period == periods_sub$name[i])
    tar_past <- which(games_sub$period == periods_sub$name[i-1])
    if (length(tar_now) > 0 & length(tar_past) > 0) {
      periods_sub$move12_divergence[i] <- calc_js_divergence(games_sub$move12[tar_past], games_sub$move12[tar_now])
    }
  }

  periods_sub$iter <- iter

  if (iter == 1) {
    period_iters <- periods_sub
  } else {
    period_iters <- bind_rows(period_iters, periods_sub)
  }

}

period_iters |>
  group_by(name) |>
  summarize(
    n_players_total = mean(n_players_total),
    n_players_effective = mean(n_players_effective),
    move12_diversity = mean(move12_diversity),
    move12_divergence = mean(move12_divergence)
  ) -> periods_boot

periods$n_players_total <- periods_boot$n_players_total[match(periods$name, periods_boot$name)]
periods$n_players_effective <- periods_boot$n_players_effective[match(periods$name, periods_boot$name)]

periods$move12_diversity <- periods_boot$move12_diversity[match(periods$name, periods_boot$name)]
periods$move12_divergence <- periods_boot$move12_divergence[match(periods$name, periods_boot$name)]




cat("plot move12_diversity_divergence_year\n")

frq_max <- 1.02
diversity_min <- 2.5
diversity_max <- 20
divergence_max <- 0.4
left_year <- 1620
middle_year <- 1945
right_year <- 2025

png("figures/move12_diversity_divergence_year.png", res = 300, units = "in", height = 8, width = 8)

events <- list(
  list(
    name = "Meiji Restoration",
    year = 1868,
    my_y = 14
  ),
  list(
    name = "First Sino-Japanese War",
    year = 1894,
    my_y = 14
  ),
  list(
    name = "End of WWII",
    year = 1945,
    my_y = 11
  ),
  list(
    name = "End of Cultural Revolution",
    year = 1973,
    my_y = 11
  ),
  list(
    name = "First Online Go Server",
    year = 1992,
    my_y = 14
  ),
  list(
    name = "Alphago Defeats Lee Sedol",
    year = 2016,
    my_y = 14
  )
) |> bind_rows()


fill_alpha <- 0.8

# Set up the layout: matrix defines plot order; heights define relative row sizes
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = FALSE)
layout(mat = layout_matrix, heights = c(2, 1, 1, 2, 1, 1))

# first panel: move frequencies from 1600 to 1945

par(mar = c(0, 4, 0, 0))

plot(NULL, type = "l", xlim = c(left_year, middle_year),
  ylim = c(0, frq_max), xaxs="i", yaxs="i", xaxt = "n",
  frame.plot = FALSE, ylab = "move12 frequency", las = 1)

periods$base <- 0

for (i in 1:nrow(period_moves)) {
  tar <- which(period_moves$move == moves$name[i])
  if (length(tar) > 0) {
    ceiling <- periods$base + period_moves$prop_games[tar]
    polygon(c(period_moves$period[tar], rev(period_moves$period[tar])), c(periods$base, rev
    (ceiling)), col = col_alpha(period_moves$col[tar[1]], fill_alpha), border = gray(0.7, 0.5))
    periods$base <- ceiling
  }
}

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

shadowtext(1802, 0.25, labels = "R16,D17") # " #8cc665"
shadowtext(1682, 0.33, labels = "R16,P17") # " #e9aabd"
shadowtext(1693, 0.88, labels = "Q16,D16")  # 

# second panel: move diversity from 1600 to 1945

par(mar = c(0, 4, 0, 0))

plot(NULL, ylab = "move12 diversity", xlab = "", xlim = c(left_year, middle_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(diversity_min, diversity_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = c(0, 4, 8, 12, 16, 20), col = gray(0.5, 0.2))

points(periods$name, periods$move12_diversity, type = "o", pch = 20)

axis(2, at = seq(0, 20, by = 2), labels = FALSE, las = 1)
axis(2, at = c(0, 4, 8, 12, 16), tick = FALSE, las = 1)

abline(h = 2.5)

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)
shadowtext(events$year - 10, events$my_y, events$name, srt = 90, col = "black", bg = "white")

# third panel: move divergence from 1600 to 1945

par(mar = c(5, 4, 0, 0))

plot(NULL, ylab = "move12 divergence", xlab = "", xlim = c(left_year, middle_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(0, divergence_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = seq(0, divergence_max, .10), col = col_alpha("blue", 0.2))

points(periods$name, periods$move12_divergence, type = "o", pch = 20, col = "blue")

axis(2, at = seq(0, divergence_max, 0.05), las = 1)

axis(1, at = c(1500, seq(1650, 1900, by = 50), 1930))
abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

# fourth panel: move frequencies from 1945 to 2024

par(mar = c(0, 0, 0, 2))

plot(NULL, type = "l", xlim = c(middle_year, right_year), ylim = c(0, frq_max),
  xaxs="i", yaxs="i", yaxt = "n", xaxt = "n", frame.plot = FALSE)

periods$base <- 0

for (i in 1:nrow(period_moves)) {
  tar <- which(period_moves$move == moves$name[i])
  if (length(tar) > 0) {
    ceiling <- periods$base + period_moves$prop_games[tar]
    polygon(c(period_moves$period[tar], rev(period_moves$period[tar])), c(periods$base, rev
    (ceiling)), col = col_alpha(period_moves$col[tar[1]], fill_alpha), border = gray(0.7, 0.5))
    periods$base <- ceiling
  }
}

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

shadowtext(1994, 0.07, labels = "R16,D16") # " #46cbe5"
shadowtext(1982, 0.7, labels = "Q16,D17")  # " #44a340"
shadowtext(1985, 0.9, labels = "Q16,D16")  # " #9e0c0f"
shadowtext(1996, 0.41, labels = "Q16,D16") # " #9BC4DF"


# fifth panel: shannon diversity from 1945 to 2024

par(mar = c(0, 0, 0, 2))

plot(NULL, xlab = "year", xlim = c(middle_year, right_year),
  xaxs="i", yaxs="i", yaxt = "n", ylim = c(diversity_min, diversity_max), frame.plot = FALSE, xaxt = "n")

abline(h = c(0, 4, 8, 12, 16, 20), col = gray(0.5, 0.2))

points(periods$name, periods$move12_diversity, type = "o", pch = 20)

abline(h = 2.5)

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)
shadowtext(events$year - 2, events$my_y, events$name, srt = 90, col = "black", bg = "white")

# sixth panel: jensen-shannon divergence from 1945 to 2024

par(mar = c(5, 0, 0, 2))

plot(NULL, xlab = "", xlim = c(middle_year, right_year), xaxt = "n",
  xaxs="i", yaxs="i", ylim = c(0, divergence_max), frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = seq(0, divergence_max, .10), col = col_alpha("blue", 0.2))

points(periods$name, periods$move12_divergence, type = "o", pch = 20, col = "blue")

axis(1, at = c(seq(1950, 2024, by = 10), 2030))
abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

dev.off()




cat("plot move12_diversity_divergence_year_single\n")

frq_max <- 1.02
diversity_min <- 2.5
diversity_max <- 20
divergence_max <- 0.4
left_year <- 1620
middle_year <- 1945
right_year <- 2025

png("figures/move12_diversity_divergence_year_single.png", res = 300, units = "in", height = 8, width = 8)

fill_alpha <- 0.8

# Set up the layout: matrix defines plot order; heights define relative row sizes
layout_matrix <- matrix(c(1, 2, 3), nrow = 3, ncol = 1, byrow = FALSE)
layout(mat = layout_matrix, heights = c(2, 1, 1))

# first panel: move frequencies from 1600 to 1945

par(mar = c(0, 4, 0, 0))

plot(NULL, type = "l", xlim = c(left_year, right_year),
  ylim = c(0, frq_max), xaxs="i", yaxs="i", xaxt = "n",
  frame.plot = FALSE, ylab = "move12 frequency", las = 1)

periods$base <- 0

for (i in 1:nrow(period_moves)) {
  tar <- which(period_moves$move == moves$name[i])
  if (length(tar) > 0) {
    ceiling <- periods$base + period_moves$prop_games[tar]
    polygon(c(period_moves$period[tar], rev(period_moves$period[tar])), c(periods$base, rev
    (ceiling)), col = col_alpha(period_moves$col[tar[1]], fill_alpha), border = gray(0.7, 0.5))
    periods$base <- ceiling
  }
}

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

shadowtext(1802, 0.25, labels = "R16,D17")
shadowtext(1682, 0.33, labels = "R16,P17")
shadowtext(1693, 0.88, labels = "Q16,D16")

# second panel: move diversity from 1600 to 2024

par(mar = c(0, 4, 0, 0))

plot(NULL, ylab = "move12 diversity", xlab = "", xlim = c(left_year, right_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(diversity_min, diversity_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = c(0, 4, 8, 12, 16, 20), col = gray(0.5, 0.2))

points(periods$name, periods$move12_diversity, type = "o", pch = 20)

axis(2, at = seq(0, 20, by = 2), labels = FALSE, las = 1)
axis(2, at = c(0, 4, 8, 12, 16), tick = FALSE, las = 1)

abline(h = 2.5)

abline(v = events$year, col = gray(0.3, 0.8), lty = 2)
shadowtext(events$year - 5, events$my_y, events$name, srt = 90, col = "black", bg = "white")

# third panel: move divergence from 1600 to 1945

par(mar = c(5, 4, 0, 0))

plot(NULL, ylab = "move12 divergence", xlab = "", xlim = c(left_year, right_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(0, divergence_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = seq(0, divergence_max, .10), col = col_alpha("blue", 0.2))

points(periods$name, periods$move12_divergence, type = "o", pch = 20, col = "blue")

axis(2, at = seq(0, divergence_max, 0.05), las = 1)

axis(1, at = c(1500, seq(1650, 2024, by = 50), 2100))
abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

dev.off()





cat("calculate period statistics from 1600 using a log scale and increasing the resolution depending on the coverage\n")

# move-level diversity

n_sampled_games <- 100

for (i in 1:nrow(periods)) {
  period_games <- which(games$period == periods$name[i])
  n_period_games <- length(period_games)
  tar <- sample(period_games, min(n_sampled_games, n_period_games), replace = FALSE)
  period_game_moves <- as.data.frame(game_moves[tar,])
  ent <- move_entropy(period_game_moves) # 1 x n_moves

  divergence <- rep(NA, length(ent))

  if (i > 1) {
    last_period_games <- which(games$period == periods$name[i-1])
    last_tar <- sample(last_period_games, min(n_sampled_games, n_period_games), replace = FALSE)
    for (j in 1:length(ent)) {
      divergence[j] <- calc_js_divergence(game_moves[tar,j], game_moves[last_tar,j])
    }
  }

  add <- data.frame(
    era = periods$name[i],
    move = seq_along(ent),
    entropy = ent,
    divergence = divergence,
    n_games = length(tar)
  )
  if (i == 1) {
    out <- add
  } else {
    out <- bind_rows(out, add)
  }
}

period_div <- out

period_div$ybp <- (2024 + 1) - period_div$era

entropy_cols <- viridis(5, begin = 0, end = 0.7)
divergence_cols <- magma(6, end = 0.7)

png("figures/diversity_by_period_log.png", res = 300, units = "in", height = 4.5, width = 8)

par(mfrow = c(1, 2))

tar <- which(period_div$move == 1)
plot(period_div$ybp[tar], period_div$entropy[tar],
  xlab = "year", ylab = "Shannon entropy", xlim = c(421, 1), col = entropy_cols[1],
  ylim = c(0, log(n_sampled_games)), type = "l", pch = 20,
  log = "x", xaxt = "n")

axis(1, at = c((2024 + 1) - eras$start_year), labels = eras$start_year)

tar <- which(period_div$move == 2)
points(period_div$ybp[tar], period_div$entropy[tar],
  col = entropy_cols[2], ylim = c(0, log(n_sampled_games)), type = "l", pch = 20)

tar <- which(period_div$move == 3)
points(period_div$ybp[tar], period_div$entropy[tar],
  col = entropy_cols[3], ylim = c(0, log(n_sampled_games)), type = "l", pch = 20)

tar <- which(period_div$move == 4)
points(period_div$ybp[tar], period_div$entropy[tar],
  col = entropy_cols[4], ylim = c(0, log(n_sampled_games)), type = "l", pch = 20)

tar <- which(period_div$move == 5)
points(period_div$ybp[tar], period_div$entropy[tar],
  col = entropy_cols[5], ylim = c(0, log(n_sampled_games)), type = "l", pch = 20)

abline(h = log(n_sampled_games), lty = 2)


tar <- which(period_div$move == 1)
plot(period_div$ybp[tar], period_div$divergence[tar],
  xlab = "year", ylab = "JSD", xlim = c(421, 1), col = divergence_cols[1], type = "l", pch = 20,
  log = "x", xaxt = "n", ylim = c(0, 1))
  
axis(1, at = c((2024 + 1) - eras$start_year), labels = eras$start_year)

tar <- which(period_div$move == 2)
points(period_div$ybp[tar], period_div$divergence[tar],
  col = divergence_cols[2], type = "l", pch = 20)

tar <- which(period_div$move == 3)
points(period_div$ybp[tar], period_div$divergence[tar],
  col = divergence_cols[3], type = "l", pch = 20)

tar <- which(period_div$move == 4)
points(period_div$ybp[tar], period_div$divergence[tar],
  col = divergence_cols[4], type = "l", pch = 20)

tar <- which(period_div$move == 5)
points(period_div$ybp[tar], period_div$divergence[tar],
  col = divergence_cols[5], type = "l", pch = 20)

dev.off()



cat("calculate cumulative move diversity by era (not by period!)\n")

table(games$era) # the lowest we could go is 2930 for the early modern period

n_moves <- 26
n_sampled_games <- 2000 
# 16000 games for the 16000 in the AI era

calcs$nSampledGamesEraOpeningDiversity <- format(n_sampled_games, big.mark = ",", trim = TRUE)

game_moves <- extract_game_moves(games$opening, n_moves = n_moves, cumulative = TRUE)
# n_games x n_moves

for (i in 1:nrow(eras)) {
  focal_era <- eras$name[i]
  era_games <- which(games$era == focal_era)
  n_era_games <- length(era_games)
  tar <- sample(era_games, min(n_sampled_games, n_era_games), replace = FALSE)
  era_game_moves <- as.data.frame(game_moves[tar,])
  ent <- move_entropy(era_game_moves, base = exp(1)) # 1 x n_moves
  rich <- move_richness(era_game_moves) # 1 x n_moves
  even <- (exp(ent) - 1) / (rich - 1) # effective number of variants at each locus

  add <- data.frame(
    era = focal_era,
    move = seq_along(ent),
    entropy = ent
  )
  if (i == 1) {
    out <- add
  } else {
    out <- bind_rows(out, add)
  }
}

out$era_cat <- match(out$era, eras$name)

era_diversity <- out

my_cols <- viridis(6)
my_cols[length(my_cols)] <- "red"

periods$era <- NA
for (i in 1:nrow(periods)) {
  periods$era[i] <- eras$name[max(which(eras$start_year <= periods$name[i]))]
}

periods$era_col <- eras$color[match(periods$era, eras$name)]


png("figures/move12_diversity_by_pop.png", res = 300, units = "in", height = 4.5, width = 5)

par(mar = c(4, 4, 0, 0))

par(mfrow = c(1, 1))

plot(periods$n_players_total, periods$move12_diversity, log = "x", col = periods$era_col, pch = 20, xlab = "Number of Players", ylab = "Diversity of Opening Moves", ylim = c(4, 14), xlim = c(17, 100))

shadowtext(25, 5, labels = "Early Modern", col = "black", bg = "white")
shadowtext(30, 8, labels = "Imperial", col = "black", bg = "white")
shadowtext(45, 10.5, labels = "Cold War", col = "black", bg = "white")
shadowtext(70, 10, labels = "International", col = "black", bg = "white")
shadowtext(70, 5, labels = "Internet", col = "black", bg = "white")
shadowtext(90, 7, labels = "SAI", col = "black", bg = "white")

dev.off()




png("figures/opening_diversity_deltas.png", res = 300, units = "in", height = 5, width = 5)

for (i in 1:nrow(eras)) {
  period_games <- which(games$era == eras$name[i])
  n_period_games <- length(period_games)
  tar <- sample(period_games, min(n_sampled_games, n_period_games), replace = FALSE)
  period_game_moves <- as.data.frame(game_moves[tar,])
  ent <- move_entropy(period_game_moves) # 1 x n_moves
  rich <- move_richness(period_game_moves) # 1 x n_moves
  even <- (exp(ent) - 1) / (rich - 1) # effective number of variants at each locus

  add <- data.frame(
    era = eras$name[i],
    move = seq_along(ent),
    entropy = ent,
    richness = rich,
    evenness = even,
    n_games = length(tar)
  )
  if (i == 1) {
    out <- add
  } else {
    out <- bind_rows(out, add)
  }
}

period_div <- out

line_col <- "black"

plot(NULL, xlim = c(1, 15), ylim = c(-0.8, 1.0), frame.plot = FALSE, axes = FALSE, xlab = "", ylab = "")
box(col = line_col)
abline(h = 0, lty = 2, col = line_col)

# data points and point labels
for (i in 2:nrow(eras)) {
  points(out$entropy[which(out$era == eras$name[i])] - out$entropy[which(out$era == eras$name[i-1])], type = "b", col = eras$color[i], pch = rep(c(16, 1), 25))
}

# axes and labels
x_ticks <- 1:25
x_labs <- c(1, seq(5, 25, by = 5))
axis(1, at = setdiff(x_ticks, x_labs), labels = FALSE, col = line_col, col.ticks = line_col, col.axis = line_col)
axis(1, at = x_labs, tcl = -0.7, col = line_col, col.ticks = line_col, col.axis = line_col)
y_ticks <- seq(-0.8, 1.0, by = 0.1)
y_labs <- seq(-0.8, 1.0, by = 0.2)
mtext(side = 1, text = "Game Move", line = 3, col = line_col)
axis(2, at = setdiff(y_ticks, y_labs), labels = FALSE, col = line_col, col.ticks = line_col, col.axis = line_col)
axis(2, at = y_labs, tcl = -0.8, col = line_col, col.ticks = line_col, col.axis = line_col)
mtext(side = 2, text = "Δ Move Diversity", line = 3, col = line_col)

shadowtext(3, 0.5, labels = "SAI", col = "black", bg = "white")
shadowtext(4.5, 0.9, labels = "Imperial", col = "black", bg = "white")
shadowtext(4.3, 0.1, labels = "Cold War", col = "black", bg = "white")
shadowtext(7, -0.5, labels = "International", col = "black", bg = "white")
shadowtext(8, -0.3, labels = "Internet", col = "black", bg = "white")

dev.off()


write_yaml(calcs, "figures/calcs_opening_diversity.yaml")
writeLines(prep_latex_variables(calcs), "figures/calcsOpeningDiversity.tex")
