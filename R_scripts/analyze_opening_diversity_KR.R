
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")
move12s <- read.csv("data/move12s.csv")

country_code <- "KR" # JP, KR, CN, TW

keep <- which(games$language_black == country_code & games$language_white == country_code)

games <- games[keep,]

n_moves <- 26

game_moves <- extract_game_moves(games$opening, n_moves = n_moves, cumulative = TRUE)

stopifnot(nrow(games) == 19868)

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
left_year <- 1970
right_year <- 2025

png(glue("figures/move12_diversity_divergence_year_{country_code}.png"), res = 300, units = "in", height = 8, width = 8)

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


# abline(v = events$year, col = gray(0.3, 0.8), lty = 2)

shadowtext(1995, 0.8, label = "Q16,D4")   # " #FFAF56"
shadowtext(1983, 0.73, label = "Q16,D17")  # " #44a340"
shadowtext(2000, 0.4, label = "Q16,D16")  # " #9BC4DF"
shadowtext(2006, 0.35, label = "R16,D16") # " #46cbe5"
shadowtext(1983, 0.1, label = "R16,D17")  # " #8cc665"


# second panel: move diversity from 1600 to 1945

par(mar = c(0, 4, 0, 0))

plot(NULL, ylab = "move12 diversity", xlab = "", xlim = c(left_year, right_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(diversity_min, diversity_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = c(0, 4, 8, 12, 16, 20), col = gray(0.5, 0.2))

points(periods$name, periods$move12_diversity, type = "o", pch = 20)

axis(2, at = seq(0, 20, by = 2), labels = FALSE, las = 1)
axis(2, at = c(0, 4, 8, 12, 16), tick = FALSE, las = 1)

abline(h = 2.5)

# third panel: move divergence from 1600 to 1945

par(mar = c(5, 4, 0, 0))

plot(NULL, ylab = "move12 divergence", xlab = "", xlim = c(left_year, right_year),
  xaxt = "n", xaxs="i", yaxs="i", ylim = c(0, divergence_max),
  frame.plot = FALSE, las = 1, yaxt = "n")

abline(h = seq(0, divergence_max, .10), col = col_alpha("blue", 0.2))

points(periods$name, periods$move12_divergence, type = "o", pch = 20, col = "blue")

axis(2, at = seq(0, divergence_max, 0.05), las = 1)

axis(1, at = c(seq(1950, 2024, by = 10), 2030))

dev.off()
