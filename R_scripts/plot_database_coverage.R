
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")

nations <- list(
  list(
    name = "CN", 
    color = "#d32828"
  ),
  list(
    name = "JP", 
    color = "#000000"
  ),
  list(
    name = "TW", 
    color = "#456dee"
  ),
  list(
    name = "KR", 
    color = "#3bac45"
  ),
  list(
    name = "EN", 
    color = "#e6a442"
  )
) |> bind_rows() |> as.data.frame()

str(games)

years <- data.frame(
  year = 1600:2024,
  n_players = NA,
  n_games = NA,
  n_players_jp = NA,
  n_players_tw = NA,
  n_players_en = NA,
  n_players_kr = NA,
  n_players_cn = NA
)

for (i in 1:nrow(years)) {
  tar <- which(games$year == years$year[i])
  years$n_games[i] <- length(tar)
  years$n_players[i] <- length(unique(c(games$player_id_black[tar], games$player_id_black[tar])))
  
  tarb <- which(games$year == years$year[i] & games$language_black == "JP") 
  tarw <- which(games$year == years$year[i] & games$language_white == "JP") 
  years$n_players_jp[i] <- length(unique(c(games$player_id_black[tarb], games$player_id_black[tarw])))

  tarb <- which(games$year == years$year[i] & games$language_black == "CN") 
  tarw <- which(games$year == years$year[i] & games$language_white == "CN") 
  years$n_players_cn[i] <- length(unique(c(games$player_id_black[tarb], games$player_id_black[tarw])))

  tarb <- which(games$year == years$year[i] & games$language_black == "TW") 
  tarw <- which(games$year == years$year[i] & games$language_white == "TW") 
  years$n_players_tw[i] <- length(unique(c(games$player_id_black[tarb], games$player_id_black[tarw])))

  tarb <- which(games$year == years$year[i] & games$language_black == "KR") 
  tarw <- which(games$year == years$year[i] & games$language_white == "KR") 
  years$n_players_kr[i] <- length(unique(c(games$player_id_black[tarb], games$player_id_black[tarw])))

}


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


png("figures/gogod_games_players_year.png", res = 300, units = "in", height = 6, width = 8)

par(mfrow = c(2, 2))

par(mar = c(0, 4, 0, 0))

plot(years$year, years$n_games, frame.plot = FALSE,
  xaxt = "n", xaxs = "i", yaxs = "i", log = "y",
  las = 1, xlim = c(1600, 1945), ylim = c(0.7, 7000),
  ylab = "Number of database games", xlab = "",
  col = gray(0.6, 0.9), pch = 16
)

abline(v = 1945, col = gray(0.3, 0.5))

abline(h = 0.7, col = "black")


par(mar = c(0, 0, 0, 2))

plot(years$year, years$n_games, frame.plot = FALSE,
  xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", log = "y",
  las = 1, xlim = c(1945, 2025), ylim = c(0.7, 7000),
  col = gray(0.6, 0.9), pch = 16
)

abline(v = 1945, col = gray(0.3, 0.5))

abline(h = 0.7, col = "black")

par(mar = c(5, 4, 0, 0))

plot(years$year, years$n_players, frame.plot = FALSE,
  xaxt = "n", xaxs = "i", yaxs = "i", log = "y",
  las = 1, type = "n", xlim = c(1600, 1945), ylim = c(0.9, 800),
  ylab = "Number of players", xlab = "Year"
)
points(years$year, years$n_players_jp, type = "l", col = nations$color[which(nations$name == "JP")], pch = 20)
points(years$year, years$n_players_cn, type = "l", col = nations$color[which(nations$name == "CN")], pch = 20)
points(years$year, years$n_players_cn, type = "p", col = nations$color[which(nations$name == "CN")], pch = 20)
points(years$year, years$n_players_tw, type = "l", col = nations$color[which(nations$name == "TW")], pch = 20)
points(years$year, years$n_players_kr, type = "l", col = nations$color[which(nations$name == "KR")], pch = 20)

abline(v = 1945, col = gray(0.3, 0.8))

abline(h = 0.9, col = "black")

axis(1, at = c(1500, seq(1650, 1900, by = 50)))

par(mar = c(5, 0, 0, 2))

plot(years$year, years$n_players, frame.plot = FALSE,
  xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", log = "y",
  las = 1, type = "n", xlim = c(1945, 2025), ylim = c(0.9, 800),
  xlab = "", ylab = ""
)
points(years$year, years$n_players_jp, type = "l", col = nations$color[which(nations$name == "JP")], pch = 20)
points(years$year, years$n_players_cn, type = "l", col = nations$color[which(nations$name == "CN")], pch = 20)
points(years$year, years$n_players_tw, type = "l", col = nations$color[which(nations$name == "TW")], pch = 20)
points(years$year, years$n_players_kr, type = "l", col = nations$color[which(nations$name == "KR")], pch = 20)

text(1960, 150, labels = "Japan", cex = 1)
text(1955, 20, labels = "China", cex = 1)
text(1997, 70, labels = "S.Korea", cex = 1)
text(1988, 20, labels = "Taiwan", cex = 1)

axis(1, at = c(seq(1950, 2024, by = 10), 2030))
abline(v = 1945, col = gray(0.3, 0.8))

abline(h = 0.9, col = "black")

dev.off()
