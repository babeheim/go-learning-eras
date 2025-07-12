
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
players <- read.csv("data/players.csv")

nations <- list(
  list(
    name = "CN", 
    color = " #f47070"
  ),
  list(
    name = "JP", 
    color = " #474747"
  ),
  list(
    name = "TW", 
    color = "#597ae8"
  ),
  list(
    name = "KR", 
    color = "#579d5d"
  ),
  list(
    name = "EN", 
    color = " #e7b160"
  )
) |> bind_rows() |> as.data.frame()

nations$color <- trimws(nations$color)

players$lang_color <- nations$color[match(players$language, nations$name)]
mean(is.na(players$language)) # 60% still missing! huh?
players$lang_color[is.na(players$lang_color)] <- "gray"

i <- sample(1:nrow(players), 1)
games[which(games$player_id_black == players$player_id[i]), c("language_black")]

sum(is.na(games$player_id_black)) # 816 dont have a clear identity! bad!

# for each year, take the set of interactions as a *network*


games$period <- NA
tar <- which(games$year < 1850)
games$period[tar] <- games$year[tar] - games$year[tar] %% 50 + 25
tar <- which(games$year >= 1850 & games$year <= 1950)
games$period[tar] <- games$year[tar] - games$year[tar] %% 10 + 5
tar <- which(games$year > 1950)
games$period[tar] <- games$year[tar]

periods <- data.frame(
  name = sort(unique(games$period))
)


games$move12 <- substr(games$opening, 1, 6)

periods$move12_diversity <- NA
for (i in 1:nrow(periods)) {
  tar <- which(games$period == periods$name[i])
  periods$move12_diversity[i] <- exp(entropy(games$move12[tar]))
}

eras <- read.csv("data/eras.csv")

periods$era_color <- NA
for (i in 1:nrow(periods)) {
  periods$era_color[i] <- eras$color[max(which(eras$start_year <= periods$name[i]))]
}

for (i in 1:nrow(periods)) {

  tar <- which(games$period == periods$name[i] & !is.na(games$player_id_black) & !is.na(games$player_id_white))
  period_games <- games[tar,]

  edgelist <- period_games %>%
    mutate(
      from = pmin(player_id_black, player_id_white),
      to   = pmax(player_id_black, player_id_white),
      period = periods$name[i]
    ) %>%
    count(from, to, period, name = "n_games")
  stopifnot(sum(edgelist$n_games) == length(tar))
  filename <- paste0("data/edgelist_", periods$name[i], ".csv")
  write.csv(edgelist, filename, row.names = FALSE)
  cat(filename, "\n")

}






# Fast greedy (good for large, undirected graphs)
# A Clauset, MEJ Newman, C Moore: Finding community structure in very large networks, http://www.arxiv.org/abs/cond-mat/0408187

png("figures/network_examples.png", res = 300, units = "in", height = 7, width = 7)

par(mfrow = c(2, 2))

focal_period <- 1625
node_size <- 5
edge_width <- 1

edgelists <- list.files("data", pattern = "edgelist", full.names = TRUE)
i <- which(periods$name == focal_period)
edges <- read.csv(edgelists[i])

g <- graph_from_edgelist(as.matrix(select(edges, from, to)), directed = FALSE)
g_main <- induced_subgraph(g, components(g)$membership == which.max(components(g)$csize))
transitivity(g_main, type = "average")
mean_distance(g_main, directed = FALSE)

n_nodes <- vcount(g_main) # num. verticies (nodes), aka `n` in igraph
n_ties <- gsize(g_main)  # num. edges (ties), aka `m` in igraph

g_rand <- sample_gnm(n_nodes, n_ties)
transitivity(g_rand, type = "average")
mean_distance(g_rand, directed = FALSE)

k <- floor((2 * n_ties) / n_nodes)  # average degree
nei <- round(k / 2) # better than floor...

g_lattice <- sample_smallworld(dim = 1, size = n_nodes, nei = nei, p = 0)
transitivity(g_lattice, type = "average")
mean_distance(g_lattice)

max(components(g)$csize)

edge.attributes(g)$weight <- edges$n_games
layout_fr <- layout_with_fr(g, weights = 1 / edge.attributes(g)$weight)  # inverse weight = shorter distance
V(g)$color <- players$lang_color[match(V(g)$name, players$player_id)]

par(mar = c(0.1, 0.1, 0.1, 0.1))

plot(
  g,
  layout = layout_fr, 
  edge.width = edge_width,
  vertex.color = V(g)$color,
  vertex.label.color = "black",
  vertex.size = node_size,
  vertex.label = NA,
  node.label = NA, frame.plot = TRUE
)

text(1, 1, labels = "1600-1649", pos = 2)



focal_period <- 1935
node_size <- 5
edge_width <- 1

edgelists <- list.files("data", pattern = "edgelist", full.names = TRUE)
i <- which(periods$name == focal_period)
edges <- read.csv(edgelists[i])

g <- graph_from_edgelist(as.matrix(select(edges, from, to)), directed = FALSE)
g_main <- induced_subgraph(g, components(g)$membership == which.max(components(g)$csize))
transitivity(g_main, type = "average")
mean_distance(g_main, directed = FALSE)

n_nodes <- vcount(g_main) # num. verticies (nodes), aka `n` in igraph
n_ties <- gsize(g_main)  # num. edges (ties), aka `m` in igraph

g_rand <- sample_gnm(n_nodes, n_ties)
transitivity(g_rand, type = "average")
mean_distance(g_rand, directed = FALSE)

k <- floor((2 * n_ties) / n_nodes)  # average degree
nei <- round(k / 2) # better than floor...

g_lattice <- sample_smallworld(dim = 1, size = n_nodes, nei = nei, p = 0)
transitivity(g_lattice, type = "average")
mean_distance(g_lattice)

max(components(g)$csize)

edge.attributes(g)$weight <- edges$n_games
layout_fr <- layout_with_fr(g, weights = 1 / edge.attributes(g)$weight)  # inverse weight = shorter distance
V(g)$color <- players$lang_color[match(V(g)$name, players$player_id)]


par(mar = c(0.1, 0.1, 0.1, 0.1))

plot(
  g,
  layout = layout_fr, 
  edge.width = edge_width,
  vertex.color = V(g)$color,
  vertex.label.color = "black",
  vertex.size = node_size,
  vertex.label = NA,
  node.label = NA, frame.plot = TRUE
)

text(1, 1, labels = "1930-1939", pos = 2)


focal_period <- 1975
node_size <- 5
edge_width <- 1

edgelists <- list.files("data", pattern = "edgelist", full.names = TRUE)
i <- which(periods$name == focal_period)
edges <- read.csv(edgelists[i])

g <- graph_from_edgelist(as.matrix(select(edges, from, to)), directed = FALSE)
g_main <- induced_subgraph(g, components(g)$membership == which.max(components(g)$csize))
transitivity(g_main, type = "average")
mean_distance(g_main, directed = FALSE)

n_nodes <- vcount(g_main) # num. verticies (nodes), aka `n` in igraph
n_ties <- gsize(g_main)  # num. edges (ties), aka `m` in igraph

g_rand <- sample_gnm(n_nodes, n_ties)
transitivity(g_rand, type = "average")
mean_distance(g_rand, directed = FALSE)

k <- floor((2 * n_ties) / n_nodes)  # average degree
nei <- round(k / 2) # better than floor...

g_lattice <- sample_smallworld(dim = 1, size = n_nodes, nei = nei, p = 0)
transitivity(g_lattice, type = "average")
mean_distance(g_lattice)

max(components(g)$csize)

edge.attributes(g)$weight <- edges$n_games
layout_fr <- layout_with_fr(g, weights = 1 / edge.attributes(g)$weight)  # inverse weight = shorter distance
V(g)$color <- players$lang_color[match(V(g)$name, players$player_id)]


par(mar = c(0.1, 0.1, 0.1, 0.1))

plot(
  g,
  layout = layout_fr, 
  edge.width = edge_width,
  vertex.color = V(g)$color,
  vertex.label.color = "black",
  vertex.size = node_size,
  vertex.label = NA,
  node.label = NA, frame.plot = TRUE
)

text(1, 1, labels = "1975", pos = 2)


focal_period <- 2022
node_size <- 3
edge_width <- 1

edgelists <- list.files("data", pattern = "edgelist", full.names = TRUE)
i <- which(periods$name == focal_period)
edges <- read.csv(edgelists[i])

g <- graph_from_edgelist(as.matrix(select(edges, from, to)), directed = FALSE)
g_main <- induced_subgraph(g, components(g)$membership == which.max(components(g)$csize))
transitivity(g_main, type = "average")
mean_distance(g_main, directed = FALSE)

n_nodes <- vcount(g_main) # num. verticies (nodes), aka `n` in igraph
n_ties <- gsize(g_main)  # num. edges (ties), aka `m` in igraph

g_rand <- sample_gnm(n_nodes, n_ties)
transitivity(g_rand, type = "average")
mean_distance(g_rand, directed = FALSE)

k <- floor((2 * n_ties) / n_nodes)  # average degree
nei <- round(k / 2) # better than floor...

g_lattice <- sample_smallworld(dim = 1, size = n_nodes, nei = nei, p = 0)
transitivity(g_lattice, type = "average")
mean_distance(g_lattice)

max(components(g)$csize)

edge.attributes(g)$weight <- edges$n_games
layout_fr <- layout_with_fr(g, weights = 1 / edge.attributes(g)$weight)  # inverse weight = shorter distance
V(g)$color <- players$lang_color[match(V(g)$name, players$player_id)]


par(mar = c(0.1, 0.1, 0.1, 0.1))

plot(
  g,
  layout = layout_fr, 
  edge.width = edge_width,
  vertex.color = V(g)$color,
  vertex.label.color = "black",
  vertex.size = node_size,
  vertex.label = NA,
  node.label = NA, frame.plot = TRUE
)

text(1, 1, labels = "2024", pos = 2)

dev.off()



# it takes a while to load 

dir_init("./figures/match_network")


node_size <- 2
edge_width <- 1

edgelists <- list.files("data", pattern = "edgelist", full.names = TRUE)
force_plots <- FALSE

periods$n_players <- NA
periods$n_players_mc <- NA
periods$n_dyads <- NA
periods$degree_mean <- NA
periods$degree_median <- NA
periods$degree_sd <- NA
periods$n_components <- NA
periods$component_diversity <- NA
periods$clustering_mc <- NA
periods$avg_path_length_mc <- NA
periods$C_lattice <- NA
periods$L_lattice <- NA
periods$C_rand <- NA
periods$L_rand <- NA
periods$n_communities <- NA
periods$n_communities_mc <- NA
periods$community_size_avg <- NA
periods$community_size_mc_avg <- NA
periods$community_size_sd <- NA
periods$community_diversity <- NA
periods$community_modularity <- NA
periods$community_associativity <- NA

for (i in 1:length(edgelists)) {

  edges <- read.csv(edgelists[i])

  g <- graph_from_edgelist(as.matrix(select(edges, from, to)), directed = FALSE)
  edge.attributes(g)$weight <- edges$n_games
  V(g)$color <- players$lang_color[match(V(g)$name, players$player_id)]

  hit <- which(periods$name == edges$period[1])

  periods$n_players[hit] <- gorder(g)
  periods$n_players_mc[hit] <- max(components(g)$csize)
  periods$n_dyads[hit] <- gsize(g)
  periods$degree_mean[hit] <- mean(degree(g))
  periods$degree_median[hit] <- median(degree(g))  
  periods$degree_sd[hit] <- sd(degree(g))  
  periods$n_components[hit] <- components(g)$no

  component_weights <- components(g)$csize / gorder(g)
  stopifnot(all.equal(sum(component_weights), 1))
  periods$component_diversity[hit] <- exp(sum(component_weights * log(1/component_weights)))

  # also try community-detection_main 
  comm <- cluster_fast_greedy(g)
  periods$n_communities[hit] <- length(comm)
  periods$community_diversity[hit] <- exp(entropy(membership(comm)))
  periods$community_modularity[hit] <- modularity(comm)
  periods$community_associativity[hit] <- assortativity_nominal(g, as.numeric(membership(comm)))

  periods$community_size_avg[hit] <- mean(table(membership(comm)))
  periods$community_size_sd[hit] <- sd(table(membership(comm)))

  # calculate main-cluster statistics
  g_main <- induced_subgraph(g, components(g)$membership == which.max(components(g)$csize))
  comm_mc <- cluster_fast_greedy(g_main)
  periods$n_communities_mc[hit] <- length(comm_mc)
  periods$community_size_mc_avg[hit] <- mean(table(membership(comm_mc)))

  periods$clustering_mc[hit] <- transitivity(g_main, type = "average")
  periods$avg_path_length_mc[hit] <- mean_distance(g_main, directed = FALSE)
  
  n_nodes <- vcount(g_main) # num. verticies (nodes), aka `n` in igraph
  n_ties <- gsize(g_main)  # num. edges (ties), aka `m` in igraph

  # calculate reference network statistics
  g_rand <- sample_gnm(n_nodes, n_ties)
  periods$C_rand[hit] <- transitivity(g_rand, type = "average")
  periods$L_rand[hit] <- mean_distance(g_rand, directed = FALSE)
  k <- floor((2 * n_ties) / n_nodes)  # average degree

  nei <- floor(k / 2)    
  g_lattice <- sample_smallworld(dim = 1, size = n_nodes, nei = nei, p = 0)
  periods$C_lattice[hit] <- transitivity(g_lattice, type = "average")
  periods$L_lattice[hit] <- mean_distance(g_lattice)

  plot_name <- paste0("figures/match_network/match_network_", periods$name[hit], ".png")
  if (force_plots | !file.exists(plot_name)) {
    png(plot_name, res = 300, units = "in", height = 15, width = 15)
    layout_fr <- layout_with_fr(g, weights = 1 / edge.attributes(g)$weight)  # inverse weight = shorter distance
    plot(
      g,
      layout = layout_fr, 
      edge.width = edge_width,
      vertex.color = V(g)$color,
      vertex.label.color = "black",
      vertex.size = node_size,
      vertex.label = NA,
      node.label = NA,
      main = paste0("match network, ", periods$name[hit])
    )
    dev.off()
  }

  cat(edgelists[i], "\n")

}

periods$C <- periods$clustering_mc
periods$L <- periods$avg_path_length_mc

# length index, how un-lattice-like it is (0 = extremely latice-like, 1 = extremely random-like)
periods$Li <- (periods$L - periods$L_lattice) / (periods$L_rand - periods$L_lattice)

# clustering index, how un-random-like it is (0 = extremely random-like; 1 = extremely latice like)
periods$Ci <- (periods$C - periods$C_rand) / (periods$C_lattice - periods$C_rand)

periods$swi_mc <- periods$Li * periods$Ci



png("figures/move12_diversity_network_descriptives.png", res = 300, units = "in", height = 4, width = 9)

par(mfrow = c(1, 3))
plot(periods$n_players, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17), log = "x",
ylab = "opening move diversity", xlab = "number of players")
# abline(lm(move12_diversity ~ community_diversity, data = periods))

plot(periods$community_diversity, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17),
ylab = "opening move diversity", xlab = "community diversity")
abline(lm(move12_diversity ~ community_diversity, data = periods))

plot(periods$community_associativity, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17),
ylab = "opening move diversity", xlab = "community homophily")
abline(lm(move12_diversity ~ community_associativity, data = periods))

dev.off()



png("figures/move12_diversity_community_stuff.png", res = 300, units = "in", height = 4, width = 9)

plot(periods$n_communities, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17),
ylab = "opening move diversity", xlab = "community diversity")
abline(lm(move12_diversity ~ community_diversity, data = periods))

plot(periods$community_size_avg, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17),
ylab = "opening move diversity", xlab = "community size", log = "x")

plot(periods$community_diversity, periods$move12_diversity, col = periods$era_color, pch = 20, ylim = c(4.5, 17),
ylab = "opening move diversity", xlab = "community diversity")
abline(lm(move12_diversity ~ community_diversity, data = periods))

dev.off()

# periods[which.max(periods$move12_diversity),] 
# one point not shown: 197 players at 25.7



png("figures/move12_diversity_pop_size_structure.png", res = 300, units = "in", height = 3.5, width = 7.5)

par(mfrow = c(1, 2))

par(mar = c(4, 4, 0, 1))

plot(periods$n_players, periods$move12_diversity, col = periods$era_color, pch = 20,
ylab = "opening diversity", xlab = "number of players", log = "x", ylim = c(4, 17), xlim = c(20, 1500),
frame.plot = TRUE, las = 1)

txt_cex <- 0.8

shadowtext(50, 6, labels = "Early Modern", col = "black", bg = "white", cex = txt_cex)
shadowtext(60, 9, labels = "Imperial", col = "black", bg = "white", cex = txt_cex)
shadowtext(140, 11, labels = "Cold War", col = "black", bg = "white", cex = txt_cex)
shadowtext(400, 12, labels = "International", col = "black", bg = "white", cex = txt_cex)
shadowtext(500, 6, labels = "Internet", col = "black", bg = "white", cex = txt_cex)
shadowtext(1200, 7, labels = "SAI", col = "black", bg = "white", cex = txt_cex)

par(mar = c(4, 4, 0, 1))

plot(periods$community_size_avg, periods$n_communities, col = periods$era_color, pch = 20,
ylab = "number of communities", xlab = "average community size", log = "x", xlim = c(3, 50), ylim = c(2, 95),
frame.plot = TRUE, las = 1)

dev.off()




png("figures/move12_diversity_pop_size_structure_mc.png", res = 300, units = "in", height = 3.5, width = 7.5)

par(mfrow = c(1, 2))

par(mar = c(4, 4, 0, 1))

plot(periods$n_players, periods$move12_diversity, col = periods$era_color, pch = 20,
ylab = "opening diversity", xlab = "number of players", log = "x", ylim = c(4, 17), xlim = c(20, 1500),
frame.plot = TRUE, las = 1)

txt_cex <- 0.8

shadowtext(50, 6, labels = "Early Modern", col = "black", bg = "white", cex = txt_cex)
shadowtext(60, 9, labels = "Imperial", col = "black", bg = "white", cex = txt_cex)
shadowtext(140, 11, labels = "Cold War", col = "black", bg = "white", cex = txt_cex)
shadowtext(400, 12, labels = "International", col = "black", bg = "white", cex = txt_cex)
shadowtext(500, 6, labels = "Internet", col = "black", bg = "white", cex = txt_cex)
shadowtext(1200, 7, labels = "SAI", col = "black", bg = "white", cex = txt_cex)

par(mar = c(4, 4, 0, 1))

plot(periods$community_size_mc_avg, periods$n_communities_mc, col = periods$era_color, pch = 20,
ylab = "number of communities", xlab = "average community size", log = "x", xlim = c(6, 82), ylim = c(3, 23),
frame.plot = TRUE, las = 1)

dev.off()




png("figures/community_stats_year.png", res = 300, units = "in", height = 4, width = 9)

par(mfrow = c(1, 3))

plot(periods$name, periods$n_communities, col = periods$era_color, pch = 20,
xlab = "year", ylab = "num. communities", log = "y")

plot(periods$name, periods$community_size_avg, col = periods$era_color, pch = 20,
xlab = "year", ylab = "community size avg.", log = "y")

plot(periods$name, periods$community_size_sd, col = periods$era_color, pch = 20,
xlab = "year", ylab = "community size sd.", log = "y")

dev.off()

# as community diversity declines, so does the move diversity
# associativity within communities, e.g. the disposition to play outsiders
# contemporary era has largest umber of players, lowest number of communities, and 

png("figures/network_stats_year.png", res = 300, units = "in", height = 8, width = 10)

par(mfrow = c(3, 2))

plot(periods$name, periods$move12_diversity, col = periods$era_color, type = "l", 
ylab = "opening diversity", xlab = "year", xlim = c(1600, 1945))

plot(periods$name, periods$move12_diversity, col = periods$era_color, type = "l", 
ylab = "opening diversity", xlab = "year", xlim = c(1945, 2025))

plot(periods$name, periods$n_communities, col = periods$era_color, type = "l", 
ylab = "num. communities", xlab = "year", xlim = c(1600, 1945))

plot(periods$name, periods$n_communities, col = periods$era_color, type = "l", 
ylab = "num. communities", xlab = "year", xlim = c(1945, 2025))

plot(periods$name, periods$community_associativity, col = periods$era_color, type = "l", 
ylab = "associativity", xlab = "year", xlim = c(1600, 1945))

plot(periods$name, periods$community_associativity, col = periods$era_color, type = "l", 
ylab = "associativity", xlab = "year", xlim = c(1945, 2025))

dev.off()



# both players and dyads increase tremendously in recent years

# avg degree goes way up too, much higher resolution picture of social networks

png("figures/degree_mean_year.png", res = 300, units = "in", height = 4, width = 7)
par(mfrow = c(1, 2))
plot(periods$name, periods$degree_mean, col = periods$era_color, pch = 20)
plot(periods$name, periods$degree_median, col = periods$era_color, pch = 20)
dev.off()


png("figures/community_component_diversities.png", res = 300, units = "in", height = 5, width = 10)
par(mfrow = c(1, 3))
plot(periods$community_diversity, periods$n_communities, col = periods$era_color, pch = 20, log = "x")
plot(periods$component_diversity, periods$n_communities, col = periods$era_color, pch = 20, log = "x")
plot(periods$component_diversity, periods$community_diversity, col = periods$era_color, pch = 20, log = "x")
dev.off()

png("figures/network_pathlength_clustering_swi.png", res = 300, units = "in", height = 7, width = 7)

par(mfrow = c(2, 2))
plot(periods$n_players, periods$move12_diversity, col = periods$era_color, pch = 20, log = "x") # expected
plot(periods$n_players, periods$avg_path_length_mc, col = periods$era_color, pch = 20, log = "x")
plot(periods$n_players, periods$clustering_mc, col = periods$era_color, pch = 20, log = "x")
plot(periods$n_players, periods$swi_mc, col = periods$era_color, pch = 20, log = "x")

dev.off()


png("figures/diversity_diversity_plot.png", res = 300, unit = "in", height = 8, width = 8)
par(mfrow = c(2, 2))
plot(periods$n_components, periods$move12_diversity, col = periods$era_color, pch = 20, log = "x", ylim = c(5, 15))
plot(periods$n_communities, periods$move12_diversity, col = periods$era_color, pch = 20, log = "x", ylim = c(5, 15))
plot(periods$community_diversity, periods$move12_diversity, col = periods$era_color, pch = 20, log = "x", ylim = c(5, 15))
plot(periods$component_diversity, periods$move12_diversity, col = periods$era_color, pch = 20, log = "x", ylim = c(5, 15))
dev.off()



# how big is the main component, versus the total network size?
png("figures/n_players_vs_main_component.png", res = 300, unit = "in", height = 6, width = 6)
plot(periods$n_players, periods$n_players_mc, pch = 20, col = periods$era_color)
abline(0, 1, lty = 2)
dev.off()


png("figures/network_smallworld_popsize_trends.png", res = 300, units = "in", height = 3.5, width = 10)

par(mfrow = c(1, 3))

plot(periods$n_players_mc, periods$Ci, type = "p", log = "x", ylim = c(0.2, 0.7), pch = 20, col = periods$era_color)

plot(periods$n_players_mc, periods$Li, type = "p", log = "x", ylim = c(0.85, 1.0), pch = 20, col = periods$era_color)

plot(periods$n_players_mc, periods$swi_mc, type = "p", log = "x", pch = 20, col = periods$era_color, ylim = c(0, 1))

dev.off()



png("figures/network_smallworld_year_trends.png", res = 300, units = "in", height = 3.5, width = 10)

par(mfrow = c(1, 3))

plot(periods$name, periods$Ci, type = "p", log = "x", ylim = c(0.2, 0.7), pch = 20, col = periods$era_color)

plot(periods$name, periods$Li, type = "p", log = "x", ylim = c(0.85, 1.0), pch = 20, col = periods$era_color)

plot(periods$name, periods$swi_mc, type = "p", log = "x", pch = 20, col = periods$era_color, ylim = c(0, 1))

dev.off()
