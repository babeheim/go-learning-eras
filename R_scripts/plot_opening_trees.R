
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")
move12s <- read.csv("data/move12s.csv")

table(games$era)

n_sampled_games <- 2900

keep <- c()

for (i in 1:nrow(eras)) {
  tar <- which(games$era == eras$name[i])
  stopifnot(length(tar) >= n_sampled_games)
  keep <- c(keep, sample(tar, n_sampled_games))
}

games <- games[sort(keep),]

stopifnot(all(games$era %in% eras$name))

n_moves <- 7

game_moves <- extract_game_moves(games, n_moves = n_moves, cumulative = TRUE)

era_nodes <- vector("list", length = nrow(eras))

for (k in 1:nrow(eras)) {

  games_era <- games[which(games$era == eras$name[k]),]

  moves <- extract_game_moves(games_era, n_moves = n_moves)
  cmoves <- extract_game_moves(games_era, n_moves = n_moves, cumulative = TRUE)

  nodes <- data.frame(
    move = 0,
    name = "",
    parent = NA,
    game_count = nrow(cmoves),
    sib_freq = 1,
    reply_entropy = NA
  ) # useful for tree visualization purposes

  # beginning with root, identify qualified replies and add to the nodes table, also calculating properties of each reply node

  use_entropy_cutoff <- TRUE
  use_absolute_cutoff <- FALSE

  absolute_cutoff <- 1

  for (move in 1:ncol(cmoves)) {

    parent_nodes <- unique(nodes$name[which(nodes$move == (move - 1))])

    cat("move", move, "has", length(parent_nodes), "parent nodes", "\n")

    for (j in 1:length(parent_nodes)) {

      parent_row <- which(nodes$name == parent_nodes[j])

      if (move == 1) {
        parent_games <- 1:nrow(cmoves)
      } else {
        parent_games <- which(cmoves[,(move-1)] == parent_nodes[j])
      }

      obs_replies <- cmoves[parent_games, move]
      reply_counts <- sort(table(obs_replies), decreasing = TRUE)

      # count all unique replies, pre-filtering
      nodes$reply_entropy[parent_row] <- entropy(obs_replies)

      # potential reply cutoff rules:
      # - drop very rare replies based on entropy count
      # e.g. if there are 5 replies but only 2.3 *effective* replies, we take the first 3 replies
      # note that *every* node must have at least *one* reply by this method, and when there's only 1 or 2 games, every reply will be added 
      # in other words, it prunes rare branches when on a well-travelled path
      # but it keeps EVERY downstream branch 
      if (use_entropy_cutoff) {
        cutoff <- min(ceiling(exp(nodes$reply_entropy[parent_row])), length(obs_replies))
        reply_counts <- reply_counts[1:cutoff]
      }
      if (use_absolute_cutoff) {
        reply_counts <- reply_counts[which(reply_counts > absolute_cutoff)]
      }

      if (length(reply_counts) > 0) {

        replies <- names(reply_counts)
        reply_freqs <- reply_counts / sum(reply_counts)

        if (move == 1) {
          replies_start <- c("pd", "qd", "qc")
          replies_start <- intersect(replies_start, replies)
          replies_new <- c(replies_start, sort(setdiff(replies, replies_start)))
          o <- match(replies, replies_new)
          replies <- replies[o]
          reply_freqs <- reply_freqs[o]
          reply_counts <- reply_counts[o]
        }

        if (move == 2) {
          if (substr(replies[1], 1, 2) == "pd") {
            replies_start <- c("pddd", "pddp", "pddc", "pdcq", "pdcc")
          } else if (substr(replies[1], 1, 2) == "qd") {
            # if (k == 1) stop()
            replies_start <- c("qddc", "qddd", "qdpp", "qddp")
          } else if (substr(replies[1], 1, 2) == "qc") {
            replies_start <- c("qcdp", "qcdc", "qccd", "qcdd")
          } else {
            replies_start <- character()
          }
          replies_start <- intersect(replies_start, replies)
          replies_new <- c(replies_start, sort(setdiff(replies, replies_start)))
          o <- match(replies_new, replies)
          replies <- replies[o]
          reply_freqs <- reply_freqs[o]
          reply_counts <- reply_counts[o]
        }

        if (move == 1) {
          parent_node <- rep("", length(replies))
        } else {
          parent_node <- substr(replies, 1, nchar(replies) - 2)
        }

        add <- data.frame(
          move = move,
          parent = parent_node,
          game_count = as.vector(reply_counts),
          sib_freq = as.vector(reply_freqs),
          name = replies
        )

        stopifnot(all(add$parent %in% nodes$name))

        nodes <- bind_rows(nodes, add)
      }
    }

  }

  stopifnot(sum(nodes$move == 0) == 1)
  stopifnot(nodes$name[nodes$move == 0] == "")

  nodes$n_unpruned_replies <- NA
  for (i in 1:nrow(nodes)) {
    nodes$n_unpruned_replies[i] <- sum(nodes$parent == nodes$name[i], na.rm = TRUE)
  }
  # noice

  # nrow(nodes) == 7838 # with 6 moves
  # nrow(nodes) == 10694 # with 10 moves

  nodes$is_parent <- nodes$name %in% nodes$parent
  nodes$is_tip <- !nodes$is_parent

  n_nodes <- length(unique(nodes$name))
  n_tips <- sum(!(nodes$name %in% nodes$parent))

  stopifnot(n_tips < n_nodes)
  stopifnot(!any(nodes[which(nodes$move == n_moves),]$name %in% nodes$parent))
  stopifnot(nrow(nodes) == n_nodes)
  stopifnot((n_nodes - n_tips + 1) == length(unique(nodes$parent))) # plus 1 for root?

  era_nodes[[k]] <- nodes

}

eras$line_weight <- 0.6
eras$flip_x <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
eras$flip_y <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
eras$rotate_ccw <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

transform_layout <- function(layout, flip_x, flip_y, rotate90_ccw) {
  if (flip_x) layout[, 1] <- 1 - layout[, 1]
  if (flip_y) layout[, 2] <- 1 - layout[, 2]
  if (rotate90_ccw) {
    rotation_matrix <- matrix(c(0, -1, 1, 0), nrow = 2, byrow = TRUE)
    layout <- layout %*% t(rotation_matrix)
  }
  return(layout)
}


png("figures/opening_tree_eras.png", res = 300, units = "in", height = 15, width = 10)

par(mfrow = c(3, 2))

for (k in 1:nrow(eras)) {
 
  nodes <- era_nodes[[k]]

  links <- select(nodes, parent, name, game_count, sib_freq)
  links <- rename(links, from = parent, to = name)
  links <- links[-which(is.na(links$from)),]

  links$edge_col <- move12s$col[match(substr(links$to, 1, 4), move12s$pattern)]
  links$edge_col[which(links$to == "pd")] <- freckles[1]
  links$edge_col[which(links$to == "qd")] <- links$edge_col[which(links$from == "qd")][1]
  links$edge_col[which(links$to == "qe")] <- links$edge_col[which(links$from == "qe")][1]

  links$edge_col[which(is.na(links$edge_col))] <- gray(0.7, 0.8)

  nodes <- select(nodes, name, move, game_count)

  net <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

  # tree using the reingold tilford algorithm
  l <- layout_as_tree(net, circular = TRUE)

  l <- transform_layout(l, eras$flip_x[k], eras$flip_y[k], eras$rotate_ccw[k])

  # if (eras$flip_x[k]) {
  #   l[,1] <- 1 - l[,1]
  # }
  # if (eras$flip_y[k]) {
  #   l[,2] <- 1 - l[,2]
  # }
  # if (eras$rotate_ccw[k]) {
  #   rotation_matrix <- matrix(c(0, -1, 1, 0), nrow = 2, byrow = TRUE)
  #   # Apply rotation
  #   l <- l %*% t(rotation_matrix)
  # }

  par(mar = c(0.7, 0.7, 0.7, 0.7))
  plot(net, edge.arrow.size=0, vertex.color = col.alpha("gray", 0.01),
    vertex.frame.color = col.alpha("gray", 0.01), vertex.size = 0.0001,
    edge.width = eras$line_weight[k] * (links$game_count)^(1/3),
    edge.color = col_alpha(links$edge_col, 1.0), layout = l, vertex.label = NA,
    ylim = c(-1, 1), xlim = c(-1, 1), main = eras$label[k], cex.main = 1.5)
    line_col <- gray(0.1, 0.1)

  points(0, 0, pch = 20, col = "gray", cex = 3)

  if (k == 6) {
    shadowtext(-0.7, 0.41, "qd,dc", col = "black", bg = "white", cex = 2)
    shadowtext(-0.2, 0.8, "qd,dd", col = "black", bg = "white", cex = 2)
    shadowtext(-0.8, -0.25, "pd,dc", col = "black", bg = "white", cex = 2)
    shadowtext(0.6, -0.6, "pd,dd", col = "black", bg = "white", cex = 2)
    shadowtext(-0.3, -0.8, "pd,dp", col = "black", bg = "white", cex = 2)
    shadowtext(0.7, 0.5, "qd,dp", col = "black", bg = "white", cex = 2)
    shadowtext(-0.8, 0.05, "pd,cq", col = "black", bg = "white", cex = 2)
    shadowtext(0.2, 0.8, "qd,pp", col = "black", bg = "white", cex = 2)
  }

}

dev.off()


