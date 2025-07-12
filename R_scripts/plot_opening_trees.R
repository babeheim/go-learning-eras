
rm(list = ls())

source("project_support.R")

games <- read.csv("data/games.csv")
eras <- read.csv("data/eras.csv")
move12s <- read.csv("data/move12s.csv")

table(games$era)

n_sampled_games <- 2900 # the total number of games available in the early modern era

keep <- c()

for (i in 1:nrow(eras)) {
  tar <- which(games$era == eras$name[i])
  stopifnot(length(tar) >= n_sampled_games)
  keep <- c(keep, sample(tar, n_sampled_games))
}

games <- games[sort(keep),]

stopifnot(all(games$era %in% eras$name))

n_moves <- 7

era_nodes <- vector("list", length = nrow(eras))
era_links <- vector("list", length = nrow(eras))

for (k in 1:nrow(eras)) {

  games_era <- games[which(games$era == eras$name[k]),]

  cmoves <- extract_game_nodes(games_era$opening, n_moves = n_moves)

  # included games are listed by row
  # each game's nodes are listed in each column
  # from this, we'll construct a `nodes` data frame, one row per node

  # beginning with root, identify qualified replies and add to the nodes table, also calculating properties of each reply node

  use_entropy_cutoff <- TRUE

  use_absolute_cutoff <- FALSE
  absolute_cutoff <- 1 # branch must have more than this number of games

  nodes <- data.frame(
    move = 0,
    name = "",
    child_richness = NA,       # for this node, how many unique child nodes?
    child_entropy = NA         # for this node, what's the diversity of child nodes?
  )

  links <- data.frame(parent_name = character(), name = character(), move = integer())

  for (move in 1:ncol(cmoves)) {

    # iterate over the branches we are tracking in `nodes`
    tracked_parent_nodes <- unique(nodes$name[nodes$move == (move - 1)])

    cat("move", move, "has", length(tracked_parent_nodes), "parent nodes", "\n")

    # now go through these parent nodes one by one and add all the new child nodes
    for (j in 1:length(tracked_parent_nodes)) {

      parent_entry <- which(nodes$name == tracked_parent_nodes[j])

      if (move == 1) {
        parent_games <- 1:nrow(cmoves)
      } else {
        parent_games <- which(cmoves[,(move-1)] == tracked_parent_nodes[j])
      }

      child_node_counts <- rev(sort(table(cmoves[parent_games, move])))

      child_nodes <- data.frame(
        move = move,
        parent_name = tracked_parent_nodes[j],
        name = names(child_node_counts),
        game_count = as.numeric(child_node_counts)
      )

      # count all unique replies, pre-filtering
      nodes$child_richness[parent_entry] <- nrow(child_nodes)
      nodes$child_entropy[parent_entry] <- entropy(cmoves[parent_games, move])

      # potential reply cutoff rules:
      # - drop very rare replies based on entropy count
      # e.g. if there are 5 replies but only 2.3 *effective* replies, we take the first 3 replies
      # note that *every* node must have at least *one* reply by this method, and when there's only 1 or 2 games, every reply will be added 
      # in other words, it prunes rare branches when on a well-travelled path
      # but it keeps EVERY downstream branch 

      if (use_entropy_cutoff) {
        # does this work?
        effective_num_children <- exp(nodes$child_entropy[parent_entry])
        cutoff <- min(ceiling(effective_num_children), nrow(child_nodes))
        child_nodes <- child_nodes[1:cutoff,]
      }
      if (use_absolute_cutoff) {
        child_nodes <- child_nodes[which(child_nodes$game_count > absolute_cutoff),]
      }

      if (nrow(child_nodes) > 0) {
        nodes_to_add <- select(child_nodes, -parent_name, -game_count)
        nodes_to_add <- filter(nodes_to_add, !(name %in% nodes$name))
        nodes <- bind_rows(nodes, nodes_to_add)

        links_to_add <- child_nodes
        links <- bind_rows(links, links_to_add)
      }

    }

  }

  nodes$n_parents <- NA
  nodes$game_count <- NA

  for (i in 1:nrow(nodes)) {
    tar <- which(links$name == nodes$name[i])
    nodes$n_parents[i] <- length(tar)
    nodes$game_count[i] <- sum(links$game_count[tar])
  } 

  nodes$is_parent <- nodes$name %in% links$parent_name
  nodes$is_tip <- !nodes$is_parent

  stopifnot(!any(duplicated(nodes$name)))
  stopifnot(all(links$name %in% nodes$name))
  stopifnot(!any(links$parent_name == links$name))
  stopifnot(all(links$parent_name %in% nodes$name))
  
  era_nodes[[k]] <- nodes
  era_links[[k]] <- links


}


move12s$node <- extract_game_nodes(move12s$pattern, n_moves = 2)[,2]

eras$line_weight <- 0.6



png("figures/opening_tree_eras.png", res = 300, units = "in", height = 15, width = 10)

move12_cex <- 2

par(mfrow = c(3, 2))

for (k in 1:nrow(eras)) {
 
  nodes <- era_nodes[[k]]
  links <- era_links[[k]]

  links[which(links$move <= 2),]
  links[which(links$move <= 2 & grepl("^Bqc", links$name)),]

  # reorganize according to rules

  m1s <- nodes[which(nodes$move == 1),]
  tar <- which(m1s$name == "Bqc")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m1s), tar)
    m1s <- m1s[o,]
  }
  tar <- which(m1s$name == "Bqd")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m1s), tar)
    m1s <- m1s[o,]
  }
  tar <- which(m1s$name == "Bpd")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m1s), tar)
    m1s <- m1s[o,]
  }
  nodes[which(nodes$move == 1),] <- m1s

  slice <- which(nodes$move == 2 & grepl("^Bpd", nodes$name))
  m2s <- nodes[slice,]
  tar <- which(m2s$name == "BpdWdc")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BpdWdd")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BpdWdp")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  nodes[slice,] <- m2s

  slice <- which(nodes$move == 2 & grepl("^Bqd", nodes$name))
  m2s <- nodes[slice,]
  tar <- which(m2s$name == "BqdWpp")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqdWdc")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqdWdp")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqdWdd")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  nodes[slice,] <- m2s

  slice <- which(nodes$move == 2 & grepl("^Bqc", nodes$name))
  m2s <- nodes[slice,]
  tar <- which(m2s$name == "BqcWcp")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqcWdc")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqcWdd")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  tar <- which(m2s$name == "BqcWdp")
  if (length(tar) > 0) {
    o <- move_to_start(1:nrow(m2s), tar)
    m2s <- m2s[o,]
  }
  nodes[slice,] <- m2s

  # initialize the first nodes with colors
  hits <- which(nodes$name %in% move12s$node)
  nodes$color <- NA
  nodes$color[hits] <- move12s$col[match(nodes$name[hits], move12s$node)]

  for (move in 3:n_moves) {
    kid_nodes <- nodes$name[which(nodes$move == move)]
    for (i in 1:length(kid_nodes)) {
      my_parents <- links$parent_name[which(links$name == kid_nodes[i])]
      my_parent_counts <- links$game_count[which(links$name == kid_nodes[i])]
      my_parent_colors <- nodes$color[match(my_parents, nodes$name)]
      nodes$color[which(nodes$name == kid_nodes[i])] <- interpolate_colors(my_parent_colors, my_parent_counts)
    }
  }

  # then every link takes the parent node color
  links$edge_col <- nodes$color[match(links$parent_name, nodes$name)]

  # for the links btw each move2 and their move3, we take the move2 node color
  tar <- which(links$move == 2)
  links$edge_col[tar] <- nodes$color[match(links$name[tar], nodes$name)]

  links[which(is.na(links$edge_col)),]

  links <- rename(links, from = parent_name, to = name)

  move1s <- links$to[which(links$move == 1)]
  for (i in 1:length(move1s)) {
    my_child_colors <- links$edge_col[which(links$from == move1s[i])]
    my_child_counts <- links$game_count[which(links$from == move1s[i])]
    links$edge_col[which(links$to == move1s[i])] <- interpolate_colors(my_child_colors, my_child_counts)
  }

  links$edge_col[which(is.na(links$edge_col))] <- gray(0.7, 0.8)

  nodes <- select(nodes, name, move, game_count)

  net <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

  # tree using the reingold tilford algorithm
  l <- layout_as_tree(net, circular = TRUE)

  par(mar = c(0.7, 0.7, 0.7, 0.7))

  # Set up empty plot to reserve the plotting space and draw background rings
  plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "")
  for (i in 1:n_moves) draw_circle(i/n_moves, border = gray(0.6, 0.3))
  
  par(new = TRUE)

  plot(net, edge.arrow.size=0, vertex.color = col.alpha("gray", 0.01),
    vertex.frame.color = col.alpha("gray", 0.01), vertex.size = 0.0001,
    edge.width = eras$line_weight[k] * (links$game_count)^(1/3),
    edge.color = col_alpha(links$edge_col, 1.0), layout = l, vertex.label = NA,
    ylim = c(-1, 1), xlim = c(-1, 1), main = eras$label[k], cex.main = 1.5)
    line_col <- gray(0.1, 0.1)

  m1_x <- l[which(names(V(net)) == "Bpd"),1] / (n_moves)
  m1_y <- (l[which(names(V(net)) == "Bpd"),2] - n_moves) / (n_moves)
  shadowtext(m1_x, m1_y, labels = "4-4", col = "black", bg = "white", cex = 2)

  m1_x <- l[which(names(V(net)) == "Bqd"),1] / (n_moves)
  m1_y <- (l[which(names(V(net)) == "Bqd"),2] - n_moves) / (n_moves)
  shadowtext(m1_x, m1_y, labels = "3-4", col = "black", bg = "white", cex = 2)

  if (any(names(V(net)) == "Bqc")) {
    m1_x <- l[which(names(V(net)) == "Bqc"),1] / (n_moves)
    m1_y <- (l[which(names(V(net)) == "Bqc"),2] - n_moves) / (n_moves)
    shadowtext(m1_x, m1_y, labels = "3-3", col = "black", bg = "white", cex = 2)
  }

  if (any(names(V(net)) == "Bqe")) {
    m1_x <- l[which(names(V(net)) == "Bqe"),1] / (n_moves)
    m1_y <- (l[which(names(V(net)) == "Bqe"),2] - n_moves) / (n_moves)
    shadowtext(m1_x, m1_y, labels = "3-5", col = "black", bg = "white", cex = 2)
  }
  
  points(0, 0, pch = 20, col = "black", cex = 3)

  if (k == 1) {
    shadowtext(0.58, 0.55, labels = "R15,R4", cex = move12_cex)   # qe;qp; " #31688EFF"
    shadowtext(-0.28, 0.74, labels = "R16,R4", cex = move12_cex)  # qd;qp; " #f1c6d0"
    shadowtext(-0.57, 0.54, labels = "R16,E16", cex = move12_cex) # qd;ed; " #a94689"
    shadowtext(-0.77, 0.14, labels = "R16,P17", cex = move12_cex) # qd;oc; " #e9aabd"
    shadowtext(-0.13, -0.78, label = "R16,D17", cex = move12_cex) # qd;dc; " #8cc665"
  }
  if (k == 2) {
    shadowtext(0.77, -0.16, label = "Q16,D4", cex = move12_cex)    # " #FFAF56"
    shadowtext(0.57, -0.55, label = "Q16,D17", cex = move12_cex)   # " #44a340"
    shadowtext(-0.59, -0.53, label = "R16,D4", cex = move12_cex)   # " #ff874b"
    shadowtext(-0.70, 0.36, label = "R16,Q4", cex = move12_cex)   # # "qd;pp;", col = " #6297d1"
    shadowtext(-0.23, -0.75, label = "R16,D16", cex = move12_cex)  # " #46cbe5"
    shadowtext(-0.78, -0.12, label = "R16,D17", cex = move12_cex)  # " #8cc665"
  }
  if (k == 3) {
    shadowtext(0.76, -0.18, label = "Q16,D4", cex = move12_cex)    # " #FFAF56"
    shadowtext(0.27, -0.73, label = "Q16,D17", cex = move12_cex)   # " #44a340"
    shadowtext(0.26, 0.74, labels = "R16,R4", cex = move12_cex)   # qd;qp; " #f1c6d0"
    shadowtext(-0.08, 0.79, label = "R16,Q4", cex = move12_cex)   # # "qd;pp;", col = " #6297d1"
    shadowtext(-0.63, -0.46, label = "R16,D16", cex = move12_cex)  # " #46cbe5"
    shadowtext(-0.6, 0.44, label = "R16,D17", cex = move12_cex)    # " #8cc665"
  }
  if (k == 4) {
    shadowtext(0.70, -0.37, label = "Q16,D4", cex = move12_cex)    # " #FFAF56"
    shadowtext(-0.62, -0.45, label = "Q16,D17", cex = move12_cex)  # " #44a340"
    shadowtext(0.17, -0.77, labels = "Q16,D16", cex = move12_cex)  # " #9BC4DF"
    shadowtext(-0.76, 0.18, label = "Q16,C3", cex = move12_cex)    # " #FF6200"
    shadowtext(-0.24, 0.75, label = "R16,D4", cex = move12_cex)    # " #ff874b"
    shadowtext(-0.58, 0.53, label = "R16,D16", cex = move12_cex)   # " #46cbe5"
    shadowtext(0.25, 0.73, label = "R16,D17", cex = move12_cex)    # " #8cc665"
  }
  if (k == 5) {
    shadowtext(0.69, -0.39, label = "Q16,D4", cex = move12_cex)    # " #FFAF56"
    shadowtext(-0.40, -0.67, label = "Q16,D16", cex = move12_cex)  # " #44a340"
    shadowtext(-0.76, 0.07, labels = "Q16,D17", cex = move12_cex)  # " #9BC4DF"
    shadowtext(0.09, 0.78, label = "R16,D4", cex = move12_cex)     # " #ff874b"
    shadowtext(-0.48, 0.62, label = "R16,D16", cex = move12_cex)   # " #46cbe5"
    shadowtext(0.60, 0.48, label = "R16,D17", cex = move12_cex)    # " #8cc665"
  }
  if (k == 6) {
    shadowtext(0.69, -0.39, label = "Q16,D4", cex = move12_cex)    # " #FFAF56"
    shadowtext(-0.40, -0.67, label = "Q16,D16", cex = move12_cex)  # " #44a340"
    shadowtext(-0.76, 0.07, labels = "Q16,D17", cex = move12_cex)  # " #9BC4DF"
    shadowtext(0.09, 0.78, label = "R16,D4", cex = move12_cex)     # " #ff874b"
    shadowtext(-0.48, 0.62, label = "R16,D16", cex = move12_cex)   # " #46cbe5"
    shadowtext(0.60, 0.48, label = "R16,D17", cex = move12_cex)    # " #8cc665"
  }

}

dev.off()
