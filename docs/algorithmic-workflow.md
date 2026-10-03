# Algorithmic workflow

This document describes the computational algorithms implemented by the project, in the order used by the canonical analysis workflow. It is intended as a technical companion to `docs/execution-guide.md`: the execution guide explains **how to run the project**, whereas this document explains **what the code computes**.

The descriptions below follow the code as currently written. Where an implementation detail has consequences for interpretation, it is stated explicitly rather than replaced with a presumed intended behavior.

## 1. Overview of the computational pipeline

The canonical runner executes seven analysis scripts in this order:

```text
plot_openings.R
    ↓
plot_opening_trees.R
    ↓
plot_database_coverage.R
    ↓
calc_game_distances.R
    ↓
calc_match_networks.R
    ↓
analyze_opening_diversity.R
    ↓
analyze_speed_evolution.R
```

These scripts can be grouped into four substantive layers:

1. **Opening representation and visualization** — represent early Go moves as SGF coordinate sequences or cumulative board states, then visualize common openings and branching structures.
2. **Cultural diversity and divergence** — quantify the richness, Shannon diversity, and Jensen–Shannon divergence of opening traditions through time.
3. **Player interaction networks** — reconstruct match networks, detect communities, calculate network structure, and compare observed networks with random and lattice reference graphs.
4. **Temporal pace of cultural change** — track annual changes in selected opening traits and estimate a smoothly varying variance in standardized frequency change with a Gaussian-process model.

The main input is `data/games.csv`, supplemented by `data/eras.csv`, `data/move12s.csv`, and `data/players.csv` where needed.

---

# 2. Shared representations and statistical quantities

Several algorithms recur across multiple scripts. Understanding these first makes the script-level workflow easier to interpret.

## 2.1 SGF opening strings

The `opening` column of `games.csv` stores early game moves as semicolon-delimited SGF coordinates. A simplified opening might look like:

```text
pd;dd;pq;dp;...
```

Each coordinate is a two-letter SGF board location on a 19 × 19 board.

Two related encodings are used in the analyses.

### Ordered cumulative opening prefixes

`extract_game_moves(..., cumulative = TRUE)` converts each game into a sequence of prefixes. If the first four moves are

```text
pd;dd;pq;dp
```

then the row returned for that game is conceptually:

```text
move 1: pd
move 2: pddd
move 3: pdddpq
move 4: pddd pqdp
```

with the actual strings formed by direct concatenation without separators.

Thus, column `j` does **not** contain only the move played at turn `j`. It contains the complete ordered opening history through turn `j`.

Consequently, quantities such as entropy calculated on column `j` measure the diversity of **opening sequences through depth `j`**.

### Cumulative board-state nodes

`extract_game_nodes()` uses a different representation for opening trees.

For each game:

1. The first `n_moves` SGF coordinates are extracted.
2. Odd-numbered moves are prefixed with `B`, and even-numbered moves with `W`.
3. At each depth `j`, all color-coordinate tokens observed through that depth are sorted.
4. The sorted tokens are concatenated into a node identifier.

For example, a sequence beginning

```text
B[pd], W[dd], B[pq]
```

might produce cumulative node identities equivalent to:

```text
Bpd
BpdWdd
BpdBpqWdd
```

The sorting step means the node represents the **set of colored stone placements reached by that depth**, rather than the exact chronological path by which those placements were reached. Different move orders that reach the same represented state can therefore collapse onto the same node.

This is why the resulting opening structure is more accurately understood as a directed state graph than as a strictly unique-history tree.

## 2.2 `move12` traits

Most opening-frequency analyses define

```r
games$move12 <- substr(games$opening, 1, 6)
```

which corresponds to the first two SGF moves including the trailing semicolon, for example:

```text
pd;dd;
```

The speed-of-evolution analysis instead uses

```r
d$move12 <- substr(d$opening, 1, 5)
```

so its two-move traits omit the final semicolon:

```text
pd;dd
```

The distinction is purely representational but should be preserved when reproducing trait definitions.

## 2.3 Shannon entropy

The project defines Shannon entropy as

\[
H(X) = \sum_i p_i \log\left(\frac{1}{p_i}\right)
     = -\sum_i p_i \log p_i,
\]

using natural logarithms by default.

In code:

```r
entropy <- function(x, base = exp(1)) {
  p <- prop.table(table(x))
  sum(p * log(1/p, base = base))
}
```

Entropy is therefore measured in nats unless another base is supplied.

## 2.4 Effective diversity

Many analyses exponentiate Shannon entropy:

\[
D = e^H.
\]

This is the Hill number of order 1: the number of equally frequent categories that would produce the observed Shannon entropy.

The code refers to this quantity as *diversity*. For example:

```r
exp(entropy(games$move12))
```

is the effective number of two-move opening types.

This distinction matters because `richness()` is separately defined as the literal number of unique categories.

## 2.5 Richness

Richness is simply

\[
R = \text{number of distinct observed categories}.
\]

The implementation is:

```r
richness <- function(x) {
  length(unique(x))
}
```

`move_richness()` applies this independently to every opening depth in a matrix of cumulative opening prefixes.

## 2.6 Evenness

In several parts of the opening-diversity analysis, evenness is calculated as

\[
E = \frac{e^H - 1}{R - 1},
\]

where `R` is richness and `e^H` is effective Shannon diversity.

This maps a distribution toward 1 when the observed variants are relatively evenly represented and toward 0 when diversity is concentrated in a small subset of the observed variants.

## 2.7 Jensen–Shannon divergence

`calc_js_divergence(x, y)` compares the empirical categorical distributions in two samples.

The algorithm is:

1. Form the union of all categories observed in either sample.
2. Count each category separately in `x` and `y`.
3. Convert counts to probability vectors `P` and `Q`.
4. Pass the two vectors to `philentropy::JSD(..., unit = "log2")`.

Conceptually,

\[
\operatorname{JSD}(P,Q)
=
\frac{1}{2}D_{KL}(P\|M)
+
\frac{1}{2}D_{KL}(Q\|M),
\]

where

\[
M = \frac{P+Q}{2}.
\]

Because the implementation uses base-2 logarithms, the conventional two-distribution JSD is expressed in bits. Identical empirical distributions have JSD 0; increasingly different distributions produce larger values.

## 2.8 HPDI calculation

Posterior uncertainty in the speed-of-evolution analysis is summarized with a local `HPDI()` function.

For a posterior sample of size `n` and target probability `p`, the implementation:

1. sorts the posterior draws;
2. sets an index width to `floor(p * n)`;
3. considers every interval between two sorted draws separated by that width;
4. selects the interval with the smallest numerical width.

The default target probability is 0.89.

---

# 3. `plot_openings.R`: opening examples and local continuation frequencies

This script is primarily visual, but it contains several substantive algorithms.

## 3.1 Annual komi summaries

For each year from 1920 through 2024, the script identifies all games in that year and calculates:

- mean komi;
- median komi;
- modal komi;
- first quartile;
- third quartile.

The resulting series is used to visualize changes in compensation to White through time.

## 3.2 Parsing example openings

Selected SGF strings are parsed with internal `kaya` functions. The resulting move table supplies:

- board column;
- board row;
- stone color;
- SGF coordinate.

A 19 × 19 board is drawn directly with base graphics, and moves are overlaid in chronological order.

## 3.3 Empirical next-move algorithm

For several hand-selected opening prefixes, the script estimates likely continuations from the complete game database.

For a focal opening prefix:

1. Collapse the focal moves into the same semicolon-delimited coordinate string used in `games$opening`.
2. Find every database game whose opening contains that prefix with `grep()`.
3. Extract the two-character SGF coordinate immediately following the prefix.
4. Tabulate those next moves and sort them from most to least frequent.
5. Convert counts to empirical frequencies `p_i`.
6. Calculate the effective number of next moves:

   \[
   N_{\mathrm{eff}} = \exp\left(\sum_i p_i \log(1/p_i)\right).
   \]

7. Retain at most the top

   \[
   \lceil N_{\mathrm{eff}} \rceil
   \]

   next moves.
8. Apply an additional frequency threshold, retaining only next moves with empirical frequency greater than 0.11.
9. Convert surviving frequencies to percentages and draw those percentages at the corresponding board coordinates.

Thus the annotations are not model-based predictions. They are filtered empirical continuation frequencies conditional on the selected opening prefix.

## 3.4 Common two- and three-move opening boards

The latter part of the script contains manually specified collections of common opening patterns. Each SGF pattern is parsed and drawn as a miniature board.

`sgf_to_korschelt()` converts SGF coordinates into traditional Go board notation by:

- mapping SGF columns `a:s` to `A:T`, excluding `I`;
- reversing the row index so SGF row positions become conventional numbered rows;
- joining multi-move sequences with commas.

For common two-move openings, board background colors are matched from `move12s.csv`, creating a visual correspondence with colors used later in frequency and opening-tree plots.

---

# 4. `plot_opening_trees.R`: pruned opening-state graphs

This script constructs one opening-state graph for each historical era.

## 4.1 Equal-size sampling across eras

The script sets

```r
n_sampled_games <- 2900
```

and samples exactly 2,900 games from every era.

Because eras differ greatly in database coverage, this equalizes the number of games contributing to each era-specific opening graph. The sample is without replacement.

The project-level random seed is set before this script runs, so the samples are reproducible within the canonical workflow as long as preceding random-number use is unchanged.

## 4.2 State construction through seven moves

For each era, the first seven moves of each sampled game are converted with `extract_game_nodes()` into cumulative colored board-state identifiers.

The result is a matrix:

- rows = games;
- columns = move depth 1 through 7;
- cells = cumulative board-state identifier at that depth.

## 4.3 Recursive branch construction

The graph begins with a synthetic root node at move 0.

At each move depth `m`:

1. Identify all currently tracked parent nodes at depth `m - 1`.
2. Find the sampled games belonging to each parent state.
3. Tabulate their observed child states at depth `m`.
4. Sort child states by game count, largest first.
5. Before pruning, calculate for the parent:
   - **child richness** = number of distinct observed child states;
   - **child entropy** = Shannon entropy of the child-state distribution.

## 4.4 Entropy-based branch pruning

The default pruning rule is based on the effective number of child states.

For a parent with child entropy `H`, calculate

\[
D_{child}=e^H.
\]

Then retain the most frequent

\[
\min(\lceil D_{child}\rceil, R_{child})
\]

children, where `R_child` is the literal number of observed child states.

This produces an adaptive cutoff:

- a highly concentrated branch keeps relatively few children;
- a more diverse branch keeps more children;
- a node with very few observations still retains its observed downstream paths.

An alternative absolute-count cutoff exists in the code but is disabled.

## 4.5 Multiple parents and state convergence

When a candidate child state has already been added to the node table, it is not duplicated. A new edge is nevertheless added.

Therefore a node can have more than one parent when different preceding histories converge on the same cumulative state representation.

The code explicitly calculates `n_parents` for each node. The structure is consequently a **directed acyclic state graph**, even though it is subsequently visualized with a tree-layout algorithm.

## 4.6 Node and edge frequencies

For each node, `game_count` is calculated by summing the counts of all incoming links.

For each link, `game_count` is the number of sampled games following that specific parent-to-child transition.

Edge width in the final plot is proportional to

\[
(\text{game count})^{1/3},
\]

multiplied by an era-specific line-weight constant. The cube-root transformation compresses large count differences so low-frequency branches remain visible.

## 4.7 Color propagation through the graph

Two-move states are assigned the predefined colors in `move12s.csv`.

For deeper nodes, color is inherited by weighted interpolation from parent colors. If a node has parent colors `c_1, ..., c_k` and incoming game counts `w_1, ..., w_k`, the RGB values of the colors are combined using normalized weights

\[
\tilde w_i = \frac{w_i}{\sum_j w_j}.
\]

Thus a convergent state receives a mixture representing the relative frequencies of its incoming colored lineages.

Edges generally take the color of their parent state, with special handling around the first two move levels so the early branches visually match the predefined opening palette.

## 4.8 Layout

The graph is converted to an `igraph` object and passed to a circular Reingold–Tilford tree layout:

```r
layout_as_tree(net, circular = TRUE)
```

Before layout, several important first- and second-move nodes are manually reordered. This is a display algorithm rather than a statistical transformation: it stabilizes the visual orientation of historically important branches across panels.

Concentric circles mark move depth, and selected opening branches receive human-readable Go-coordinate labels.

---

# 5. `plot_database_coverage.R`: temporal database coverage

This script summarizes the amount and composition of source material represented in `games.csv`.

## 5.1 Annual game counts

For every year from 1600 through 2024, the code counts the number of game records with that year.

These counts are plotted on a logarithmic y-axis, separately for the periods before and after 1945.

## 5.2 Annual player counts

The code also attempts to calculate the number of unique players represented in each year, both overall and by language/national grouping.

The intended conceptual quantity is annual unique-player coverage. However, the current implementation has an important detail discussed in the implementation-notes section below: several expressions use `player_id_black` for both the Black and White selections.

The plots therefore document the database as calculated by the current code, but these player-count lines should be reviewed before being treated as final coverage statistics.

---

# 6. `calc_game_distances.R`: sequence edit distance and multidimensional scaling

This script constructs a low-dimensional representation of opening-sequence similarity.

## 6.1 Balanced era sampling

For each era in `eras.csv`, the algorithm samples exactly 1,000 games without replacement.

If there are `E` eras, the distance analysis therefore contains

\[
1000E
\]

games, with equal representation from each era regardless of original database size.

The project random seed is reset immediately before sampling:

```r
set.seed(project_seed)
```

so this particular subsample is reproducible independently of prior stochastic operations.

## 6.2 Fifty-move ordered sequence representation

For each sampled game, the first 50 moves are converted into an ordered cumulative opening prefix.

For the distance calculation, SGF coordinates are first mapped to distinct Unicode symbols. There are 361 possible board coordinates and at least 361 symbols are generated, so each board coordinate is represented by a single symbol rather than a two-character SGF string.

At move 50, each game is therefore represented as a 50-symbol sequence.

This encoding is important because edit distance then treats one board move as one sequence element.

## 6.3 Levenshtein distance

For every pair of sampled games, the script calculates Levenshtein distance on the 50-move strings:

```r
stringdistmatrix(..., method = "lv")
```

Levenshtein distance is the minimum number of single-element:

- insertions;
- deletions;
- substitutions

required to transform one sequence into another.

Thus this distance treats opening similarity as **ordered sequence similarity**, not geometric similarity between board positions.

A substitution of one move coordinate for another costs one unit regardless of how near or far apart those coordinates are on the Go board.

## 6.4 Classical multidimensional scaling

The complete pairwise distance matrix is passed to classical multidimensional scaling:

```r
cmdscale(distance_matrix, k = 2)
```

This finds a two-dimensional Euclidean configuration that approximates the pairwise edit-distance structure.

The two resulting MDS axes are independently standardized with `scale()` to mean zero and unit standard deviation.

The saved object contains:

```text
x
 y
 hash_id
```

and is cached as:

```text
data/distance_mds.RDS
```

If that file already exists, the distance matrix and MDS are not recomputed.

---

# 7. `calc_match_networks.R`: player networks, communities, and small-world structure

This script reconstructs player interaction networks for the same variable-resolution historical periods used by the diversity analysis.

## 7.1 Historical periodization

Each game receives a period label according to its year:

- before 1850: 50-year bins, represented by their midpoint-like label;
- 1850 through 1950: 10-year bins, represented by a `+5` label;
- after 1950: individual years.

In code:

\[
\text{period}=
\begin{cases}
y - (y \bmod 50) + 25, & y < 1850,\\
y - (y \bmod 10) + 5, & 1850 \le y \le 1950,\\
y, & y > 1950.
\end{cases}
\]

This gives higher temporal resolution where the database is denser.

## 7.2 Opening diversity by period

For each period, the script calculates two-move opening diversity as

\[
D_t = \exp(H(\text{move12}_t)).
\]

These values are later compared descriptively with network properties.

## 7.3 Match edgelists

For each period:

1. Retain games in which both player identities are known.
2. For each game, sort the two player IDs so the lower ID is always `from` and the higher ID is always `to`.
3. Group identical unordered player pairs.
4. Count the number of games between each pair as `n_games`.

Each row of the resulting edgelist is therefore a unique **undirected dyad**, with match frequency stored as an edge weight.

Period-specific edgelists are written to files such as:

```text
data/edgelist_1975.csv
```

## 7.4 Network construction

For each edgelist, an undirected `igraph` network is built:

- vertices = identified players appearing in at least one retained match;
- edges = observed player dyads;
- edge weight = number of games played by the dyad.

Player language information is mapped to vertex color for visualization.

## 7.5 Basic network statistics

For every period, the code records:

- number of players `n_players`;
- number of dyads `n_dyads`;
- mean degree;
- median degree;
- standard deviation of degree;
- number of connected components;
- size of the largest connected component.

For an undirected graph, degree is the number of distinct opponents represented by a player, not the number of games played.

## 7.6 Component diversity

Let connected component sizes be `n_1, ..., n_K`, and let

\[
p_k = \frac{n_k}{N}.
\]

The script calculates component diversity as

\[
D_{components}
=
\exp\left(\sum_k p_k \log(1/p_k)\right).
\]

This is the effective number of equally sized connected components represented by the observed component-size distribution.

## 7.7 Fast-greedy community detection

Communities are detected with `igraph::cluster_fast_greedy()`, the Clauset–Newman–Moore agglomerative modularity algorithm for undirected graphs.

The graph has `n_games` stored as its edge-weight attribute when the algorithm is called.

For the detected partition, the script records:

- number of communities;
- average community size;
- standard deviation of community size;
- modularity;
- nominal assortativity by community membership.

## 7.8 Community diversity

Community membership is treated as a categorical variable over players. If `p_c` is the fraction of players assigned to community `c`, the code calculates

\[
D_{community}=\exp\left(-\sum_c p_c \log p_c\right).
\]

This is the effective number of equally sized communities.

It differs from the literal number of detected communities because it discounts very small communities.

## 7.9 Community assortativity

The code calculates:

```r
assortativity_nominal(g, as.numeric(membership(comm)))
```

which measures the tendency for edges to connect players assigned to the same detected community rather than different communities.

The plotting code describes this quantity as community homophily or associativity.

## 7.10 Largest-component statistics

Several graph-distance and clustering quantities require connected paths, so the script extracts the largest connected component:

```r
g_main <- induced_subgraph(
  g,
  components(g)$membership == which.max(components(g)$csize)
)
```

Within this main component it calculates:

- number of players;
- fast-greedy community count;
- average community size;
- average local transitivity / clustering coefficient;
- mean shortest-path length.

These quantities receive the `_mc` suffix in the period table.

## 7.11 Random reference network

For each observed main component with `N` nodes and `M` edges, the script generates one random `G(N,M)` graph:

```r
sample_gnm(n_nodes, n_ties)
```

It records the random graph's:

- clustering coefficient `C_rand`;
- mean path length `L_rand`.

The random graph therefore preserves node count and edge count, but not the observed degree sequence or edge weights.

## 7.12 Lattice reference network

The code estimates observed mean degree as

\[
k = \left\lfloor \frac{2M}{N} \right\rfloor
\]

and sets

\[
\text{nei}=\left\lfloor\frac{k}{2}\right\rfloor.
\]

It then generates a one-dimensional Watts–Strogatz-style ring lattice with rewiring probability zero:

```r
sample_smallworld(
  dim = 1,
  size = n_nodes,
  nei = nei,
  p = 0
)
```

and records:

- lattice clustering `C_lattice`;
- lattice mean path length `L_lattice`.

## 7.13 Normalized small-world indices

For the observed largest component, let:

- `C` = observed clustering;
- `L` = observed mean path length;
- `C_rand`, `L_rand` = random-reference values;
- `C_lattice`, `L_lattice` = lattice-reference values.

The code defines a normalized path-length index:

\[
L_i =
\frac{L-L_{lattice}}
     {L_{rand}-L_{lattice}},
\]

and a normalized clustering index:

\[
C_i =
\frac{C-C_{rand}}
     {C_{lattice}-C_{rand}}.
\]

The comments interpret these as:

- `L_i = 0`: lattice-like path length;
- `L_i = 1`: random-like path length;
- `C_i = 0`: random-like clustering;
- `C_i = 1`: lattice-like clustering.

The final small-world index is

\[
SWI = L_i C_i.
\]

A network scores highly when it combines relatively random-like short path lengths with relatively lattice-like high clustering.

## 7.14 Fruchterman–Reingold network visualization

For network figures, edge weight is converted to layout distance through its reciprocal:

```r
layout_with_fr(g, weights = 1 / edge_weight)
```

Thus players who have played more games together exert a stronger attraction / shorter effective layout distance.

The many subsequent figures are descriptive comparisons of opening diversity, population size, community structure, degree, connectivity, and small-world measures through historical time.

---

# 8. `analyze_opening_diversity.R`: temporal diversity and cultural turnover

This is the central opening-diversity analysis.

## 8.1 Opening-prefix matrix

The first 26 moves of every game are converted to ordered cumulative prefixes with:

```r
extract_game_moves(
  games$opening,
  n_moves = 26,
  cumulative = TRUE
)
```

Column `j` therefore identifies the complete opening sequence through move `j`.

This matrix is later used for move-depth-specific entropy and divergence.

## 8.2 Overall two-move opening statistics

The script defines each game's two-move opening (`move12`) and computes:

### Richness

\[
R = \text{number of distinct move12 strings}.
\]

### Effective Shannon diversity

\[
D = e^{H(move12)}.
\]

### Concentration counts

Move types are ranked by observed frequency and cumulative frequency is calculated. The script records the number of ranked opening types falling below cumulative thresholds of 0.90 and 0.99.

These statistics quantify how many common openings account for most games.

## 8.3 Variable-resolution historical periods

The same periodization used by the network analysis is assigned:

- 50-year bins before 1850;
- decadal bins from 1850 through 1950;
- annual bins after 1950.

The period table records the number of games in each observed period.

## 8.4 Ordering opening types for stacked-frequency plots

Every observed two-move opening is tabulated.

For each opening, the code counts its occurrences in:

- an “old” group: early modern + imperial eras;
- a “new” group: cold war + international + internet + superhuman-AI eras.

The main initial ordering is descending old-period count. Several named opening types are then manually moved to the front or back of the ordering so the stacked polygons have a desired visual arrangement.

For every period-opening combination, the code calculates:

\[
f_{o,t}
=
\frac{n_{o,t}}{N_t},
\]

where `n_{o,t}` is the number of games using opening `o` in period `t` and `N_t` is the number of games in the period.

These frequencies form the stacked opening-composition plots.

## 8.5 Equal-sample bootstrap across periods

To make opening diversity less directly dependent on unequal database coverage, the script uses a period-stratified bootstrap.

Parameters are:

```text
bootstrap iterations = 100
sample size per period = 100 games
sampling = with replacement
```

For each bootstrap iteration:

1. For every historical period, draw 100 games from that period with replacement.
2. Combine these period-specific samples.
3. For each period, calculate:
   - effective two-move opening diversity;
   - unique player count;
   - effective player diversity.
4. For each period after the first, calculate JSD between its two-move opening distribution and the immediately preceding period's distribution.
5. Save the iteration-specific statistics.

After all 100 iterations, the code groups results by period and takes the arithmetic mean of each statistic across bootstrap iterations.

No bootstrap quantile interval is retained in the current implementation; the bootstrap is used to obtain equal-effort mean estimates.

## 8.6 Period-to-period cultural divergence

For consecutive periods `t-1` and `t`, opening turnover is measured as

\[
JSD(P_{t-1},P_t),
\]

where the distributions are empirical frequencies of two-move opening types in the bootstrap samples.

This is a measure of compositional change, not simply gain or loss of diversity. Two periods can have similar Shannon diversity but high JSD if the opening types making up that diversity differ substantially.

## 8.7 Move-depth-specific diversity

The code next samples up to 100 games without replacement from each period and calculates entropy separately at each opening depth.

Because column `j` of `game_moves` is the cumulative prefix through move `j`, the resulting quantity is

\[
H_j(t)
=
H(\text{opening prefixes through move }j\text{ in period }t).
\]

It is therefore **cumulative opening-sequence diversity through move `j`**, not the marginal entropy of the move made at turn `j`.

For example, at move 5, two games are treated as different variants if any of their first five moves differ.

## 8.8 Move-depth-specific temporal divergence

For each period after the first, and for each move depth `j`, the script calculates JSD between cumulative opening-prefix distributions in the current and previous periods:

\[
JSD_j(t)
=
JSD(P_{j,t-1},P_{j,t}).
\]

This gives a depth-resolved picture of historical change: early move positions can be stable while longer opening sequences change substantially, or vice versa.

## 8.9 Era-level cumulative opening diversity

A separate analysis compares the six broad eras rather than the variable-resolution periods.

For each era:

1. sample up to 2,000 games without replacement;
2. calculate cumulative-prefix entropy at each of the first 26 moves;
3. calculate prefix richness at each depth;
4. calculate evenness from effective diversity and richness.

The main stored era-level result is entropy by move depth.

## 8.10 Diversity change between consecutive eras

The final diversity figure compares each era with the preceding era. At each move depth `j`, it plots

\[
\Delta H_j(e)
=
H_j(e)-H_j(e-1).
\]

Positive values indicate that the repertoire of opening sequences through that depth became more diverse relative to the preceding era; negative values indicate contraction.

## 8.11 Diversity versus observed population size

The script associates each period with a broad era and compares bootstrap-estimated opening diversity with bootstrap-estimated player counts.

The player-count axis is logarithmic. This analysis is descriptive; no inferential population-size model is fitted in this script.

---

# 9. Country-specific opening-diversity analyses

The repository also contains separate scripts for China, Japan, and Korea:

```text
analyze_opening_diversity_CN.R
analyze_opening_diversity_JP.R
analyze_opening_diversity_KR.R
```

These are not currently run by the canonical project runner, but their algorithm is a restricted version of the main opening-diversity analysis.

For each country code:

1. retain only games in which **both** Black and White have that language/country code;
2. define the same two-move opening traits;
3. apply the same variable-resolution historical periodization;
4. calculate period-specific opening frequencies;
5. perform the same 100-iteration, 100-games-per-period bootstrap;
6. calculate effective opening diversity;
7. calculate consecutive-period JSD;
8. plot the resulting opening composition, diversity, and divergence for the modern time range.

The substantive algorithm is therefore held constant while the interaction pool is restricted to within-country games.

---

# 10. `analyze_speed_evolution.R`: rate of opening-trait change

This script asks a different question from the entropy analyses. Instead of measuring the number of opening variants, it estimates how rapidly selected trait frequencies change from year to year.

## 10.1 Annual time scale

The analysis uses years 1955 through 2024 as periods:

```r
periods$name <- 1955:2024
```

Every game is assigned directly to its calendar year.

## 10.2 Focal opening traits

The analysis tracks ten traits:

- nine specific two-move opening strings;
- one first-move trait: Black's first move is `pd`.

For every trait `g` and year `t`, the empirical frequency is

\[
p_{g,t}
=
\frac{\text{games with trait }g\text{ in year }t}
       {\text{games in year }t}.
\]

For the first-move `pd` trait, the numerator is the number of games whose first SGF coordinate is `pd`.

## 10.3 Year-to-year frequency change

For each trait independently, the code calculates

\[
\Delta p_{g,t}
=
p_{g,t}-p_{g,t-1}.
\]

The first year for each trait has no previous-year difference and is assigned `NA`.

## 10.4 Standardization by each trait's long-run volatility

Traits naturally differ in how much their frequencies fluctuate. To place them on a common scale, the script calculates for each trait:

\[
s_g
=
SD_t(\Delta p_{g,t}).
\]

Each annual frequency change is then standardized:

\[
y_{g,t}
=
\frac{\Delta p_{g,t}}{s_g}.
\]

Thus a value of `y = 1` means the trait changed by one of its own historical standard deviations of annual frequency change.

The model therefore measures cultural-change speed in **trait-specific standard-deviation units**, rather than raw percentage-point change.

## 10.5 Raw annual pace statistic

Before fitting the Bayesian models, the script calculates the standard deviation of `y` across focal traits within each year:

\[
s^{raw}_t = SD_g(y_{g,t}).
\]

These empirical annual standard deviations are plotted as points in the final figure.

## 10.6 Model 1: constant variance

The first Stan model assumes all standardized trait changes arise from one normal distribution:

\[
y_i \sim Normal(\mu,\sigma).
\]

Priors are:

\[
\mu \sim Normal(0,10),
\]

\[
\sigma \sim Exponential(1).
\]

This is a baseline model with one global pace parameter.

Its fit is not used in the final plot; it serves as an intermediate modeling step.

## 10.7 Model 2: independent annual variances

The second model gives each year its own scale parameter:

\[
y_i \sim Normal(\mu,\sigma_{t[i]}).
\]

with

\[
\sigma_t \sim Exponential(1)
\]

independently across years.

The mean `mu` remains common across all observations.

This allows the pace of cultural change to vary freely from year to year but provides no temporal smoothing between adjacent years.

This fit is also superseded by the third model for the final analysis.

## 10.8 Model 3: Gaussian-process evolution of annual variance

The final model treats the log annual standard deviation as a smooth latent function of time.

Let

\[
\eta_t = \log \sigma_t.
\]

The code constructs a squared-exponential Gaussian-process covariance matrix:

\[
K_{tt'}
=
\alpha^2
\exp\left[
-\frac{(x_t-x_{t'})^2}{2\rho^2}
\right],
\]

where annual locations are coded as equally spaced integers.

A small diagonal jitter (`1e-6`) is added for numerical stability.

Instead of sampling the correlated latent vector directly, the model uses a non-centered parameterization:

\[
z \sim Normal(0,I),
\]

\[
\eta = L_K z,
\]

where `L_K` is the Cholesky factor of `K`.

The annual pace parameter is then

\[
\sigma_t = e^{\eta_t}.
\]

The observation model is

\[
y_i \sim Normal(\mu,\sigma_{t[i]}).
\]

Priors are:

\[
\mu \sim Normal(0,10),
\]

\[
\rho \sim Normal(6,1), \qquad \rho > 0,
\]

\[
\alpha \sim Normal(1,0.5), \qquad \alpha > 0.
\]

Here:

- `rho` controls the temporal correlation length: larger values enforce smoother changes across years;
- `alpha` controls the amplitude of latent variation in log standard deviation;
- `sigma_t` is the modeled year-specific pace of change.

## 10.9 Posterior summary

For each year, the posterior draws of `sigma_t` are summarized by:

- posterior mean;
- lower 89% HPDI bound;
- upper 89% HPDI bound.

The final figure displays:

- a line for posterior mean `sigma_t`;
- a shaded 89% HPDI band;
- raw annual cross-trait standard deviations as points;
- selected historical-event annotations.

The figure therefore contrasts a smoothed latent estimate of evolutionary pace with the unsmoothed empirical annual statistic.

---

# 11. Relationship among the principal cultural measures

The project uses several measures that answer different questions and should not be conflated.

| Quantity | Question answered | Unit / interpretation |
|---|---|---|
| Richness | How many distinct opening variants are observed? | Count of categories |
| `exp(entropy)` | How many effectively common variants are represented? | Effective number of categories |
| JSD | How different are the compositions of two repertoires? | Distributional divergence |
| Cumulative-prefix entropy | How diverse are complete opening histories through move `j`? | Shannon entropy |
| Levenshtein distance | How many sequence edits separate two 50-move openings? | Edit operations |
| Network community diversity | How many effectively large player communities exist? | Effective number of communities |
| `sigma_t` in speed model | How variable are standardized trait-frequency changes in year `t`? | Trait-SD units |

In particular, high diversity does not necessarily imply rapid change. A year can contain many evenly represented opening types but have low temporal turnover, while another can contain relatively few types whose frequencies change rapidly.

---

# 12. Important implementation details and review points

The descriptions above intentionally follow the current code. Several implementation details should be kept visible because they affect either reproducibility or interpretation.

## 12.1 Player counts duplicate Black IDs in some analyses

In `analyze_opening_diversity.R`, bootstrap player vectors are currently constructed as:

```r
players_sub <- c(
  games_sub$player_id_black[tar],
  games_sub$player_id_black[tar]
)
```

so Black-player IDs are duplicated and White-player IDs are not included.

Likewise, `plot_database_coverage.R` contains several player-count expressions that select `player_id_black` for both Black- and White-filtered game sets.

This does **not** affect the match-network construction, which uses both `player_id_black` and `player_id_white` to create dyads. It does affect the player-count and effective-player-diversity quantities in the affected scripts.

If the intended quantity is the set of all participants, these expressions should use `player_id_white` for the White side.

## 12.2 The 1950 period boundary collides with 1955

The periodization rule includes 1950 in the decadal expression:

```r
year - year %% 10 + 5
```

so year 1950 receives period label 1955.

Years greater than 1950 are represented individually, meaning actual year 1955 also receives period label 1955.

As coded, games from 1950 and 1955 are therefore pooled into the same period. If this was not intentional, the transition between decadal and annual resolution should be redefined.

## 12.3 The opening-type filter in the main diversity script is effectively exhaustive

`moves` is initially constructed from all unique observed `move12` values. The later test

```r
mean(games$move12 %in% moves$name)
```

therefore describes an exhaustive set under the current construction, and the subsequent filter to `moves$name` should not remove ordinary observed opening types.

The nearby comment saying the set represents 98.6% of games appears inconsistent with the current implementation and likely reflects an earlier version in which only selected opening types were retained.

## 12.4 Bootstrap outputs are means, not uncertainty intervals

The opening-diversity bootstrap equalizes period sample size and averages statistics over 100 resamples. The current code does not retain bootstrap quantiles or standard errors.

The resulting values should therefore be interpreted as resampling-based standardized estimates rather than confidence/credible intervals.

## 12.5 Random and lattice network references use one graph per period

The small-world calculation creates a single `G(N,M)` random graph and a single lattice reference graph for each period.

The reference statistics therefore contain Monte Carlo variation. A more stable reference calculation would generate many random/reference graphs per period and average their clustering and path-length statistics, ideally retaining uncertainty.

## 12.6 The lattice does not explicitly preserve the observed edge count

The random reference graph matches both observed `N` and `M`. The lattice is instead parameterized from floored mean degree through `nei`.

Consequently, the lattice's exact edge count need not always equal the observed edge count.

## 12.7 Stan sampling has no explicit seed

The project sets an R seed, but the calls to `model$sample()` do not currently pass an explicit CmdStan seed.

The same data and model should yield statistically equivalent posterior inference across runs, but posterior draws are not guaranteed to be identical.

## 12.8 The first two Stan models are intermediate and overwritten

`analyze_speed_evolution.R` fits:

1. a constant-variance model;
2. an independent-year variance model;
3. the Gaussian-process variance model.

The `fit` object is overwritten after each model. Only the third model's posterior draws are used for the final plotted posterior summaries.

Thus the first two models currently function as developmental/baseline fits rather than retained model-comparison outputs.

## 12.9 Game distance is sequence distance, not board geometry

The Levenshtein analysis treats every move coordinate as a discrete symbol. Replacing `pd` with a neighboring coordinate and replacing it with the opposite corner both incur one substitution.

The distance therefore quantifies similarity of symbolic opening sequences, not the spatial magnitude of board changes.

## 12.10 Opening-tree nodes collapse some historical paths

Because `extract_game_nodes()` sorts cumulative colored placements, two chronological sequences reaching the same represented set of placements can map to the same node.

This is deliberate or at least consequential: the displayed object is a state-transition graph with possible convergence, not a pure trie of exact move sequences.

---

# 13. Concise dataflow summary

The main algorithmic dataflow can be summarized as:

```text
games.csv
   │
   ├── SGF opening strings
   │      ├── first 2 moves ──> frequency, entropy, JSD
   │      ├── cumulative prefixes ──> depth-specific diversity/JSD
   │      ├── cumulative states ──> pruned opening graphs
   │      └── first 50 moves ──> Levenshtein distances ──> 2D MDS
   │
   ├── player IDs
   │      └── period-specific dyads ──> weighted match networks
   │                                  ├── components
   │                                  ├── communities
   │                                  ├── clustering/path length
   │                                  └── small-world indices
   │
   └── calendar year
          ├── variable-resolution historical periods
          │      └── opening diversity and network structure
          │
          └── annual 1955–2024 frequencies
                 └── trait Δp
                       └── trait-standardized Δp
                             └── GP-smoothed annual σ
```

The project therefore uses the same historical game corpus to describe cultural structure at several scales: individual opening sequences, distributions of opening traditions, interaction networks among players, and the temporal pace at which selected opening traits change.
