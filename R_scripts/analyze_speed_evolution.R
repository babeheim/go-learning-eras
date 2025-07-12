
rm(list = ls())

source("project_support.R")

d <- read.csv("data/games.csv")

periods <- data.frame(
  name = 1955:2024
)
d$period <- d$year

d$move12 <- substr(d$opening, 1, 5)

periods$trait_frq <- NA
for (i in 1:nrow(periods)) {
  tar <- which(d$period == periods$name[i])
  periods$trait_frq[i] <- mean(substr(d$opening[tar], 1, 2) == "pd")
}

moves <- data.frame(
  trait = c("qd;pp", "qd;dp", "pd;dc", "qd;dd", "qd;dc", "pd;dp", "pd;dd", "pd;cq", "qd;qp")
)

period_traits <- expand.grid(
  period = periods$name,
  trait = moves$trait
)
for (i in 1:nrow(period_traits)) {
  tar <- which(d$period == period_traits$period[i])
  if (length(tar) > 0) {
    period_traits$trait_frq[i] <- mean( d$move12[tar] == period_traits$trait[i])
  }
}

add <- select(periods, period = name, trait_frq)
add$trait <- "pd"
period_traits <- bind_rows(period_traits, add)

moves <- bind_rows(moves, data.frame(trait = "pd"))


moves$trait_sd <- NA
period_traits$delta_p <- NA
period_traits$delta_p_su <- NA
for (i in 1:nrow(moves)) {
  tar <- which(period_traits$trait == moves$trait[i])
  period_traits$delta_p[tar] <- c(NA, diff(period_traits$trait_frq[tar]))
  moves$trait_sd[i] <- sd(period_traits$delta_p[tar], na.rm = TRUE)
  period_traits$delta_p_su[tar] <- period_traits$delta_p[tar] / moves$trait_sd[i]
}

periods$delta_p_su_sd <- NA
for (i in 1:nrow(periods)) {
  tar <- which(period_traits$period == periods$name[i])
  periods$delta_p_su_sd[i] <- sd(period_traits$delta_p_su[tar])
}

model <- cmdstan_model(write_stan_file("
data {
  int<lower = 1> N;
  array[N] real y;
}
parameters {
  real mu;
  real<lower = 0> sigma;
}
model {
  mu ~ normal(0, 10);
  sigma ~ exponential(1);
  target += normal_lpdf(y | mu , sigma);
}
"))

period_traits <- dplyr::filter(period_traits, !is.na(delta_p_su))

stan_data <- list(
  N = nrow(period_traits),
  y = period_traits$delta_p_su
)

fit <- model$sample(data = stan_data)



model <- cmdstan_model(write_stan_file("
data {
  int<lower = 1> N;
  int<lower = 1> N_periods;
  array[N] real y;
  array[N] int<lower=1, upper = N_periods> period;
}
parameters {
  real mu;
  array[N_periods] real<lower = 0> sigma;
}
model {
  mu ~ normal(0, 10);
  sigma ~ exponential(1);
  for (i in 1:N) {
    target += normal_lpdf(y[i] | mu, sigma[period[i]]);
  }
}
"))

period_traits$period_i <- match(period_traits$period, periods$name)

stan_data <- list(
  N = nrow(period_traits),
  N_periods = nrow(periods),
  y = period_traits$delta_p_su,
  period = period_traits$period_i
)

fit <- model$sample(parallel_chains = n_chains, chains = n_chains,
  iter_warmup = floor(n_iter/2), iter_sampling = n_iter, adapt_delta = adapt_delta,
  max_treedepth = 15, data = stan_data, step_size = 0.1,
  refresh = 100)

samples <- as_draws_rvars(fit$draws())


model <- cmdstan_model(write_stan_file("
data {
  int<lower=1> N;
  int<lower=1> N_periods;
  array[N] real y;
  array[N] int<lower=1, upper=N_periods> period;
  array[N_periods] real<lower=1, upper=N_periods> period_locs;
}
parameters {
  real mu;
  vector[N_periods] z;       // instead of eta
  real<lower=0> rho;
  real<lower=0> alpha;
}
transformed parameters {
  vector[N_periods] eta;
  vector<lower=0>[N_periods] sigma;
  {
    matrix[N_periods, N_periods] K = gp_exp_quad_cov(period_locs, alpha, rho);
    for (i in 1:N_periods)
      K[i, i] += 1e-6;
    eta = cholesky_decompose(K) * z;
  }
  sigma = exp(eta);
}
model {
  mu ~ normal(0, 10);
  rho ~ normal(6, 1);
  alpha ~ normal(1, 0.5);
  z ~ normal(0, 1); // standard normal
  for (i in 1:N) {
    target += normal_lpdf(y[i] | mu, sigma[period[i]]);
  }
}
"))

stan_data <- list(
  N = nrow(period_traits),
  N_periods = nrow(periods),
  y = period_traits$delta_p_su,
  period = period_traits$period_i,
  period_locs = 1:nrow(periods)
)


fit <- model$sample(parallel_chains = n_chains, chains = n_chains,
  iter_warmup = floor(n_iter/2), iter_sampling = n_iter, adapt_delta = adapt_delta,
  max_treedepth = 15, data = stan_data, step_size = 0.1,
  refresh = 100)

samples <- as_draws_rvars(fit$draws())

sigma_mu <- apply(draws_of(samples$sigma), 2, mean)
sigma_lb <- apply(draws_of(samples$sigma), 2, HPDI)[1,]
sigma_ub <- apply(draws_of(samples$sigma), 2, HPDI)[2,]

events <- list(
  list(
    name = "End of Cultural Revolution",
    year = 1973,
    my_y = 0.65
  ),
  list(
    name = "First Online Go Server",
    year = 1992,
    my_y = 1.3
  ),
  list(
    name = "Alphago Defeats Lee Sedol",
    year = 2016,
    my_y = 0.65
  )
) |> bind_rows()

png("figures/move12_evolution_pace.png", res = 300, units = "in", height = 4.5, width = 7)

par(mar = c(4, 4, 0, 0))

plot(periods$name, sigma_mu, type = "n", ylim = c(0, 2.1), ylab = "speed of trait evolution (s.d.'s)", xlab = "year")
polygon(c(periods$name, rev(periods$name)), c(sigma_lb, rev(sigma_ub)), col = col_alpha("dodgerblue", 0.3), border = NA)
points(periods$name, sigma_mu, type = "l")

points(periods$name, periods$delta_p_su_sd, pch = 20)

abline(v = events$year, lty = 2)
shadowtext(events$year, events$my_y, labels = events$name, srt = 90, col = "black", bg = "white")

dev.off()
