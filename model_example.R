library(tidyverse)

library(modelr)
options(na.action = na.warn)


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

model1 <- function(a, data) {
  y = a[1] + data$x * a[2]
  y
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(mse = map2_dbl(a1, a2, sim1_dist))

ggplot(sim1, aes(x, y)) + 
  geom_point() +
  geom_abline(aes(intercept = a1, slope = a2, color = -mse),
              data = filter(models, rank(mse) <= 10))

ggplot(models, aes(a1, a2, colour = -mse)) + 
  geom_point(data = filter(models, rank(mse) <= 10),
             colour = "red", size = 4) +
  geom_point()


ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_abline(data = filter(grid, rank(mse) <= 10), aes(intercept = a1, slope = a2, colour = -mse))

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

mod1 <- lm(y ~ x, data = sim1a)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

mse <-
  model1(coef(mod1), sim1a) %>%
  measure_distance(sim1a)

coef_mse <- optim(c(0, 0), measure_distance, data = sim1a)$par

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

abs_diff <-
  model1(coef(mod1), sim1a) %>%
  measure_distance(sim1a)

coef_abs <- optim(c(0, 0), measure_distance, data = sim1a)$par

df <- tibble(coef_mse, coef_abs)

ggplot(sim1a, aes(x, y)) + geom_point() +
  geom_abline(intercept = as.numeric(df[1, ]), slope = as.numeric(df[2, ])) +
  annotate("text", x = 1.5, y = 20,
           label = paste0("mse = ", round(mse, 2),"\n abs_diff = ", round(abs_diff, 2)))


sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2),
  z = rnorm(30) * y + x
)
mod1 <- lm(y ~ x + z, data = sim1a)

sim1a %>%
  data_grid(x) %>%
  add_predictions(mod1)

sim1a %>%
  add_residuals(mod1) %>%
  ggplot(aes(x, resid)) +
  geom_point()
