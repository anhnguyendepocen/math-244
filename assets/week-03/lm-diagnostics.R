## ------------------------------------------------------------------------
set.seed(794)
n <- 80
x <- runif(n)
beta_1 <- 3.5
sigma <- .6
y <- beta_1 * x + rnorm(n, sd = sigma)
df <- data.frame(x, y)
m1 <- lm(y ~ x, data = df)

## ----echo = FALSE--------------------------------------------------------
library(ggplot2)
ggplot(df, aes(x = x, y = y)) + 
  geom_point(col = "steelblue") + 
  geom_abline(intercept = m1$coef[1], slope = m1$coef[2]) +
  theme_bw()

## ---- fig.width = 4, fig.align="center"----------------------------------
plot(m1, 1)

## ---- fig.width = 4, fig.align="center"----------------------------------
plot(m1, 2)

## ---- fig.width = 4, fig.align="center"----------------------------------
plot(m1, 3)

## ---- fig.width = 4, fig.align="center"----------------------------------
plot(m1, 4)

## ---- fig.width = 4, fig.align="center"----------------------------------
par(mfrow = c(2, 2))
plot(m1)

## ------------------------------------------------------------------------
it <- 5000
beta_hats <- rep(NA, it)
capture <- rep(FALSE, it)
for(i in 1:it) {
  y <- beta_1 * x + rnorm(n, sd = sigma)
  m <- lm(y ~ x)
  beta_hats[i] <- m$coef[2]
  ci <- confint(m)[2, ]
  capture[i] <- (ci[1] < beta_1 & beta_1 < ci[2])
}

## ------------------------------------------------------------------------
hist(beta_hats)
abline(v = mean(beta_hats), col = "tomato")

## ------------------------------------------------------------------------
mean(beta_hats) - beta_1

## ------------------------------------------------------------------------
mean(capture)

## ------------------------------------------------------------------------
set.seed(794)
n <- 80
x <- runif(n)
y <- beta_1 * x + rnorm(n, sd = .93 * x)
df <- data.frame(x, y)
m1 <- lm(y ~ x, data = df)

## ---- echo = FALSE-------------------------------------------------------
ggplot(df, aes(x = x, y = y)) + 
  geom_point(col = "steelblue") + 
  geom_abline(intercept = m1$coef[1], slope = m1$coef[2]) +
  theme_bw()

## ---- fig.width = 4, fig.align="center", echo = FALSE--------------------
par(mfrow = c(2, 2))
plot(m1)

## ------------------------------------------------------------------------
it <- 5000
beta_hats <- rep(NA, it)
capture <- rep(FALSE, it)
for(i in 1:it) {
  y <- beta_1 * x + rnorm(n, sd = .93 * x)
  m <- lm(y ~ x)
  beta_hats[i] <- m$coef[2]
  ci <- confint(m)[2, ]
  capture[i] <- (ci[1] < beta_1 & beta_1 < ci[2])
}

## ------------------------------------------------------------------------
hist(beta_hats)
abline(v = mean(beta_hats), col = "tomato")

## ------------------------------------------------------------------------
mean(beta_hats) - beta_1

## ------------------------------------------------------------------------
mean(capture)

