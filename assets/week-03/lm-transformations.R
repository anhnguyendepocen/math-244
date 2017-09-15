## ----echo = FALSE, warning=FALSE, message=FALSE--------------------------
library(tidyverse)
library(gridExtra)
pickups <- read.csv("http://andrewpbray.github.io/data/pickup.csv")
ggplot(pickups, aes(x = year, y = price)) +
  geom_point(col = "steelblue") +
  theme_bw()

## ----echo = FALSE--------------------------------------------------------
pickups <- filter(pickups, year >= 1994)
ggplot(pickups, aes(x = year, y = price)) +
  geom_point(col = "steelblue") +
  theme_bw()
m1 <- lm(price ~ year, data = filter(pickups, year >= 1994))

## ---- echo=FALSE---------------------------------------------------------
summary(m1)$coef
ggplot(pickups, aes(x = year, y = price)) +
  geom_point(col = "steelblue") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, col = "orchid")

## ---- echo=FALSE---------------------------------------------------------
par(mfrow = c(1, 2))
plot(m1, 1:2)

## ----echo = FALSE--------------------------------------------------------
par(mfrow = c(1, 2))
plot(m1, c(3, 5))

## ---- echo=1, fig.height=3, message = FALSE, warning=FALSE---------------
pickups <- mutate(pickups, log_price = log(price))
p1 <- ggplot(pickups, aes(x = price)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()
p2 <- ggplot(pickups, aes(x = log_price)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()
grid.arrange(p1, p2, ncol = 2)

## ---- echo=FALSE---------------------------------------------------------
m2 <- lm(log_price ~ year, data = pickups)
summary(m2)$coef
ggplot(pickups, aes(x = year, y = log_price)) +
  geom_point(col = "steelblue") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, col = "orchid")

## ---- echo=FALSE---------------------------------------------------------
par(mfrow = c(1, 2))
plot(m2, 1:2)

## ----echo = FALSE--------------------------------------------------------
par(mfrow = c(1, 2))
plot(m2, c(3, 5))

