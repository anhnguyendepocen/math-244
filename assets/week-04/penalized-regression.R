## ------------------------------------------------------------------------
credit <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv")[, -1]
dim(credit)
names(credit)

## ------------------------------------------------------------------------
m1 <- lm(Balance ~ ., data = credit)
summary(m1)$coef

## ----message = FALSE-----------------------------------------------------
X <- model.matrix(Balance ~ ., data = credit)[, -1]
X[1:2, ]
Y <- credit$Balance
lambdas <- seq(from = 1e4, to = 1e-2, length.out = 100)
library(glmnet)
rm1 <- glmnet(x = X, y = Y, alpha = 0, lambda = lambdas, standardize = TRUE)
dim(coef(rm1))

## ------------------------------------------------------------------------
rm1$lambda[100]
coef(rm1)[1:4, 100]
coef(m1)[1:4]

## ------------------------------------------------------------------------
rm1$lambda[5]
coef(rm1)[1:4, 5]
coef(rm1)[1:4, 100]

## ---- echo = FALSE, eval = FALSE-----------------------------------------
## l <- rev(rep(lambdas, 3))
## a <- as.matrix(coef(rm1)[2:4, ])
## v <- c(t(a[, seq(100, 1)]))
## coef <- as.factor(rep(row.names(coef(rm1))[2:4], each = length(lambdas)))
## df <- data.frame(l, v, coef)
## 
## library(ggplot2)
## ggplot(df, aes(x = l, y = v, color = coef)) +
##   geom_line()

