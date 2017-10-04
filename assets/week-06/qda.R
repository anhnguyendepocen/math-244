## ----echo = FALSE--------------------------------------------------------
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE)

## ----echo = FALSE--------------------------------------------------------
library(ISLR)
m1 <- glm(default ~ balance, data = Default, family = binomial)
my_log_pred <- ifelse(m1$fit < 0.5, "No", "Yes")
conf_log <- table(my_log_pred, Default$default)

## ------------------------------------------------------------------------
conf_log

## ------------------------------------------------------------------------
my_log_pred <- ifelse(m1$fit < 0.6, "No", "Yes")
conf_log_6 <- table(my_log_pred, Default$default)
conf_log_6
conf_log

## ------------------------------------------------------------------------
my_log_pred <- ifelse(m1$fit < 0.7, "No", "Yes")
conf_log_7 <- table(my_log_pred, Default$default)
conf_log_7
conf_log_6

## ------------------------------------------------------------------------
thresh <- c(0.5, 0.6, 0.7)
FPR <- c(conf_log["Yes", "No"]/sum(conf_log[, "No"]),
        conf_log_6["Yes", "No"]/sum(conf_log_6[, "No"]),
        conf_log_7["Yes", "No"]/sum(conf_log_7[, "No"]))
FPR

## ------------------------------------------------------------------------
thresh <- c(0.5, 0.6, 0.7)
TPR <- c(conf_log["Yes", "Yes"]/sum(conf_log[, "Yes"]),
        conf_log_6["Yes", "Yes"]/sum(conf_log_6[, "Yes"]),
        conf_log_7["Yes", "Yes"]/sum(conf_log_7[, "Yes"]))
TPR

## ---- fig.height=4.5, fig.width = 5.2, echo = FALSE----------------------
# ROC function
library(ggplot2)
plotROC <- function(model, nthresh = 1000) {
  k <- seq(0, 1, length.out = nthresh)
  TPR <- rep(NA, nthresh)
  FPR <- rep(NA, nthresh)
  for(i in 1:nthresh) {
    pred <- as.factor(ifelse(model$fit < k[i], "No", "Yes"))
    if (levels(pred) == "Yes") {levels(pred) <- c("Yes", "No")}
    if (levels(pred) == "No") {levels(pred) <- c("No", "Yes")}
    conf <- table(pred, Default$default)
    TPR[i] <- conf["Yes", "Yes"]/ sum(conf[, "Yes"])
    FPR[i] <- conf["Yes", "No"]/ sum(conf[, "No"])
  }
  df <- data.frame(TPR, FPR)
  ggplot(df, aes(x = FPR, y = TPR)) +
    geom_line(col = "forestgreen", lwd = 1.3) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate")
}

plotROC(m1)

## ------------------------------------------------------------------------
head(iris)

## ----echo = FALSE--------------------------------------------------------
library(ggplot2)

p1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()

p1

## ------------------------------------------------------------------------
library(MASS)
mlda <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)
mlda_pred <- predict(mlda)
(conf <- table(mlda_pred$class, iris$Species))

## ------------------------------------------------------------------------
(sum(conf) - sum(diag(conf)))/sum(conf)

## ----echo=FALSE----------------------------------------------------------
# credit to dean young for this plot
contour_data <- expand.grid(Sepal.Length = seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length = 300),
  Sepal.Width = seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length = 300))

lda_predict <- data.frame(contour_data, Species=as.numeric(predict(mlda, contour_data)$class))

p2 <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col=Species)) +
  stat_contour(aes(x=Sepal.Length, y=Sepal.Width, z=Species), data=lda_predict, 
               col = 1, lineend = "round") +
  ggtitle("LDA Decision Boundaries")

p2


## ------------------------------------------------------------------------
mqda <- qda(Species ~ Sepal.Length + Sepal.Width, data = iris)
mqda_pred <- predict(mqda)
(conf <- table(mqda_pred$class, iris$Species))

## ------------------------------------------------------------------------
(sum(conf) - sum(diag(conf)))/sum(conf)

## ----echo=FALSE----------------------------------------------------------
# credit to dean young for this plot
qda_predict <- data.frame(contour_data, Species=as.numeric(predict(mqda, contour_data)$class))

p3 <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col=Species)) +
  stat_contour(aes(x=Sepal.Length, y=Sepal.Width, z=Species), data=qda_predict, col = 1) +
  ggtitle("QDA Decision Boundaries")

p3


