library(tidyverse)

train <- read.csv("http://andrewpbray.github.io/data/crime-train.csv")
test <- read.csv("crime-test.csv")

n_groups <- 8
group <- LETTERS[1:n_groups]
p <- rep(1, n_groups)
MSE_train <- rep(0, n_groups)
MSE_test <- rep(0, n_groups)
results <- data.frame(group, 
                      p, 
                      MSE_train, 
                      MSE_test)

# GROUP A

group_A_fit <- function(training_data) {
  # We first take the log of our response variable and exclude those that end up as -Inf (originally 0)
  # In our training dataset, this resulted in an exclusion of 6 observations
  d1 <- mutate(training_data, logViolentCrimesPerPop = log(ViolentCrimesPerPop)) %>%
    filter(logViolentCrimesPerPop != -Inf)
  
  # Our model
  m1 <- lm(data = d1, logViolentCrimesPerPop ~ 
             PctSameHouse85:racePctWhite + PctVacantBoarded +  racePctWhite +  PctNotHSGrad + pctUrban + racePctWhite:PctFam2Par)
  
  #return model
  m1
}


group_A_MSE <- function(model, data) {
  # Since our model was predicting for log(ViolentCrimesPerPop), we need to exp() our predicted responses
  predicted <- predict.lm(model, data) %>% 
    exp()
  
  # MSE calculation
  mean((data$ViolentCrimesPerPop - predicted)^2 )
}

m <- group_A_fit(train)
results[1, 2] <- length(coef(m)) - 1
results[1, 3] <- group_A_MSE(m, train)
results[1, 4] <- group_A_MSE(m, test)


# GROUP B

group_B_fit <- function(training_data) {
  training_data$pctUrban_2 <- training_data$pctUrban^2
  training_data$PctNotHSGrad_2 <- training_data$PctNotHSGrad^2
  m1 <- lm(ViolentCrimesPerPop ~ pctUrban_2 + PctNotHSGrad_2 + 
             NumInShelters + PctKids2Par + PctIlleg + 
             medIncome*PctPopUnderPov + HousVacant + racePctWhite, 
           training_data)
  m1
}

group_B_MSE <- function(model, data) {
  data$pctUrban_2 <- data$pctUrban^2
  data$PctNotHSGrad_2 <- data$PctNotHSGrad^2
  y_hat <- predict(model, data)
  MSE <- sum((data$ViolentCrimesPerPop - y_hat)^2)/length(data$ViolentCrimesPerPop)
  MSE
}

m <- group_B_fit(train)
results[2, 2] <- length(coef(m)) - 1
results[2, 3] <- group_B_MSE(m, train)
results[2, 4] <- group_B_MSE(m, test)


# GROUP C

group_C_fit <- function(training_data) {
  lm(ViolentCrimesPerPop~ population + numbUrban + 
       PctKids2Par + PctIlleg + PctHousOccup + 
       PctHousOwnOcc + NumStreet + racePctWhite*PctWorkMom + 
       MalePctDivorce*PctPersDenseHous, 
     data=training_data)
}

group_C_MSE <- function(model, data){ 
  data <- data %>%
    mutate(y_hat = predict(model, data),
           sq_error = (ViolentCrimesPerPop-y_hat)^2)
  mean(data$sq_error)
}

m <- group_C_fit(train)
results[3, 2] <- length(coef(m)) - 1
results[3, 3] <- group_C_MSE(m, train)
results[3, 4] <- group_C_MSE(m, test)


# GROUP D
group_D_fit <- function(training_data){
  m1 <- lm(ViolentCrimesPerPop~PctUnemployed + I(PctUnemployed^2) + medIncome + I(medIncome^2)  + PctNotHSGrad + I(PctNotHSGrad^2) + PctPopUnderPov + I(PctPopUnderPov^2) + MedRentPctHousInc + I(MedRentPctHousInc^2) + PctTeen2Par + I(PctTeen2Par^2) + PopDens+ racepctblack+ racePctHisp+ racePctAsian+ racePctWhite+ medIncome+ PctPopUnderPov+ PctLess9thGrade+ PctNotHSGrad+ PctUnemployed+ PctEmploy+ TotalPctDiv+PctTeen2Par+ MedRentPctHousInc, data=training_data)
  m1 
}

group_D_MSE <- function(model, data){
  y_hat <- predict.lm(model, data)
  test_mse <- mean((data$ViolentCrimesPerPop-y_hat)^2)
  test_mse
}

m <- group_D_fit(train)
results[4, 2] <- length(coef(m)) - 1
results[4, 3] <- group_D_MSE(m, train)
results[4, 4] <- group_D_MSE(m, test)


# GROUP E

group_E_fit <- function(training_data){
  m1 <- lm(sqrt(ViolentCrimesPerPop)~
             log(medIncome)+PctKids2Par+sqrt(PctRecentImmig)+
             sqrt(PctLargHouseFam)*sqrt(NumUnderPov), 
           data=training_data)
  m1
}

group_E_MSE <- function(model, data){
  # you need to back-transform the response before computing residuals!
  mean((predict(model, data)^2-data$ViolentCrimesPerPop)^2)
}

m <- group_E_fit(train)
results[5, 2] <- length(coef(m)) - 1
results[5, 3] <- group_E_MSE(m, train)
results[5, 4] <- group_E_MSE(m, test)


# GROUP F
group_F_fit <- function(train) {
  m <- lm(ViolentCrimesPerPop ~
                PctKids2Par +
                I(PctKids2Par^2) +
                racePctWhite +
                MalePctDivorce +
                I(MalePctDivorce^2) +
                log(pctWInvInc) +
                NumInShelters +
                PopDens,
              data = train)
  m
  }

group_F_MSE <- function(model, data){
  y <- data$ViolentCrimesPerPop
  y_hat <- predict(model, data = data)
  mean((y - y_hat)^2)
}

m <- group_F_fit(train)
results[6, 2] <- length(m$coefficients) - 1
results[6, 3] <- group_F_MSE(m, train)
results[6, 4] <- group_F_MSE(m, test)


# GROUP G

group_G_fit <- function(d) {
  m1 <- lm(ViolentCrimesPerPop ~ PctKids2Par + PctIlleg + racePctWhite, data = d)
  m1
}

group_G_MSE <- function(model, data) {
  y <- data$ViolentCrimesPerPop
  y_hat <- predict(model, newdata = data)
  MSE <- mean((y - y_hat)^2)
  MSE
}

m <- group_G_fit(train)
results[7, 2] <- length(coef(m))- 1
results[7, 3] <- group_G_MSE(m, train)
results[7, 4] <- group_G_MSE(m, test)


# GROUP H

group_H_fit <- function(training_data) {
  d1 <- training_data %>% 
    dplyr::select(-(1:4)) %>%
    dplyr::mutate(
      PolicBudgPerPop = as.numeric(PolicBudgPerPop),
      PolicOperBudg = as.numeric(PolicOperBudg),
      PolicAveOTWorked = as.numeric(PolicAveOTWorked),
      NumKindsDrugsSeiz = as.numeric(NumKindsDrugsSeiz),
      PolicReqPerOffic = as.numeric(PolicReqPerOffic),
      LemasSwFTFieldPerPop = as.numeric(LemasSwFTFieldPerPop),
      LemasSwFTPerPop = as.numeric(LemasSwFTPerPop),
      LemasTotReqPerPop = as.numeric(LemasTotReqPerPop),
      PctPolicMinor = as.numeric(PctPolicMinor),
      LemasPctPolicOnPatr = as.numeric(LemasPctPolicOnPatr),
      OfficAssgnDrugUnits = as.numeric(OfficAssgnDrugUnits),
      PolicReqPerOffic = as.numeric(PolicReqPerOffic),
      majUrban = dplyr::if_else(pctUrban > .5, true = 1, false = 0),
      majUrban = as.factor(majUrban),
      sqrtCrime = sqrt(ViolentCrimesPerPop),
      LemasSwornFT = as.numeric(LemasSwornFT),
      LemasSwFTFieldOps = as.numeric(LemasSwFTFieldOps),
      LemasTotalReq = as.numeric(LemasTotalReq),
      PolicPerPop = as.numeric(PolicPerPop),
      RacialMatchCommPol = as.numeric(RacialMatchCommPol),
      PctPolicWhite = as.numeric(PctPolicWhite),
      PctPolicBlack = as.numeric(PctPolicBlack),
      PctPolicHisp = as.numeric(PctPolicHisp),
      PctPolicAsian = as.numeric(PctPolicAsian),
      PolicCars = as.numeric(PolicCars)
    ) 
  
  m1 <- lm(sqrtCrime ~ (poly(householdsize,2) + majUrban + 
                          medIncome + poly(PctPopUnderPov,2)+ PctNotHSGrad +
                          PctKids2Par+ PctIlleg+ PctNotSpeakEnglWell+ 
                          PctPersDenseHous+ PctHousOccup+ NumStreet+ 
                          LemasSwFTPerPop+ LemasSwFTFieldPerPop+ 
                          PolicReqPerOffic+ PolicBudgPerPop)^2,
           data=d1)
  return(m1)
}

group_H_MSE <- function(model, data) {
  data.modified <- data %>% 
    dplyr::select(-(1:4)) %>%
    dplyr::mutate(
      PolicBudgPerPop = as.numeric(PolicBudgPerPop),
      PolicOperBudg = as.numeric(PolicOperBudg),
      PolicAveOTWorked = as.numeric(PolicAveOTWorked),
      NumKindsDrugsSeiz = as.numeric(NumKindsDrugsSeiz),
      PolicReqPerOffic = as.numeric(PolicReqPerOffic),
      LemasSwFTFieldPerPop = as.numeric(LemasSwFTFieldPerPop),
      LemasSwFTPerPop = as.numeric(LemasSwFTPerPop),
      LemasTotReqPerPop = as.numeric(LemasTotReqPerPop),
      PctPolicMinor = as.numeric(PctPolicMinor),
      LemasPctPolicOnPatr = as.numeric(LemasPctPolicOnPatr),
      OfficAssgnDrugUnits = as.numeric(OfficAssgnDrugUnits),
      PolicReqPerOffic = as.numeric(PolicReqPerOffic),
      majUrban = dplyr::if_else(pctUrban > .5, true = 1, false = 0),
      majUrban = as.factor(majUrban),
      sqrtCrime = sqrt(ViolentCrimesPerPop),
      LemasSwornFT = as.numeric(LemasSwornFT),
      LemasSwFTFieldOps = as.numeric(LemasSwFTFieldOps),
      LemasTotalReq = as.numeric(LemasTotalReq),
      PolicPerPop = as.numeric(PolicPerPop),
      RacialMatchCommPol = as.numeric(RacialMatchCommPol),
      PctPolicWhite = as.numeric(PctPolicWhite),
      PctPolicBlack = as.numeric(PctPolicBlack),
      PctPolicHisp = as.numeric(PctPolicHisp),
      PctPolicAsian = as.numeric(PctPolicAsian),
      PolicCars = as.numeric(PolicCars)
    ) 
  model.predict = predict(model, data.modified)
  mod.sum <- summary(model)
  mse <- mean((data.modified$sqrtCrime-model.predict)^2)
  return(mse)
}

m <- group_H_fit(train)
results[8, 2] <- length(coef(m)) - 1
results[8, 3] <- group_H_MSE(m, train)
results[8, 4] <- group_H_MSE(m, test)

