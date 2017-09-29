library(tidyverse)
library(stringr)

train <- read.csv("http://andrewpbray.github.io/data/crime-train.csv")
test <- read.csv("/Users/andrewbray/Dropbox/Teaching/2017-fall/math-243/assets/week-04/crime-test.csv")

n <- 8
group <- rep(LETTERS[1:n], 4)
setting <- rep(rep(c("train", "test"), each = n), 2)
model_type <- rep(c("bespoke", "automated"), each = n * 2)
p <- rep(1, n * 4)
MSE <- rep(NA, n * 4)
results <- data.frame(group, 
                      setting,
                      model_type,
                      p,
                      MSE)

process_models <- function(f_fit, f_MSE, group, m_type, train, test, results) {
  m <- f_fit(train)
  results[results$group == group & 
            results$model_type == m_type, "p"] <- length(coef(m)) - 1
  results[results$group == group & 
            results$model_type == m_type & 
            results$setting == "train", "MSE"] <- f_MSE(m, train)
  results[results$group == group & 
            results$model_type == m_type & 
            results$setting == "test", "MSE"] <- f_MSE(m, test)
  results
}

# GROUP A

f_fit <- function(training_data) {
  # Our model
  m1 <- lm(data = training_data, log(ViolentCrimesPerPop + .0001) ~ 
             PctSameHouse85:racePctWhite + PctVacantBoarded +  racePctWhite +  PctNotHSGrad + pctUrban + racePctWhite:PctFam2Par)
  
  #return model
  m1
}

f_best_fit <- function(training_data) {
  # a new model, selected from an automated procedure "forwards"
  # Our model
  m1<-lm(data=training_data, log(ViolentCrimesPerPop + .0001) ~ 
           population+racePctWhite+agePct65up+pctUrban+pctWInvInc+MalePctDivorce+PctKids2Par+PctTeen2Par+NumIlleg+PctPersDenseHous+PctHousOccup+
           RentLowQ+RentHighQ+MedOwnCostPctIncNoMtg+PctForeignBorn )
  
  #return model
  m1
}


f_MSE <- function(model, data) {
  # Since our model was predicting for log(ViolentCrimesPerPop), we need to exp() our predicted responses
  predicted <- predict.lm(model, data) %>% 
    exp()
  
  # MSE calculation
  mean((data$ViolentCrimesPerPop - predicted)^2 )
}

results <- process_models(f_fit, f_MSE, group = "A", m_type = "bespoke", train, test, results)
results <- process_models(f_best_fit, f_MSE, group = "A", m_type = "automated",train, test, results)


# GROUP B

f_fit <- function(training_data) {
  training_data$pctUrban_2 <- training_data$pctUrban^2
  training_data$PctNotHSGrad_2 <- training_data$PctNotHSGrad^2
  m1 <- lm(ViolentCrimesPerPop ~ pctUrban_2 + PctNotHSGrad_2 + 
             NumInShelters + PctKids2Par + PctIlleg + 
             medIncome*PctPopUnderPov + HousVacant + racePctWhite, 
           training_data)
  m1
}

f_best_fit <- function(training_data) {
  m1 <- lm(data = training_data, ViolentCrimesPerPop ~ racePctWhite + racePctHisp + pctUrban + PctNotHSGrad + PctEmploy + MalePctDivorce + FemalePctDiv + PctIlleg + PctHousOccup + PctVacantBoarded + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet)
  m1
}

f_MSE <- function(model, data) {
  data$pctUrban_2 <- data$pctUrban^2
  data$PctNotHSGrad_2 <- data$PctNotHSGrad^2
  y_hat <- predict(model, data)
  MSE <- sum((data$ViolentCrimesPerPop - y_hat)^2)/length(data$ViolentCrimesPerPop)
  MSE
}

results <- process_models(f_fit, f_MSE, group = "B", m_type = "bespoke", train, test, results)
results <- process_models(f_best_fit, f_MSE, group = "B", m_type = "automated",train, test, results)


# GROUP C

f_fit <- function(training_data) {
  lm(ViolentCrimesPerPop~ population + numbUrban + 
       PctKids2Par + PctIlleg + PctHousOccup + 
       PctHousOwnOcc + NumStreet + racePctWhite*PctWorkMom + 
       MalePctDivorce*PctPersDenseHous, 
     data=training_data)
}

f_best_fit <- function(training_data) {
  lm(ViolentCrimesPerPop ~ population + racePctWhite + numbUrban + pctWSocSec + MalePctDivorce + PctIlleg + PersPerOccupHous + PctHousLess3BR + RentLowQ + MedRent + MedOwnCostPctIncNoMtg + NumStreet, data = training_data)
}

f_MSE <- function(model, data){ 
  data <- data %>%
    mutate(y_hat = predict(model, data),
           sq_error = (ViolentCrimesPerPop-y_hat)^2)
  mean(data$sq_error)
}

results <- process_models(f_fit, f_MSE, group = "C", m_type = "bespoke",
                          train, test, results) %>%
  process_models(f_best_fit, f_MSE, group = "C", m_type = "automated",
                 train, test, .)


# GROUP D
f_fit <- function(training_data){
  m1 <- lm(ViolentCrimesPerPop~PctUnemployed + I(PctUnemployed^2) + medIncome + I(medIncome^2)  + PctNotHSGrad + I(PctNotHSGrad^2) + PctPopUnderPov + I(PctPopUnderPov^2) + MedRentPctHousInc + I(MedRentPctHousInc^2) + PctTeen2Par + I(PctTeen2Par^2) + PopDens+ racepctblack+ racePctHisp+ racePctAsian+ racePctWhite+ medIncome+ PctPopUnderPov+ PctLess9thGrade+ PctNotHSGrad+ PctUnemployed+ PctEmploy+ TotalPctDiv+PctTeen2Par+ MedRentPctHousInc, data=training_data)
  m1 
}

f_best_fit <- function(d){
  lm(ViolentCrimesPerPop~pctWInvInc + pctWPubAsst + whitePerCap +
       NumUnderPov,data = d)
}

f_MSE <- function(model, data){
  y_hat <- predict.lm(model, data)
  test_mse <- mean((data$ViolentCrimesPerPop-y_hat)^2)
  test_mse
}

results <- process_models(f_fit, f_MSE, group = "D", m_type = "bespoke", train, test, results)
results <- process_models(f_best_fit, f_MSE, group = "D", m_type = "automated",train, test, results)


# GROUP E

f_fit <- function(training_data){
  m1 <- lm(sqrt(ViolentCrimesPerPop)~
             log(medIncome)+PctKids2Par+sqrt(PctRecentImmig)+
             sqrt(PctLargHouseFam)*sqrt(NumUnderPov), 
           data=training_data)
  m1
}

f_best_fit <- function(d){
  d$race3 <- d$racePctWhite^3
  d$urban3 <- (d$numbUrban)^.3
  d$kids2 <- (d$PctKids2Par)^1.7
  d$illeg4 <- (d$PctIlleg)^.4
  d$house3 <- (d$HousVacant)^.3
  m1 <- lm(sqrt(ViolentCrimesPerPop) ~ race3 + urban3 + kids2 + PctIlleg + house3, data = d)
  m1
}

f_MSE <- function(model, data){
  # you need to back-transform the response before computing residuals!
  mean((predict(model, data)^2-data$ViolentCrimesPerPop)^2)
}

f_best_MSE <- function(model, d){
  d$race3 <- d$racePctWhite^3
  d$urban3 <- (d$numbUrban)^.3
  d$kids2 <- (d$PctKids2Par)^1.7
  d$illeg4 <- (d$PctIlleg)^.4
  d$house3 <- (d$HousVacant)^.3
  # you need to back-transform the response before computing residuals!
  mean((predict(model, d)^2-d$ViolentCrimesPerPop)^2)
}

results <- process_models(f_fit, f_MSE, group = "E", m_type = "bespoke", train, test, results)
results <- process_models(f_best_fit, f_best_MSE, group = "E", m_type = "automated", train, test, results)


# GROUP F
f_fit <- function(train_data){
  model <- lm(ViolentCrimesPerPop ~
                poly(PctKids2Par, 2) +
                racePctWhite +
                poly(MalePctDivorce, 2) +
                log(pctWInvInc) +
                NumInShelters +
                PopDens,
              data = train_data)
  model
}

f_best_fit <- function(train_data){
  model <- lm(ViolentCrimesPerPop ~
                racePctWhite + 
                pctUrban + 
                pctWSocSec +
                TotalPctDiv +
                PctIlleg +
                PctPersDenseHous +
                PctHousOccup + 
                PctVacantBoarded + 
                NumStreet +
                MedOwnCostPctIncNoMtg +
                PctNotHSGrad,
              data = train_data) 
  model
}

f_MSE <- function(model, data){
  y_obs <- data$ViolentCrimesPerPop
  y_hat <- predict(model, data)
  mean((y_hat - y_obs)^2)
}

results <- process_models(f_fit, f_MSE, group = "F", m_type = "bespoke",
                          train, test, results) %>%
  process_models(f_best_fit, f_MSE, group = "F", m_type = "automated",
                 train, test, .)


# GROUP G

f_fit <- function(d) {
  m1 <- lm(ViolentCrimesPerPop ~ PctKids2Par + PctIlleg + racePctWhite, data = d)
  m1
}


f_MSE <- function(model, data) {
  y <- data$ViolentCrimesPerPop
  y_hat <- predict(model, newdata = data)
  MSE <- mean((y - y_hat)^2)
  MSE
}

#results <- process_models(f_fit, f_MSE, group = "F", m_type = "bespoke", train, test, results)
#results <- process_models(f_best_fit, f_best_MSE, group = "F", m_type = "automated", train, test, results)


# GROUP H

f_fit <- function(training_data) {
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

f_best_fit <- function(training_data) {
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
  
  m1 <- lm(sqrt(ViolentCrimesPerPop)~householdsize+racePctWhite+racePctAsian+
             agePct65up+pctUrban+MalePctDivorce+PctWorkMom+PctKids2Par+
             PctIlleg+PctNotSpeakEnglWell+PctPersDenseHous+NumInShelters+
             LemasSwFTPerPop+LemasSwFTFieldPerPop+OfficAssgnDrugUnits+OfficAssgnDrugUnits
           , data = d1)
  return(m1)
}

f_MSE <- function(model, data) {
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
  mse <- mean((data.modified$sqrtCrime^2-model.predict^2)^2)
  return(mse)
}

f_best_MSE <- function(model, data) {
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
  mse <- mean((data.modified$sqrtCrime^2-model.predict^2)^2)
  return(mse)
}

results <- process_models(f_fit, f_MSE, group = "H", m_type = "bespoke", train, test, results)
results <- process_models(f_best_fit, f_best_MSE, group = "H", m_type = "automated", train, test, results)

