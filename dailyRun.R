#### Script to create the dataset used by first draft model ####
library(dplyr)
library(reshape)

setwd("~/")

#### Read in raw dataset #######
electionsRaw <- read.csv("midterms/inputData/electionsNew2006_2018.csv", stringsAsFactors = TRUE, na.strings = c("#N/A", "NA"))
#setwd("~/MSPA/590-Thesis") # Work
#electionsRaw <- read.csv("electionsNew2006_2018.csv", stringsAsFactors = TRUE, na.strings = c("#N/A", "NA"))

### Update Sabato Scores and Numbers, re-write data ###
## Last updated: 13 June 2018
sabatoUpdate <- data.frame(ID = c("2018SC1", "2018WV3", "2018CA45", "2018NJ3", "2018NJ2", "2018VA7", "2018VA10", "2018VA2", "2018AR2", "2018FL16", "2018FL13", 
                                  "2018IA4", "2018IA3", "2018IL6", "2018IN9", "2018IN2", "2018KY6", "2018MI8", "2018NM2", "2018OH1", "2018PA16", "2018TX31", 
                                  "2018TX7", "2018KS3", "2018MI11", "2018NJ11", "2018NY27", "2018PA17", "2018PA7", "2018WA3", "2018WA5"), 
                           Sabato2 = c("Likely R", "R Toss-up", "R Toss-up", "Leans R", "Likely D", "R Toss-up", "Leans D", "R Toss-up", "Leans R", "Leans R", "D", 
                                       "Likely R", "R Toss-up", "R Toss-up", "Likely R", "Likely R", "R Toss-up", "R Toss-up", "Leans R", "R Toss-up", "Likely R", "Likely R", 
                                       "R Toss-up", "R Toss-up", "Leans D", "Leans D", "Likely R", "Leans D", "Leans D", "Leans R", "R Toss-up"), 
                           SabatoNum2 = c(3, 2, 1, 2, -3, 1, -2, 1, 2, 2, -5, 
                                          3, 1, 1, 3, 3, 1, 1, 2, 1, 3, 3, 
                                          1, 1, -2, -2, 3, -2, -2, 2, 1))
electionsRaw <- left_join(electionsRaw, sabatoUpdate, by = "ID")
electionsRaw$Sabato[!is.na(electionsRaw$Sabato2)] <- electionsRaw$Sabato2[!is.na(electionsRaw$Sabato2)]
electionsRaw$SabatoNum[!is.na(electionsRaw$SabatoNum2)] <- electionsRaw$SabatoNum2[!is.na(electionsRaw$SabatoNum2)]
electionsRaw$Sabato2 <- NULL
electionsRaw$SabatoNum2 <- NULL
rm(sabatoUpdate)

### Refresh DW_NOMINATE scores
nom_dat <- read.csv("https://voteview.com/static/data/out/members/HSall_members.csv")
nom_dat <- nom_dat[nom_dat$congress == 115 & nom_dat$chamber == "House",]
nom_dat$ID <- paste0("2018", nom_dat$state_abbrev, nom_dat$district_code)
nom_dat <- nom_dat[,c("ID", "nominate_dim1")]
nom_dat <- nom_dat[-which(duplicated(nom_dat$ID)),]
electionsRaw <- left_join(electionsRaw, nom_dat, by = "ID")
electionsRaw$INC_DW_NOM[electionsRaw$year == "2018"] <- electionsRaw$nominate_dim1[electionsRaw$year == "2018"]
electionsRaw$nominate_dim1 <- NULL
rm(nom_dat)

### Refresh Spending
disb <- read.csv("https://classic.fec.gov/data/CandidateSummary.do?format=csv&election_yr=2018&can_nam=&can_off=&can_off_sta=&can_off_dis=&can_par_aff=&can_inc_cha_ope_sea=&tot_rec=&tot_dis=&cas_on_han_clo_of_per=&deb_owe_by_com=&cov_dat=&sortField=can_nam&sortOrder=0")

disb <- subset(disb, can_off == "H")
disb$can_off_dis[disb$can_off_dis == 0] <- 1
disb$ID <- paste0("2018", disb$can_off_sta, disb$can_off_dis)
disb$tot_dis <- sub('\\$','',as.character(disb$tot_dis))
disb$tot_dis <- as.numeric(sub('\\,','',as.character(disb$tot_dis)))
disb$tot_dis[is.na(disb$tot_dis)] <- 0
disb$can_par_aff <- as.character(disb$can_par_aff)
disb$can_par_aff[disb$can_par_aff != "DEM" & disb$can_par_aff != "REP"] <- "OTHER"

disbTable <- cast(disb, ID ~ can_par_aff, sum, value = 'tot_dis')

electionsRaw <- left_join(electionsRaw, disbTable, by = "ID")
electionsRaw$Dcont[electionsRaw$year == "2018"] <- electionsRaw$DEM[electionsRaw$year == "2018"]
electionsRaw$Rcont[electionsRaw$year == "2018"] <- electionsRaw$REP[electionsRaw$year == "2018"]
electionsRaw$Ocont[electionsRaw$year == "2018"] <- electionsRaw$OTHER[electionsRaw$year == "2018"]
electionsRaw$DEM <- NULL
electionsRaw$REP <- NULL
electionsRaw$OTHER <- NULL

rm(disb, disbTable)

#### Subset and create new variables ####
# Incumbent winner indicator
electionsRaw$IncWin <- 0
electionsRaw$IncWin[electionsRaw$INC == electionsRaw$Winner] <- 1
# Incumbent party registration
electionsRaw$IncReg <- 0
electionsRaw$IncReg[electionsRaw$INC == "D"] <- electionsRaw$Dreg[electionsRaw$INC == "D"]
electionsRaw$IncReg[electionsRaw$INC == "R"] <- electionsRaw$Rreg[electionsRaw$INC == "R"]
# Non-Incumbent party registration
electionsRaw$NIncReg <- 0
electionsRaw$NIncReg[electionsRaw$INC == "D"] <- electionsRaw$Rreg[electionsRaw$INC == "D"]
electionsRaw$NIncReg[electionsRaw$INC == "R"] <- electionsRaw$Dreg[electionsRaw$INC == "R"]
# Registration difference
electionsRaw$RegDiff <- electionsRaw$IncReg - electionsRaw$NIncReg
# Existence of third party candidate indicator
electionsRaw$thirdParty <- 0
electionsRaw$thirdParty[electionsRaw$oPct > .01] <- 1
# Create third party effect variable
electionsRaw$thirdPartyEffect <- electionsRaw$Oreg*electionsRaw$thirdParty
# Re-code Pres_Incumbent_SameParty
electionsRaw$Pres_Incumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 1] <- 1
electionsRaw$Pres_Incumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 0] <- -1
# Create midterm effect variable
electionsRaw$midtermEffect <- electionsRaw$Midterm. * electionsRaw$Pres_Incumbent_SameParty
# Create Ideology Effect variable
electionsRaw$ideologyEffect <- electionsRaw$PVI...100.to.100. * electionsRaw$INC_DW_NOM
# Incumbent campaign spending variable
electionsRaw$IncCont <- 0
electionsRaw$NincCont <- 0
electionsRaw$IncCont[electionsRaw$INC == "R"] <- electionsRaw$Rcont[electionsRaw$INC == "R"]
electionsRaw$NincCont[electionsRaw$INC == "R"] <- electionsRaw$Dcont[electionsRaw$INC == "R"]
electionsRaw$IncCont[electionsRaw$INC == "D"] <- electionsRaw$Dcont[electionsRaw$INC == "D"]
electionsRaw$NincCont[electionsRaw$INC == "D"] <- electionsRaw$Rcont[electionsRaw$INC == "D"]
electionsRaw$IncContDiff <- log(electionsRaw$IncCont + 1) - log(electionsRaw$NincCont + 1)
electionsRaw$dContDiff <- electionsRaw$Dcont - electionsRaw$Rcont
# Log incumbent spending variable
electionsRaw$logIncCont <- log(electionsRaw$IncCont + 1)
# Republican indicator
electionsRaw$Repub <- 1
electionsRaw$Repub[electionsRaw$INC == "D"] <- -1
# PVI effect variable
electionsRaw$PVIeffect <- electionsRaw$PVI...100.to.100. * electionsRaw$Repub
# Two-party vote shares
electionsRaw$dPct2 <- electionsRaw$D/(electionsRaw$D + electionsRaw$R)
electionsRaw$rPct2 <- electionsRaw$R/(electionsRaw$D + electionsRaw$R)
# Dem win?
electionsRaw$dWin <- 0
electionsRaw$dWin[electionsRaw$dPct2 > 0.5] <- 1

### Change variable classes for EDA (e.g. change year to factor instead of integer) ###
electionsRaw$year <- as.factor(electionsRaw$year)
electionsRaw$district <- as.factor(electionsRaw$district)
electionsRaw$IncWin <- as.factor(electionsRaw$IncWin)
electionsRaw$thirdParty <- as.factor(electionsRaw$thirdParty)
electionsRaw$Midterm. <- as.factor(electionsRaw$Midterm.)
electionsRaw$Pres_Incumbent_SameParty <- as.factor(electionsRaw$Pres_Incumbent_SameParty)
electionsRaw$midtermEffect <- as.factor(electionsRaw$midtermEffect)
electionsRaw$Winner <- relevel(electionsRaw$Winner, "R")
electionsRaw$INC <- relevel(electionsRaw$INC, "R")
#Oreg has a lot of 1s because districts that do not require/collect party registration for voters are given 1 under Oreg
electionsRaw[which(electionsRaw$Oreg == 1),"Oreg"] <- NA
#Get rid of "O" factor for INC
electionsRaw$INC <- factor(electionsRaw$INC)
# Log NincCont
electionsRaw$logNincCont <- log(electionsRaw$NincCont + 1)
# Inc 2-party share
electionsRaw$IncPct2 <- electionsRaw$D/(electionsRaw$D + electionsRaw$R)
electionsRaw$IncPct2[electionsRaw$INC == "R"] <- electionsRaw$R[electionsRaw$INC == "R"]/(electionsRaw$D[electionsRaw$INC == "R"] + electionsRaw$R[electionsRaw$INC == "R"])
# Sabato
electionsRaw$SabatoScore <- electionsRaw$SabatoNum * electionsRaw$Repub
#Challenged
electionsRaw$Challenge[electionsRaw$year %in% c("2006", "2008", "2010", "2012", "2014", "2016")] <- 1
electionsRaw$Challenge[electionsRaw$year %in% c("2006", "2008", "2010", "2012", "2014", "2016") & (electionsRaw$dPct2 == 1 | electionsRaw$dPct2 == 0)] <- 0

### Refresh Polls, Open races, and whether races are Challenged ###


#dk <- gs_title("Daily Kos Elections House open seat tracker")
library(gsheet)
open <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12RhR9oZZpyKKceyLO3C5am84abKzu2XqLWjP2LnQDgI/edit#gid=0")
colnames(open) <- open[1,]
open <- open[-1,]
#open2 <- gs_read(ss = dk, ws = "2018 - open seats", skip = 1)
open <- filter(open, Clinton %in% c(as.character(seq(0,100,1))))
open$Dist2 <- gsub("AL","1", substr(open$District,4,6))
open$Dist2[substr(open$Dist2,1,1) == "0"] <- substr(open$Dist2[substr(open$Dist2,1,1) == "0"], 2,2)
open$state <- substr(open$District,1,2)
open$ID <- paste0("2018", open$state, open$Dist2)
open$Open2 <- 1
open <- open[,12:13]
electionsRaw <- left_join(electionsRaw, open, by = "ID")
electionsRaw$Open2[is.na(electionsRaw$Open2)] <- 0
electionsRaw$Open[electionsRaw$year == "2018"] <- electionsRaw$Open2[electionsRaw$year == "2018"]
electionsRaw$Open2 <- NULL

unchallenged <- gsheet2tbl("https://docs.google.com/spreadsheets/d/12RhR9oZZpyKKceyLO3C5am84abKzu2XqLWjP2LnQDgI/edit#gid=195784761")
colnames(unchallenged) <- unchallenged[1,]
unchallenged <- unchallenged[-1,]
#unchallenged <- gs_read(ss = dk, ws = "2018 - uncontested seats", skip = 1)
unchallenged <- filter(unchallenged, Clinton %in% c(as.character(seq(0,100,1))))
unchallenged$Dist2 <- gsub("AL","1", substr(unchallenged$District,4,6))
unchallenged$Dist2[substr(unchallenged$Dist2,1,1) == "0"] <- substr(unchallenged$Dist2[substr(unchallenged$Dist2,1,1) == "0"], 2,2)
unchallenged$state <- substr(unchallenged$District,1,2)
unchallenged$ID <- paste0("2018", unchallenged$state, unchallenged$Dist2)
unchallenged$Challenge2 <- 0
unchallenged <- unchallenged[,10:11]
electionsRaw <- left_join(electionsRaw, unchallenged, by = "ID")
electionsRaw$Challenge2[is.na(electionsRaw$Challenge2)] <- 1
electionsRaw$Challenge[electionsRaw$year == "2018"] <- electionsRaw$Challenge2[electionsRaw$year == "2018"]
electionsRaw$Challenge2 <- NULL

pollYear <- data.frame(year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018"), Polls = c(7.9,10.7,-6.8,1.2,-5.7,-1.1,8.2), dSwing = c(10.5,2.8,-17.4,8,-6.9,4.6,9.3))
polls <- read.csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_topline.csv")
pollYear$Polls[7] <- round(polls$dem_estimate[3] - polls$rep_estimate[3], digits = 1)
pollYear$dSwing[7] <- pollYear$Polls[7] - pollYear$Polls[6]
electionsRaw <- left_join(electionsRaw, pollYear, by = "year")

rm(open, polls, pollYear, unchallenged)

## Other modeling variables ##
modelData <- electionsRaw
modelData$PrevElectionD2 <- modelData$PrevElectionD./(modelData$PrevElectionD. + modelData$PrevElectionR.)
modelData$Prev2ElectionD2 <- modelData$PrevElection2D./(modelData$PrevElection2D. + modelData$PrevElection2R.)
modelData$Prev2ElectionD2Diff <- (modelData$PrevElection2D./(modelData$PrevElection2D. + modelData$PrevElection2R.)) -.5
modelData$OpenInc <- "Open"
modelData$OpenInc[modelData$Open == 0] <- as.character(modelData$INC[modelData$Open == 0])
modelData$midPres <- "On-cycle"
modelData$midPres[modelData$Midterm. == 1] <- as.character(modelData$PresParty[modelData$Midterm. == 1])
modelData$PrevElectionD2Diff <- modelData$PrevElectionD2 - .5
modelData$PrevElectionD2DiffSwing <- modelData$PrevElectionD2Diff - (modelData$Prev2ElectionD2 - .5)

modelData$Dcont[is.na(modelData$Dcont)] <- 0
modelData$Rcont[is.na(modelData$Rcont)] <- 0

modelData$logDCont <- log(modelData$Dcont + 1)
modelData$logRCont <- log(modelData$Rcont + 1)
modelData$logDContDiff <- modelData$logDCont - modelData$logRCont

#modelData$logDContDiff <- 0
#modelData$logDContDiff[modelData$Dcont >= modelData$Rcont] <- log(modelData$Dcont[modelData$Dcont >= modelData$Rcont] - modelData$Rcont[modelData$Dcont >= modelData$Rcont] + 1)
#modelData$logDContDiff[modelData$Rcont >= modelData$Dcont] <- log(modelData$Rcont[modelData$Rcont >= modelData$Dcont] - modelData$Dcont[modelData$Rcont >= modelData$Dcont] + 1)*-1

modelData$OpenInc <- as.factor(modelData$OpenInc)
modelData$midPres <- as.factor(modelData$midPres)

modelData$ideologyEffect2 <- modelData$ideologyEffect*modelData$Repub

modelData$crossPressure <- 0
modelData$crossPressure[sign(modelData$INC_DW_NOM) != sign(modelData$PVI2) & modelData$INC == "D"] <- -1
modelData$crossPressure[sign(modelData$INC_DW_NOM) != sign(modelData$PVI2) & modelData$INC == "R"] <- 1

## Final model data before subsetting to challenged becomes exploratory dataset
explore <- modelData
# Additional (unchallenged) seats -- we will add this data to our final dataset later
rfTestAdd <- subset(modelData, year == "2018" & Challenge == 0)
# Subset model data -- must be a challenged race
modelData <- subset(modelData, Challenge == 1)


### Fix exploratory dataset
#colnames(explore)
explore <- subset(explore, select = c("year", "state", "district", "oPct", "ID", "INC", "Winner", "Open", "Challenge", "Dcont", "Rcont", "Ocont", "Midterm.", "PVI...100.to.100.", 
                                      "PresParty", "Pres_Incumbent_SameParty", "PrevPresR.", "PrevPresD.", "PrevPresImcumb.", "YearsIncumbPartyControl", "TermsDemControl", 
                                      "TermsRepControl", "Age.25.to.44", "Age.65.and.over", "Race.White", "Race.Black.or.African.American", "Race.American.Indian.and.Alaska.Native",
                                      "Race.Asian", "Race.Native.Hawaiian.and.Other.Pacific.Islander", "Race.Some.other.race", "Race.Two.or.more.races", 
                                      "Race.Hispanic.or.Latino", "Unemployment.Rate", "Delta.unemployment.Rate", "Median.household.income", "PercentDelta.median.household.income",
                                      "Percent.below.poverty.level", "INC_DW_NOM", "Sabato", "SabatoNum", "PVI2", "IncWin", "thirdParty", "midtermEffect", "ideologyEffect", 
                                      "IncCont", "NincCont", "IncContDiff", "dContDiff", "logIncCont", "PVIeffect", "dPct2", "rPct2", "dWin", "logNincCont", "IncPct2", "SabatoScore",
                                      "Polls", "dSwing", "PrevElectionD2", "Prev2ElectionD2", "Prev2ElectionD2Diff", "OpenInc", "midPres", "PrevElectionD2Diff", "PrevElectionD2DiffSwing",
                                      "logDCont", "logRCont", "logDContDiff", "ideologyEffect2", "crossPressure"))

colnames(explore) <- c("year", "state", "district", "otherPct", "id", "INC", "winner", "open", "challenged", "demExpenditures", "repExpenditures", "otherExpenditures", "midterm", "cookPVI", 
                       "presParty", "presINCsameParty", "prevPresR", "prevPresD", "prevPresINC", "yrsINCpartyControl", "termsDemControl", 
                       "termsRepControl", "age25to44", "age65andover", "raceWhite", "raceBlackOrAfricanAmerican", "raceAmericanIndianAndAlaskaNative",
                       "raceAsian", "raceNativeHawaiianAndOtherPacificIslander", "RaceSomeOtherRace", "raceTwoOrMoreRaces", 
                       "raceHispanicOrLatino", "unemploymentRate", "deltaUnemploymentRate", "medianHouseholdIncome", "percentDeltaMedianHouseholdIncome",
                       "percentBelowPovertyLevel", "INCdwNom", "sabato", "sabatoNum", "weightedPVI", "incWin", "thirdParty", "midtermEffect", "ideologyEffect", 
                       "incExpenditures", "nincExpenditures", "incExpDiff", "dExpDiff", "logIncExp", "PVIeffect", "dPct2", "rPct2", "dWin", "logNincExp", "incPct2", "sabatoScore",
                       "polls", "dSwing", "prevElectionDem", "Prev2ElectionDem", "Prev2ElectionDemDiff", "OpenInc", "midPres", "prevElectionDemDiff", "prevElectionDemDiffSwing",
                       "logDemExp", "logRepExp", "logDemContDiff", "ideologyEffectDem", "crossPressure")

explore$dWin[explore$year == "2018"] <- "NA"
explore$incWin[explore$year == "2018"] <- "NA"
explore$thirdParty[explore$year == "2018"] <- "NA"

write.csv(explore, "midterms/outputData/explore.csv", row.names = FALSE)

#########################################################################################################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################

# Random forest
library(randomForest)

rfData <- subset(modelData, select = -c(Dreg, Rreg, Oreg, RegDiff, IncReg, NIncReg, PVI..0.to.100., PresApprov, PresDisapp, PresApprovDiff, ECI, thirdPartyEffect,
                                        total_votes, D, O, R, dPct, rPct, oPct, IncPct, ID2, Pres_Incumbent_SameParty, PrevElectionR., PrevElectionD., PrevElectionIncumbParty., 
                                        PrevElection2R., PrevElection2D., PrevElection2IncumbParty., PrevPresImcumb., PrevPresSameAsInc., YearsIncumbPartyControl, Sabato,
                                        IncWin, IncCont, NincCont, IncContDiff, logIncCont, PVIeffect, logNincCont, IncPct2, SabatoScore))

rfTrain <- na.omit(subset(rfData, year == "2006" | year == "2008" | year == "2010" | year == "2012" | year == "2014" | year == "2016"))
rfTest <- subset(rfData, year == "2018")
rfTest[is.na(rfTest)] <- 0
levels(rfTest$Winner) <- c("R", "D", "0")
rfTest$Winner[is.na(rfTest$Winner)] <- "0"

#Training data (subset of variables)
X.train <- subset(rfTrain, select = -c(year, state, district, ID, Winner, Repub, rPct2, dWin, logDCont, logRCont, ideologyEffect, ideologyEffect2, Prev2ElectionD2, Dcont, Rcont, PVI...100.to.100., PrevPresD., PrevPresR., midtermEffect, Challenge, PresYrsIncumb, PrevPresParty, PrevElectionD2, demoScore))

#colnames(rfData)

# Random forest generation and prediction
set.seed(1)
rf <- randomForest(x = X.train[,-28], y = X.train[,28], mtry = 10, importance = TRUE, ntree = 500)
yhat.rf <- predict(rf, newdata = rfTest)

#par(pty = "s")
#plot(yhat.rf, rfTest$dPct2, main = "Predicted versus actual", xlab = "Predicted Incumbent Vote Share", ylab = "Actual Incumbent Vote Share", xlim = c(.1,1), ylim = c(.1,1))
#abline(0,1)
#sqrt(mean((yhat.rf - rfTest$dPct2)^2))
#mean(abs(yhat.rf - rfTest$dPct2))
#par(pty = "m")
#varImpPlot(rf, n.var = 36)

#table(yhat.rf >= 0.5, rfTest$dPct2 >= 0.5)
#table(yhat.rf >= 0.5)
#hist(yhat.rf - rfTest$dPct2, breaks = 24, main = "Histogram of errors (predicted - actual)", xlab = "Error", xlim = c(-.15,.15))

########################################################################################
#### Simulation -- counting the number of seats won by each party in each iteration ####
########################################################################################
numSim <- 20000
dfCount <- as.data.frame(seq(1,numSim,1))
colnames(dfCount) <- "sim"
dfCount$dWin <- 0
dfCount$rWin <- 0

# Dataset used for shiny app, which includes the predicted vote shares from rf and number of times the Dem won each seat
rfTestCheck <- rfTest
rfTestCheck$dPctPred <- yhat.rf
rfTestCheck$Pred <- 0

# 
wins <- numeric(numSim)
counts <- numeric(nrow(rfTest))

# Might need this package to create distributions
library(statmod)

# Iterate!
start_time <- Sys.time()
for(i in 1:numSim){
  rfTest2 <- rfTest
  natError <- rnorm(1, mean = 0, sd = 3)
  rfTest2$dSwing <- rfTest2$dSwing + natError
  rfTest2$Polls <- rfTest2$Polls + natError
  #districtError <- rnorm(nrow(rfTest2), mean = natError/100, sd=.05)
  districtError <- rnorm(nrow(rfTest2), mean = natError/100, sd=(.5 - abs(yhat.rf-.5))/7)
  #districtError <- rinvgauss(nrow(rfTest2), mean = (.5 - abs(yhat.rf-.5))/8, shape = .2) # or should we use inverse normal dist? rinvgauss()
  #districtError <- rfTest2$demoScore + natError/100
  #districtError <- rfTest2$demoScore + rnorm(nrow(rfTest2), mean = natError/100, sd=.05)
  yhat.rf <- predict(rf, newdata = rfTest2)
  wins[i] <- sum(yhat.rf + districtError >= 0.5)
  counts <- counts + (yhat.rf + districtError >= 0.5)
}
end_time <- Sys.time()

# Count the number of times the Democrats took control of the House, and how many times the Dem candidate won each district
dfCount$dWin <- wins
rfTestCheck$Pred <- counts

# Add the data we removed earlier for races that are unchallenged
rfTestAdd <- subset(rfTestAdd, select = -c(Dreg, Rreg, Oreg, RegDiff, IncReg, NIncReg, PVI..0.to.100., PresApprov, PresDisapp, PresApprovDiff, ECI, thirdPartyEffect,
                                        total_votes, D, O, R, dPct, rPct, oPct, IncPct, ID2, Pres_Incumbent_SameParty, PrevElectionR., PrevElectionD., PrevElectionIncumbParty., 
                                        PrevElection2R., PrevElection2D., PrevElection2IncumbParty., PrevPresImcumb., PrevPresSameAsInc., YearsIncumbPartyControl, Sabato,
                                        IncWin, IncCont, NincCont, IncContDiff, logIncCont, PVIeffect, logNincCont, IncPct2, SabatoScore))
rfTestAdd$Pred <- numSim
rfTestAdd$Pred[rfTestAdd$SabatoNum > 0] <- 0
rfTestAdd$Pred[rfTestAdd$SabatoNum < 0] <- numSim
rfTestAdd$dPctPred <- 1
rfTestAdd$dPctPred[rfTestAdd$SabatoNum > 0] <- 0

rfTestCheck <- rbind(rfTestCheck, rfTestAdd)

# How many seats were unchallenged? We need this number for the shiny app
demUnchallenged <- nrow(subset(electionsRaw, year == "2018" & Challenge == 0 & INC == "D"))
dfCount$demUnchallenged <- demUnchallenged
#hist(dfCount$dWin + demUnchallenged, breaks = seq(0,435,1), xlim = c(150,300), main = "")
#abline(v = 218, lty = "dashed")
#abline(v = median(dfCount$dWin) + demUnchallenged)
#table(dfCount$dWin + demUnchallenged >= 218)

# What are the odds that the Dems take control of the House?
dProb <- table(dfCount$dWin + demUnchallenged >= 218)[2]/numSim

#ggplot(dfCount, aes(dWin + demUnchallenged)) + geom_histogram(binwidth = 1, aes(fill = dWin + demUnchallenged >= 218))
#ggplot(dfCount, aes(dWin + demUnchallenged - 218)) + geom_histogram(binwidth = 5, aes(fill = dWin + demUnchallenged - 218 > 0))
#ggplot(rfTestCheck, aes(round(Pred/numSim, 1))) + geom_dotplot(dotsize = .5)

# Write out the data that tells us the Dems chances AND the individual district data (once to use for the app, and once for archiving in case we need it later)
write.csv(dfCount, "midterms/outputData/dfCount.csv", row.names = FALSE)
write.csv(rfTestCheck, "midterms/outputData/rfTestCheck.csv", row.names = FALSE)

write.csv(rfTestCheck, paste0("midterms/outputData/Archive/rfTestCheck", Sys.Date(), ".csv"), row.names = FALSE)

# Write out the data that shows the Dems chances to take control for every day of the election season
#dWinDf <- data.frame(Date = seq(as.Date('2018-06-14'),as.Date('2018-11-06'),by = 1), dProb = NA)
#write.csv(dWinDf, "dWin.csv", row.names = FALSE)
dWin <- read.csv("midterms/outputData/dWin.csv")
dWin$Date <- as.Date(dWin$Date, format = "%Y-%m-%d")
dWin$dProb[dWin$Date == Sys.Date()] <- dProb
write.csv(dWin, "midterms/outputData/dWin.csv", row.names = FALSE)


#rf2 <- randomForest(as.factor(dWin) ~ . -year -state -district -ID -Winner -Repub -rPct2 -dPct2 -incSwing -logDCont -logRCont -SabatoNum -ideologyEffect -Dcont -Rcont -PrevPresD. -PrevPresR. -midtermEffect, data = rfTrain, mtry = 10, importance = TRUE, ntree = 500)
#yhat.rf2 <- predict(rf2, newdata = rfTest, type = "prob")
#rf2Inspect <- cbind(rfTest, dProb = yhat.rf2[,2])

#plot(rf2Inspect$dProb, rfTestCheck$Pred/10000)
#abline(0,1)

# Refresh the shiny app!
#quit(save = "no")
rsconnect::setAccountInfo(name='pquinn1991', token='74383A64C5E471D7D7F661261D4E08DE', secret='z9OMYbFWp6HYA3mYOy6LeqQM52RdBC/zEVV+/NzW')
library(rsconnect)
setwd("midterms")
deployApp(account = "pquinn1991", launch.browser = FALSE, appName = "midterms")
Y

