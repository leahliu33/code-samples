#/*===========================================================================
#  Project     : CESOP analysis — compulsory voting & vote choice (Brazil)
#  Author      : Ziqian (Leah) Liu
#  Created     : Nov 2025
#
#  Overview
#  --------
#  This script analyzes Brazilian individual-level survey data (IBOPE and
#  Datafolha, national and state samples) to estimate how compulsory voting
#  affects vote choice and turnout. Identification compares respondents just
#  below and just above Brazil's compulsory-voting age cutoffs (17-18 vs. 69-70).
#
#  Structure
#  ---------
#  PART 0  Program setup — packages and working directory
#  PART 1  Helper functions — randomization-inference test + results-table builder
#  PART 2  Load data & construct variables
#  PART 3  Descriptive discontinuity check across ages
#  PART 4  Main analysis — difference-in-proportions tests (Bolsonaro vote, turnout)
#  PART 5  Robustness — two-year window, placebo cutoffs, demographic controls
#  PART 6  Pooled analysis — stacked national + state surveys
#
#  Inputs      : IBOPE.RData, DATAFOLHA.RData, IBOPE_REGIONAL.RData,
#                DATAFOLHA_REGIONAL.RData; R_Functions/PermutationTest.R
#  Outputs     : LaTeX tables in result/table/ (CESOP_Bolsonaro, CESOP_Turnout,
#                CESOP_Placebo, and the control-model tables)
#===========================================================================*/


#===============================================================================================
#                              PART 0: Program setup
#===============================================================================================

# Packages actually used in this script
library(xtable)      # export result tables to LaTeX
library(stargazer)   # regression tables
library(estimatr)    # lm_robust() and starprep() for robust standard errors
# (Base stats::t.test and a sourced permutation-test function do the inference.)

# set working directory 
setwd("data/raw/cesop")
getwd()

# remove all objects from the current workspace
rm(list = ls())


#===============================================================================================
#                              PART 1: Helper functions
#===============================================================================================

# Two helpers used throughout the analysis
#   (1) a randomization-inference (permutation) test, sourced from R_Functions;
#   (2) tests(): assembles a difference-in-means results table for two datasets.

## 1. Call RI function

source("R_Functions/PermutationTest.R")

# 2. Define function for results

  tests<- function(y1,c1,t1,y2,c2,t2){ # y1: outcome var; c1: control group for dataset 1; t1: treatment group indicator for dataset 1
  
  # mean
  dom1<- round(t.test(y1[c1 == 1], y1[t1==1])$estimate,3) # estimate the mean value for this group, rounds to 3 decimal places
  diff1<-dom1[2]-dom1[1] # the difference in mean in two groups (compulsory and voluntary in dataset 1)
  dom2<- round(t.test(y2[c2 == 1], y2[t2==1])$estimate,3)
  diff2<-dom2[2]-dom2[1]
  
  # standardp-value
  pval1<- round(t.test(y1[c1 == 1], y1[t1==1])$p.value,3)
  pval2<- round(t.test(y2[c2 == 1], y2[t2==1])$p.value,3)
  
  #sample size
  N1 <- c(sum(!is.na(y1[c1 == 1])), sum(!is.na(y1[t1 == 1])))
  N2 <- c(sum(!is.na(y2[c2 == 1])), sum(!is.na(y2[t2 == 1])))
  
  # extract data for each group, prepares data for permutation test
  Y1a <- y1[c1 == 1]
  Y1b <- y1[t1 == 1]
  Y2a <- y2[c2 == 1]
  Y2b <- y2[t2 == 1]
  
  # ri p value
  ri1<- PermutationTest(Y1a,Y1b,na.rm = TRUE)
  ri2<- PermutationTest(Y2a,Y2b,na.rm = TRUE)
  
  # combine result into tables
  results<- cbind(c(round(dom1,3), round(pval1,4), round(ri1,4),  N1),
                  c(round(dom2,3), round(pval2,4), round(ri2,4),  N2))
  rownames(results)<- c("Mean Voluntary","Mean Compulsory", "p-value (t-test)", "p-value (ri)", "N Voluntary", "N Compulsory")
  colnames(results)<-c("State","National")
  return(results)
}


#===============================================================================================
#                     PART 2: Load data & construct variables
#===============================================================================================

# load data
load("IBOPE.RData")
load("DATAFOLHA.RData")
load("IBOPE_REGIONAL.RData")
load("DATAFOLHA_REGIONAL.RData")

# create compilers
Ibope_R2$comp_all = ifelse(Ibope_R2$comp_lower== 1 | Ibope_R2$comp_upper == 1, 1, 0) # if either comp_lower or comp_upper = 1, then comp_all = 1. otherwise = 0 
Datafolha_R2$comp_all = ifelse(Datafolha_R2$comp_lower== 1 | Datafolha_R2$comp_upper == 1, 1, 0)
ibope_surveys$comp_all = ifelse(ibope_surveys$comp_lower== 1 | ibope_surveys$comp_upper == 1, 1, 0)
datafolha_surveys$comp_all = ifelse(datafolha_surveys$comp_lower== 1 | datafolha_surveys$comp_upper == 1, 1, 0)

# Create new vars
ibope_surveys$survey_id<-unlist(ibope_surveys$survey_id) # convert from a list format to a vector

#===============================================================================================
#                     PART 3: Descriptive discontinuity check
#===============================================================================================

# Inspect the raw proportion voting for Bolsonaro by single year of age, to see
# the jump around the compulsory-voting cutoffs before running formal tests.

  # ibope
  prop.table(table(Ibope_R2$bolsonaro_a[Ibope_R2$age==16]))
  prop.table(table(Ibope_R2$bolsonaro_a[Ibope_R2$age==17]))
  prop.table(table(Ibope_R2$bolsonaro_a[Ibope_R2$age==18]))
  prop.table(table(Ibope_R2$bolsonaro_a[Ibope_R2$age==19]))
  
  prop.table(table(ibope_surveys$bolsonaro_a[ibope_surveys$age==16]))
  prop.table(table(ibope_surveys$bolsonaro_a[ibope_surveys$age==17]))
  prop.table(table(ibope_surveys$bolsonaro_a[ibope_surveys$age==18]))
  prop.table(table(ibope_surveys$bolsonaro_a[ibope_surveys$age==19]))

  #datafolha (similar)
  prop.table(table(Datafolha_R2$bolsonaro_a[Datafolha_R2$age==16]))
  prop.table(table(Datafolha_R2$bolsonaro_a[Datafolha_R2$age==17]))
  prop.table(table(Datafolha_R2$bolsonaro_a[Datafolha_R2$age==18]))
  prop.table(table(Datafolha_R2$bolsonaro_a[Datafolha_R2$age==19]))
  
  prop.table(table(datafolha_surveys$bolsonaro_a[datafolha_surveys$age==16]))
  prop.table(table(datafolha_surveys$bolsonaro_a[datafolha_surveys$age==17]))
  prop.table(table(datafolha_surveys$bolsonaro_a[datafolha_surveys$age==18]))
  prop.table(table(datafolha_surveys$bolsonaro_a[datafolha_surveys$age==19]))
  

#===============================================================================================
#              PART 4: Main analysis — difference-in-proportions tests
#===============================================================================================

  # from now on, we've focus on ibope data
  # we did a bunch of analysis in "discontinuity.R", and find out ibope data serves a more reliable and informative data source
  
## Tests Main --------------------------------------------------------------------

# Bolsonaro (1 year winder (17-18, 69-70)) 

  tablall<- cbind(tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol_lower_R,ibope_surveys$comp_lower_R, Ibope_R2$bolsonaro_a, Ibope_R2$vol_lower_R,Ibope_R2$comp_lower_R),
                  tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol_upper_R,ibope_surveys$comp_upper_R, Ibope_R2$bolsonaro_a, Ibope_R2$vol_upper_R,Ibope_R2$comp_upper_R))
  
  print(xtable(tablall, type = "latex",caption = "Difference in the proportion of respondents that voted for Bolsonaro by compulsory voting status",
               label = "tab:bolsonaro_cesop",align = "lcccc"), caption.placement = "top",
        file = "result/table/CESOP_Bolsonaro.tex")

# Turnout (1 year) 

  tablall<- cbind(tests(ibope_surveys$voted_first, ibope_surveys$vol_lower_R,ibope_surveys$comp_lower_R, Ibope_R2$voted_first, Ibope_R2$vol_lower_R,Ibope_R2$comp_lower_R),
                  tests(ibope_surveys$voted_first, ibope_surveys$vol_upper_R,ibope_surveys$comp_upper_R, Ibope_R2$voted_first, Ibope_R2$vol_upper_R,Ibope_R2$comp_upper_R))
  
  print(xtable(tablall, type = "latex",caption = "Difference in the proportion of respondents that turned out to vote in the first round by compulsory voting status",
               label = "tab:turnout_cesop",align = "lcccc"), caption.placement = "top",
        file = "result/table/CESOP_Turnout.tex")


#===============================================================================================
#              PART 5: Robustness — window, placebo, controls
#===============================================================================================

# 1. Two-year window 
  # bolsonaro
  tablall<- cbind(tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol_lower,ibope_surveys$comp_lower, Ibope_R2$bolsonaro_a, Ibope_R2$vol_lower,Ibope_R2$comp_lower),
                  tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol_upper,ibope_surveys$comp_upper, Ibope_R2$bolsonaro_a, Ibope_R2$vol_upper,Ibope_R2$comp_upper))
  
  print(xtable(tablall, type = "latex",caption = "Difference in the proportion of respondents that voted for Bolsonaro by compulsory voting status",
               label = "tab:bolsonaro_cesop_tyw",align = "lcccc"), caption.placement = "top",
        file = "result/table/CESOP_Bolsonaro_TwoYearWindow.tex")


## 2. Placebo (one-year window to be consistent)
  
  # fake discontinuty at: 21 and 66
  Ibope_R2$vol1_placebo = as.numeric(Ibope_R2$age == 20)
  Ibope_R2$comp1_placebo = as.numeric(Ibope_R2$age==21)
  Ibope_R2$vol2_placebo = as.numeric(Ibope_R2$age == 66)
  Ibope_R2$comp2_placebo = as.numeric(Ibope_R2$age==67)
  ibope_surveys$vol1_placebo = as.numeric(ibope_surveys$age == 20)
  ibope_surveys$comp1_placebo = as.numeric(ibope_surveys$age==21)
  ibope_surveys$vol2_placebo = as.numeric(ibope_surveys$age == 66)
  ibope_surveys$comp2_placebo = as.numeric(ibope_surveys$age==67)
  
  tablall<- cbind(tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol1_placebo,ibope_surveys$comp1_placebo, Ibope_R2$bolsonaro_a, Ibope_R2$vol1_placebo,Ibope_R2$comp1_placebo),
                  tests(ibope_surveys$bolsonaro_a, ibope_surveys$vol2_placebo,ibope_surveys$comp2_placebo, Ibope_R2$bolsonaro_a, Ibope_R2$vol2_placebo,Ibope_R2$comp2_placebo))
  
  rownames(tablall)<- c("Mean Younger","Mean Older", "p-value (t-test)", "p-value (ri)", "N Younger", "N Older")
  
  print(xtable(tablall, type = "latex",
               caption = "Difference in the proportion of respondents that voted for Bolsonaro in the first round between 20-21 and 66-67 olds",
               label = "tab:placebo_cesop",align = "lcccc"), 
        caption.placement = "top",
        file = "result/table/CESOP_Placebo.tex")


## 3. controls (using stargazer) ---------------

  # National 
  
  restricted = Ibope_R2[(Ibope_R2$age>=17 & Ibope_R2$age<=18) | (Ibope_R2$age>=69 & Ibope_R2$age<=70),]
  restricted$comp_all = ifelse(restricted$comp_lower== 1 | restricted$comp_upper == 1, 1, 0)
  restricted$lower = ifelse(restricted$age %in% 17:18,1,0)
  
  fit1<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) + comp_all *scale(pid_pt) + comp_all *scale(pid_psdb) + 
             as.factor(survey) +  comp_all * scale(lower), data= restricted) 
  
  restricted = Ibope_R2[Ibope_R2$age>=17 & Ibope_R2$age<=18,]
  restricted$comp_all = restricted$comp_lower
  fit2<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) + comp_all *scale(pid_pt) + comp_all *scale(pid_psdb) + as.factor(survey),
           data= restricted) 
  
  restricted = Ibope_R2[Ibope_R2$age>=69 & Ibope_R2$age<=70,]
  restricted$comp_all = restricted$comp_upper
  fit3<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) + comp_all *scale(pid_pt) + comp_all *scale(pid_psdb) + as.factor(survey),
           data= restricted) 
  
  # State
  
  restricted = ibope_surveys[(ibope_surveys$age>=17 & ibope_surveys$age<=18) | (ibope_surveys$age>=69 & ibope_surveys$age<=70),]
  restricted$comp_all = ifelse(restricted$comp_lower== 1 | restricted$comp_upper == 1, 1, 0)
  restricted$lower = ifelse(restricted$age %in% 17:18,1,0)
  restricted$bolsonaro_a2 = restricted$bolsonaro_a
  
  fit4<-lm(bolsonaro_a2 ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) + comp_all * scale(lower) + as.factor(survey_id), data= restricted) 
  
  restricted = ibope_surveys[ibope_surveys$age>=17 & ibope_surveys$age<=18,]
  restricted$comp_all = restricted$comp_lower
  restricted$bolsonaro_a2 = restricted$bolsonaro_a
  
  fit5<-lm(bolsonaro_a2 ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) + as.factor(survey_id),
           data= restricted) 
  
  
  restricted = ibope_surveys[ibope_surveys$age>=69 & ibope_surveys$age<=70,]
  restricted$comp_all = restricted$comp_upper
  restricted$bolsonaro_a2 = restricted$bolsonaro_a
  fit6<-lm(bolsonaro_a2 ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
             comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman),
           data= restricted) 
  
  # Table 
  stargazer(fit1, fit2, fit3, fit4, fit5, fit6, 
            se = starprep(fit1, fit2, fit3, fit4, fit5, fit6),          
            title="The effect of compulsory on having voted for Bolsonaro in the first round", 
            covariate.labels = c("Compulsory Voting"), notes.align = "c",
            omit = c("primary","secondary","higher","black","white","catholic","evangelic",
                     "pid_pt","pid_psdb","woman","survey","survey_id","lower"),
            column.labels = c("All","17-18","69-70", "All","17-18","69-70"),
            dep.var.labels = c("National Surveys", "State Surveys"),  style="ajps",
            df = FALSE, model.numbers=FALSE,  label=c("tab:controls"),
            add.lines = list(c("Controls","Yes","Yes", "Yes", "Yes","Yes","Yes")))


#===============================================================================================
#                PART 6: Pooled analysis — stacked national + state
#===============================================================================================

  # create variable to identify national and state survey
  Ibope_R2$id="National"
  ibope_surveys$id="State"
  Datafolha_R2$id="National"
  datafolha_surveys$id="State"
  
  # combine dataset
  selection<- c("age","bolsonaro_a","voted_first","vol_lower","vol_lower_R","comp_lower","comp_lower_R","vol_upper","vol_upper_R","comp_upper","comp_upper_R","id",
                "primary","secondary","higher","black","white","catholic","evangelic","woman")
  
  Ibope_all<-rbind(Ibope_R2[,selection], ibope_surveys[,selection])
  Datafolha_all<-rbind(Datafolha_R2[,selection], datafolha_surveys[,selection])
  
  Ibope_all$state = as.numeric(Ibope_all$id=="State")
  Datafolha_all$state = as.numeric(Datafolha_all$id=="State")
  
      # not appeared in the paper, but same test was conducted in the discontinuty_comp R 
      tablall<- cbind(tests(Ibope_all$bolsonaro_a, Ibope_all$vol_lower_R,Ibope_all$comp_lower_R,Ibope_all$vol_upper_R,Ibope_all$comp_upper_R),
                      tests(Datafolha_all$bolsonaro_a, Datafolha_all$vol_lower_R,Datafolha_all$comp_lower_R,Datafolha_all$vol_upper_R,Datafolha_all$comp_upper_R))
      
      #print(xtable(tablall, type = "latex",caption = "Difference in the proportion that voted for Bolsonaro by compulsory voting status (one-year window)",
      #             label = "tab:bolsonaro_cesop_state",align = "lcccc"), caption.placement = "top",
      #      file = "CESOP_Bolsonaro_state_oyw.tex")


  #all
  restricted = Ibope_all[(Ibope_all$age>=17 & Ibope_all$age<=18) | (Ibope_all$age>=69 & Ibope_all$age<=70),]
  restricted$comp_all = ifelse(restricted$comp_lower_R== 1 | restricted$comp_upper_R == 1, 1, 0)
  
  lm_robust(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) 
            + comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
            data= restricted)
  lm_robust(bolsonaro_a ~ comp_all * scale(state), data= restricted )
  fit1<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) 
           + comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
           data= restricted)
  
  #lower
  restricted = Ibope_all[(Ibope_all$age>=17 & Ibope_all$age<=18),]
  restricted$comp_all = restricted$comp_lower_R
  lm_robust(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) +
              comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
            data= restricted)
  lm_robust(bolsonaro_a ~ comp_all * scale(state), data= restricted )
  lm_robust(bolsonaro_a ~ comp_all , data= restricted )
  fit2<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) 
           + comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
           data= restricted)
  
  #upper
  restricted = Ibope_all[Ibope_all$age>=69 & Ibope_all$age<=70,]
  restricted$comp_all = restricted$comp_upper_R
  lm_robust(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) 
            + comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
            data= restricted)
  lm_robust(bolsonaro_a ~ comp_all * scale(state), data= restricted)
  lm_robust(bolsonaro_a ~ comp_all , data= restricted )
  fit3<-lm(bolsonaro_a ~ comp_all * scale(primary) + comp_all *scale(secondary) + comp_all *scale(higher) + comp_all *scale(black) + comp_all *scale(white) 
           + comp_all *scale(catholic) + comp_all *scale(evangelic) + comp_all *scale(woman) +  comp_all *scale(state),
           data= restricted)
  
  stargazer(fit1, fit2, fit3, 
            se = starprep(fit1, fit2, fit3),          
            title="The effect of compulsory having voted for Bolsonaro in the first round using state and national surveys", 
            covariate.labels = c("Compulsory Voting"),
            omit = c("primary","secondary","higher","black","white","catholic","evangelic",
                     "state","woman"),
            column.labels = c("All","Lower", "Upper"),
            dep.var.labels   = c("Voted for Bolsonaro"), style="ajps",
            df = FALSE, model.numbers=FALSE,  label=c("tab:controls_all"),
            add.lines = list(c("Controls", "Yes", "Yes", "Yes")))


