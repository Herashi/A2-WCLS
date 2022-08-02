## This file generates direct effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

setwd("~/Documents/MRT_competition/Case Study")
library(zoo)
source("xzoo.R")
load("IHS_MRT.RData")

mood_results = mood_results2 =  mood_results3 = mood_results4 = list()
step_results = step_results2 = step_results3 = step_results4 =  list()

library(geepack)
library(mitml)

num_impute = impute_list$num_impute

for(impute_iter in 1:num_impute){

  analysis_dat_gee = IHS_MRT[[impute_iter]]
  
  # MOOD 
  
  gee_result_mood = geeglm(MOOD ~ week_category_new_c , data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result_mood2 = geeglm(MOOD ~ week_category_new_c + MOODprev_c, data = analysis_dat_gee, weights = weights, id = UserID, scale.fix = T)
  gee_result_mood3 = geeglm(MOOD ~ week_category_new_c * MOODprev_c, data = analysis_dat_gee, weights = weights, id = UserID, scale.fix = T)
  
  centering_model = lm(MOODprev~ I(study_week-1),data = analysis_dat_gee)
  analysis_dat_gee$MOODprev_c = analysis_dat_gee$MOODprev - centering_model$fitted.values
  
  gee_result_mood4 = geeglm(MOOD ~ week_category_new_c * (study_week + MOODprev_c), data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)

  
  # RE_MOOD[impute_iter] = (summary(gee_result_mood_agg)$coefficients[,2][2]/summary(gee_result_mood_agg2)$coefficients[,2][2])^2

  mood_results[[impute_iter]] = gee_result_mood
  mood_results2[[impute_iter]] = gee_result_mood2
  mood_results3[[impute_iter]] = gee_result_mood3
  mood_results4[[impute_iter]] = gee_result_mood4
  
  # STEP
  
  gee_result = geeglm(STEP_COUNT ~ week_category_new_c , data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  gee_result2 = geeglm(STEP_COUNT ~ week_category_new_c + STEP_COUNTprev_c, data = analysis_dat_gee, weights = weights, id = UserID, scale.fix = T)
  gee_result3 = geeglm(STEP_COUNT ~ week_category_new_c * STEP_COUNTprev_c, data = analysis_dat_gee, weights = weights, id = UserID, scale.fix = T)
  
  centering_model = lm(STEP_COUNTprev~ I(study_week-1),data = analysis_dat_gee)
  analysis_dat_gee$STEP_COUNTprev_c = analysis_dat_gee$STEP_COUNTprev - centering_model$fitted.values
  
  gee_result4 = geeglm(STEP_COUNT ~ week_category_new_c * (study_week + STEP_COUNTprev_c), data = analysis_dat_gee,weights = weights, id = UserID, scale.fix = T)
  
  
  
  step_results[[impute_iter]] = gee_result
  step_results2[[impute_iter]] = gee_result2
  step_results3[[impute_iter]] = gee_result3
  step_results4[[impute_iter]] = gee_result4
  
  print(impute_iter)
}

testEstimates(mood_results)
testEstimates(mood_results2)
testEstimates(mood_results3)
testEstimates(mood_results4)

testEstimates(step_results)
testEstimates(step_results2)
testEstimates(step_results3)
testEstimates(step_results4)

