## This file generates direct effect estimates 
## If RDS files for the models already exist
## Go to the file "present_results.R" to avoid re-running 
## as code takes a while to generate the MI estimates.

## Code to combine data over several impuations and properly pools the results, 
## Since the randomization probability is constant, we can do this with gee

setwd("~/Documents/MRT_competition/Case Study")

#####################
# DATA PREP
####################

# library(zoo)
library(geepack)

# load("imputation_list_daily_separated_20.RData")
# library(readr)
# survey_dat = read_csv("IHSdata_2018.csv")
# 
# num_impute = impute_list$num_impute
# # RE_MOOD = rep(NA,num_impute)
# # RE_STEP = rep(NA,num_impute)
# # RE_SLEEP= rep(NA,num_impute)
# 
# IHS_MRT = list()
# 
# for(impute_iter in 1:num_impute){
#   cur_list = impute_list[[impute_iter]]
#   
#   all_baseline = cur_list$all_baseline
#   full_data = cur_list$full_data
#   colnames(full_data)[1] = "UserID"
#   full_data_complete = cur_list$full_data_complete
#   
#   all_baseline$UserID = as.factor(all_baseline$UserID)
#   full_data_complete$UserID = as.factor(full_data_complete$UserID)
#   
#   baseline_step = apply(all_baseline[,c(10,13,16)], MARGIN = 1, FUN = mean)
#   baseline_sleep = apply(all_baseline[,c(11,14,17)], MARGIN = 1, FUN = mean)
#   baseline_mood = apply(all_baseline[,c(12,15,18)], MARGIN = 1, FUN = mean)
#   baseline_average = data.frame(UserID = all_baseline$UserID, 
#                                 STEP_COUNTprev = baseline_step, 
#                                 SLEEP_COUNTprev=baseline_sleep, 
#                                 MOODprev = baseline_mood)
#   baseline_average$study_week = 1
#   # names(baseline_average)[2:4] = c('STEP_COUNTprev', 'SLEEP_COUNTprev', 'MOODprev')
#   baseline_average$week_categoryprev = NA
#   
#   aggregate_weekly = aggregate(full_data_complete[, 5:7], by = full_data_complete[,c(1,3,4)], FUN = mean)
#   aggregate_weekly2 = aggregate_weekly
#   names(aggregate_weekly2)[3:6] = paste(names(aggregate_weekly2)[3:6], "prev", sep = '')
#   aggregate_weekly2$study_week = aggregate_weekly2$study_week + 1
#   aggregate_weekly2 = rbind(aggregate_weekly2, baseline_average)
#   
#   aggregate_weekly_new = merge(aggregate_weekly, aggregate_weekly2, by = c('UserID', 'study_week'), all.x = TRUE)
#   
#   aggregate_weekly_new1 = merge(aggregate_weekly_new, all_baseline[,1:9], by = 'UserID', all.x = TRUE)
#   
#   analysis_dat = aggregate_weekly_new1[, -7]
#   analysis_dat = analysis_dat[analysis_dat$week_category != 'unsure', ]
#   analysis_dat$week_category_new = as.numeric(analysis_dat$week_category != 'None')
#   analysis_dat$week_category = relevel(analysis_dat$week_category, ref = 'None')
#   analysis_dat_gee = analysis_dat[order(analysis_dat$UserID, analysis_dat$study_week), ]
#   analysis_dat_gee$week_category= droplevels(analysis_dat_gee$week_category)
#   
#   temp = merge(analysis_dat_gee, survey_dat[,c(1,4,5)], by = 'UserID', all.x = TRUE)
#   temp$INSTITUTION_STANDARD = as.factor(temp$INSTITUTION_STANDARD)
#   temp$Specialty = as.factor(temp$Specialty)
#   # temp = temp[!is.na(temp$Specialty),]
#   #test = aggregate(UserID ~ Specialty, data = temp, FUN = function(x){length(unique(x))})
#   temp = temp[!is.na(temp$INSTITUTION_STANDARD),]
#   test = aggregate(UserID ~ INSTITUTION_STANDARD, data = temp, FUN = function(x){length(unique(x))})
#   
#   
#   # Boruvka's weights
#   p_tilde = mean(analysis_dat_gee$week_category_new)
#   analysis_dat_gee$weights = ifelse(analysis_dat_gee$week_category_new==1,p_tilde/(3/4),
#                                     (1-p_tilde)/(1/4))
#   
#   analysis_dat_gee$week_category_new_c = analysis_dat_gee$week_category_new - p_tilde
#   
#   
#   # MOOD 
#   # centering_model = lm(MOODprev~ I(study_week-1),data = analysis_dat_gee)
#   # analysis_dat_gee$MOODprev_c = analysis_dat_gee$MOODprev - centering_model$fitted.values
#   
#   ## centering a bunch of significant predictors
#   analysis_dat_gee$MOODprev_c = analysis_dat_gee$MOODprev - mean(analysis_dat_gee$MOODprev)
#   analysis_dat_gee$STEP_COUNTprev_c = analysis_dat_gee$SLEEP_COUNTprev - mean(analysis_dat_gee$SLEEP_COUNTprev)
#   analysis_dat_gee$SLEEP_COUNTprev_c = analysis_dat_gee$SLEEP_COUNTprev - mean(analysis_dat_gee$SLEEP_COUNTprev)
#   analysis_dat_gee$pre_intern_mood_c = analysis_dat_gee$pre_intern_mood - mean(analysis_dat_gee$pre_intern_mood)
#   analysis_dat_gee$Neu0_c = analysis_dat_gee$Neu0 - mean(analysis_dat_gee$Neu0)
#   analysis_dat_gee$depr0_c = analysis_dat_gee$depr0 - mean(analysis_dat_gee$depr0)
#   analysis_dat_gee$pre_intern_sleep_c = analysis_dat_gee$pre_intern_sleep - mean(analysis_dat_gee$pre_intern_sleep)
#   analysis_dat_gee$PHQtot0_c = analysis_dat_gee$PHQtot0 - mean(analysis_dat_gee$PHQtot0)
#   analysis_dat_gee$pre_intern_sqrt_step_c = analysis_dat_gee$pre_intern_sqrt_step - mean(analysis_dat_gee$pre_intern_sqrt_step)
#   
#   
#   IHS_MRT[[impute_iter]] = analysis_dat_gee
#   
#   print(impute_iter)
# }

# save(IHS_MRT,file = "IHS_MRT.RData")




#########################
# Select a subset of moderators
#########################
load("IHS_MRT.RData")
library(sets)

moderators = c("STEP_COUNTprev","SLEEP_COUNTprev","MOODprev","PHQtot0","Neu0",
               "depr0","pre_intern_mood","pre_intern_sleep","pre_intern_sqrt_step")
centered_moderators = paste0(moderators,"_c")

all_subsets  = as.vector(set_power(centered_moderators))
# to_be_selected = data.frame(subsest_var = 2:length(all_subsets),
#                             n = 0)
# 
# num_impute = 20
# 
# 
# 
# for(impute_iter in 1:num_impute){
#   print(impute_iter)
#   df = IHS_MRT[[impute_iter]]
#   
#   # the reference WCLS model
#   # need to adjust
#   
#   terms_wcls = c("week_category_new_c")
#   form_1 = as.formula("MOOD ~ week_category_new_c")
#   gee_MOOD_wcls = geeglm(formula = form_1, data = df, weights = weights, id = UserID, scale.fix = T)
#   stderr_wcls = summary(gee_MOOD_wcls)$coefficients["week_category_new_c","Std.err"]
#   
#   for (i in 2:length(all_subsets)){
#     
#     moderator_c = unlist(all_subsets[[i]])
#     
#     if(length(moderator_c)>1){
#       moderator_c = paste0(moderator_c, collapse = "+")
#       terms_cwcls = paste0("week_category_new_c * (",moderator_c, ")")
#     }else{
#       terms_cwcls = paste0("week_category_new_c * ",moderator_c)
#     }
#     
#     form_2 = as.formula(paste0("MOOD ~ ", terms_cwcls))
#     
#     tryCatch({
#       gee_MOOD_cwcls<- invisible(geeglm(formula = form_2, data = df, weights = weights, id = UserID, scale.fix = T))
#       
#       stderr_cwcls = summary(gee_MOOD_cwcls)$coefficients["week_category_new_c","Std.err"]
#       
#       if(stderr_wcls > stderr_cwcls){
#         to_be_selected[i-1,"n"]= to_be_selected[i-1,"n"]+1
#       }
#     }, 
#     warning = function(w) {
#       
#     },
#     error = function(e){
#       
#     })
#   }
# }
# 
# save(to_be_selected, file = "table.RData")

############################################################
# Change the WCLS model to include moderators as main effect
###########################################################
# load("IHS_MRT.RData")
# library(sets)
# 
# moderators = c("STEP_COUNTprev","SLEEP_COUNTprev","MOODprev","PHQtot0","Neu0",
#                "depr0","pre_intern_mood","pre_intern_sleep","pre_intern_sqrt_step")
# centered_moderators = paste0(moderators,"_c")
# 
# all_subsets  = as.vector(set_power(centered_moderators))
# to_be_selected = data.frame(subsest_var = 2:length(all_subsets),
#                             n = 0)
# 
# num_impute = 20
# 
# 
# 
# for(impute_iter in 1:num_impute){
#   print(impute_iter)
#   df = IHS_MRT[[impute_iter]]
#   
#   # the reference WCLS model
#   # need to adjust
#   
#   for (i in 2:length(all_subsets)){
#     
#     moderator_c = unlist(all_subsets[[i]])
#     
#     if(length(moderator_c)>1){
#       moderator_c = paste0(moderator_c, collapse = "+")
#       terms_cwcls = paste0("week_category_new_c * (",moderator_c, ")")
#     }else{
#       terms_cwcls = paste0("week_category_new_c * ",moderator_c)
#     }
#     
#     terms_wcls = paste0("week_category_new_c +",moderator_c)
#     form_1 = as.formula(paste0("MOOD ~ ", terms_wcls))
#     form_2 = as.formula(paste0("MOOD ~ ", terms_cwcls))
#     
#     tryCatch({
#       gee_MOOD_wcls = geeglm(formula = form_1, data = df, weights = weights, id = UserID, scale.fix = T)
#       stderr_wcls = summary(gee_MOOD_wcls)$coefficients["week_category_new_c","Std.err"]
#       
#       gee_MOOD_cwcls<- geeglm(formula = form_2, data = df, weights = weights, id = UserID, scale.fix = T)
#       
#       stderr_cwcls = summary(gee_MOOD_cwcls)$coefficients["week_category_new_c","Std.err"]
#       
#       if(stderr_wcls > stderr_cwcls){
#         to_be_selected[i-1,"n"]= to_be_selected[i-1,"n"]+1
#       }
#     }, 
#     warning = function(w) {
#       
#     },
#     error = function(e){
#       
#     })
#   }
# }
# 
# save(to_be_selected, file = "table_another.RData")

########################################
# Pick the model
########################################

load("table.RData")

# MOOD ~ week_category_new_c
# vs
# MOOD ~ week_category_new_c * moderators_c

selected = subset(to_be_selected, n==20)[,"var"]
selected_subsets = all_subsets[selected]

table(unlist(selected_subsets))
# depr0_c                 Neu0_c              PHQtot0_c      pre_intern_mood_c 
# 84                     96                     85                     96 
# pre_intern_sleep_c pre_intern_sqrt_step_c      SLEEP_COUNTprev_c       STEP_COUNTprev_c 
# 86                     82                     52                     52 


load("table_another.RData")

# MOOD ~ week_category_new_c + moderators_c
# vs
# MOOD ~ week_category_new_c * moderators_c


selected = subset(to_be_selected, n==20)[,"var"]
selected_subsets = all_subsets[selected]

table(unlist(selected_subsets))

# 
# depr0_c                 Neu0_c              PHQtot0_c      pre_intern_mood_c 
# 89                     94                     88                     96 
# pre_intern_sleep_c pre_intern_sqrt_step_c      SLEEP_COUNTprev_c       STEP_COUNTprev_c 
# 96                     91                     57                     57 



#################################
# Output the improvement
################################

# load("IHS_MRT.RData")
# num_impute = length(IHS_MRT)
# library(sets)
# 
# moderators = c("STEP_COUNTprev","SLEEP_COUNTprev","MOODprev","PHQtot0","Neu0",
#                "depr0","pre_intern_mood","pre_intern_sleep","pre_intern_sqrt_step")
# centered_moderators = paste0(moderators,"_c")
# 
# all_subsets  = as.vector(set_power(centered_moderators))
# to_be_selected = data.frame(subsest_var = 2:length(all_subsets),
#                             n = 0)
# 
# dif = matrix(NA, nrow = nrow(to_be_selected), ncol = num_impute)
# 
# to_be_selected = cbind(to_be_selected,dif)
# 
# 
# 
# for(impute_iter in 1:num_impute){
#   print(impute_iter)
#   df = IHS_MRT[[impute_iter]]
#   
#   # the reference WCLS model
#   # need to adjust
#   
#   for (i in 2:length(all_subsets)){
#     
#     moderator_c = unlist(all_subsets[[i]])
#     
#     if(length(moderator_c)>1){
#       moderator_c = paste0(moderator_c, collapse = "+")
#       terms_cwcls = paste0("week_category_new_c * (",moderator_c, ")")
#     }else{
#       terms_cwcls = paste0("week_category_new_c * ",moderator_c)
#     }
#     
#     terms_wcls = paste0("week_category_new_c +",moderator_c)
#     form_1 = as.formula(paste0("MOOD ~ ", terms_wcls))
#     form_2 = as.formula(paste0("MOOD ~ ", terms_cwcls))
#     
#     tryCatch({
#       gee_MOOD_wcls = geeglm(formula = form_1, data = df, weights = weights, id = UserID, scale.fix = T)
#       stderr_wcls = summary(gee_MOOD_wcls)$coefficients["week_category_new_c","Std.err"]
#       
#       gee_MOOD_cwcls<- geeglm(formula = form_2, data = df, weights = weights, id = UserID, scale.fix = T)
#       
#       stderr_cwcls = summary(gee_MOOD_cwcls)$coefficients["week_category_new_c","Std.err"]
#       
#       if(stderr_wcls > stderr_cwcls){
#         to_be_selected[i-1,"n"]= to_be_selected[i-1,"n"]+1
#         to_be_selected[i-1,2+impute_iter] = stderr_wcls - stderr_cwcls
#       }
#     }, 
#     warning = function(w) {
#       
#     },
#     error = function(e){
#       
#     })
#   }
# }
# 
# save(to_be_selected, file = "table_last.RData")

###################################
# Choose the best model (include the moderators in the main effect)
###################################

load("table_last.RData")
max(to_be_selected[,3:22],na.rm = T)
# 0.0001022066

loc = which(to_be_selected[,3:22] == max(to_be_selected[,3:22],na.rm = T), arr.ind = TRUE)
#      row col
# [1,] 164   9

# The best model is: 
unlist(all_subsets[loc[1]+1])
# "Neu0_c"                 "pre_intern_mood_c"      "pre_intern_sleep_c"     "pre_intern_sqrt_step_c"

# look at average improvement to choose the model
# only calculate the mean from n=20
to_be_selected$avg = rowMeans(to_be_selected[,3:22])

loc = which.max(to_be_selected[,"avg"])
# 466

# The best model is: 
unlist(all_subsets[loc+1])
# [1] "Neu0_c"                 "PHQtot0_c"              "STEP_COUNTprev_c"       "depr0_c"               
# [5] "pre_intern_mood_c"      "pre_intern_sleep_c"     "pre_intern_sqrt_step_c"

###################################
# Choose the best model (only A_t-p_t)
###################################

load("table_original_last.RData")

max(to_be_selected[,3:22],na.rm = T)
# 0.0002342594

loc = which(to_be_selected[,3:22] == max(to_be_selected[,3:22],na.rm = T), arr.ind = TRUE)
#      row col
# [1,] 146  12

# The best model is: 
unlist(all_subsets[loc[1]+1])
# "Neu0_c"                 "PHQtot0_c"              "pre_intern_mood_c"      "pre_intern_sqrt_step_c"


# look at average improvement to choose the model
# only calculate the mean from n=20
to_be_selected$avg = rowMeans(to_be_selected[,3:22])

loc = which.max(to_be_selected[,"avg"])
# 382

# The best model is: 
unlist(all_subsets[loc+1])
# [1] "Neu0_c"                 "PHQtot0_c"              "depr0_c"                "pre_intern_mood_c"     
# [5] "pre_intern_sleep_c"     "pre_intern_sqrt_step_c"












