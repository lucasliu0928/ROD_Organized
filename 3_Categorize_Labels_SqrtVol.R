source("Ultility.R")
library(ggplot2)

###Input and output directory
in_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"

###################################################
### Load feature data
###################################################
#1. staionary feature data
final_df <- read.csv(paste0(in_dir,"data_stationary.csv"), stringsAsFactors = F)
rownames(final_df) <- final_df$X
final_df <- final_df[,-1]

#2. Load Diff feature data
final_delta_df <- read.csv(paste0(in_dir,"data_Diff.csv"), stringsAsFactors = F)
rownames(final_delta_df) <- final_delta_df$X
final_delta_df <- final_delta_df[,-1]

reorder_tomatch_df <- final_delta_df[match(rownames(final_delta_df), rownames(final_df)),]
comb_df <- cbind(final_df, reorder_tomatch_df)

###################################################
##1. catogorical sqrtvolA outcome for baseline, 1yr , 2yr
#Non-high risk : sqrtvol < 19.1  (at baseline or 1st or 2nd yr.)
#High Risk:      sqrtvol >= 19.1 (at baseline or 1st or 2nd yr.)
###################################################
CatOutcome_df1 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUMEA_bl","bl") #baseline
CatOutcome_df2 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUME1A_yr1","yr1") #baseline
CatOutcome_df3 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUME2A_yr2","yr2") #baseline

#output label file: 0: sqrtvol < 400 ; 1:sqrtvol >= 400
Stationary_outcome_df <- cbind.data.frame(CatOutcome_df1,CatOutcome_df2,CatOutcome_df3)
table(Stationary_outcome_df$SqRtVOLUME_Cat_bl)
table(Stationary_outcome_df$SqRtVOLUME_Cat_yr1)
table(Stationary_outcome_df$SqRtVOLUME_Cat_yr2)

Stationary_outcome_df <- Stationary_outcome_df[-which(is.na(Stationary_outcome_df$Starting_SqRtVOLUME_LABEL)==T),]

###################################################
### Updated 11/24, seperate to 2 groups:
### 1. Start high (SqRtVOLUME_Cat_bl == high)
### 2. Start non-high (SqRtVOLUME_Cat_bl == non-high)
###################################################
start_high_grp_idexes <- which(Stationary_outcome_df$SqRtVOLUME_Cat_bl==1)
start_non_high_grp_idexes <- which(Stationary_outcome_df$SqRtVOLUME_Cat_bl==0)
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL <- NA
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL[start_high_grp_idexes] <- 1
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL[start_non_high_grp_idexes] <- 0

start_high_grp_Ids <- rownames(Stationary_outcome_df)[start_high_grp_idexes]
start_nonhigh_grp_Ids <- rownames(Stationary_outcome_df)[start_non_high_grp_idexes]

###################################################
### Updated 01/03/21, 
### For Start non-high group (SqRtVOLUME_Cat_bl == non-high): 
###    A. start non-high, stay non-high
###    B. start non-high,  get higher 
###################################################
#1. Get high and non-high group idxes
h_grp_idxes <- which(rownames(comb_df) %in% start_high_grp_Ids)
nonh_grp_idxes <- which(rownames(comb_df) %in% start_nonhigh_grp_Ids)

h_grp_diff_df <- comb_df[h_grp_idxes,] #99
nonh_grp_diff_df <- comb_df[nonh_grp_idxes,] #72


##2A, start high, stay high group:
#for pts has yr2 value:
cond1 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==1 & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]==1
#for ptshas yr1 value, but not yr2 value:
cond2 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==1 & is.na(Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]) == T & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr1"]==1
all_H_H_idxes <- which(cond1 | cond2)
length(all_H_H_idxes)
Stationary_outcome_df$Start


##2B.start non-high, stay non-high
analysis_grp_df <- nonh_grp_diff_df
#for pts has yr2 value:
nonH_nonH_indexes1 <- which(analysis_grp_df[,"SqRtVOLUMEA_bl"] < 19.1 
                           & analysis_grp_df[,"SqRtVOLUME2A_yr2"] < 19.1)
#for ptshas yr1 value, but not yr2 value:
nonH_nonH_indexes2 <- which(analysis_grp_df[,"SqRtVOLUMEA_bl"] < 19.1 
                            & is.na(analysis_grp_df[,"SqRtVOLUME2A_yr2"])==T
                            & analysis_grp_df[,"SqRtVOLUME1A_yr1"] < 19.1)
all_nonH_nonH_idxes <- c(nonH_nonH_indexes1,nonH_nonH_indexes2)

##3.start non-high,get higher
#for pts has yr2 value:
nonH_H_indexes1 <- which(analysis_grp_df[,"SqRtVOLUMEA_bl"] < 19.1 
                            & analysis_grp_df[,"SqRtVOLUME2A_yr2"] >= 19.1)
#for ptshas yr1 value, but not yr2 value:
nonH_H_indexes2 <- which(analysis_grp_df[,"SqRtVOLUMEA_bl"] < 19.1 
                            & is.na(analysis_grp_df[,"SqRtVOLUME2A_yr2"])==T
                            & analysis_grp_df[,"SqRtVOLUME1A_yr1"] >= 19.1)
all_nonH_H_idxes <- c(nonH_H_indexes1,nonH_H_indexes2)

#22 pts, has no yr1 nor yr2 value
check <-  analysis_grp_df[-c(all_nonH_nonH_idxes,all_nonH_H_idxes),c("SqRtVOLUMEA_bl","SqRtVOLUME1A_yr1","SqRtVOLUME2A_yr2")]



#add manully checked labels for pts who has negative changes with no Increase_atanytime 
#'@note: 5 of them are still being checked 1207
decrease_idxes <- which(trend_outcome_df2$Trend_Label %in% c(4,6,9))
for (i in 1:length(decrease_idxes)){
  curr_idx <- decrease_idxes[i]
  curr_id <- rownames(trend_outcome_df2)[curr_idx]
  if (curr_id %in% manul_startnonH_increase_IDs){ 
    trend_outcome_df2$New_Trend_Label[curr_idx] <- 1
  }else if (curr_id %in% manul_startnonH_littlechange_IDs){
    trend_outcome_df2$New_Trend_Label[curr_idx] <- 0
  }
}
trend_outcome_df2$New_Trend_Label[decrease_idxes]

#remove NA label pts
trend_outcome_df2 <- trend_outcome_df2[-which(is.na(trend_outcome_df2$New_Trend_Label) == T),]
write.csv(trend_outcome_df2,paste0(out_dir,"Outcome_trend_start_nonH_grp.csv"))
table(trend_outcome_df2$New_Trend_Label)


######################################################################################################
### Update Stationary_outcome_df to only cover the patients in trend_outcome1 and trend_outcome2
######################################################################################################
kept_idxes <- which(rownames(Stationary_outcome_df) %in% c(rownames(trend_outcome_df1),rownames(trend_outcome_df2)))
Stationary_outcome_df <- Stationary_outcome_df[kept_idxes,]
write.csv(Stationary_outcome_df,paste0(out_dir,"Outcome_Start_SqRtVOLUME.csv"))
