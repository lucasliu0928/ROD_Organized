source("Ultility.R")
library(ggplot2)

###Input and output directory
in_dir <- "../Intermediate_Data/0117_21/"
out_dir <- "../Intermediate_Data/0117_21/"

###################################################
###I. Load feature data
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

#3. Combine stationary feature and feature difference.
comb_df <- cbind(final_df, reorder_tomatch_df)

###################################################
##II. catogorical sqrtvolA outcome for baseline, 1yr , 2yr
#Non-high risk : sqrtvol < 19.1  (at baseline or 1st or 2nd yr.)
#High Risk:      sqrtvol >= 19.1 (at baseline or 1st or 2nd yr.)
###################################################
CatOutcome_df1 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUMEA_bl","bl") #baseline
CatOutcome_df2 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUME1A_yr1","yr1") #baseline
CatOutcome_df3 <- create_sqrtVol_label_each_year(comb_df,"SqRtVOLUME2A_yr2","yr2") #baseline

#output label file: 0: sqrtvol < 19.1 ; 1:sqrtvol >= 19.1
Stationary_outcome_df <- cbind.data.frame(CatOutcome_df1,CatOutcome_df2,CatOutcome_df3)
table(Stationary_outcome_df$SqRtVOLUME_Cat_bl)
table(Stationary_outcome_df$SqRtVOLUME_Cat_yr1)
table(Stationary_outcome_df$SqRtVOLUME_Cat_yr2)


###################################################
### Updated 11/24, seperate to 2 groups:
### 1. Start high (SqRtVOLUME_Cat_bl == high)
### 2. Start non-high (SqRtVOLUME_Cat_bl == non-high)
###################################################
start_high_grp_idexes <- which(Stationary_outcome_df$SqRtVOLUME_Cat_bl==1)
start_non_high_grp_idexes <- which(Stationary_outcome_df$SqRtVOLUME_Cat_bl==0)
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL <- NA
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL[start_high_grp_idexes] <- "start_H"
Stationary_outcome_df$Starting_SqRtVOLUME_LABEL[start_non_high_grp_idexes] <- "start_nonH"
table(Stationary_outcome_df$Starting_SqRtVOLUME_LABEL) #49 58 


###################################################
### Updated 01/03/21, 
### For Start high group : 
###   A. start high, stay high

### For Start non-high group : 
###    A. start non-high, stay non-high
###    B. start non-high,  get higher 
###################################################
Stationary_outcome_df$Changing_Label <- NA
##1A, start high, stay high group:
#for pts has yr2 value:
cond1 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==1 & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]==1
#for pts has yr1 value, but not yr2 value:
cond2 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==1 & is.na(Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]) == T & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr1"]==1
all_H_H_idxes <- which(cond1 | cond2)
length(all_H_H_idxes)

##1B, start non-high, stay non-high group:
#for pts has yr2 value:
cond1 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==0 & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]==0
#for pts has yr1 value, but not yr2 value:
cond2 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==0 & is.na(Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]) == T & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr1"]==0
all_nonH_nonH_idxes <- which(cond1 | cond2)
length(all_nonH_nonH_idxes)


##1C, start non-high, get higher group:
#for pts has yr2 value:
cond1 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==0 & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]==1
#for pts has yr1 value, but not yr2 value:
cond2 <- Stationary_outcome_df[,"SqRtVOLUME_Cat_bl"]==0 & is.na(Stationary_outcome_df[,"SqRtVOLUME_Cat_yr2"]) == T & Stationary_outcome_df[,"SqRtVOLUME_Cat_yr1"]==1
all_nonH_H_idxes <- which(cond1 | cond2)
length(all_nonH_H_idxes)


Stationary_outcome_df$Changing_Label[all_H_H_idxes]<-"H_H"
Stationary_outcome_df$Changing_Label[all_nonH_nonH_idxes]<-"nonH_nonH"
Stationary_outcome_df$Changing_Label[all_nonH_H_idxes]<-"nonH_H"
table(Stationary_outcome_df$Changing_Label)

write.csv(Stationary_outcome_df,paste0(out_dir,"Outcome_SqRtVOLUME.csv"))






