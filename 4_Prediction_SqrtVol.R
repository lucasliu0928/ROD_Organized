library(openxlsx)
source("Ultility.R")
source("Ultils_LoadData_Funcs.R")
library(reshape2)
library(ggplot2)
library(xgboost)

###Input and output directory
in_dir <- "../Intermediate_Data/0117_21/"
out_dir <- "../Intermediate_Data/0117_21/out1/"
out_dir2 <- "../Intermediate_Data/0117_21/out2/"
out_dir3 <-  "../Intermediate_Data/0117_21/out3/"

dir.create(file.path(out_dir))
dir.create(file.path(out_dir2))
dir.create(file.path(out_dir3))

###################################################
### Load feature data
###################################################
#1. staionary feature data
final_df <- read.csv(paste0(in_dir,"data_stationary.csv"), stringsAsFactors = F)
rownames(final_df) <- final_df$X
final_df <- final_df[,-1]

#2. Load Diff feature data
final_delta_df <- read.csv(paste0(in_dir,"data_Diff_POSandNeg.csv"), stringsAsFactors = F)
rownames(final_delta_df) <- final_delta_df$X
final_delta_df <- final_delta_df[,-1]
reorder_tomatch_df <- final_delta_df[match(rownames(final_delta_df), rownames(final_df)),]

#3. Combine stationary feature and feature difference.
comb_df <- cbind(final_df, reorder_tomatch_df)

#4. remove outcome related features SqRtVOLUMEA,CASCOREA(BL,yr1,yr2,Diff0_2,Diff1_2,Diff0_1) #remove orignal label columns
original_stalabel_names <- c("CASCOREA_bl" , "CASCORE1A_yr1", "CASCORE2A_yr2", 
                             "SqRtVOLUMEA_bl", "SqRtVOLUME1A_yr1", "SqRtVOLUME2A_yr2")
original_trendlabel_names <- c("CASCOREA_Diff0_1" , "CASCOREA_Diff1_2", "CASCOREA_Diff0_2", 
                               "SqRtVOLUMEA_Diff0_1", "SqRtVOLUMEA_Diff1_2", "SqRtVOLUMEA_Diff0_2")
pos_neg_trendlabel_names <- c("CASCOREA_Diff0_1_pos" , "CASCOREA_Diff0_1_neg",
                              "CASCOREA_Diff1_2_pos","CASCOREA_Diff1_2_neg",
                              "CASCOREA_Diff0_2_pos",  "CASCOREA_Diff0_2_neg", 
                              "SqRtVOLUMEA_Diff0_1_pos", "SqRtVOLUMEA_Diff0_1_neg",
                              "SqRtVOLUMEA_Diff1_2_pos","SqRtVOLUMEA_Diff1_2_neg",
                              "SqRtVOLUMEA_Diff0_2_pos", "SqRtVOLUMEA_Diff0_2_neg")


updated_comb_df <- comb_df[, -which(colnames(comb_df) %in% c(original_stalabel_names,original_trendlabel_names,pos_neg_trendlabel_names))]


#5. get feature names
Final_bl_features <- colnames(updated_comb_df)[which(grepl("bl",colnames(updated_comb_df))==T)] #68 #4 feature has no yr1 and yr2 values
Final_yr1_features <- colnames(updated_comb_df)[which(grepl("yr1",colnames(updated_comb_df))==T)] #64
Final_yr2_features <- colnames(updated_comb_df)[which(grepl("yr2",colnames(updated_comb_df))==T)] #64
Diff0_1_features <- colnames(updated_comb_df)[which(grepl("Diff0_1",colnames(updated_comb_df))==T)] #128
Diff1_2_features <- colnames(updated_comb_df)[which(grepl("Diff1_2",colnames(updated_comb_df))==T)] #128
Diff0_2_features <- colnames(updated_comb_df)[which(grepl("Diff0_2",colnames(updated_comb_df))==T)] #128



##########################################################################
####I. Load all catogrized labels
###A. Get catogrized label of starting value of SqRtVOLUME
#  A1. start Non-high risk : SqRtVOLUME < 19.1 (0)
#  A2. start High Risk:      SqRtVOLUME >= 19.1 (1)
###B. Changing label
###   1.	Start high, stay high
###   2.  start non-high, stay non-high
###   3.  start non-high, get higher 
##########################################################################
Label_df <- read.csv(paste0(in_dir,"Outcome_SqRtVOLUME.csv"), stringsAsFactors = F)
rownames(Label_df) <- Label_df$X
Label_df <- Label_df[,-1]


##########################################################################
###Prediction 1: start non high vs high
##########################################################################
#Add corresponding label to the data 
label_col_name <- "Starting_SqRtVOLUME_LABEL"
grp0_name <- "start_nonH"
grp1_name <- "start_H"
data_input <- experiment_setup_func(updated_comb_df, Label_df, label_col_name, grp0_name,grp1_name)

##generating list of features and labels for each experiments
label_col_seq <- c(rep(label_col_name,3))
experiment_features_names_list <- c("bl,yr1,yr2","Diff0_1_and_Diff1_2","ALL")
initial_features_list <- get_feature_list_func(experiment_features_names_list,Final_bl_features,Final_yr1_features,Final_yr2_features, Diff0_1_features,Diff1_2_features)

##Run exoeriments predict label in label_col_seq, using features in experiment_features_names_list
res <- experiment_func(label_col_seq,initial_features_list,data_input,out_dir)
Final_perf <- res[[1]]
initail_fs <- res[[2]]
Final_fs <- res[[3]]
label_table <- res[[4]]
all_labeltb <- do.call(rbind,label_table)
top_features_names_list1 <- res[[5]]
for(i in 1:length(top_features_names_list1)){
  curr_important_fs <- top_features_names_list1[[i]]
  write.csv(curr_important_fs,paste0(out_dir,"/FeatureSet",i,"_important_fs",".csv"))
  
}

Final_perf$Features <- experiment_features_names_list
Final_perf$Outcome <- label_col_seq
Final_perf$initail_fs <- initail_fs
Final_perf$Final_fs <- Final_fs
Final_perf$all_labeltb <- all_labeltb
write.csv(Final_perf,paste0(out_dir,"/Perf_tb.csv"))


##########################################################################
###Prediction 2:  Start High, stays high vs. Start non-high, stay non-high
##########################################################################
#first remvoe nonH_H grp
pts_toremove <- which(Label_df[,"Changing_Label"] == "nonH_H") 
updated_label_df <- Label_df[-pts_toremove,]

#Add corresponding label to the data 
label_col_name <- "Changing_Label"
grp0_name <- "nonH_nonH"
grp1_name <- "H_H"
data_input <- experiment_setup_func(updated_comb_df, updated_label_df, label_col_name, grp0_name,grp1_name)

##generating list of features and labels for each experiments
label_col_seq <- c(rep(label_col_name,3))
experiment_features_names_list <- c("bl,yr1,yr2","Diff0_1_and_Diff1_2","ALL")
initial_features_list <- get_feature_list_func(experiment_features_names_list,Final_bl_features,Final_yr1_features,Final_yr2_features, Diff0_1_features,Diff1_2_features)

##Run exoeriments predict label in label_col_seq, using features in experiment_features_names_list
res <- experiment_func(label_col_seq,initial_features_list,data_input,out_dir2)
Final_perf <- res[[1]]
initail_fs <- res[[2]]
Final_fs <- res[[3]]
label_table <- res[[4]]
all_labeltb <- do.call(rbind,label_table)
top_features_names_list2 <- res[[5]]
for(i in 1:length(top_features_names_list2)){
  curr_important_fs <- top_features_names_list2[[i]]
  write.csv(curr_important_fs,paste0(out_dir2,"/FeatureSet",i,"_important_fs",".csv"))
  
}

Final_perf$Features <- experiment_features_names_list
Final_perf$Outcome <- label_col_seq
Final_perf$initail_fs <- initail_fs
Final_perf$Final_fs <- Final_fs
Final_perf$all_labeltb <- all_labeltb
write.csv(Final_perf,paste0(out_dir2,"/Perf_tb.csv"))

##########################################################################
###Prediction 3:  Start non-high, get higher vs. Start non-high, stays non-high 
##########################################################################
#first remvoe H_H grp
pts_toremove <- which(Label_df[,"Changing_Label"] == "H_H") 
updated_label_df <- Label_df[-pts_toremove,]

#Add corresponding label to the data 
label_col_name <- "Changing_Label"
grp0_name <- "nonH_nonH"
grp1_name <- "nonH_H"
data_input <- experiment_setup_func(updated_comb_df, updated_label_df, label_col_name, grp0_name,grp1_name)

##generating list of features and labels for each experiments
label_col_seq <- c(rep(label_col_name,3))
experiment_features_names_list <- c("bl,yr1,yr2","Diff0_1_and_Diff1_2","ALL")
initial_features_list <- get_feature_list_func(experiment_features_names_list,Final_bl_features,Final_yr1_features,Final_yr2_features, Diff0_1_features,Diff1_2_features)

res <- experiment_func(label_col_seq,initial_features_list,data_input,out_dir3)
Final_perf <- res[[1]]
initail_fs <- res[[2]]
Final_fs <- res[[3]]
label_table <- res[[4]]
all_labeltb <- do.call(rbind,label_table)
top_features_names_list2 <- res[[5]]
for(i in 1:length(top_features_names_list2)){
  curr_important_fs <- top_features_names_list2[[i]]
  write.csv(curr_important_fs,paste0(out_dir3,"/FeatureSet",i,"_important_fs",".csv"))
  
}

Final_perf$Features <- experiment_features_names_list
Final_perf$Outcome <- label_col_seq
Final_perf$initail_fs <- initail_fs
Final_perf$Final_fs <- Final_fs
Final_perf$all_labeltb <- all_labeltb
write.csv(Final_perf,paste0(out_dir3,"/Perf_tb.csv"))
