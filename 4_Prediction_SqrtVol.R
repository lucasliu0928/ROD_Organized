library(openxlsx)
source("Ultility.R")
source("Ultils_LoadData_Funcs.R")
library(reshape2)
library(ggplot2)
library(xgboost)


###Input and output directory
in_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/out1/"
out_dir2 <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/out2/"
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
comb_df <- cbind(final_df, reorder_tomatch_df)

#3remove outcome related features SqRtVOLUMEA,CASCOREA(BL,yr1,yr2,Diff0_2,Diff1_2,Diff0_1)
#remove orignal label columns
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
updated_delta_df <- final_delta_df[, -which(colnames(final_delta_df) %in% c(original_trendlabel_names,pos_neg_trendlabel_names))]
updated_static_df <- final_df[, -which(colnames(final_df) %in% c(original_stalabel_names))]


#get feaures
Final_bl_features <- colnames(updated_comb_df)[which(grepl("bl",colnames(updated_comb_df))==T)]
Final_yr1_features <- colnames(updated_comb_df)[which(grepl("yr1",colnames(updated_comb_df))==T)]
Final_yr2_features <- colnames(updated_comb_df)[which(grepl("yr2",colnames(updated_comb_df))==T)]
Diff0_1_features <- colnames(updated_comb_df)[which(grepl("Diff0_1",colnames(updated_comb_df))==T)]
Diff1_2_features <- colnames(updated_comb_df)[which(grepl("Diff1_2",colnames(updated_comb_df))==T)]
Diff0_2_features <- colnames(updated_comb_df)[which(grepl("Diff0_2",colnames(updated_comb_df))==T)]



##########################################################################
####I. Load all catogrized labels
###A. Get catogrized label of SqRtVOLUME at bl,yr1 and yr2 
#  A1. start Non-high risk : SqRtVOLUME < 19.1 (0)
#  A2. start High Risk:      SqRtVOLUME >= 19.1 (1)
##########################################################################
Label_df <- read.csv(paste0(in_dir,"Outcome_SqRtVOLUME.csv"), stringsAsFactors = F)
rownames(Label_df) <- Label_df$X
Label_df <- Label_df[,-1]


##########################################################################
###Prediction 1: start non high vs high
###
##########################################################################
analysiis_label_df <- Label_df
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_nonH")] <- 0 
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_H")] <- 1
table(analysiis_label_df$Starting_SqRtVOLUME_LABEL) #49 58

data_input <- updated_comb_df
label_col_name <- "Starting_SqRtVOLUME_LABEL"
analysis_IDs <- rownames(analysiis_label_df) #107

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#reoder to match and add label to data input
data_input$Starting_SqRtVOLUME_LABEL <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),label_col_name]


##generating list of features for each  experiment
label_col_seq <- c(rep("Starting_SqRtVOLUME_LABEL",3))
initial_features_names_list <- c("bl,yr1,yr2","Diff0_1_and_Diff1_2","ALL")

initial_features_list <- list()
for (i in 1:length(initial_features_names_list)){
  curr_feature_list_names <- initial_features_names_list[i]
  
  if (curr_feature_list_names == "bl"){
    initial_features_list[[i]] <- Final_bl_features
  }else if (curr_feature_list_names == "bl,yr1"){
    initial_features_list[[i]] <- c(Final_bl_features,Final_yr1_features)
  }else if (curr_feature_list_names == "bl,yr1,yr2"){
    initial_features_list[[i]] <- c(Final_bl_features,Final_yr1_features,Final_yr2_features)
  }else if (curr_feature_list_names == "Diff0_1"){
    initial_features_list[[i]]  <- Diff0_1_features
  }else if (curr_feature_list_names == "Diff1_2"){
    initial_features_list[[i]]  <- Diff1_2_features
  }else if (curr_feature_list_names == "Diff0_2"){
    initial_features_list[[i]]  <- Diff0_2_features
  }else if (curr_feature_list_names == "Diff0_1_and_Diff1_2"){
    initial_features_list[[i]]  <- c(Diff0_1_features,Diff1_2_features)
  }else if (curr_feature_list_names == "ALL"){
    initial_features_list[[i]]  <- c(Final_bl_features,Final_yr1_features,Final_yr2_features,
                                     Diff0_1_features,Diff1_2_features)
  }
  
}


res <- experiment_func(label_col_seq,initial_features_list,data_input,out_dir)
Final_perf <- res[[1]]
initail_fs <- res[[2]]
Final_fs <- res[[3]]
label_table <- res[[4]]
all_labeltb <- do.call(rbind,label_table)
top_features_names_list1 <- res[[5]]
for(i in 1:length(top_features_names_list1)){
  curr_important_fs <- top_features_names_list1[[i]]
  write.csv(curr_important_fs,paste0(out_dir,"/Exp",i,"_important_fs",".csv"))
  
}

Final_perf$Features <- initial_features_names_list
Final_perf$Outcome <- label_col_seq
Final_perf$initail_fs <- initail_fs
Final_perf$Final_fs <- Final_fs
Final_perf$all_labeltb <- all_labeltb
write.csv(Final_perf,paste0(out_dir,"/Perf_tb.csv"))


##########################################################################
###Prediction 2:  Start High, stays high vs. Start non-high, stay non-high
##########################################################################
analysiis_label_df <- Label_df
analysiis_label_df$New_Trend_Label <- NA
analysiis_label_df$New_Trend_Label[which(analysiis_label_df$Changing_Label == "nonH_nonH")] <- 0 
analysiis_label_df$New_Trend_Label[which(analysiis_label_df$Changing_Label == "H_H")] <- 1
table(analysiis_label_df$New_Trend_Label) #34 58
analysiis_label_df <- analysiis_label_df[-which(is.na(analysiis_label_df$New_Trend_Label)==T),]

data_input <- updated_comb_df
label_col_name <- "New_Trend_Label"
analysis_IDs <- rownames(analysiis_label_df) #92
length(analysis_IDs)

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#reoder to match and add label to data input
data_input$New_Trend_Label <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),label_col_name]


##generating list of features for each  experiment
label_col_seq <- c(rep("New_Trend_Label",3))
initial_features_names_list <- c("bl,yr1,yr2","Diff0_1_and_Diff1_2","ALL")

initial_features_list <- list()
for (i in 1:length(initial_features_names_list)){
  curr_feature_list_names <- initial_features_names_list[i]
  
  if (curr_feature_list_names == "bl"){
    initial_features_list[[i]] <- Final_bl_features
  }else if (curr_feature_list_names == "bl,yr1"){
    initial_features_list[[i]] <- c(Final_bl_features,Final_yr1_features)
  }else if (curr_feature_list_names == "bl,yr1,yr2"){
    initial_features_list[[i]] <- c(Final_bl_features,Final_yr1_features,Final_yr2_features)
  }else if (curr_feature_list_names == "Diff0_1"){
    initial_features_list[[i]]  <- Diff0_1_features
  }else if (curr_feature_list_names == "Diff1_2"){
    initial_features_list[[i]]  <- Diff1_2_features
  }else if (curr_feature_list_names == "Diff0_2"){
    initial_features_list[[i]]  <- Diff0_2_features
  }else if (curr_feature_list_names == "Diff0_1_and_Diff1_2"){
    initial_features_list[[i]]  <- c(Diff0_1_features,Diff1_2_features)
  }else if (curr_feature_list_names == "ALL"){
    initial_features_list[[i]]  <- c(Final_bl_features,Final_yr1_features,Final_yr2_features,
                                     Diff0_1_features,Diff1_2_features)
  }
  
}


res <- experiment_func(label_col_seq,initial_features_list,data_input,out_dir2)
Final_perf <- res[[1]]
initail_fs <- res[[2]]
Final_fs <- res[[3]]
label_table <- res[[4]]
all_labeltb <- do.call(rbind,label_table)
top_features_names_list2 <- res[[5]]
for(i in 1:length(top_features_names_list2)){
  curr_important_fs <- top_features_names_list2[[i]]
  write.csv(curr_important_fs,paste0(out_dir2,"/Exp",i,"_important_fs",".csv"))
  
}

Final_perf$Features <- initial_features_names_list
Final_perf$Outcome <- label_col_seq
Final_perf$initail_fs <- initail_fs
Final_perf$Final_fs <- Final_fs
Final_perf$all_labeltb <- all_labeltb
write.csv(Final_perf,paste0(out_dir2,"/Perf_tb.csv"))


