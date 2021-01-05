source("Ultils_LoadData_Funcs.R")
source("Ultility.R")

###Input and output directory
in_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Data/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"
data_filename <-  "CombinedROD_BMD_CAC_Bioch_Echo.csv"
reviewed_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Data/"

##############################################################################################
##                Section 1: Load ROD Data and Clean and remove unwanted Features           ##
##############################################################################################
raw_data_df <- read.csv(paste0(in_dir, data_filename), stringsAsFactors = F)

#1. Identify original features required for the study
byyear_res <-   Get_ByYear_Features(raw_data_df)
bl_features <- byyear_res[[1]]
yr1_features <- byyear_res[[2]]
yr2_features <- byyear_res[[3]]
outcomes <- byyear_res[[4]] 


##############################################################################################
# Section1B: Load Manully checked negative trend pts (added 01/03/21)
##############################################################################################
reviewd_df <- read.xlsx(paste0(reviewed_dir,"deltacacsquvol.xlsx"))

###Get Update sqrtvolumn and CASCORE values
toupdate_idxes <- which(is.na(reviewd_df[,"Rescore.A"])==F 
                        | is.na(reviewd_df[,"Rescore.1A"])==F
                        | is.na(reviewd_df[,"Rescore.2A"])==F)
updated_df <-as.data.frame(matrix(NA, nrow = length(toupdate_idxes),ncol = 7))
colnames(updated_df) <-c("ID","updated_CASCORE","updated_sqrtvol","updated_CASCORE1","updated_sqrtvol1","updated_CASCORE2","updated_sqrtvol2")
for (i in 1:length(toupdate_idxes)){
  curr_idx <- toupdate_idxes[i]
  curr_id <- reviewd_df[curr_idx,"ID"]
  
  update_value <- reviewd_df[curr_idx,"Rescore.A"]
  updated_CASCORE <- as.numeric(strsplit(update_value,"\\(")[[1]][1])
  updated_sqrtvol <- as.numeric(gsub("\\(|\\)","",strsplit(update_value,"\\(")[[1]][2]))
  
  update_value <- reviewd_df[curr_idx,"Rescore.1A"]
  updated_CASCORE1 <- as.numeric(strsplit(update_value,"\\(")[[1]][1])
  updated_sqrtvol1 <- as.numeric(gsub("\\(|\\)","",strsplit(update_value,"\\(")[[1]][2]))
  
  
  update_value <- reviewd_df[curr_idx,"Rescore.2A"]
  updated_CASCORE2 <- as.numeric(strsplit(update_value,"\\(")[[1]][1])
  updated_sqrtvol2 <- as.numeric(gsub("\\(|\\)","",strsplit(update_value,"\\(")[[1]][2]))
  
  
  
  updated_df[i,"ID"] <- curr_id
  updated_df[i,"updated_CASCORE"] <- updated_CASCORE
  updated_df[i,"updated_sqrtvol"] <- updated_sqrtvol
  updated_df[i,"updated_CASCORE1"] <- updated_CASCORE1
  updated_df[i,"updated_sqrtvol1"] <- updated_sqrtvol1
  updated_df[i,"updated_CASCORE2"] <- updated_CASCORE2
  updated_df[i,"updated_sqrtvol2"] <- updated_sqrtvol2
  
  
}

###################################################
## update values in raw_data_df
###################################################
for (i in 1:nrow(updated_df)){
  curr_id <- updated_df[i,"ID"]
  curr_idx <- which(gsub(" |\\.","",raw_data_df$ID) == gsub(" |\\.","",curr_id))
  print(curr_idx)
  
  if ( length(curr_idx) > 0){ 
    ##CASCORE
    updated_CASCORE <- updated_df[i,"updated_CASCORE"]
    if (is.na(updated_CASCORE) == F) {
      raw_data_df[curr_idx,"CASCOREA"] <- updated_CASCORE
    }
    
    updated_CASCORE <- updated_df[i,"updated_CASCORE1"]
    if (is.na(updated_CASCORE) == F) {
      raw_data_df[curr_idx,"CASCORE1A"] <- updated_CASCORE
    }
    
    updated_CASCORE <- updated_df[i,"updated_CASCORE2"]
    if (is.na(updated_CASCORE) == F) {
      raw_data_df[curr_idx,"CASCORE2A"] <- updated_CASCORE
    }
    
    
    ##SqRtVOLUMEA_bl SqRtVOLUME1A_yr1 SqRtVOLUME2A_yr2
    updated_sqrtvol <- updated_df[i,"updated_sqrtvol"]
    if (is.na(updated_sqrtvol) == F) {
      raw_data_df[curr_idx,"SqRtVOLUMEA"] <- updated_sqrtvol
    }
    
    updated_sqrtvol <- updated_df[i,"updated_sqrtvol1"]
    if (is.na(updated_sqrtvol) == F) {
      raw_data_df[curr_idx,"SqRtVOLUME1A"] <- updated_sqrtvol
    }
    
    updated_sqrtvol <- updated_df[i,"updated_sqrtvol2"]
    if (is.na(updated_sqrtvol) == F) {
      raw_data_df[curr_idx,"SqRtVOLUME2A"] <- updated_sqrtvol
    }
    
  }
}

##############################################################################################
##                Section 2: Excluding outlier patients on some features                    ##
##############################################################################################
#2.exclude outliers from CASCOREA,RFN3Dcortical,PO4BIND1A,SqRtVOLUMEA 
#the outlier of sqrtvolumns was not removed for cascore predition model, check this funciton below
updated_raw_df_outlier_removed <- remove_outlier_pts_func(raw_data_df)

#3.Exclude  pts CASCORE == 0 
updated_raw_df_outlier_removed[which(updated_raw_df_outlier_removed[,"CASCOREA"] == 0),"CASCOREA"] <- NA
updated_raw_df_outlier_removed[which(updated_raw_df_outlier_removed[,"CASCORE1A"] == 0),"CASCORE1A"] <- NA
updated_raw_df_outlier_removed[which(updated_raw_df_outlier_removed[,"CASCORE2A"] == 0),"CASCORE2A"] <- NA


#4.Exclude  pts CASCORE <= 10 all time
#CASCORE <= 10 or NA at bl, and (cascore <= 10 or NA at yr1) and (cascore <= 10 or NA at yr2)
cond1 <- updated_raw_df_outlier_removed[,"CASCOREA"] <= 10 | is.na(updated_raw_df_outlier_removed[,"CASCOREA"]) ==T
cond2 <- updated_raw_df_outlier_removed[,"CASCORE1A"] <= 10 | is.na(updated_raw_df_outlier_removed[,"CASCORE1A"]) ==T
cond3 <- updated_raw_df_outlier_removed[,"CASCORE2A"] <= 10 | is.na(updated_raw_df_outlier_removed[,"CASCORE2A"]) ==T
pts_cascoreLT10_idxes <- which(cond1 & cond2 & cond3)
length(pts_cascoreLT10_idxes) #294
updated_raw_df_outlier_removed <- updated_raw_df_outlier_removed[-pts_cascoreLT10_idxes,]

##############################################################################################
##                Section 3: Updated/Clean and remove unwanted Features                     ##
##############################################################################################
#3.exclude unwanted features from email or meeting
updated_byyear_fs <- exclude_features_Func(bl_features,yr1_features,yr2_features)
updated_bl_features <- updated_byyear_fs[[1]]
updated_yr1_features <- updated_byyear_fs[[2]]
updated_yr2_features <-  updated_byyear_fs[[3]]


#4. update/recode feature values 
raw_data_df_withUpdated_values <- update_features_inData(updated_raw_df_outlier_removed)

#5. add manually computed features : catogrized turnover and (average of TLH3D and TRH3D : TH3D)
raw_data_df_withExtra_features <- add_features_inData(raw_data_df_withUpdated_values)


#6A. update bl_feature,yr1_features,yr2_features
updated_bl_features <- c(updated_bl_features,"turnover","TH3D")
updated_yr1_features <- c(updated_yr1_features,"turnover1","TH3D1")
updated_yr2_features <- c(updated_yr2_features,"turnover2","TH3D2")
#6B remove TLH3D and TRH3D since TH3D is computed from these two
Final_bl_features <- updated_bl_features[-which(updated_bl_features %in% c("TLH3D","TRH3D"))]
Final_yr1_features <-updated_yr1_features[-which(updated_yr1_features %in% c("TLH3D1","TRH3D1"))]
Final_yr2_features <- updated_yr2_features[-which(updated_yr2_features %in% c("TLH3D2","TRH3D2"))]


#7. Match original features and outcome by years and group them into one table
byyear_feature_outcome_table <- match_byyear_featureAndoutcome_func(Final_bl_features,Final_yr1_features,Final_yr2_features, outcomes)


##############################################################################################
##                Section 4:  Final analysis data set  
#                            A.Subseting raw data with updated analysis features            ##
##                           B. Rename features
##############################################################################################
#A. analysis data only include byyear feautres and outcomes
analysis_df <- raw_data_df_withExtra_features[,c(Final_bl_features,Final_yr1_features,Final_yr2_features,outcomes)]
rownames(analysis_df) <- raw_data_df_withExtra_features[,"ID"]

#B. Re-name features and outcome by years
colnames(analysis_df) <- change_feature_name_func(colnames(analysis_df),byyear_feature_outcome_table[,"BL"],byyear_feature_outcome_table[,"Yr1"],byyear_feature_outcome_table[,"Yr2"])
byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"BL"])==F),"BL"] <- paste0(byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"BL"])==F),"BL"],"_bl")
byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"Yr1"])==F),"Yr1"] <- paste0(byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"Yr1"])==F),"Yr1"],"_yr1")
byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"Yr2"])==F),"Yr2"] <- paste0(byyear_feature_outcome_table[which(is.na(byyear_feature_outcome_table[,"Yr2"])==F),"Yr2"],"_yr2")


write.csv(analysis_df, paste0(out_dir,"data_stationary.csv"))
write.csv(byyear_feature_outcome_table, paste0(out_dir,"byyearfeature_table.csv"))


