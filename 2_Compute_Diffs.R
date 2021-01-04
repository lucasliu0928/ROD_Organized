###Input and output directory
in_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ROD_Project/Intermediate_Data/0103_21/"

###################################################
### Load staionary feature data
###################################################
final_df <- read.csv(paste0(in_dir,"data_stationary.csv"), stringsAsFactors = F)
rownames(final_df) <- final_df$X
final_df <- final_df[,-1]


###################################################
### Load stationary feature table by year
###################################################
byyear_feature_table <- read.csv(paste0(in_dir,"byyearfeature_table.csv"), stringsAsFactors = F)
byyear_feature_table <- byyear_feature_table[,-1]
byyear_feature_table_noNAs <- byyear_feature_table[complete.cases(byyear_feature_table),]

###################################################
### Compute normlized diff for all features and outcomes
### _Diff0_1 : (f_yr1 - f_bl) / f_bl
### _Diff1_2 : (f_yr2 - f_yr1) / f_bl
### _Diff0_2 : (f_yr2 - f_bl) / f_bl
###################################################
diff_df <- as.data.frame(matrix(NA,nrow = nrow(final_df), ncol = nrow(byyear_feature_table_noNAs)*3))
colnames(diff_df) <- c(paste0(gsub("_bl","",byyear_feature_table_noNAs$BL),"_Diff0_1"),paste0(gsub("_bl","",byyear_feature_table_noNAs$BL),"_Diff1_2"),paste0(gsub("_bl","",byyear_feature_table_noNAs$BL),"_Diff0_2"))
rownames(diff_df) <- rownames(final_df)
for (j in 1:nrow(byyear_feature_table_noNAs)){
  bl_f <-  byyear_feature_table_noNAs$BL[j]
  yr1_f <- byyear_feature_table_noNAs$Yr1[j]
  yr2_f <- byyear_feature_table_noNAs$Yr2[j]
  
  #time_vec <- c(0,1,2) #0,1 2 years
  trend_f_name <- paste0(gsub("_bl","",bl_f),"_Diff0_1")
  trend_f_name2 <- paste0(gsub("_bl","",bl_f),"_Diff1_2")
  trend_f_name3 <- paste0(gsub("_bl","",bl_f),"_Diff0_2")
  for(p in 1:nrow(final_df)){ #for each pt
    curr_df <- final_df[p,c(bl_f,yr1_f, yr2_f)]
    # bl_toYr1 <- (curr_df[,2] - curr_df[,1])/ (time_vec[2] - time_vec[1])
    # yr1_toYr2 <- (curr_df[,3] - curr_df[,2])/ (time_vec[3] - time_vec[2])
    # bl_toYr2 <- (curr_df[,3] - curr_df[,1])/ (time_vec[3] - time_vec[1])
    # 
    # bl_toYr1 <- (curr_df[,2] - curr_df[,1]) #just the diff no time considered
    # yr1_toYr2 <- (curr_df[,3] - curr_df[,2])
    # bl_toYr2 <- (curr_df[,3] - curr_df[,1]) 
    
    bl_toYr1 <- (curr_df[,2] - curr_df[,1])/curr_df[,1] #normlized by bl value
    yr1_toYr2 <- (curr_df[,3] - curr_df[,2])/curr_df[,1]
    bl_toYr2 <- (curr_df[,3] - curr_df[,1]) /curr_df[,1]
    
    diff_df[p,trend_f_name] <- bl_toYr1
    diff_df[p,trend_f_name2] <- yr1_toYr2
    diff_df[p,trend_f_name3] <- bl_toYr2
  }
  
}

#'@Note there are some patients has baseline value = 0, so it retuns Inf diff, 
#'thus, we code it as NA
for (p in 1:nrow(diff_df)){
  for (j in 1:ncol(diff_df)){
    curr_value <- diff_df[p,j]
    if (is.infinite(curr_value) == T){
      diff_df[p,j] <- NA
    }
  }
}

write.csv(diff_df, paste0(out_dir,"data_Diff.csv"))

###################################################
### Convert to pos diff features, neg diff features
### Ex: feature_POS: if <=0 : coded as 0   else  Actual pos values
### Ex: feature_Neg: if >= 0:  coded as 0   else  Actual neg values
###################################################
Pos_neg_diff_df <- as.data.frame(matrix(NA,nrow = nrow(diff_df), ncol = ncol(diff_df)*2))
pos_names <- paste0(colnames(diff_df), "_pos")
neg_names <- paste0(colnames(diff_df), "_neg")
colnames(Pos_neg_diff_df) <- c(pos_names,neg_names)
rownames(Pos_neg_diff_df) <- rownames(diff_df)

for (j in 1:ncol(Pos_neg_diff_df)){
  curr_f <- colnames(Pos_neg_diff_df)[j]
  if (grepl("pos",curr_f) == T){
    curr_acutal_name <- gsub("_pos","",curr_f)
    index_in_diff_df <- which(colnames(diff_df) == curr_acutal_name)
    neg_idxes <- which(diff_df[,index_in_diff_df] <= 0)
    pos_idxes <- which(diff_df[,index_in_diff_df] > 0)
    Pos_neg_diff_df[neg_idxes,j] <- 0
    Pos_neg_diff_df[pos_idxes,j] <- diff_df[pos_idxes,index_in_diff_df]
    
  }else if (grepl("neg",curr_f) == T){
    curr_acutal_name <- gsub("_neg","",curr_f)
    index_in_diff_df <- which(colnames(diff_df) == curr_acutal_name)
    neg_idxes <- which(diff_df[,index_in_diff_df] < 0)
    pos_idxes <- which(diff_df[,index_in_diff_df] >= 0)
    Pos_neg_diff_df[neg_idxes,j] <- diff_df[neg_idxes,index_in_diff_df]
    Pos_neg_diff_df[pos_idxes,j] <- 0
  }
  
}

write.csv(Pos_neg_diff_df, paste0(out_dir,"data_Diff_POSandNeg.csv"))

