###########################################################################
###                    Features related     FUNCTIONS                   ###
###########################################################################
#1.remove features in a column range
remove_FeaturesInRange_func <- function(data_df,start_feature,end_feature){
  ## remove from first BX to CAP_CIPX_RATIO
  col_to_remove_start <- which(colnames(data_df) == start_feature) 
  col_to_remove_end    <-which(colnames(data_df) == end_feature)
  
  dat_updated<-data_df[,-(col_to_remove_start:col_to_remove_end)]
  return(dat_updated)
}
#2.remove individual functions
remove_Features_func <- function(data_df, features_toremove){
  ## other features need to excluded from Dan's email
  remove_Indexes <- which(colnames(data_df) %in% features_toremove)
  dat_updated <- data_df[,-remove_Indexes]
  return(dat_updated)
}

#3.Get features in a column range
get_FeaturesInRange_func <- function(data_df,start_feature,end_feature){
  col_start<-which(colnames(data_df) == start_feature)  
  col_end <- which(colnames(data_df) == end_feature) 
  data_inrange <- data_df[,c(col_start:col_end)]
  features_inRange <- colnames(data_inrange)
  return(features_inRange)
}

###########################################################################
###                         Load Data  FUNCTIONS                        ###
###                       get features seperate by years
###                       update CASCORE
###                       remove unwatned features
###                       remove outlier patients
###                         recode gender, race
###                        categorize turnover
###########################################################################
#1. This function Exclude unwanted features in Dr.Davenport's Email and in meetings
exclude_features_Func <- function(bl_features,yr1_features,yr2_features){
  features_selected <- c(bl_features,yr1_features,yr2_features)
  #1. exlude meaningless features
  meaningless_features_toremove <- c("DateBaselineVisit", "DateYear1Visit" , "DateYear2Visit",
                                     "ID1","ID2",
                                     "MARRIED1","MARRIED2",
                                     "HDCAUSE","ConversionNOV","MosONDIALYSISBaseline") #use "MosOnDIALYSISatConsent" instead of "MosONDIALYSISBaseline" 
  
  meaningless_features_toremove2 <- features_selected[which(grepl("NOV|CTX|Crosslaps|VITDR",features_selected,ignore.case = T)==T)]
  
  #.2. exlude DXA features
  DXA_features_toremove <- features_selected[which(grepl("DXA",features_selected,ignore.case = T))]
  
  #3. exlude t-score features
  tscore_feautres_toremove <- features_selected[which(grepl("_t\\b|_t1\\b|_t2\\b",features_selected,ignore.case = T))]
  
  #4. exlude feature only has one patient record
  oneP_features_toremove <- c("PO4BINDDOSEC","PO4BINDDOSE1C","PO4BINDDOSE2C")
  
  #5. exclude features associated with the spine (start with L1, L2, L3, or L4 anything)
  spine_features_toremove <- features_selected[which(grepl("^L1|^L2|^L3|^L4",features_selected)==T)]
  
  #6. exclude associated with the femoral neck (LFN, RFN anything) 
  femoral_neck_toremove <- features_selected[which(grepl("^LFN|^RFN",features_selected)==T)]
  
  #7. remove KtV
  ktv_feature <- c("KtV","KtV1","KtV2")
  
  #8. remove DM1 and DM2
  DM_features <- c("DM1","DM2")
  
  all_features_toexclude <- c(meaningless_features_toremove,
                              meaningless_features_toremove2,
                              DXA_features_toremove,
                              tscore_feautres_toremove,
                              oneP_features_toremove,
                              spine_features_toremove,
                              femoral_neck_toremove,
                              ktv_feature,
                              DM_features)
  
  f_toexclude_indexes_bl <- which(bl_features %in% all_features_toexclude)
  f_toexclude_indexes_yr1 <- which(yr1_features %in% all_features_toexclude)
  f_toexclude_indexes_yr2 <- which(yr2_features %in% all_features_toexclude)
  
  
  updated_bl_features <- bl_features[-f_toexclude_indexes_bl]
  updated_yr1_features <- yr1_features[-f_toexclude_indexes_yr1]
  updated_yr2_features <- yr2_features[-f_toexclude_indexes_yr2]
  
  return(list(updated_bl_features,updated_yr1_features,updated_yr2_features))
}



#1. This function get seperate features by year
Get_ByYear_Features<- function(original_data_df){
  #baseline features
  bl_features <- get_FeaturesInRange_func(original_data_df,"DateBaselineVisit","CAP_CIP_RATIO")
  bl_otherfeatures_toadd<-c("MALE","RACE","AGE","HT","WT","BMI","HD","MosOnDIALYSISatConsent")
  updated_bl_features <- c(bl_otherfeatures_toadd,bl_features)
  
  #1year features
  yr1_features <- get_FeaturesInRange_func(original_data_df,"ID1","CAP_CIP1_RATIO")

  #2year features
  yr2_features <- get_FeaturesInRange_func(original_data_df,"ID2","CAP_CIP2_RATIO")

  #Outcomes
  outcome_related_features <- c("CASCOREA","CASCORE1A","CASCORE2A",
                                "SqRtVOLUMEA","SqRtVOLUME1A","SqRtVOLUME2A") 
  #other possible label "Diff1CACSRV","Diff2CACSRV","Diff1THQCT3D","Diff2THQCT3D"
  ##'@note: THQCT3D = (TRHQCT + TLHQCT)/2 it is aslo considered a feature for prediction CASCOREA



  return(list(updated_bl_features,yr1_features,yr2_features,outcome_related_features))
}

#2. This function update CASCORE by checking if CASCOREADD is avaiable
# #use CASCOREADD instead of CASCORE whenver CASCOREADD is avaliable
update_CASCORE_func <- function(dat){
  #2.Clean CASCORE by checking if CASCOREADD is avaiable
  #use CASCOREADD instead of CASCORE whenver CASCOREADD is avaliable
  casadd_indexes <- which(is.na(dat$CASCOREADD)==F)
  if(length(casadd_indexes) !=0){
    dat$CASCOREA[casadd_indexes] <- dat$CASCOREADD[casadd_indexes] #update the corresponding cascore
  }
  casadd_indexes2 <- which(is.na(dat$CASCOREADD1)==F)
  if(length(casadd_indexes2) !=0){
    dat$CASCORE1A[casadd_indexes2] <- dat$CASCOREADD1[casadd_indexes2] #update the corresponding cascore
  }

  casadd_indexes3 <- which(is.na(dat$CASCOREADD2)==F)
  if(length(casadd_indexes3) !=0){
    dat$CASCORE1A[casadd_indexes3] <- dat$CASCOREADD2[casadd_indexes3] #update the corresponding cascore
  }
  
 return(dat)
}


#4. This function remove outlier patients for particular features (RFN3Dcortical, and CASCOREA)
remove_outlier_pts_func <-function(dat){
      dat <- raw_data_df
      
      #muanlly remove two outlier extreme values
      dat <- dat[-which(dat[,"RFN3Dcortical"] >=30000),]
      dat <- dat[-which(dat[,"PO4BIND1A"] >= 4000),]
      
      #Remove CASCORE outliers <p2.5 or <p97.5
      all_cascore_values <- unlist(dat[,c("CASCOREA","CASCORE1A","CASCORE2A")])
      p2_5 <- quantile(all_cascore_values,na.rm = T,c(0.025))
      p97_5 <- quantile(all_cascore_values,na.rm = T,c(0.975))
      outlier_indexes <- which(  dat[,"CASCOREA"] < p2_5 | dat[,"CASCOREA"] > p97_5 
                                 | dat[,"CASCORE1A"] < p2_5 | dat[,"CASCORE1A"] > p97_5 
                                 | dat[,"CASCORE2A"] < p2_5 | dat[,"CASCORE2A"] > p97_5 )
      outlier_CASCORE_data <- dat[outlier_indexes,c("ID","CASCOREA","CASCORE1A","CASCORE2A")]
      
      #Remove SqrVOlumn outliers from the entire data
      all_sqrtV_values <- unlist(dat[,c("SqRtVOLUMEA","SqRtVOLUME1A","SqRtVOLUME2A")])
      p2_5 <- quantile(all_sqrtV_values,na.rm = T,c(0.025))
      p97_5 <- quantile(all_sqrtV_values,na.rm = T,c(0.975))
      outlier_indexes2 <- which(  dat[,"SqRtVOLUMEA"] < p2_5 | dat[,"SqRtVOLUMEA"] > p97_5 
                                  | dat[,"SqRtVOLUME1A"] < p2_5 | dat[,"SqRtVOLUME1A"] > p97_5 
                                  | dat[,"SqRtVOLUME2A"] < p2_5 | dat[,"SqRtVOLUME2A"] > p97_5 )
      outlier_sqrtV_data <- dat[outlier_indexes2,c("ID","SqRtVOLUMEA","SqRtVOLUME1A","SqRtVOLUME2A")]
      
      all_outliers <- unique(c(outlier_CASCORE_data[,"ID"], outlier_sqrtV_data[,"ID"]))
      
      updated_dat<- dat[-which(dat[,"ID"] %in% all_outliers),]
      return(updated_dat)
}


#5. this fucntion matches features and outcome by year, 
# returns a feature and outcome by year table

match_byyear_featureAndoutcome_func <- function(bl_features,yr1_features,yr2_features, outcomes){
  matched_feature_table <- as.data.frame(matrix(NA,nrow = length(bl_features) ,ncol = 3))
  colnames(matched_feature_table) <- c("BL","Yr1","Yr2")
  for (f in 1:length(bl_features)){
    curr_f <- bl_features[f]
    matched_feature_table[f,1] <- curr_f
    
    if (curr_f == "CO2"| curr_f =="L2Cortical" | curr_f =="L2QCT" | curr_f == "L2QCT_t" | curr_f == "L2DXA" | curr_f == "L2DXA_t" | curr_f == "FGF23"){
      match_term1 <- gsub("0","",tolower(curr_f))
      matched_term2 <- gsub("1|\\_1","",tolower(yr1_features))
      matched_term3 <- gsub("2$|\\_2","",tolower(yr2_features))
    }else if (curr_f =="L1Cortical" | curr_f == "L1QCT" | curr_f == "L1QCT_t" | curr_f == "L1DXA" | curr_f == "L1DXA_t"){
      match_term1 <- gsub("0","",tolower(curr_f))
      matched_term2 <- gsub("1$|","",tolower(yr1_features))
      matched_term3 <- gsub("2$","",tolower(yr2_features))
    }else if(curr_f == "MosOnDIALYSISatConsent"){
      match_term1 <- gsub("atconsent","",tolower(curr_f))
      matched_term2 <- gsub("yr1","",tolower(yr1_features))  #MosOnDialysisYr1
      matched_term3 <-  gsub("monthsondiaysis2","mosondialysis",tolower(yr2_features)) #Monthsondiaysis2
      
    }else if (curr_f == "PINP"){
      match_term1 <- gsub("","",tolower(curr_f))
      matched_term2 <- gsub("1","",tolower(yr1_features)) 
      matched_term3 <-  gsub("p1np2","pinp",tolower(yr2_features))  #replace 1 with I
      
    }else{
      match_term1 <- gsub("0","",tolower(curr_f))
      matched_term2 <- gsub("1|\\_1","",tolower(yr1_features))
      matched_term3 <- gsub("2|\\_2","",tolower(yr2_features))
    }
    #yr1
    match_idxes <- which(matched_term2 == match_term1)
    #yr2
    match_idxes2 <- which(matched_term3 == match_term1)
    
    #yr1
    if(length(match_idxes) >0){
      matched_fes <-yr1_features[match_idxes]
      matched_feature_table[f,2] <- matched_fes
    }
    
    #yr2
    if(length(match_idxes2) >0){
      matched_fes2 <-yr2_features[match_idxes2]
      matched_feature_table[f,3] <- matched_fes2
    }
    
    
  }
  
  #final outcome by year
  yr1_idxes <- which(grepl("1",outcomes))
  yr2_idxes <- which(grepl("2",outcomes))
  yr1_outcome <- outcomes[yr1_idxes]
  yr2_outcome <- outcomes[yr2_idxes]
  bl_outcome <- outcomes[-c(yr1_idxes,yr2_idxes)]
  matched_outcome_table <- cbind.data.frame(bl_outcome,yr1_outcome[1:2],yr2_outcome[1:2])
  colnames(matched_outcome_table) <- c("BL","Yr1","Yr2")
  
  matched_feature_outcome_table <- rbind(matched_feature_table,matched_outcome_table)
  return(matched_feature_outcome_table)
}


change_feature_name_func <-function(feature_name_list,bl_features,yr1_features,yr2_features){
  bl_idxes <- which(feature_name_list %in% bl_features)
  yr1_idxes <- which(feature_name_list %in% yr1_features)
  yr2_idxes <- which(feature_name_list %in% yr2_features)
  
  feature_name_list[bl_idxes] <- paste0(feature_name_list[bl_idxes],"_","bl")
  feature_name_list[yr1_idxes] <- paste0(feature_name_list[yr1_idxes],"_","yr1")
  feature_name_list[yr2_idxes] <- paste0(feature_name_list[yr2_idxes],"_","yr2")
  
  
  return(feature_name_list)
}

#this function update CASCORE, recode gender and race
update_features_inData <- function(data_df){
  #1. Update CASCORE
  updated_data_df <- update_CASCORE_func(data_df)
  
  #2.recode for gender, #Male:1 ; female:0
  gender_column <- updated_data_df[,"MALE"]
  updated_data_df$MALE <- as.numeric(recode_gender_func(gender_column))
  
  #5.recode for Race,  0: African american ; 1: WHITE or Others
  race_column <- updated_data_df[,"RACE"]
  updated_data_df$RACE <- as.numeric(recode_race_func(race_column))
  
  #4. recode Dialysate == -1 as NA
  neg1_idx1 <- which(updated_data_df["Dialysate"] == -1)
  neg1_idx2 <- which(updated_data_df["DIALYSATE1"] == -1)
  neg1_idx3 <- which(updated_data_df["DIALYSATE2"] == -1)

  if(length(neg1_idx1) > 0){
    updated_data_df[neg1_idx1,"Dialysate"] <- NA
  }
  if(length(neg1_idx2) > 0){
     updated_data_df[neg1_idx2,"DIALYSATE1"] <- NA
  }
  if(length(neg1_idx3) > 0){
    updated_data_df[neg1_idx3,"DIALYSATE2"] <- NA
  }
    
  return(updated_data_df)
}


#this function computed features : catogrized turnover and (average of TLH3D and TRH3D : TH3D)
add_features_inData  <- function(data_df) {
  #data_df <- raw_data_df_withUpdated_values
  #1.compute catogrized turnover and add turnover feature to the feaure list
  turnover_flg_df <- catogorize_turnover_func(data_df)
  turnover_flg_df_matchedID <- turnover_flg_df[match(data_df[,"ID"],turnover_flg_df[,"ID"]),]
  updated_data_df <- cbind(data_df, turnover_flg_df_matchedID[,2:4])
  
  #2.Compute average of TLH3D and TRH3D, called it TH3D, (it is the same as THQCT3D in orignal data
  #'@note THQCT3D = (TLH3D + TRH3D)/2 it is also considered as a outcome
  updated_data_df$TH3D <- (updated_data_df[,"TLH3D"] + updated_data_df[,"TRH3D"])/2
  updated_data_df$TH3D1 <- (updated_data_df[,"TLH3D1"] + updated_data_df[,"TRH3D1"])/2
  updated_data_df$TH3D2 <- (updated_data_df[,"TLH3D2"] + updated_data_df[,"TRH3D2"])/2
  
  
  #3. remove TLH3D and TRH3D since they are used to compute new feature TH3D
  updated_data_df<- updated_data_df[,-which(colnames(updated_data_df) %in% c("TLH3D","TLH3D1","TLH3D2",
                                                                             "TRH3D","TRH3D1", "TRH3D2"))]
  
  
  return(updated_data_df)
  
}

## This function recode for gender, #Male:1 ; female:0
recode_gender_func <- function(gender_column){
  m_idxes <- which(gender_column == "Male") 
  f_idxes <- which(gender_column == "Female") 
  
  gender_column[m_idxes] <- 1 
  gender_column[f_idxes] <- 0 
  
  return(gender_column)
  
}

##############################################################################################
##       This function Re_Code for Race                                                     ##
##       From CODEBOOK: 0: African american
##                      1: Caucasian
##                      2: HISPanic
##                      3: Asian
##   Recode: 0: African american
##           1: WHITE or Others
##############################################################################################
recode_race_func <- function(race_column){
  raceToChange_idxes <- which(race_column == 2 | race_column == 3)
  race_column[raceToChange_idxes] <- 1 
  return(race_column)
}



### Catogorize Turnover (High/Low) 
###Update changed threhold 100 to 150 for lowr turnover state
compute_turnover_func <- function(curr_race, curr_pth, curr_pth_ratio,curr_trap5b){
  turnover_flag <- NA
  if(is.na(curr_race) == T | is.na(curr_pth) == T | is.na(curr_pth_ratio) == T | is.na(curr_trap5b) == T ){
    turnover_flag <- NA  #"Missing requried Paramter"
  }else if(curr_pth < 150){ #patients with PTH <150 pg/mL, low turnover state.
    turnover_flag <- 0
  }else{
    if (curr_race == 0){ #if black
      cond1 <- (curr_pth < 350 & curr_pth_ratio < 1.2)
      cond2 <- ((curr_pth >= 350 & curr_pth<=800) & curr_pth_ratio < 1.6 & curr_trap5b < 6.7)
      
      if (cond1 | cond2){
        turnover_flag <- 0
      }else{
        turnover_flag <- 1
      }
      
    }else if (curr_race == 1){#if white
      cond1_w <- (curr_pth < 400 & curr_pth_ratio < 1.0)
      cond2_w <- (curr_pth < 400 & curr_pth_ratio >=1.0 & curr_trap5b < 6.7)
      if (cond1_w | cond2_w){
        turnover_flag <- 0
      }else{
        turnover_flag <- 1
      }
    }
  }
  
  return(turnover_flag)
}


catogorize_turnover_func <- function(dat){
  
  turnover_flag_df <- as.data.frame(matrix(NA,nrow = nrow(dat), ncol = 4))
  colnames(turnover_flag_df) <- c("ID","turnover","turnover1","turnover2")
  for (p in 1:nrow(dat)){
    curr_race <- dat[p,"RACE"]
    
    curr_pth_bl <- dat[p,"PTH"]
    curr_pth_yr1 <- dat[p,"PTH1"]
    curr_pth_yr2 <- dat[p,"PTH2"]
    
    curr_pthratio_bl <- dat[p,"CAP_CIP_RATIO"]
    curr_pthratio_yr1 <- dat[p,"CAP_CIP1_RATIO"]
    curr_pthratio_yr2 <- dat[p,"CAP_CIP2_RATIO"]
    
    
    curr_trap5b_bl <- dat[p,"TRAP5B"]
    curr_trap5b_yr1 <- dat[p,"TRAP5B1"]
    curr_trap5b_yr2 <- dat[p,"TRAP5B2"]
    
    turnover_flag_df[p,"ID"] <- dat[p,"ID"]
    turnover_flag_df[p,"turnover"] <- compute_turnover_func(curr_race,curr_pth_bl,curr_pthratio_bl,curr_trap5b_bl)
    turnover_flag_df[p,"turnover1"] <- compute_turnover_func(curr_race,curr_pth_yr1,curr_pthratio_yr1,curr_trap5b_yr1)
    turnover_flag_df[p,"turnover2"] <- compute_turnover_func(curr_race,curr_pth_yr2,curr_pthratio_yr2,curr_trap5b_yr2)
    
  }
  
  return(turnover_flag_df)
}



# ##################################################################
# ##                          Section 2:                          ##
# ##                       Check Histo data                       ##
# ##################################################################
# ROD1_Histo_data <- read.xlsx(paste0(data_dir, "ROD1 histo only.xlsx"), sheet = 1)
# #Clean Data
# ROD1_Histo_data <- ROD1_Histo_data[, -1]
# colnames(ROD1_Histo_data) <- as.character(ROD1_Histo_data[1,])
# ROD1_Histo_data <- ROD1_Histo_data[-1, ]
# ROD1_Histo_data$ID<-gsub(" |\\.","",ROD1_Histo_data$ID)    #baseline ID
# 
# ###get outcome 
# shared_Ids_indices <- which(ROD1_Histo_data$ID %in% updated_ROD_data$ID)
# updated_ROD1_Histo_data <- ROD1_Histo_data[shared_Ids_indices,]
# 
# CASCOREA_values<- updated_ROD_data[match(updated_ROD1_Histo_data[,"ID"], updated_ROD_data[,"ID"]),"CASCOREA"]
# CASCORE1A_values<- updated_ROD_data[match(updated_ROD1_Histo_data[,"ID"], updated_ROD_data[,"ID"]),"CASCORE1A"]
# CASCORE2A_values<- updated_ROD_data[match(updated_ROD1_Histo_data[,"ID"], updated_ROD_data[,"ID"]),"CASCORE2A"]
# 
# updated_ROD1_Histo_data$CASCOREA <- CASCOREA_values
# updated_ROD1_Histo_data$CASCORE1A <- CASCORE1A_values
# updated_ROD1_Histo_data$CASCORE2A <- CASCORE2A_values
# 
# 