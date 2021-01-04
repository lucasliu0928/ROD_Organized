

###########################################################################
###                         Labelling  FUNCTIONS                       ###
###########################################################################

Get_Trend_label_func <- function(analysis_df){
  ##'@Note: Normlize the sqrtvolumn and x axis to 0 to 1, 
  ##'so that x(time) and y(value) are the same scale so that we can compute the slope and angle
  normalized_df <- analysis_df #initialization
  
  all_SqRtVOLUME <- c(normalized_df$SqRtVOLUMEA,normalized_df$SqRtVOLUME1A,normalized_df$SqRtVOLUME2A)
  global_max <- max(all_SqRtVOLUME,na.rm = T)
  global_min <- min(all_SqRtVOLUME,na.rm = T)
  
  ## Normalization
  normalized_df$SqRtVOLUMEA <- (normalized_df$SqRtVOLUMEA-global_min)/(global_max-global_min)
  normalized_df$SqRtVOLUME1A <- (normalized_df$SqRtVOLUME1A-global_min)/(global_max-global_min)
  normalized_df$SqRtVOLUME2A <- (normalized_df$SqRtVOLUME2A-global_min)/(global_max-global_min)
  
  x_values <- c(0,1,2) #time point
  x_values_norm <-  (x_values-0)/(2-0) #normalization for time point
  
  ## Compute slope for 0-1 (From baseline to 1st yr), and 1-2 (From 1st yr to 2nd yr)
  slope0_1 <- (normalized_df$SqRtVOLUME1A - normalized_df$SqRtVOLUMEA) / (x_values_norm[2]-x_values_norm[1]) #From baseline to 1st yr slope
  slope1_2 <- (normalized_df$SqRtVOLUME2A - normalized_df$SqRtVOLUME1A) / (x_values_norm[3]-x_values_norm[2]) #From 1st to 2nd yr slope
  angle0_1 <- atan(slope0_1)*180/pi #convert radian to degreen, 1 radian= 180/PI
  angle1_2 <- atan(slope1_2)*180/pi #convert radian to degreen, 1 radian= 180/PI
  slope_df <- cbind.data.frame(normalized_df$ID, slope0_1, slope1_2, angle0_1 , angle1_2)
  colnames(slope_df)[1] <- "ID"
  all_timepoint_slope <- c(slope0_1,slope1_2)
  all_timepoint_angle <- c(angle0_1,angle1_2)
  all_time_slope_angle_df <- cbind.data.frame(all_timepoint_slope,all_timepoint_angle)
  colnames(all_time_slope_angle_df) <- c("Slope","Angle")
  
  ## plot in slope in tan
  seq_breaks <- c(seq(min(all_time_slope_angle_df[,"Slope"],na.rm = T),max(all_time_slope_angle_df[,"Slope"],na.rm = T),by=0.1))
  seq_label <-  seq_breaks
  plot_histgram_func(all_time_slope_angle_df,"Slope", "Slope" , "Count","Distribution of all slopes",0.02,seq_breaks,seq_label)
  ## plot in angles in degrees
  seq_breaks<-c(-20,-10,0,10,20,30,40)
  seq_label <-  seq_breaks
  all_angle_p <- plot_histgram_func(all_time_slope_angle_df,"Angle", "Arctan(slope) in degrees" , "Count","Distribution of all angles",1,seq_breaks,seq_label)
  all_angle_p
  
  
  ###########################################################################
  ##  Define catoriies of angle changing: increase, decrease, and little change
  ###########################################################################
  ## found threhold for increase, decrease and little change
  all_postive_angles<-all_time_slope_angle_df$Angle[which(all_time_slope_angle_df$Angle>0)]
  all_negtive_angles<-all_time_slope_angle_df$Angle[which(all_time_slope_angle_df$Angle<0)]
  sd_pos_angle <- sd(all_postive_angles,na.rm = T)
  sd_neg_angle <- sd(all_negtive_angles,na.rm = T)
  threshold_high <- 0 + sd_pos_angle
  threshold_lower <- 0 - sd_neg_angle
  
  #plot histgram with threhould
  p <- add_verline_toplot_function(all_angle_p,round(threshold_high,2),30,"darkblue",2)
  p <- add_verline_toplot_function(p,round(threshold_lower,2),30,"darkred",2)
  p
  
  ###################################################################################
  ##  Assign Labels
  ##  3 possible angles + ,- , little change for two duration,  
  ##  3^2(9) possible label
  ###################################################################################
  ## Define no change as little change within a range
  angle0_1_nochange_critiron <- (slope_df$angle0_1 >= threshold_lower & slope_df$angle0_1 <= threshold_high)
  angle1_2_nochange_critiron <- (slope_df$angle1_2 >= threshold_lower & slope_df$angle1_2 <= threshold_high)
  
  #increase, increase(if avaiable)
  label1_indexes <- which(slope_df$angle0_1 > threshold_high & (slope_df$angle1_2 > threshold_high | is.na(slope_df$angle1_2)==T)) #further group this one by increase value
  #increase, decrease (if avaiable)
  label2_indexes <- which(slope_df$angle0_1 > threshold_high & (slope_df$angle1_2 < threshold_lower | is.na(slope_df$angle1_2)==T))
  #increase, little change  (if avaiable)
  label3_indexes <- which(slope_df$angle0_1 > threshold_high &  (angle1_2_nochange_critiron | is.na(slope_df$angle1_2)==T))
  #decrease, decrease  (if avaiable)
  label4_indexes <- which(slope_df$angle0_1 < threshold_lower & (slope_df$angle1_2 < threshold_lower | is.na(slope_df$angle1_2)==T))
  #decrease, increase  (if avaiable)
  label5_indexes <- which(slope_df$angle0_1 < threshold_lower & (slope_df$angle1_2 > threshold_high | is.na(slope_df$angle1_2)==T))
  #decrease, little change  (if avaiable)
  label6_indexes <- which(slope_df$angle0_1 < threshold_lower & (angle1_2_nochange_critiron | is.na(slope_df$angle1_2)==T))
  #little change, little change  (if avaiable)
  label7_indexes <- which(angle0_1_nochange_critiron & (angle1_2_nochange_critiron | is.na(slope_df$angle1_2)==T))
  #little change, increase  (if avaiable)
  label8_indexes <- which(angle0_1_nochange_critiron & (slope_df$angle1_2 > threshold_high | is.na(slope_df$angle1_2)==T))
  #little change, decreasae  (if avaiable)
  label9_indexes <- which(angle0_1_nochange_critiron & (slope_df$angle1_2 < threshold_lower |  is.na(slope_df$angle1_2)==T))
  
  slope_df$Label<-NA
  slope_df$Label[label1_indexes] <- 1 #increase, increase
  slope_df$Label[label2_indexes] <- 2
  slope_df$Label[label3_indexes] <- 3 #increase, little change
  slope_df$Label[label4_indexes] <- 4
  slope_df$Label[label5_indexes] <- 5
  slope_df$Label[label6_indexes] <- 6
  slope_df$Label[label7_indexes] <- 7 #little change, little change  (if avaiable)
  slope_df$Label[label8_indexes] <- 8 #little change, increase  (if avaiable)
  slope_df$Label[label9_indexes] <- 9 #little change, decreasae  (if avaiable)
  
  table(slope_df$Label)
  #Add labels to  the orignal df
  analysis_df$Trend_Label <- NA
  analysis_df$Trend_Label <- slope_df$Label[match(slope_df$ID, analysis_df$ID)]
  table(analysis_df$Trend_Label)
  return(analysis_df)
}



create_CASCORE_label_each_year <-function(analysis_data,col_names,year_label){
  ###Updated outcome for
  #Non-high risk : CASCORE < 400 
  #High Risk:      CASCORE >= 400 
  
  #find indexes
  highrisk_index  <- which(analysis_data[,col_names] >= 400 ) 
  nonhigh_risk_index  <- which(analysis_data[,col_names] < 400 ) 
  
  #label df 
  label_df <- data.frame(matrix(NA, nrow = nrow(analysis_data) ,ncol = 1 ))
  label_name <- paste0("CASCOREA_Cat_",year_label)
  colnames(label_df) <- label_name
  rownames(label_df) <- rownames(analysis_data)
  label_df[highrisk_index,label_name]<-1
  label_df[nonhigh_risk_index,label_name]<-0
  
  
  return(label_df)
}


create_Delta_CASCORE_label_each_year <-function(analysis_data,col_names,delta_label){
  #find indexes
  increase_indes  <- which(analysis_data[,col_names] >= 0 ) 
  decrease_indes  <- which(analysis_data[,col_names] < 0 ) 
  #add label column
  label_name <- paste0("Delta_CASCOREA_Cat_",delta_label)
  analysis_data[,label_name]<-NA
  analysis_data[increase_indes,label_name] <- 1
  analysis_data[decrease_indes,label_name] <- 0
  
  return(analysis_data)
}

##Trend lables 1102
check_trend_direction_func <- function(col_values,thres_high,thres_low){
  if (is.na(col_values) == F){
      if(col_values > thres_high){
        trend_flg <- "Increase"
      }else if(col_values < thres_low){
        trend_flg <- "Decrease"
      }else{
        trend_flg <-"Little change"
      }
      }else{
        trend_flg <- NA
      }
  return(trend_flg)
}

create_Trend_CASCORE_label_func <-function(final_delta_df,threshold_lower,threshold_high){
  
  trend_label_df <- as.data.frame(matrix(NA,nrow = nrow(final_delta_df),ncol = 4))
  colnames(trend_label_df) <- c("ID","Trend1","Trend2","Trend_Label")
  for (p in 1:nrow(final_delta_df)){
    trend_label_df[p, "ID"] <- rownames(final_delta_df)[p]
    curr_p_dat <- final_delta_df[p,c("CASCOREA_Diff0_1","CASCOREA_Diff1_2")]
    
    curr_trend1 <- check_trend_direction_func(curr_p_dat[,"CASCOREA_Diff0_1"],threshold_high,threshold_lower)
    curr_trend2 <- check_trend_direction_func(curr_p_dat[,"CASCOREA_Diff1_2"],threshold_high,threshold_lower)
    trend_label_df[p, "Trend1"] <- curr_trend1
    trend_label_df[p, "Trend2"] <- curr_trend2
    
    if (is.na(curr_trend1) ==T & is.na(curr_trend2) == T){
      trend_label_df[p, "Trend_Label"] <- NA
    }else {
      if (curr_trend1 == "Increase" & (curr_trend2 == "Increase" | is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 1
      }else if (curr_trend1 == "Increase" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 2
      }else if (curr_trend1 == "Increase" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 3
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 4
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Increase"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 5
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 6
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 7
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Increase"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 8
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 9
      }
    }
    
    
  }
  
  
  
  return(trend_label_df)
}


#SqrtVol 120720

create_sqrtVol_label_each_year <-function(analysis_data,col_names,year_label){
  ###Updated outcome for
  #Non-high risk : sqrtVol < 19.1 
  #High Risk:      sqrtVol >= 19.1 
  
  #find indexes
  highrisk_index  <- which(analysis_data[,col_names] >= 19.1 ) 
  nonhigh_risk_index  <- which(analysis_data[,col_names] < 19.1 ) 
  
  #label df 
  label_df <- data.frame(matrix(NA, nrow = nrow(analysis_data) ,ncol = 1 ))
  label_name <- paste0("SqRtVOLUME_Cat_",year_label)
  colnames(label_df) <- label_name
  rownames(label_df) <- rownames(analysis_data)
  label_df[highrisk_index,label_name]<-1
  label_df[nonhigh_risk_index,label_name]<-0
  
  
  return(label_df)
}

create_Trend_sqrtVol_label_func <-function(final_delta_df,threshold_lower,threshold_high){
  
  trend_label_df <- as.data.frame(matrix(NA,nrow = nrow(final_delta_df),ncol = 4))
  colnames(trend_label_df) <- c("ID","Trend1","Trend2","Trend_Label")
  for (p in 1:nrow(final_delta_df)){
    trend_label_df[p, "ID"] <- rownames(final_delta_df)[p]
    curr_p_dat <- final_delta_df[p,c("SqRtVOLUMEA_Diff0_1","SqRtVOLUMEA_Diff0_2")]
    
    curr_trend1 <- check_trend_direction_func(curr_p_dat[,"SqRtVOLUMEA_Diff0_1"],threshold_high,threshold_lower)
    curr_trend2 <- check_trend_direction_func(curr_p_dat[,"SqRtVOLUMEA_Diff0_2"],threshold_high,threshold_lower)
    trend_label_df[p, "Trend1"] <- curr_trend1
    trend_label_df[p, "Trend2"] <- curr_trend2
    
    if (is.na(curr_trend1) ==T & is.na(curr_trend2) == T){
      trend_label_df[p, "Trend_Label"] <- NA
    }else {
      if (curr_trend1 == "Increase" & (curr_trend2 == "Increase" | is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 1
      }else if (curr_trend1 == "Increase" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 2
      }else if (curr_trend1 == "Increase" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 3
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 4
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Increase"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 5
      }else if (curr_trend1 == "Decrease" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 6
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Little change"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 7
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Increase"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 8
      }else if (curr_trend1 == "Little change" & (curr_trend2 == "Decrease"| is.na(curr_trend2) == T)){
        trend_label_df[p, "Trend_Label"] <- 9
      }
    }
    
    
  }
  
  
  
  return(trend_label_df)
}


###########################################################################
###                         STATISTIC FUNCTIONS                         ###
###########################################################################
IQR.outliers <- function(x) {
  Q3 <- quantile(x, 0.75,na.rm = T)
  Q1 <- quantile(x, 0.25,na.rm = T)
  IQR <- (Q3-Q1)
  lower_th <- (Q1-(1.5*IQR))
  higher_th <- (Q3+(1.5*IQR))

  lower_outliers_indexes  <- which(x < lower_th)
  higher_outliers_indexes <- which(x > higher_th)
  all_outlier_indexes <- c(lower_outliers_indexes, higher_outliers_indexes)
  outlier_value <- x[all_outlier_indexes]
  
  return(all_outlier_indexes)
  
}




############################################################################
############################################################################
###                                                                      ###
###                            PLOT FUNCTIONS                            ###
###                                                                      ###
############################################################################
############################################################################
Plot_VaraibleAt_3TimePoint_func <- function(plot_data,x_name,yname,min_y,max_y,y_breaks_gap,title_name,legendpos_flag,xaxis_size){
  #plot_data <- outcome2_dat
  #yname <- "SqRtVOLUME"
  
  reshaped_dat <- melt(plot_data, id = "ID") #reshapce data by IDs, hold Id as constant, and other columns has varibale
  p <- ggplot(reshaped_dat, aes(x = variable, y = value, group = ID,color = ID)) +
    geom_line(aes(color = ID)) + 
    geom_point(aes(color = ID)) +
    theme(legend.position = legendpos_flag)+
    labs(title = title_name, x = x_name, y = yname)+
    theme(axis.text.x = element_text(face="bold", size=xaxis_size),
          axis.text.y = element_text(face="bold", size=5),
          axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"))+
    scale_y_continuous(limits = c(min_y, max_y), breaks = seq(min_y, max_y, by = y_breaks_gap))
  
  
  return(p)
}

#this fucntion add color by another var which is CASCAORE
Plot_VaraibleAt_3TimePoint_func2 <- function(plot_data,x_name,yname,label_name,min_y,max_y,y_breaks_gap,title_name,legendpos_flag,xaxis_size){
  #  # plot_data <- curr_data[, c(1,5,6,7,9)]
  # yname <- "SqRtVOLUME"
  # min_y<- min_glable
  # max_y <-  max_glable
  # x_name<-""
  # title_name<-""
  # plot_data <- plot_data1
  # label_name <- "COMB_LABEL"
  # xaxis_size <- 2
  
  #USE corVar as reshape ID for plot by groups
  reshaped_dat <- melt(plot_data[,1:4], id = "ID") #reshapce data by Label, hold Label as constant, and other columns has varibale
  #add new labels
  reshaped_dat$new_labels<-plot_data[,label_name][match(reshaped_dat$ID, plot_data$ID)]
  
  p <- ggplot(reshaped_dat, aes(x = variable, y = value, group = ID,color = new_labels)) +
    geom_line(aes(color = new_labels)) + 
    geom_point(aes(color = new_labels)) +
    theme(legend.title = element_blank(),legend.position = legendpos_flag,legend.text = element_text(size = 15))+
    labs(title = title_name, x = x_name, y = yname)+
    theme(axis.text.x = element_text(face="bold", size=xaxis_size),
          axis.text.y = element_text(face="bold", size=16),
          axis.text=element_text(size=18),
          axis.title=element_text(size=16,face="bold"))+
    scale_y_continuous(limits = c(min_y, max_y), breaks = seq(min_y, max_y, by = y_breaks_gap)) 
    #scale_color_manual(values=c("seagreen", "lightblue", "red3","pink3"))
  p
  
  return(p)
}


plot_histgram_func <- function(plot_data, col_name, xname, yname, titlename, binw,seq_breaks,seq_label){
  # plot_data <- all_cascores_df
  # col_name <- "updated_cascore"
  # xname <- ""
  # yname <- ""
  # titlename <- ""
  # binw <- 10
  # seq_breaks <- c(seq(0,max(plot_data[,col_name]),by=100))
  # seq_label <-      c(seq_breaks[-length(seq_breaks)],">1000")    
  
  p<-ggplot(plot_data, aes_string(x=col_name)) + 
    geom_histogram(color="black", binwidth = binw,fill="green",alpha = .2) +
    scale_x_continuous(breaks = seq_breaks, 
                       labels = seq_label)+
    labs(title = titlename,
         x = xname, y = yname) +
    theme(axis.text.x = element_text(face="bold", size=14),
          axis.text.y = element_text(face="bold", size=16),
          axis.text=element_text(size=18),
          axis.title=element_text(size=18,face="bold"))
  p
  return(p)
}



add_verline_toplot_function <- function(p, xpos,ypos,color,xpos_offset,textsize){
  newp <- p + geom_vline(xintercept = xpos, linetype="dotted", color = color, size=1.5) +
    geom_text(aes(x= xpos+xpos_offset, label= xpos, y= ypos), colour=color, angle=0,size = textsize)
  
  return(newp)
}

add_horizline_toplot_function <- function(p, xpos,ypos,color,ypos_offset){
  # p <- grp_plot
  # ypos <- 10
  # xpos <- 0.5
  # color <- "darkred"
  # ypos_offset <- 200
  newp <- p + geom_hline(yintercept = ypos, linetype="dotted", color = color, size=1.5)+
    geom_text(aes(x= xpos, label= ypos, y= ypos+ypos_offset), colour=color, angle=0)
  newp
  
  return(newp)
}



############################################################################
############################################################################
###                                                                      ###
###                Parameter Tuning FUNCTIONS                            ###
###                                                                      ###
############################################################################
############################################################################

parmeter_tuning_func <- function(train_df){
# Training Settings
set.seed(1)

# Training Parameters
CV_folds <- 5 # number of folds
CV_repeats <- 3 # number of repeats
minimum_resampling <- 5 # minimum number of resamples

# trainControl object for standard repeated cross-validation
train_control <- caret::trainControl(method = "repeatedcv", number = CV_folds, repeats = CV_repeats, 
                                     verboseIter = FALSE, returnData = FALSE) 

# trainControl object for repeated cross-validation with grid search
adapt_control_grid <- caret::trainControl(method = "adaptive_cv", number = CV_folds, repeats = CV_repeats, 
                                          adaptive = list(min = minimum_resampling, # minimum number of resamples tested before model is excluded
                                                          alpha = 0.05, # confidence level used to exclude parameter settings
                                                          method = "gls", # generalized least squares
                                                          complete = TRUE), 
                                          search = "grid", # execute grid search
                                          verboseIter = FALSE, returnData = FALSE) 

# trainControl object for repeated cross-validation with random search
adapt_control_random <- caret::trainControl(method = "adaptive_cv", number = CV_folds, repeats = CV_repeats, 
                                            adaptive = list(min = minimum_resampling, # minimum number of resamples tested before model is excluded
                                                            alpha = 0.05, # confidence level used to exclude parameter settings
                                                            method = "gls", # generalized least squares
                                                            complete = TRUE), 
                                            search = "random", # execute random search
                                            verboseIter = FALSE, returnData = FALSE) 

# Create grid
XGBoost_Linear_grid <- expand.grid(
  nrounds = c(50, 100), # number of boosting iterations
  eta = c(0.01, 0.1, 1),  # learning rate, low value means model is more robust to overfitting
  lambda = c(0.1, 0.5, 1), # L2 Regularization (Ridge Regression)
  alpha =  c(0.1, 0.5, 1) # L1 Regularization (Lasso Regression)
) 

library(parallel)
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

set.seed(1); 
# Train model with grid search
GS_XGBoost_Linear_model <- caret::train(Label ~., 
                                        data = train_df,
                                        method = "xgbtree",
                                        trControl = adapt_control_grid,
                                        verbose = FALSE, 
                                        silent = 1,
                                        # tuneLength = 20
                                        tuneGrid = XGBoost_Linear_grid,
                                        na.action = "na.omit"
                                    
)

stopCluster(cluster) # shut down the cluster 
registerDoSEQ(); #  force R to return to single threaded processing




}

#########################################################################################
####### Prediction Functions
#########################################################################################

#1.max_min normalization
range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}

#2.Find Important features by training all data
Find_Important_features<-function(data_to_analysis_norm,label_col_name,xgb_params,nround){
  #data_to_analysis_norm
  #label_col_name<-"Label"
  
  #Feature Importance
  data_label<-  data_to_analysis_norm[,label_col_name]
  data_part <- data_to_analysis_norm[,!(names(data_to_analysis_norm) %in% label_col_name)]
  data_dmatrix <- xgb.DMatrix(data = as.matrix(data_part), label=data_label)
  
  
  bst_model <- xgb.train(params = xgb_params,data = data_dmatrix,nrounds = nround)
  
  # get the feature real names
  names <-  colnames(data_part)
  # compute feature importance matrix
  importance_matrix = xgb.importance(feature_names = names, model = bst_model)
  return(importance_matrix)
  
}

Plot_FeatureImportance_func <- function(importantce_Matrix,f_type,plot_top_flag,top_n){
  matrix_to_plot<-importantce_Matrix
  
  #top
  if(plot_top_flag==1){
    matrix_to_plot10<-matrix_to_plot[1:top_n,]
    gp = xgb.ggplot.importance(matrix_to_plot10,n_clusters = 1)
    
    final_p <- gp +
      theme_bw()+
      theme(legend.position = "none",
            #panel.grid.major = element_blank(), #remove grid
            plot.title = element_text(size= 22),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title = element_text(size=20)) +
      ggtitle(paste0("Feature Importance\n",f_type))
      #scale_fill_manual(values=c("brown1","brown1", "#F8716F","#30B0BE"))
  }else{
    gp = xgb.ggplot.importance(matrix_to_plot,n_clusters = 1)
    final_p <- gp+
              theme_bw()+
              theme(legend.position = "none",
                    #panel.grid.major = element_blank(),
                    plot.title = element_text(size= 22),
                    axis.text.x = element_text(size = 10),
                    axis.text.y = element_text(size = 30),
                    axis.title = element_text(size=20))+
              ggtitle(paste0("Feature Importance\n",f_type))
              #scale_fill_manual(values=c("brown1","brown1", "#F8716F","#30B0BE"))
    final_p
    
  }
  return(final_p)
}


#3.Validation function for binary classification using gbtree or gblinear
Validation_function<-function(Validation_norm,label_col,xgb_params,nround,upsample_flag,important_feature_flag,important_fs){
  #Validation_norm <- data_to_analysis_norm
  #important_fs <- top_important_features
  #label_col <- "Label"
  
  if (important_feature_flag == 1){
     feature_to_select_plusLabel <- c(important_fs,label_col)
   }else{
     feature_to_select_plusLabel <- colnames(Validation_norm)
   }
   col_selected <- which(colnames(Validation_norm) %in% feature_to_select_plusLabel)
   Validation_norm <- Validation_norm[,col_selected]
  
   predict_acutal_table_list<-list()
  for(r in 1:5){ #five times sampling for training data part
    #manually loocv
    pred<-NA
    actual<-NA
    patientID<-NA
    for(i in 1:nrow(Validation_norm)){
      test_data<-Validation_norm[i,]
      test_data_Id<-rownames(test_data)
      test_label<-test_data[,label_col]
      test_data_part <- test_data[,!(names(test_data) %in% label_col)]
      dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
      
      #remove test data from train sets
      train_data_Id_index<-which(rownames(Validation_norm) != test_data_Id)
      train_data<-Validation_norm[train_data_Id_index,]
      
      #upsampling
      library(caret)
      if(upsample_flag==1){
        set.seed(r)
        up_train <- upSample(x = train_data[, -ncol(train_data)],
                             y = as.factor(train_data[,label_col]), yname = label_col)                         
        train_label <- up_train[,label_col]
        train_label<-as.numeric(train_label)-1
        train_data_part<-up_train[,!(names(up_train) %in% label_col)]
        train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
      }else if(upsample_flag==0){ #downsample
        set.seed(r)
        down_train <- downSample(x = train_data[, -ncol(train_data)],
                                 y = as.factor(train_data[,label_col]), yname = label_col)                         
        table(down_train[,label_col]) 
        
        train_label <- down_train[,label_col]
        train_label<-as.numeric(train_label)-1
        train_data_part<-down_train[,!(names(down_train) %in% label_col)]
        train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
      }else{
        train_label <- train_data[,label_col]
        train_data_part<-train_data[,!(names(train_data) %in% label_col)]
        train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
      }
      
      bst_model <- xgb.train(params = xgb_params,
                             data = train_matrix,
                             nrounds = nround)
      pred[i] <- predict(bst_model, dtest)
      actual[i]<-test_label
      patientID[i]<-test_data_Id
    }
    
    
    
    predict_acutal_table_list[[r]]<-cbind.data.frame(patientID,actual,pred)
    
  }
  return(predict_acutal_table_list)
}




#4.update predction by threshold. ex: <threshld:0, >threhosld:1 (Note: for cases that only have 2 duration labels)
updated_predction_func<-function(predicted,threshold){
  #unique acutal labels
  updated_pred<-NA
  for(i in 1:length(predicted)){
    current_pred<-predicted[i]
    if(current_pred<threshold){
      updated_pred[i]<-0
    }else {
      updated_pred[i]<-1
    }
    
  }
  return(updated_pred)
}


#########################################################################################
####### Updated Prediction Functions OCT/20
#########################################################################################
#########################################################################################
##mRMR function
#minize the correlation between features (reduncancy)
#maxiums the mutual infor between feature with target (revelance)
#The scores method returns the scores of individual features in respect to previously selected features as per standard mRMR procedure. For each target, the score of a feature is defined as the mutual information between the target and this feature minus the average mutual information of previously selected features and this feature.
#The causality data is compute using the co-information lattice algorithm on each V-structure (feature, target, feature). Given that this procedure is co
#########################################################################################
library(mRMRe)
mrmr_func <- function(data_to_anlaysis,label_col,n_features_traget){
  
  #convert to numeric
  for (j in 1:ncol(data_to_anlaysis)){
    data_to_anlaysis[,j] <- as.numeric(data_to_anlaysis[,j])
    
  }
  
  dd <- mRMR.data(data = data_to_anlaysis)
  label_col_idx <- which(colnames(data_to_anlaysis) == label_col)
  mrmr_res <- mRMR.classic(data = dd, target_indices = label_col_idx, feature_count = n_features_traget)
  
  mutual_info_matrix<-mrmr_res@mi_matrix #this one could be wrong, check and ignore
  
  #MI is computed based on estimation of correaltion 
  #"https://cran.r-project.org/web/packages/mRMRe/vignettes/mRMRe.pdf" , thats why we have negative
  mi2<-mim(dd) 
  
  ## print the index of the selected features for each distinct mRMR solutions
  #print(solutions(mrmr_res)[[1]])
  
  ## print the names of the selected features for each distinct mRMR solutions
  selected_features<-  mrmr_res@feature_names[solutions(mrmr_res)[[1]]]
  #print(selected_features)
  
  #causality_list alsways return for all features
  causality_df<-cbind(mrmr_res@feature_names,mrmr_res@causality_list[[1]])
  
  
  
  return(list(mutual_info_matrix,mi2,selected_features,causality_df))
}


#####Train and validation function using XGBoost
Train_andValidaiton_func <- function(data_to_run, feature_selected,label_col){
  ##########################################################################################
  ## Find important Features by training on Full Data
  ##########################################################################################
  data_to_analysis <-  data_to_run[, feature_selected]
  rownames(data_to_analysis) <- data_to_analysis$ID
  
  ID_indxes <- which(colnames(data_to_analysis)=="ID")
  if (length(ID_indxes) >0){
    data_to_analysis <- data_to_analysis[,-ID_indxes]
  }
  
  #remove all NA columns
  all_NA_feature_idxes <- which(colSums(is.na(data_to_analysis))==nrow(data_to_analysis))
  if (length(all_NA_feature_idxes)>0){
    data_to_analysis <- data_to_analysis[,-all_NA_feature_idxes]
  }
  
  colnames(data_to_analysis) [which(colnames(data_to_analysis) == label_col)] <- "Label"
  for (j in 1:ncol(data_to_analysis)){
    data_to_analysis[,j] <- as.numeric(data_to_analysis[,j])
  }
  
  
  #normalization
  label_col_index <- which(colnames(data_to_analysis) == "Label")
  data_to_analysis_norm<-as.data.frame(apply(data_to_analysis[,-label_col_index], 2, range01))
  data_to_analysis_norm$Label <- data_to_analysis$Label
  numberOfClasses <- length(unique(data_to_analysis_norm$Label))
  xgb_params <- list(booster = "gblinear",
                     "objective" = "binary:logistic", #binary:logistic
                     "eval_metric" = "logloss",
                     gamma = 20,  #The range is 0 to ∞. Larger the gamma more conservative the algorithm is.
                     eta = 1, #the range is 0 to 1. Low eta value means model is more robust to overfitting) 
                     max_depth = 12,
                     min_child_weight = 12,  #The range is 0 to ∞. #he larger, the more conservative the algorithm will be. 
                     max_delta_step = 0)
  
  
  nround<-20
  
  
  important_features_matrix <- Find_Important_features(data_to_analysis_norm,"Label",xgb_params,nround)
  if (length(important_features_matrix$Feature) > 10){
    top_important_features <- important_features_matrix$Feature[1:10] #top10
  }else{
    top_important_features <- important_features_matrix$Feature[1:length(important_features_matrix$Feature)] #all
  }
  
  feature_included <- as.data.frame(colnames(data_to_analysis_norm)) #110 features
  
  #write.csv(bl_features,"fs_included_bl.csv")
  ##########################################################################################
  #LOOCV for 5 times, each time do up or downsampling or neithers
  #'##########################################################################################
  label_table <- table(data_to_analysis_norm$Label)
  upsample_flag <- 1
  important_feature_flag <- 1
  res <- Validation_function(data_to_analysis_norm,"Label",xgb_params,nround,upsample_flag,
                             important_feature_flag,top_important_features)
  
  #Compute perofrmance
  Final_perforamnce_df <- as.data.frame(matrix(NA, nrow = 5,ncol = 8))
  rownames(Final_perforamnce_df) <- paste0("Sampling",seq(1,5))
  colnames(Final_perforamnce_df) <- c("AUC","ACC","C0_Prec","C0_Recall","C0_F1","C1_Prec","C1_Recall","C1_F1")
  
  for (r in 1:5){
    curr_results <- res[[r]]
    
    pred <- curr_results$pred 
    actual <- curr_results$actual
    
    library(pROC)
    roc_obj <-  roc(actual,pred,quiet = T)
    auc_score<- round(auc(roc_obj),2)
    
    cutoff_results<-coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                  "precision", "recall"), transpose = FALSE)
    
    cut_off_thres<-cutoff_results$threshold[1]  ###best accuracy might happend at three point, so choose the first one,loeast threshold
    
    #get cut off scores and confustion matrix
    updated_pred<-updated_predction_func(pred,cut_off_thres)
    
    
    
    
    tab = table(actual, updated_pred)
    
    #for class==1:
    cm <- confusionMatrix(tab, mode = "everything",positive = "1")
    acc <- round(cm$overall[1],2)
    curr_byclass_perf <- as.data.frame(t(round(cm$byClass,2)))
    Final_perforamnce_df[r,"AUC"] <- auc_score
    Final_perforamnce_df[r,"ACC"] <- acc
    Final_perforamnce_df[r,c("C1_Prec","C1_Recall","C1_F1")] <- curr_byclass_perf[,c("Precision", "Recall", "F1")]
    
    #for class==0:
    cm <- confusionMatrix(tab, mode = "everything",positive = "0")
    curr_byclass_perf <- as.data.frame(t(round(cm$byClass,2)))
    Final_perforamnce_df[r,c("C0_Prec","C0_Recall","C0_F1")] <- curr_byclass_perf[,c("Precision", "Recall", "F1")]
    
    
    
    #print(acc)
  }
  
  #Average Perf of 5 sampling
  Final_perforamnce_df[6,] <- round(colMeans(Final_perforamnce_df,na.rm = T),2)
  rownames(Final_perforamnce_df)[6] <- "Average"
  Final_perforamnce_df
  
  return(list(data_to_analysis,important_features_matrix,top_important_features,Final_perforamnce_df))
}

#####Main function for runing for different label and data
main_func <- function(data_to_anlaysis,label_col){
  #remove NA labels patients 
  na_idxes <- which(is.na(data_to_anlaysis[,label_col])==T) 
  
  if (length(na_idxes) >0){
    updated_data_to_anlaysis<- data_to_anlaysis[-na_idxes,]
    table(updated_data_to_anlaysis[,label_col]) #BL: 123:99, n=222
  }else{
    updated_data_to_anlaysis <- data_to_anlaysis
  }
  ###1. mRMR , run for different target n of features in mRMR
  n_features_traget<-seq(5,ncol(updated_data_to_anlaysis)-1, by= 5 )
  all_perfs <- as.data.frame(matrix(NA, nrow = length(n_features_traget), ncol = 8))
  colnames(all_perfs) <- c("AUC","ACC","C0_Prec","C0_Recall","C0_F1","C1_Prec","C1_Recall","C1_F1")
  rownames(all_perfs) <- paste0("n_fs_",n_features_traget)
  selected_features_list <- list(NA)
  for (i in 1:length(n_features_traget)){
    mrmr_res <- mrmr_func(updated_data_to_anlaysis,label_col,n_features_traget[i])
    mi1 <- mrmr_res[[1]]
    mi2 <- mrmr_res[[2]]
    mrmr_selected_features <- mrmr_res[[3]]
    causality_df <- mrmr_res[[4]]
    
    #if label col is found in sleect features meaning some other feature has never been added to the list
    #so just drop the label col
    label_col_idxes <- which(mrmr_selected_features == label_col)
    if(length(label_col_idxes) > 0){
      mrmr_selected_features <- mrmr_selected_features[-which(mrmr_selected_features == label_col)]
    }else{
      mrmr_selected_features <- mrmr_selected_features
    }
    
    selected_features_list[[i]] <- mrmr_selected_features
    ###2. Classfication
    res <- Train_andValidaiton_func(updated_data_to_anlaysis,c(mrmr_selected_features,label_col),label_col)
    data_analysied <- res[[1]]
    label_table <- table(data_analysied[,"Label"])
    #print(label_table)
    important_features_matrix <- res[[2]]
    top_features_names <- res[[3]]
    bl_perforamnces<- res[[4]]
    bl_perforamnces
    
    
    all_perfs[i, ] <- bl_perforamnces[which(rownames(bl_perforamnces) == "Average"),]
  }
  
  #return best idxes
  max_auc_idxes <- which(all_perfs[,"AUC"] == max(all_perfs[,"AUC"]))
  updated_all_perfs <- all_perfs[max_auc_idxes,]
  max_acc_idx_inmaxauc <- which(updated_all_perfs[,"ACC"] == max(updated_all_perfs[,"ACC"]))
  best_idx <- max_auc_idxes[max_acc_idx_inmaxauc]
  final_selected_features <- selected_features_list[[best_idx[1]]]
  
  #re-run classifiaction for the best number of mrmr features
  res <- Train_andValidaiton_func(updated_data_to_anlaysis,c(final_selected_features,label_col),label_col)
  data_analysied <- res[[1]]
  label_table <- table(data_analysied[,"Label"])
  print(label_table)
  important_features_matrix <- res[[2]]
  top_features_names <- res[[3]]
  final_perforamnces<- res[[4]]
  final_perforamnces
  
  final_features <- colnames(data_analysied) #Final anlaysis features and labels
  return(list(final_features, label_table, all_perfs,final_perforamnces,important_features_matrix,top_features_names))
}


experiment_func <- function(label_col_seq,initial_features_list,data_input,plot_outdir){
  final_perforamnces_list <- list()
  important_features_matrix_list <- list()
  top_features_names_list <- list()
  n_of_inital_features <- NA #before mrmr
  n_of_final_features <- NA
  label_table <- list()
  for (i in 1 : length(initial_features_list)){
    print(i)

    label_col <- label_col_seq[i]
    initial_features <- c(initial_features_list[[i]],label_col)
    data_to_anlaysis <- data_input[,initial_features]
    
    final_res <- main_func(data_to_anlaysis,label_col)
    n_of_inital_features[i] <- ncol(data_to_anlaysis) -1
    
    final_features <- final_res[[1]]
    n_of_final_features[i] <- length(final_features) -1
    label_table[[i]] <- final_res[[2]]
    #print(label_table[[i]])
    all_mRMRperfs <- final_res[[3]]
    final_perforamnces_list[[i]] <- final_res[[4]]
    important_features_matrix_list[[i]] <- final_res[[5]]
    top_features_names_list[[i]] <- final_res[[6]]
    
  }
  
  #plot important features
  for (i in 1:length(initial_features_list)){
    curr_important_features_matrix <- important_features_matrix_list[[i]]
    png(paste0(plot_outdir,"Exp_",i,"all_fs_",".png"),height = 3000,width = 2000,res = 100)
    p<-Plot_FeatureImportance_func(curr_important_features_matrix,"",0,0)
    print(p)
    dev.off()
    
    png(paste0(plot_outdir,"Exp_",i,"top_fs_",".png"),height = 800,width = 800,res = 100)
    if (length(curr_important_features_matrix$Feature) > 10){
      top_important_features <- curr_important_features_matrix$Feature[1:10] #top10
      p2<-Plot_FeatureImportance_func(curr_important_features_matrix,"",1,10)
      print(p2)
      dev.off()
    }else{
      top_important_features <- curr_important_features_matrix$Feature[1:length(curr_important_features_matrix$Feature)] #all
      p2<-Plot_FeatureImportance_func(curr_important_features_matrix,"",1,length(curr_important_features_matrix$Feature))
      print(p2)
      dev.off()
    }

  }
  
  Overall_performance <- do.call(rbind,final_perforamnces_list)
  avg_score_indexes <- seq(6,6*length(initial_features_list),6)
  updated_Overall_performance <- Overall_performance[avg_score_indexes,] #return the average perf(from Sampling) 
  
  return(list(updated_Overall_performance,n_of_inital_features,n_of_final_features,label_table,top_features_names_list))
}

remove_features_50perc_missing_func <-function(curr_data_input,feature_set_list){
  n_values<- NA
  for (j in 1:ncol(curr_data_input)){
    n_values[j] <- length(which(is.na(curr_data_input[,j])==T))
  }
  
  #find features has more than 0.5p values missing
  features_missing_50perc <- colnames(curr_data_input)[which(n_values/nrow(curr_data_input)>=0.5)]
  
  #remove and update feature list
  updated_features_list <- list(NA)
  for (f in 1:length(feature_set_list)){
    idx_toremove <- which(feature_set_list[[f]] %in% features_missing_50perc)
    if (length(idx_toremove) > 0){
      updated_features_list[[f]] <- feature_set_list[[f]][-idx_toremove]
    }else{
      updated_features_list[[f]] <- feature_set_list[[f]]
    }
  }
  return(updated_features_list)
}



find_distribution_CASCORE_diff01_and_diff12 <- function(analysis_grp_df){
  #analysis_grp_df <- h_grp_diff_df
  
  all_cascore_diff <-  c(analysis_grp_df[,"CASCOREA_Diff0_1"],analysis_grp_df[,"CASCOREA_Diff1_2"])
  p97_5 <- quantile(all_cascore_diff,na.rm = T,c(0.975))
  p2_5 <- quantile(all_cascore_diff,na.rm = T,c(0.025))
  
  outlier_indexes <-which(all_cascore_diff > p97_5 | all_cascore_diff < p2_5)
  all_cascore_diff_outlier_exlucede <- all_cascore_diff[-outlier_indexes]
  
  ## found threhold for increase, decrease and little change
  all_postive_diff<- all_cascore_diff_outlier_exlucede[which(all_cascore_diff_outlier_exlucede>0)]
  all_negtive_diff<-all_cascore_diff_outlier_exlucede[which(all_cascore_diff_outlier_exlucede<0)]
  sd_pos_diff <- sd(all_postive_diff,na.rm = T)
  sd_neg_diff <- sd(all_negtive_diff,na.rm = T)
  threshold_high <- round(0 + sd_pos_diff,3)
  threshold_lower <- round(0 - sd_neg_diff,3)
  return(list(all_cascore_diff,threshold_lower,threshold_high))
}


#updated DEC07, 
#1. bl-yr1, bl-yr2 , 
#2. do not consider negatives when computing distribution 
find_distribution_SqrtVol_diff01_and_diff02 <- function(analysis_grp_df){
  all_diff <-  c(analysis_grp_df[,"SqRtVOLUMEA_Diff0_1"],analysis_grp_df[,"SqRtVOLUMEA_Diff0_2"])
  p97_5 <- quantile(all_diff,na.rm = T,c(0.975))
  p2_5 <- quantile(all_diff,na.rm = T,c(0.025))
  
  outlier_indexes <-which(all_diff > p97_5 | all_diff < p2_5)
  all_diff_outlier_exlucede <- all_diff[-outlier_indexes]
  
  ## found threhold for increase, decrease and little change
  #increase threshold = 0 + SD (SD of postive changes)
  #decrease threhold = 0 
  all_postive_diff<- all_diff_outlier_exlucede[which(all_diff_outlier_exlucede>0)]
  sd_pos_diff <- sd(all_postive_diff,na.rm = T)
  threshold_high <- round(0 + sd_pos_diff,3)
  threshold_lower <- 0
  return(list(all_diff,threshold_lower,threshold_high))
}
