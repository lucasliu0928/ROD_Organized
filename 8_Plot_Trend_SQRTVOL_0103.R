source("Ultility.R")
source("Ultils_LoadData_Funcs.R")
library(reshape2)
library(ggplot2)
library(xgboost)
library(openxlsx)


###Input and output directory
in_dir <- "../Intermediate_Data/0112_21/"
f_dir1 <- "../Intermediate_Data/0112_21/out1/"
f_dir2 <- "../Intermediate_Data/0112_21/out2/"
f_dir3 <- "../Intermediate_Data/0112_21/out3/"

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

#3remove outcome realted features SqRtVOLUMEA,CASCOREA(BL,yr1,yr2,Diff0_2,Diff1_2,Diff0_1)
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
####Add label to original data
##########################################################################
analysiis_label_df <- Label_df
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_nonH")] <- 0 
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_H")] <- 1
table(analysiis_label_df$Starting_SqRtVOLUME_LABEL) #49 58

data_input <- final_df
data_input$ID <- rownames(data_input)
analysis_IDs <- rownames(analysiis_label_df) #98

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#Remove the co2 value at yr1 == 106 and -1
ptsID_to_remove <- rownames(data_input)[which(data_input[,"CO21_yr1"] %in% c(106,-1))]
data_input <- data_input[-which(rownames(data_input) %in% ptsID_to_remove),]
analysiis_label_df <- analysiis_label_df[-which(rownames(analysiis_label_df) %in% ptsID_to_remove),]

#reoder to match and add label to data input
data_input$Starting_CASCOREA_LABEL <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),"Starting_CASCOREA_LABEL"]
data_input$Changing_Label <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),"Changing_Label"]
table(data_input$Changing_Label)

#new group for plot:
###########################plot
plot_data <- data_input[,c("ID","SqRtVOLUMEA_bl","SqRtVOLUME1A_yr1","SqRtVOLUME2A_yr2","Changing_Label")]
plot_data$Changing_Label <- as.factor(plot_data$Changing_Label)

plot_data1 <- plot_data[,c("ID","SqRtVOLUMEA_bl","SqRtVOLUME1A_yr1","SqRtVOLUME2A_yr2","Changing_Label")]


max_glable <- round(max(plot_data1[,2:4],na.rm = T),2) #use globalscale
min_glable <- round(min(plot_data1[,2:4],na.rm = T),2)
legendpos_flag <- "bottom"
y_breaks_gap <- 10
plot_group_names <- ""

grp_plot <- Plot_VaraibleAt_3TimePoint_func2(plot_data1, "", "SqRtVOLUME", "Changing_Label",
                                             min_glable, max_glable, y_breaks_gap,
                                             plot_group_names, legendpos_flag,12)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,3,"darkblue",-1)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,9.2,"darkorange",-1)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,19.1,"darkred",-1)
print(grp_plot)

##########################################################################################################
#Prediction task 1 violin plot
##########################################################################################################
data_input <- updated_comb_df

analysiis_label_df <- Label_df
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_nonH")] <- 0 
analysiis_label_df$Starting_SqRtVOLUME_LABEL[which(analysiis_label_df$Starting_SqRtVOLUME_LABEL == "start_H")] <- 1
table(analysiis_label_df$Starting_SqRtVOLUME_LABEL) #49 58

analysis_IDs <- rownames(analysiis_label_df) #107

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#Remove the co2 value at yr1 == 106 and -1
ptsID_to_remove <- rownames(data_input)[which(data_input[,"CO21_yr1"] %in% c(106,-1))]
data_input <- data_input[-which(rownames(data_input) %in% ptsID_to_remove),]
analysiis_label_df <- analysiis_label_df[-which(rownames(analysiis_label_df) %in% ptsID_to_remove),]

#reoder to match and add label to data input
data_input$Starting_SqRtVOLUME_LABEL <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),"Starting_SqRtVOLUME_LABEL"]

# Basic violin plot for critical features
critical_features_df <- read.csv(paste0(f_dir1, "/Exp1_important_fs.csv"),stringsAsFactors = F)
critical_features <- critical_features_df$x

plot_outdir <- paste0(f_dir1,"violin_plot/")
dir.create(file.path(plot_outdir))
for (f in 1:length(critical_features)){
  plot_x <- "Starting_SqRtVOLUME_LABEL"
  plot_y <-  critical_features[f]
  
  voilin_plot_data <- data_input[which(is.na(data_input$Starting_SqRtVOLUME_LABEL)==F),]
  grp0_idex <- which(voilin_plot_data$Starting_SqRtVOLUME_LABEL==0)
  grp1_idex <- which(voilin_plot_data$Starting_SqRtVOLUME_LABEL==1)
  
  voilin_plot_data$Starting_SqRtVOLUME_LABEL[grp0_idex] <- "(Grp 0) start non-high \n"
  voilin_plot_data$Starting_SqRtVOLUME_LABEL[grp1_idex] <- "(Grp 1) start high \n"
  
  voilin_plot_data$Starting_SqRtVOLUME_LABEL <-as.factor(voilin_plot_data$Starting_SqRtVOLUME_LABEL)
  
  feature_unique_values_n <- length(levels(factor(voilin_plot_data[,plot_y])))
  
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  if (feature_unique_values_n > 2){
      p <- ggplot(voilin_plot_data, aes_string(x=plot_x, y=plot_y)) + 
        geom_violin() +
        geom_boxplot(width=0.1) + 
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20,face="bold")) +
        labs(y = plot_y,x = "")
  }else if (feature_unique_values_n == 2){ #if binary, use histogram
    #Factorize the var, 
    plot_data <- voilin_plot_data
    plot_data[,plot_y] <- factor(plot_data[,plot_y])
    na_idxes <- which(is.na(plot_data[,plot_y])==T)
    if (length(na_idxes) > 0){ #and remove NA for plot
     plot_data <- plot_data[-na_idxes,]
    }

    p <- ggplot(plot_data, aes_string(x=plot_y, fill = plot_x)) +
      geom_histogram(alpha = 0.8,position="dodge",stat = "count") +
      theme(legend.position='bottom',legend.title = element_blank()) +
      scale_fill_manual(values=c("darkgreen", "darkred"))+
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20),legend.text = element_text(size = 20))
  }
  print(p)
  dev.off()
}


##########################################################################
#normality test for each group
##########################################################################
#normality test function
normality_test_func <- function(data_input,group_name,feature_name){
  # Shapiro-Wilk normality test for grp 0 
  f0_idexes <- which(data_input[,group_name] == 0)
 
  if (length(unique(data_input[f0_idexes,feature_name])) != 1){   #check if all values are identical
    res0 <- shapiro.test(data_input[f0_idexes,feature_name])
    p0 <- res0$p.value
  }else{ # if indeitical, then p = -INF < 0.05 -> not norm
    p0 <- -Inf
  }
  
  # Shapiro-Wilk normality test for grp 1
  f1_idexes <- which(data_input[,group_name] == 1)
  if (length(unique(data_input[f1_idexes,feature_name])) != 1){
      res1 <- shapiro.test(data_input[f1_idexes,feature_name])
      p1 <- res1$p.value
  }else {
    p1 <- -Inf
  }
  
  all_pvalues <- cbind.data.frame(p0,p1)
  return(all_pvalues)
}

#if p>0.05,it is norm
#if p<0.05, it is not norm
outcome_label <- "Starting_SqRtVOLUME_LABEL"
norm_results <- list()
for (f in 1:length(critical_features)){
  curr_f <- critical_features[f]
  norm_results[[f]] <- normality_test_func(data_input,outcome_label,curr_f)
}
all_norm_pvalues <- do.call(rbind,norm_results)
#not normed distributed features
NOT_normed_fs <- critical_features[which(all_norm_pvalues$p0<0.05 | all_norm_pvalues$p1<0.05)]
normed_fs <- critical_features[which(all_norm_pvalues$p0>0.05 & all_norm_pvalues$p1>0.05)]

##########################################################################
###Grp diff test
##########################################################################
grp_diff_test <- function(data_input, feature_to_test,outcome_name,NOT_normed_fs,normed_fs){
  f0_idxes <- which(data_input[,outcome_name]==0)
  frac0_f_values <- data_input[f0_idxes,feature_to_test]
  f1_idxes <- which(data_input[,outcome_name]==1)
  frac1_f_values <- data_input[f1_idxes,feature_to_test]
  
  if (feature_to_test %in% NOT_normed_fs){
    #if not norm:Mann-Whitney U test
    less_res <- wilcox.test(frac0_f_values,frac1_f_values,alternative = "less",   exact = FALSE) #less: f0 < f1
    greater_res <- wilcox.test(frac0_f_values,frac1_f_values,alternative = "greater",   exact = FALSE) #greater: f0 > f1
  }else if (feature_to_test %in% normed_fs){
    #if norm:
    less_res <- t.test(frac0_f_values,frac1_f_values, alternative = "less", var.equal = FALSE)
    greater_res <- t.test(frac0_f_values,frac1_f_values, alternative = "greater", var.equal = FALSE)
    
  }
  
  p_f0_less_f1 <- less_res$p.value
  p_f0_greater_f1<- greater_res$p.value
  p_all <- cbind.data.frame(p_f0_greater_f1,p_f0_less_f1)
  rownames(p_all) <- feature_to_test
  return(p_all)
}


grp_diff_results <- list()
for (f in 1:length(critical_features)){
  curr_f <- critical_features[f]
  grp_diff_results[[f]] <- grp_diff_test(data_input,curr_f,outcome_label,NOT_normed_fs,normed_fs)
}
all_grp_dff_pvalues <- do.call(rbind,grp_diff_results)

signif_idxes <- which(all_grp_dff_pvalues$p_f0_less_f1 <=0.05 | all_grp_dff_pvalues$p_f0_greater_f1 <=0.05)
all_grp_dff_pvalues[signif_idxes,]
write.csv(all_grp_dff_pvalues,paste0(plot_outdir,"grp_diff_test.csv"))




#####################################################
#Prediction task 2 violin plot
#####################################################
data_input <- updated_comb_df

analysiis_label_df <- Label_df
analysiis_label_df$New_Trend_Label <- NA
analysiis_label_df$New_Trend_Label[which(analysiis_label_df$Changing_Label == "nonH_nonH")] <- 0 
analysiis_label_df$New_Trend_Label[which(analysiis_label_df$Changing_Label == "H_H")] <- 1
table(analysiis_label_df$New_Trend_Label) #34 58
analysiis_label_df <- analysiis_label_df[-which(is.na(analysiis_label_df$New_Trend_Label)==T),]

analysis_IDs <- rownames(analysiis_label_df) #92

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#Remove the co2 value at yr1 == 106 and -1
ptsID_to_remove <- rownames(data_input)[which(data_input[,"CO21_yr1"] %in% c(106,-1))]
data_input <- data_input[-which(rownames(data_input) %in% ptsID_to_remove),]
analysiis_label_df <- analysiis_label_df[-which(rownames(analysiis_label_df) %in% ptsID_to_remove),]


#reoder to match and add label to data input
data_input$New_Trend_Label <- analysiis_label_df[match(rownames(analysiis_label_df),rownames(data_input)),"New_Trend_Label"]

# Basic violin plot for critical features
critical_features_df <- read.csv(paste0(f_dir2, "/Exp1_important_fs.csv"),stringsAsFactors = F)
critical_features <- critical_features_df$x

plot_outdir <- paste0(f_dir2,"violin_plot/")
dir.create(file.path(plot_outdir))
for (f in 1:length(critical_features)){
  plot_x <- "New_Trend_Label"
  plot_y <-  critical_features[f]
  
  voilin_plot_data <- data_input[which(is.na(data_input$New_Trend_Label)==F),]
  grp0_idex <- which(voilin_plot_data$New_Trend_Label==0)
  grp1_idex <- which(voilin_plot_data$New_Trend_Label==1)
  
  voilin_plot_data$New_Trend_Label[grp0_idex] <- "(Grp 0) start non-high, \n stay non-high"
  voilin_plot_data$New_Trend_Label[grp1_idex] <- "(Grp 1) start high, \n stay high"
  
  voilin_plot_data$New_Trend_Label <-as.factor(voilin_plot_data$New_Trend_Label)
  
  feature_unique_values_n <- length(levels(factor(voilin_plot_data[,plot_y])))
  
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  if (feature_unique_values_n > 2){
    p <- ggplot(voilin_plot_data, aes_string(x=plot_x, y=plot_y)) + 
      geom_violin() +
      geom_boxplot(width=0.1) + 
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20,face="bold")) +
      labs(y = plot_y,x = "")
    print(p)
    dev.off()
  }else if (feature_unique_values_n == 2){ #if binary, use histogram
    #Factorize the var, and remove NA for plot
    plot_data <- voilin_plot_data
    plot_data[,plot_y] <- factor(plot_data[,plot_y])
    na_idxes <- which(is.na(plot_data[,plot_y])==T)
    if (length(na_idxes) > 0){ #and remove NA for plot
      plot_data <- plot_data[-na_idxes,]
    }
    
    p <- ggplot(plot_data, aes_string(x=plot_y, fill = plot_x)) +
      geom_histogram(alpha = 0.8,position="dodge",stat = "count") +
      theme(legend.position='bottom',legend.title = element_blank()) +
      scale_fill_manual(values=c("darkgreen", "darkred"))+
      theme(axis.text=element_text(size=20),
            axis.title=element_text(size=20),legend.text = element_text(size = 20))
  }
  print(p)
  dev.off()
}

##########################################################################
#normality test for each group
##########################################################################
#if p>0.05,it is norm
#if p<0.05, it is not norm
outcome_label <- "New_Trend_Label"
norm_results <- list()
for (f in 1:length(critical_features)){
  curr_f <- critical_features[f]
  norm_results[[f]] <- normality_test_func(data_input,outcome_label,curr_f)
}
all_norm_pvalues <- do.call(rbind,norm_results)
#not normed distributed features
NOT_normed_fs <- critical_features[which(all_norm_pvalues$p0<0.05 | all_norm_pvalues$p1<0.05)]
normed_fs <- critical_features[which(all_norm_pvalues$p0>0.05 & all_norm_pvalues$p1>0.05)]

##########################################################################
###Grp diff test
##########################################################################
grp_diff_results <- list()
for (f in 1:length(critical_features)){
  curr_f <- critical_features[f]
  grp_diff_results[[f]] <- grp_diff_test(data_input,curr_f,outcome_label,NOT_normed_fs,normed_fs)
}
all_grp_dff_pvalues <- do.call(rbind,grp_diff_results)

signif_idxes <- which(all_grp_dff_pvalues$p_f0_less_f1 <=0.05 | all_grp_dff_pvalues$p_f0_greater_f1 <=0.05)
all_grp_dff_pvalues[signif_idxes,]

write.csv(all_grp_dff_pvalues,paste0(plot_outdir,"grp_diff_test.csv"))


