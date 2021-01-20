source("Ultility.R")
source("Ultils_LoadData_Funcs.R")
library(reshape2)
library(ggplot2)
library(xgboost)
library(openxlsx)


###Input and output directory
in_dir <- "../Intermediate_Data/0117_21/"
f_dir1 <- "../Intermediate_Data/0117_21/out1/"
f_dir2 <- "../Intermediate_Data/0117_21/out2/"
f_dir3 <- "../Intermediate_Data/0117_21/out3/"

###################################################
###I. Load feature data
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

#get feaures names
Final_bl_features <- colnames(updated_comb_df)[which(grepl("bl",colnames(updated_comb_df))==T)]
Final_yr1_features <- colnames(updated_comb_df)[which(grepl("yr1",colnames(updated_comb_df))==T)]
Final_yr2_features <- colnames(updated_comb_df)[which(grepl("yr2",colnames(updated_comb_df))==T)]
Diff0_1_features <- colnames(updated_comb_df)[which(grepl("Diff0_1",colnames(updated_comb_df))==T)]
Diff1_2_features <- colnames(updated_comb_df)[which(grepl("Diff1_2",colnames(updated_comb_df))==T)]
Diff0_2_features <- colnames(updated_comb_df)[which(grepl("Diff0_2",colnames(updated_comb_df))==T)]


##########################################################################
####II. Load all catogrized labels
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
#III. Group plot for SqRtVOLUME at bl, yr1, and yr2
##########################################################################
data_input <- final_df #feature data
data_input$ID <- rownames(data_input) #add Id column for plot later

analysiis_label_df <- Label_df #label data
analysis_IDs <- rownames(analysiis_label_df) #IDs has label

#subset Ids has label
Id_idxes <- which(rownames(data_input) %in% analysis_IDs)
data_input <- data_input[Id_idxes,]

#reoder to match and add two labels to data input
data_input$Changing_Label <- Label_df[match(rownames(Label_df),rownames(data_input)),"Changing_Label"]
table(data_input$Changing_Label)

#Plot Changing_Label groups 
plot_data <- data_input[,c("ID","SqRtVOLUMEA_bl","SqRtVOLUME1A_yr1","SqRtVOLUME2A_yr2","Changing_Label")]
plot_data$Changing_Label <- as.factor(plot_data$Changing_Label)

max_glable <- round(max(plot_data[,2:4],na.rm = T),2) #use globalscale
min_glable <- round(min(plot_data[,2:4],na.rm = T),2)
legendpos_flag <- "bottom"
y_breaks_gap <- 10
plot_group_names <- ""

grp_plot <- Plot_VaraibleAt_3TimePoint_func2(plot_data, "", "SqRtVOLUME", "Changing_Label",
                                             min_glable, max_glable, y_breaks_gap,
                                             plot_group_names, legendpos_flag,12)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,3,"darkblue",-1)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,9.2,"darkorange",-1)
grp_plot <- add_horizline_toplot_function(grp_plot,0.5,19.1,"darkred",-1)
png(paste0(in_dir,"Group_plot.png"))
print(grp_plot)
dev.off()

##########################################################################################################
#Prediction task 1 violin plot
##########################################################################################################
#Add corresponding label to the data 
label_col_name <- "Starting_SqRtVOLUME_LABEL"
grp0_name <- "start_nonH"
grp1_name <- "start_H"
data_input <- experiment_setup_func(updated_comb_df, Label_df, label_col_name, grp0_name,grp1_name)
table(data_input$Starting_SqRtVOLUME_LABEL)

# Basic violin plot for critical features
critical_features_df <- read.csv(paste0(f_dir1, "/FeatureSet1_important_fs.csv"),stringsAsFactors = F)
critical_features <- critical_features_df$x

plot_outdir <- paste0(f_dir1,"violin_plot/")
dir.create(file.path(plot_outdir))
grp0_plotname <- "(Grp 0) start non-high \n"
grp1_plotname <- "(Grp 1) start high \n"
for (f in 1:length(critical_features)){
  plot_x <-  label_col_name
  plot_y <-  critical_features[f]
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  p <- violin_orBar_individual_feature_func(data_input, plot_x,plot_y,grp0_plotname,grp1_plotname)
  print(p)
  dev.off()
}


##########################################################################
#normality test for each group
##########################################################################
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
grp_diff_results <- list()
for (f in 1:length(critical_features)){
  curr_f <- critical_features[f]
  grp_diff_results[[f]] <- grp_diff_test(data_input,curr_f,outcome_label,NOT_normed_fs,normed_fs)
}
all_grp_dff_pvalues <- do.call(rbind,grp_diff_results)

write.csv(all_grp_dff_pvalues,paste0(plot_outdir,"grp_diff_test.csv"))




#####################################################
#Prediction task 2 violin plot
#####################################################
#first remvoe nonH_H grp
pts_toremove <- which(Label_df[,"Changing_Label"] == "nonH_H") 
updated_label_df <- Label_df[-pts_toremove,]

#Add corresponding label to the data 
label_col_name <- "Changing_Label"
grp0_name <- "nonH_nonH"
grp1_name <- "H_H"
data_input <- experiment_setup_func(updated_comb_df, updated_label_df, label_col_name, grp0_name,grp1_name)
table(data_input$Changing_Label)

# Basic violin plot for critical features
critical_features_df <- read.csv(paste0(f_dir2, "/FeatureSet1_important_fs.csv"),stringsAsFactors = F)
critical_features <- critical_features_df$x

plot_outdir <- paste0(f_dir2,"violin_plot/")
dir.create(file.path(plot_outdir))
grp0_plotname <- "(Grp 0) start non-high, \n stay non-high"
grp1_plotname <- "(Grp 1) start high, \n stay high"
for (f in 1:length(critical_features)){
  plot_x <-  label_col_name
  plot_y <-  critical_features[f]
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  p <- violin_orBar_individual_feature_func(data_input, plot_x,plot_y,grp0_plotname,grp1_plotname)
  print(p)
  dev.off()
}



##########################################################################
#normality test for each group
##########################################################################
#if p>0.05,it is norm
#if p<0.05, it is not norm
outcome_label <- "Changing_Label"
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

write.csv(all_grp_dff_pvalues,paste0(plot_outdir,"grp_diff_test.csv"))


#####################################################
#Prediction task 3 violin plot
#####################################################
#first remvoe H_H grp
pts_toremove <- which(Label_df[,"Changing_Label"] == "H_H") 
updated_label_df <- Label_df[-pts_toremove,]

#Add corresponding label to the data 
label_col_name <- "Changing_Label"
grp0_name <- "nonH_nonH"
grp1_name <- "nonH_H"
data_input <- experiment_setup_func(updated_comb_df, updated_label_df, label_col_name, grp0_name,grp1_name)
table(data_input$Changing_Label)

# Basic violin plot for critical features
critical_features_df <- read.csv(paste0(f_dir3, "/FeatureSet1_important_fs.csv"),stringsAsFactors = F)
critical_features <- critical_features_df$x

plot_outdir <- paste0(f_dir3,"violin_plot/")
dir.create(file.path(plot_outdir))
grp0_plotname <- "(Grp 0) start non-high, \n stay non-high"
grp1_plotname <- "(Grp 1) start non-high, \n get higher"
for (f in 1:length(critical_features)){
  plot_x <-  label_col_name
  plot_y <-  critical_features[f]
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  p <- violin_orBar_individual_feature_func(data_input, plot_x,plot_y,grp0_plotname,grp1_plotname)
  print(p)
  dev.off()
}



##########################################################################
#normality test for each group
##########################################################################
#if p>0.05,it is norm
#if p<0.05, it is not norm
outcome_label <- "Changing_Label"
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
write.csv(all_grp_dff_pvalues,paste0(plot_outdir,"grp_diff_test.csv"))

#####################################################
# plot For CAP_CIP2_RATIO at bl , yr1 and yr2
#####################################################
#1. For 3 groups of Changing_Label
#Add corresponding label to the data 
label_col_name <- "Changing_Label"
grp0_name <- "nonH_nonH"
grp1_name <- "nonH_H"
grp2_name <- "H_H"
data_input <- updated_comb_df
data_input[,label_col_name] <- Label_df[match(rownames(Label_df),rownames(data_input)),label_col_name]
table(data_input[,label_col_name])

plot_outdir <- in_dir
dir.create(file.path(plot_outdir))
feature_toplot <- c("CAP_CIP_RATIO_bl","CAP_CIP1_RATIO_yr1","CAP_CIP2_RATIO_yr2")
for (f in 1:length(feature_toplot)){
  plot_x <-  label_col_name
  plot_y <-  feature_toplot[f]
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  p <- violin_orBar_individual_feature3grps_func(data_input, plot_x,plot_y)
  print(p)
  dev.off()
}


#2. For 2 groups of Starting_SqRtVOLUME_LABEL
#Add corresponding label to the data 
label_col_name <- "Starting_SqRtVOLUME_LABEL"
grp0_name <- "start_nonH"
grp1_name <- "start_H"
data_input <- experiment_setup_func(updated_comb_df, Label_df, label_col_name, grp0_name,grp1_name)
table(data_input$Starting_SqRtVOLUME_LABEL)

plot_outdir <- in_dir
dir.create(file.path(plot_outdir))
grp0_plotname <- "(Grp 0) start non-high \n"
grp1_plotname <- "(Grp 1) start high \n"


feature_toplot <- c("CAP_CIP_RATIO_bl","CAP_CIP1_RATIO_yr1","CAP_CIP2_RATIO_yr2")
for (f in 1:length(feature_toplot)){
  plot_x <-  label_col_name
  plot_y <-  feature_toplot[f]
  png(paste0(plot_outdir,f,plot_y,"VS",plot_x,".png"))
  p <- violin_orBar_individual_feature_func(data_input, plot_x,plot_y,grp0_plotname,grp1_plotname)
  print(p)
  dev.off()
}
