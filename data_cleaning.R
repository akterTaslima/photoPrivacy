library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
library(psych)
library(likert)
library(plyr)
library(tidyr)
library(tidyverse)
library(dplyr)

# --------------------------------       Functions     -----------------------------------------
col_extract <- function(quesn, cat) {
  array_name = c()
  
  if( cat == 'q'){
    for (i in 1:18){
      coln = paste("X",toString(i),"_", quesn, sep = "" )
      array_name <- c (array_name, coln)
    }
    
  }
  if( cat == 'v'){
    for (i in 1:18){
      coln = paste("X",toString(i),"_", quesn, sep = "" )
      array_name <- c (array_name, coln)
    }
  }
  
  if( cat == 'c'){
    for (i in 1:18){
      coln = paste("cond_img_", toString(i), sep = "" )
      array_name <- c (array_name, coln)
    }
    
  }
  if( cat == 'o'){
    for (i in 1:3){
      coln = paste("X",toString(i),"_", quesn, sep = "" )
      array_name <- c (array_name, coln)
    }
    
  }
  
  return(array_name)
}
# ---------------------------------------------------------------------------------------
split_cond <- function(array, cat) {
  sticker_type = c()
  sticker_val = c()
  img_val = c()
  
  for (i in array){
    split_vals = strsplit(i, "-")
    split_vals = unlist(split_vals)
    img_val <- c (img_val, split_vals[1])
    sticker_type <- c (sticker_type, split_vals[2])
    sticker_val <- c (sticker_val, split_vals[3])
    
  }
  array = cbind(sticker_type, sticker_val, img_val)
  return(array)
}

# ---------------------------------------------------------------------------------------
data_cleaning <- function(ques_array, cond_array, category, scenario) {
  nrow_matrix=nrow(ques_array)
  ID <- seq.int(nrow(ques_array))
  img= rep("i",nrow_matrix)
  sticker= rep("control",nrow_matrix)
  sticker_type= rep("control",nrow_matrix)
  condition= rep("c",nrow_matrix)
  row_bind = c()
  combine = c()
  experiment= rep(category,nrow_matrix)
  if (category == "sample"){
    for (i in 1:16){
      
      qid = paste("q",i, sep = "" )
      ques_n= rep(qid,nrow_matrix)
      combine = cbind (ID, ques_n, ques_array[,i], cond_array[,i], experiment, scenario)
      row_bind = rbind(row_bind, combine)
    }
    
    condition_split = split_cond(row_bind[,4], "c")
    
    data = cbind(row_bind, condition_split)  
  }
  if (category == "control"){
    
    
    for (i in 17:18){
      
      qid = paste("q",i, sep = "" )
      ques_n= rep(qid,nrow_matrix)
      combine = cbind (ID, ques_n, ques_array[,i], cond_array[,i], experiment, scenario)
      row_bind = rbind(row_bind, combine)
    }
    
   # condition_split = split_cond(row_bind[,4], "c")
    
    data = cbind(row_bind, sticker_type, sticker, row_bind[,4])   
  }
  
  
  return(data)
}


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

y=read.csv("pilot_new.csv",  sep=",", stringsAsFactors=FALSE) [-c(1)]

condition_extract = y[col_extract("", "c")]

scenario = y["scenario"]

Q6.2 = y[col_extract("Q6.2", "q")]
Q6.3 = y[col_extract("Q6.3", "q")]
Q6.4 = y[col_extract("Q6.4", "q")]

Q6.2_sample = data_cleaning (Q6.2, condition_extract, "sample", scenario)
colnames(Q6.2_sample) <- c( "ID","qid", "sufficiency", "condition", "experiment", "scenario" ,"sticker_type", "sticker_val", "img_val")

Q6.3_sample = data_cleaning (Q6.3, condition_extract, "sample", scenario)
colnames(Q6.3_sample) <- c( "ID","qid", "satisfaction", "condition", "experiment", "scenario", "sticker_type", "sticker_val", "img_val")

Q6.4_sample = data_cleaning (Q6.4, condition_extract, "sample", scenario)
colnames(Q6.4_sample) <- c( "ID","qid", "appealing", "condition", "experiment", "scenario", "sticker_type", "sticker_val", "img_val")

# -------------------------------------- Extract Control data

#Q6.2 = y[col_extract("Q6.2", "o")]
#Q6.3 = y[col_extract("Q6.3", "o")]
#Q6.4 = y[col_extract("Q6.4", "o")]

Q6.2_control = data_cleaning (Q6.2, condition_extract, "control", scenario)
colnames(Q6.2_control) <- c( "ID","qid", "sufficiency", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.3_control = data_cleaning (Q6.3, condition_extract, "control", scenario)
colnames(Q6.3_control) <- c( "ID","qid", "satisfaction", "condition", "experiment","scenario","sticker_type", "sticker_val", "img_val")

Q6.4_control = data_cleaning (Q6.4, condition_extract, "control", scenario)
colnames(Q6.4_control) <- c( "ID","qid", "appealing", "condition", "experiment","scenario","sticker_type", "sticker_val", "img_val")

sufficiency = rbind(Q6.2_sample , Q6.2_control)
satisfaction = rbind(Q6.3_sample , Q6.3_control)
appealing = rbind(Q6.4_sample , Q6.4_control)

# -------------------------------------- Extract valence data
Q6.1_1 = y[col_extract("Q6.1_1", "v")]
Q6.1_1_sample = data_cleaning (Q6.1_1, condition_extract, "sample", scenario)
colnames(Q6.1_1_sample) <- c( "ID","qid", "fear", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_2 = y[col_extract("Q6.1_2", "v")]
Q6.1_2_sample = data_cleaning (Q6.1_2, condition_extract, "sample", scenario)
colnames(Q6.1_2_sample) <- c( "ID","qid", "disgust", "condition", "experiment", "scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_3 = y[col_extract("Q6.1_3", "v")]
Q6.1_3_sample = data_cleaning (Q6.1_3, condition_extract, "sample", scenario)
colnames(Q6.1_3_sample) <- c( "ID","qid", "anger", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_4 = y[col_extract("Q6.1_4", "v")]
Q6.1_4_sample = data_cleaning (Q6.1_4, condition_extract, "sample", scenario)
colnames(Q6.1_4_sample) <- c( "ID","qid", "sadness", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_5 = y[col_extract("Q6.1_5", "v")]
Q6.1_5_sample = data_cleaning (Q6.1_5, condition_extract, "sample", scenario)
colnames(Q6.1_5_sample) <- c( "ID","qid", "happiness", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_6 = y[col_extract("Q6.1_6", "v")]
Q6.1_6_sample = data_cleaning (Q6.1_6, condition_extract, "sample", scenario)
colnames(Q6.1_6_sample) <- c( "ID","qid", "surprise", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_7 = y[col_extract("Q6.1_7", "v")]
Q6.1_7_sample = data_cleaning (Q6.1_7, condition_extract, "sample", scenario)
colnames(Q6.1_7_sample) <- c( "ID","qid", "amusement", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

Q6.1_8 = y[col_extract("Q6.1_8", "v")]
Q6.1_8_sample = data_cleaning (Q6.1_8, condition_extract, "sample", scenario)
colnames(Q6.1_8_sample) <- c( "ID","qid", "neutral", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

# -------------------------------------- Extract valence Control data
#Q6.1_1_control = y[col_extract("Q6.1_1", "o")]
Q6.1_1_control = data_cleaning (Q6.1_1, condition_extract, "control", scenario)
colnames(Q6.1_1_control) <- c( "ID","qid", "fear", "condition", "experiment", "scenario","sticker_type", "sticker_val", "img_val")

#Q6.1_2_sample = y[col_extract("Q6.1_2", "o")]
Q6.1_2_control = data_cleaning (Q6.1_2, condition_extract, "control", scenario)
colnames(Q6.1_2_control) <- c( "ID","qid", "disgust", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

#Q6.1_3_sample = y[col_extract("Q6.1_3", "o")]
Q6.1_3_control = data_cleaning (Q6.1_3, condition_extract, "control", scenario)
colnames(Q6.1_3_control) <- c( "ID","qid", "anger", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

#Q6.1_4 = y[col_extract("Q6.1_4", "o")]
Q6.1_4_control = data_cleaning (Q6.1_4, condition_extract, "control", scenario)
colnames(Q6.1_4_control) <- c( "ID","qid", "sadness", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

#Q6.1_5 = y[col_extract("Q6.1_5", "o")]
Q6.1_5_control = data_cleaning (Q6.1_5, condition_extract, "control", scenario)
colnames(Q6.1_5_control) <- c( "ID","qid", "happiness", "condition", "experiment", "scenario","sticker_type", "sticker_val", "img_val")

#Q6.1_6 = y[col_extract("Q6.1_6", "o")]
Q6.1_6_control = data_cleaning (Q6.1_6, condition_extract, "control", scenario)
colnames(Q6.1_6_control) <- c( "ID","qid", "surprise", "condition", "experiment", "scenario","sticker_type", "sticker_val", "img_val")

#Q6.1_7 = y[col_extract("Q6.1_7", "o")]
Q6.1_7_control = data_cleaning (Q6.1_7, condition_extract, "control", scenario)
colnames(Q6.1_7_control) <- c( "ID","qid", "amusement", "condition", "experiment","scenario", "sticker_type", "sticker_val", "img_val")

#Q6.1_8 = y[col_extract("Q6.1_8", "o")]
Q6.1_8_control = data_cleaning (Q6.1_8, condition_extract, "control", scenario)
colnames(Q6.1_8_control) <- c( "ID","qid", "neutral", "condition", "experiment", "scenario","sticker_type", "sticker_val", "img_val")

fear = rbind(Q6.1_1_sample , Q6.1_1_control)
disgust = rbind(Q6.1_2_sample , Q6.1_2_control)
anger = rbind(Q6.1_3_sample , Q6.1_3_control)
sadness = rbind(Q6.1_4_sample , Q6.1_4_control)
happiness = rbind(Q6.1_5_sample , Q6.1_5_control)
surprise = rbind(Q6.1_6_sample , Q6.1_6_control)
amusement = rbind(Q6.1_7_sample , Q6.1_7_control)
neutral = rbind(Q6.1_8_sample , Q6.1_8_control)

# -------------------------------------- Merged data
merged_data = cbind(sufficiency, satisfaction = satisfaction[,3], appealing = appealing[,3], fear = fear[,3], disgust = disgust[,3], 
                    anger = anger[,3], sadness = sadness[,3], happiness = happiness[,3], surprise = surprise[,3],
                    amusement = amusement[,3], neutral = neutral[,3])
write.csv(merged_data, "clean_data_gather.csv")

# -------------------------------------- Merged data emotion
merged_data_emotion = cbind(fear[,c(-3)], fear = fear[,3], disgust = disgust[,3], 
                    anger = anger[,3], sadness = sadness[,3], happiness = happiness[,3], surprise = surprise[,3],
                    amusement = amusement[,3], neutral = neutral[,3])
#colnames(merged_data_emotion) <- c( "ID","qid", "condition", "experiment", "sticker_type", "sticker_val", "img_val", "fear", 
#                                    "disgust", "anger", "sadness", "hapiness", "surprise", "amusement", "neutral")

write.csv(merged_data_emotion, "clean_emotion_data.csv")

valence_data = read.csv("clean_emotion_data.csv",  stringsAsFactors=FALSE) [-c(1)]
merged_data_emotion = gather(valence_data, "emotion", "valence", 9:16 )
write.csv(merged_data_emotion, "clean_emotion_data_gather.csv")
