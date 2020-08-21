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

data_sticker = read.csv("clean_data_gather.csv",  stringsAsFactors=FALSE) [-c(1)]
data_sticker$img_val[data_sticker$img_val == "happy"] <- "ihappy"
data_sticker$img_val[data_sticker$img_val == "sad"] <- "isad"
data_sticker$sticker_val[data_sticker$sticker_val == "none"] <- "blur/opaque"

#isad_ssad_data=data_sticker[data_sticker$img_val =="isad" & data_sticker$sticker_val =="ssad",]
#ihappy_shappy_data=data_sticker[data_sticker$img_val =="ihappy" & data_sticker$sticker_val =="shappy",]

#isad_none_data=data_sticker[data_sticker$img_val =="isad" & data_sticker$sticker_val =="none",]


satisfaction_sad = mean(isad_data$satisfaction)
sufficiency_sad = mean(isad_data$sufficiency)
appealing_sad = mean(isad_data$appealing)

satisfaction_sad = mean(isad_none_data$satisfaction)
sufficiency_sad = mean(isad_none_data$sufficiency)
appealing_sad = mean(isad_none_data$appealing)

# ------------------------------ Foreground vs background -----------------
movie = data_sticker[data_sticker$scenario =="movie",]
movie$img_val[movie$img_val == "ihappy"] <- "bystander"
movie$img_val[movie$img_val == "isad"] <- "subject"

pdf(file="satisfaction_img_sticker_val.pdf")
ggline(movie, x = "img_val", y = "satisfaction", color = "sticker_val", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()
pdf(file="satisfaction_img_sticker_type.pdf")
ggline(movie, x = "sticker_type", y = "satisfaction", color = "img_val", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()

pdf(file="appealing_img_sticker_val.pdf")
ggline(movie, x = "img_val", y = "appealing", color = "sticker_val", 
       add = c("mean_ci"), ylab="Appealing", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()

pdf(file="appealing_img_sticker_type.pdf")
ggline(movie, x = "sticker_type", y = "appealing", color = "img_val", 
       add = c("mean_ci"), ylab="Appealing", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()

pdf(file="sufficiency_img_sticker_val.pdf")
ggline(movie, x = "img_val", y = "sufficiency", color = "sticker_val", 
       add = c("mean_ci"), ylab="Sufficiency", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()
pdf(file="sufficiency_img_sticker_type.pdf")
ggline(movie, x = "sticker_type", y = "sufficiency", color = "img_val", 
       add = c("mean_ci"), ylab="Sufficiency", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
dev.off()
# ------------------------------ Satisfaction -----------------
ggline(data_sticker, x = "sticker_type", y = "satisfaction", color = "img_val", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


ggline(data_sticker, x = "img_val", y = "satisfaction", color = "sticker_val", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "img_val", y = "satisfaction", color = "experiment", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "sticker_val", y = "satisfaction", color = "experiment", 
       add = c("mean_ci"), ylab="Satisfaction", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ------------------------------ Sufficiency -----------------
ggline(data_sticker, x = "sticker_type", y = "sufficiency", color = "sticker_val", 
       add = c("mean_ci"), ylab="sufficiency", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "sticker_type", y = "sufficiency", color = "img_val", 
       add = c("mean_ci"), ylab="sufficiency", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "img_val", y = "sufficiency", color = "sticker_val", 
       add = c("mean_ci"), ylab="sufficiency", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "img_val", y = "sufficiency", color = "experiment", 
       add = c("mean_ci"), ylab="sufficiency", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "sticker_val", y = "sufficiency", color = "experiment", 
       add = c("mean_ci"), ylab="sufficiency", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
# ------------------------------ Appealing  -----------------
ggline(data_sticker, x = "sticker_type", y = "appealing", color = "sticker_val", 
       add = c("mean_ci"), ylab="appealing", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "sticker_type", y = "appealing", color = "img_val", 
       add = c("mean_ci"), ylab="appealing", xlab="sticker_type"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
ggline(data_sticker, x = "img_val", y = "appealing", color = "sticker_val", 
       add = c("mean_ci"), ylab="appealing", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "img_val", y = "appealing", color = "experiment", 
       add = c("mean_ci"), ylab="appealing", xlab="image_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

ggline(data_sticker, x = "sticker_val", y = "appealing", color = "experiment", 
       add = c("mean_ci"), ylab="appealing", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ------------------------------ Valence  -----------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
data_valence = read.csv("clean_emotion_data.csv",  stringsAsFactors=FALSE) [-c(1)]
data_valence_gather = read.csv("clean_emotion_data_gather.csv",  stringsAsFactors=FALSE) [-c(1)]

data_valence$img_val[data_valence$img_val == "happy"] <- "ihappy"
data_valence$img_val[data_valence$img_val == "sad"] <- "isad"
data_valence$sticker_val[data_valence$sticker_val == "none"] <- "blur/opaque"

ggline(data_valence_gather, x = "emotion", y = "valence", color = "experiment", 
       add = c("mean_ci"), ylab="valence", xlab="emotion"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
# ------------------------------ Interaction   -----------------
# --------------------------------------------------------------------------------------
# ------------------------------ Happiness  -----------------

ggline(data_valence, x = "img_val", y = "happiness", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (hapiness)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ sadness  -----------------
ggline(data_valence, x = "img_val", y = "sadness", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (sadness)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ amusement  -----------------
ggline(data_valence, x = "img_val", y = "amusement", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (amusement)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ surprise  -----------------
ggline(data_valence, x = "img_val", y = "surprise", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (surprise)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ neutral  -----------------
ggline(data_valence, x = "img_val", y = "neutral", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (neutral)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ anger  -----------------
ggline(data_valence, x = "img_val", y = "anger", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (anger)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ disgust  -----------------
ggline(data_valence, x = "img_val", y = "disgust", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (disgust)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ fear  -----------------
ggline(data_valence, x = "img_val", y = "fear", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (fear)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ------------------------------ Main effect sticker   -----------------
# --------------------------------------------------------------------------------------
# ------------------------------ Happiness  -----------------
ggline(data_valence, x = "sticker_val", y = "happiness", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (hapiness)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)
# ------------------------------ sadness  -----------------
ggline(data_valence, x = "sticker_val", y = "sadness", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (sadness)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ amusement  -----------------
ggline(data_valence, x = "sticker_val", y = "amusement", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (amusement)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ surprise  -----------------
ggline(data_valence, x = "sticker_val", y = "surprise", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (surprise)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ neutral  -----------------
ggline(data_valence, x = "sticker_val", y = "neutral", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (neutral)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ anger  -----------------
ggline(data_valence, x = "sticker_val", y = "anger", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (anger)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ disgust  -----------------
ggline(data_valence, x = "sticker_val", y = "disgust", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (disgust)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ fear  -----------------
ggline(data_valence, x = "sticker_val", y = "fear", color = "sticker_val", 
       add = c("mean_ci"), ylab="valence (fear)", xlab="sticker_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)


# ------------------------------ Main effect Image valence   -----------------
# --------------------------------------------------------------------------------------
# ------------------------------ Happiness  -----------------
ggline(data_valence, x = "img_val", y = "happiness", color = "experiment", 
       add = c("mean_ci"), ylab="valence (hapiness)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ sadness  -----------------

ggline(data_valence, x = "img_val", y = "sadness", color = "experiment", 
       add = c("mean_ci"), ylab="valence (sadness)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ amusement  -----------------

ggline(data_valence, x = "img_val", y = "amusement", color = "experiment", 
       add = c("mean_ci"), ylab="valence (amusement)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ surprise  -----------------
ggline(data_valence, x = "img_val", y = "surprise", color = "experiment", 
       add = c("mean_ci"), ylab="valence (surprise)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ neutral  -----------------
ggline(data_valence, x = "img_val", y = "neutral", color = "experiment", 
       add = c("mean_ci"), ylab="valence (neutral)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ anger  -----------------
ggline(data_valence, x = "img_val", y = "anger", color = "experiment", 
       add = c("mean_ci"), ylab="valence (anger)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ disgust  -----------------
ggline(data_valence, x = "img_val", y = "disgust", color = "experiment", 
       add = c("mean_ci"), ylab="valence (disgust)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

# ------------------------------ fear  -----------------
ggline(data_valence, x = "img_val", y = "fear", color = "experiment", 
       add = c("mean_ci"), ylab="valence (fear)", xlab="img_val"
       #palette = c("#FF0000", "#006400", "#0000CD", "#8B008B")
)

