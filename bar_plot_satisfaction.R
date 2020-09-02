library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
#library(psych)
library(likert)
library(plyr)
library(tidyr)
#library(psycho)
library(emmeans)
library(lme4)
library(lmerTest)
#install.packages("devtools")
library(devtools)
library(sjmisc)
library(lsr)
library(car)
library(rstatix)
library(heplots)
library(effsize)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

mean_sd_ci_calculate <- function(list) {
  list_mean=mean(list, na.rm=TRUE)
  list_sd=sd(list, na.rm=TRUE)
  error <- qt(0.975,df=length(list)-1)*sd(list)/sqrt(length(list))
  # error
  list_left_err <- mean(list)-error
  list_right_err <- mean(list)+error
  result = c(list_mean, list_sd , list_left_err, list_right_err)
  return(result) 
}

data = read.csv("clean_data_gather.csv",  stringsAsFactors=FALSE) [-c(1)]
#data=data[data$sticker_type !="realistic" & data$sticker_type !="mask", ] # cartoon
#data=data[data$sticker_type !="cartoon" & data$sticker_type !="mask", ] # realistic
#data=data[data$sticker_type !="realistic" & data$sticker_type !="cartoon", ] # Mask
table(data$sticker_type)


## Interaction between sticker and image valence
img_sticker_data = data
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "blur"] <- "blur"
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "opaque"] <- "opaque"
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "control"] <- "null"

img_sticker_data$img_val[img_sticker_data$img_val == "happy"] <- "ihappy"
img_sticker_data$img_val[img_sticker_data$img_val == "sad"] <- "isad"

## H1: happy image with happy stickers increases satisfaction. (Reject)
happy = img_sticker_data[img_sticker_data$img_val =="ihappy", ]
happy_sum = summarySE(happy, measurevar="satisfaction", groupvars=c("sticker_val"))
print(happy_sum)
ggplot(happy_sum, aes(reorder(sticker_val, -satisfaction), y=satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Sticker valence(happy images)", title="H1: Happy images with different stickers valence")

## H2: sad image with sad stickers has lower satisfaction.  (accept)
sad = img_sticker_data[img_sticker_data$img_val =="isad", ]
sad_sum = summarySE(sad, measurevar="satisfaction", groupvars=c("sticker_val"))
print(sad_sum)
ggplot(sad_sum, aes(reorder(sticker_val, -satisfaction), satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Sticker valence(sad images)", title="H2: Sad images with with different stickers valence")

# ------ Main effect of Image valence ------
main_img = data
main_img$img_val[main_img$img_val == "happy"] <- "ihappy"
main_img$img_val[main_img$img_val == "sad"] <- "isad"
#main_img_happy=main_img[main_img$img_val =="ihappy", ] # Mask
#useful_data$useful = as.numeric(useful_data$useful)
#mean_sd_ci_calculate(main_img_happy$satisfaction)

main_img_sum = summarySE(main_img, measurevar="satisfaction", groupvars=c("img_val"))

# Use 95% confidence intervals instead of SEM
ggplot(main_img_sum, aes(x=img_val, y=satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Image valence", title="Main effects of image valence")

# ------ Main effect of Sticker valence ------
main_sticker = data
main_sticker$sticker_val[main_sticker$sticker_type == "blur"] <- "blur"
main_sticker$sticker_val[main_sticker$sticker_type == "opaque"] <- "opaque"
main_sticker$sticker_val[main_sticker$sticker_type == "control"] <- "null"
main_sticker_sum = summarySE(main_sticker, measurevar="satisfaction", groupvars=c("sticker_val"))

## H1: Happy stickers have higher satisfaction than control. (Accept)
ggplot(main_sticker_sum, aes(reorder(sticker_val, -satisfaction), y=satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Sticker valence", title="Main effects of sticker valence")


# ------ Main effect of Sticker Type ------
data_sticker_type = data
data_sticker_type=data_sticker_type[data_sticker_type$experiment =="sample", ]
data_sticker_type$emotion[data_sticker_type$sticker_type == "blur" | data_sticker_type$sticker_type == "opaque" ] <- "No"
data_sticker_type$emotion[data_sticker_type$sticker_type == "cartoon" | data_sticker_type$sticker_type == "mask" | data_sticker_type$sticker_type == "realistic"  ] <- "yes"
data_sticker_type_sum = summarySE(data_sticker_type, measurevar="satisfaction", groupvars=c("sticker_type"))

# sticker type
ggplot(data_sticker_type_sum, aes(reorder(sticker_type, -satisfaction), y=satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Sticker type", title="Main effects of sticker type")

# Emotion : yes or no
data_sticker_emotion_sum = summarySE(data_sticker_type, measurevar="satisfaction", groupvars=c("emotion"))
ggplot(data_sticker_emotion_sum, aes(reorder(emotion, -satisfaction), y=satisfaction)) + 
  geom_bar(position=position_dodge(), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(ymin=satisfaction-ci, ymax=satisfaction+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Sticker type", title="Main effects of sticker type")