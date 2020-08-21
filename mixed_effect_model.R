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
#library(psycho)
library(emmeans)
library(lme4)
library(lmerTest)
#install.packages("devtools")
library(devtools)
library(sjmisc)

calculate_lmer <- function(dataset, dv, iv, scenarios){
  dataset=dataset[dataset$scenario ==scenarios, ]
  
  if(dv=="satisfaction"){
    baseline = lmer(satisfaction ~ 1 + (1|ID), data=dataset)  
  }
  if(iv == "img_val"){
    model <-update(baseline, .~. + img_val)
  }
  else if(iv == "sticker_val"){
    model <-update(baseline, .~. + sticker_val)
  }
  else if(iv == "emotion"){
    model <-update(baseline, .~. + emotion)
  }
  print(summary(model))
  result = anova(baseline, model)
  return(result)
}

data = read.csv("clean_data_gather.csv",  stringsAsFactors=FALSE) [-c(1)]
data=data[data$scenario =="sports", ]

# ------ Main effect of Image valence ------
main_img = data
main_img$img_val[main_img$img_val == "happy"] <- "ihappy"
main_img$img_val[main_img$img_val == "sad"] <- "isad"

## H1: Happy photo mean greater than the sad photo mean (combined all sticker type and control) (accept)
calculate_lmer(main_img, "satisfaction", "img_val", "sports")


# ------ Main effect of Sticker valence ------
main_sticker = data
main_sticker$sticker_val[main_sticker$sticker_type == "blur"] <- "blur"
main_sticker$sticker_val[main_sticker$sticker_type == "opaque"] <- "opaque"
main_sticker$sticker_val[main_sticker$sticker_type == "control"] <- "null"

## H1: Happy stickers have higher satisfaction than control. (Accept)

### H1a: Happy stickers have greater mean than the Blur stickers
shappy_blur = main_sticker[main_sticker$sticker_val =="shappy" | main_sticker$sticker_val =="blur", ]
calculate_lmer(shappy_blur, "satisfaction", "sticker_val", "sports")

### H1b: Happy stickers have greater mean than the Opaque stickers
shappy_opaque = main_sticker[main_sticker$sticker_val =="shappy" | main_sticker$sticker_val =="opaque", ]
calculate_lmer(shappy_opaque, "satisfaction", "sticker_val", "sports")

### H1c: Happy stickers have greater mean than the Null stickers
shappy_null = main_sticker[main_sticker$sticker_val =="shappy" | main_sticker$sticker_val =="null", ]
calculate_lmer(shappy_null, "satisfaction", "sticker_val", "sports")


## H2: Sad stickers have lower satisfaction than control. (Accept)
### H2a: Sad stickers have lower mean than the Blur stickers
ssad_blur = main_sticker[main_sticker$sticker_val =="ssad" | main_sticker$sticker_val =="blur", ]
calculate_lmer(ssad_blur, "satisfaction", "sticker_val", "sports")

### H2b: Sad stickers have lower mean than the Opaque stickers
ssad_opaque = main_sticker[main_sticker$sticker_val =="ssad" | main_sticker$sticker_val =="opaque", ]
calculate_lmer(ssad_opaque, "satisfaction", "sticker_val", "sports")

### H2c: Sad stickers have lower mean than the Null stickers
ssad_null = main_sticker[main_sticker$sticker_val =="ssad" | main_sticker$sticker_val =="null", ]
calculate_lmer(ssad_null, "satisfaction", "sticker_val", "sports")

## H3: Happy stickers have higher satisfaction than sad stickers. (combined all images and control)
ssad_shappy = main_sticker[main_sticker$sticker_val =="ssad" | main_sticker$sticker_val =="shappy", ]
calculate_lmer(ssad_shappy, "satisfaction", "sticker_val", "sports")

# ------ Main effect of Sticker Type ------

# H1: Sticker with emotion have higher satisfaction than sticker without emotion.
data_sticker_type = data
data_sticker_type=data_sticker_type[data_sticker_type$experiment =="sample", ]
data_sticker_type$emotion[data_sticker_type$sticker_type == "blur" | data_sticker_type$sticker_type == "opaque" ] <- "No"
data_sticker_type$emotion[data_sticker_type$sticker_type == "cartoon" | data_sticker_type$sticker_type == "mask" | data_sticker_type$sticker_type == "realistic"  ] <- "yes"

calculate_lmer(data_sticker_type, "satisfaction", "emotion", "sports")


## Interaction between sticker and image valence
img_sticker_data = data
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "blur"] <- "blur"
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "opaque"] <- "opaque"
img_sticker_data$sticker_val[img_sticker_data$sticker_type == "control"] <- "null"

img_sticker_data$img_val[img_sticker_data$img_val == "happy"] <- "ihappy"
img_sticker_data$img_val[img_sticker_data$img_val == "sad"] <- "isad"

## H1: happy image with happy stickers increases satisfaction. (Reject)
happy = img_sticker_data[img_sticker_data$img_val =="ihappy", ]
### H1a: Happy image with happy sticker mean vs Happy image with  null
happy_null = happy[happy$sticker_val =="shappy" | happy$sticker_val =="null", ]
calculate_lmer(happy_null, "satisfaction", "sticker_val", "sports")

### H1b: Happy image with happy sticker mean vs Happy image with blur
happy_blur = happy[happy$sticker_val =="shappy" | happy$sticker_val =="blur", ]
calculate_lmer(happy_blur, "satisfaction", "sticker_val", "sports")

### H1c: Happy image with happy sticker mean vs Happy image with opaque
happy_opaque = happy[happy$sticker_val =="shappy" | happy$sticker_val =="opaque", ]
calculate_lmer(happy_opaque, "satisfaction", "sticker_val", "sports")

### H1d: Happy image with happy sticker mean vs Happy image with sad sticker
happy_sad = happy[happy$sticker_val =="shappy" | happy$sticker_val =="ssad", ]
calculate_lmer(happy_sad, "satisfaction", "sticker_val", "sports")

## H2: sad image with sad stickers has lower satisfaction.  (accept)
sad = img_sticker_data[img_sticker_data$img_val =="isad", ]

### H2a: Sad image with sad sticker mean vs sad image with  null
sad_null = sad[sad$sticker_val =="ssad" | sad$sticker_val =="null", ]
calculate_lmer(sad_null, "satisfaction", "sticker_val", "sports")

### H2b: Sad image with sad sticker mean vs sad image with  blur
sad_blur = sad[sad$sticker_val =="ssad" | sad$sticker_val =="blur", ]
calculate_lmer(sad_blur, "satisfaction", "sticker_val", "sports")

### H2c: Sad image with sad sticker mean vs sad image with  opaque
sad_opaque = sad[sad$sticker_val =="ssad" | sad$sticker_val =="opaque", ]
calculate_lmer(sad_opaque, "satisfaction", "sticker_val", "sports")

### H2d: Sad image with sad sticker mean vs sad image with  happy sticker
sad_happy = sad[sad$sticker_val =="ssad" | sad$sticker_val =="shappy", ]
calculate_lmer(sad_happy, "satisfaction", "sticker_val", "sports")

# When sticker valence does not match photo valence:

## H1: Sad images with happy stickers lowers satisfaction. (accept)  

### H1a: Sad image with happy sticker mean vs sad image with  null
isad_shappy_null = sad[sad$sticker_val =="shappy" | sad$sticker_val =="null", ]
calculate_lmer(isad_shappy_null, "satisfaction", "sticker_val", "sports")

### H1b: Sad image with happy sticker mean vs sad image with  blur
isad_shappy_blur = sad[sad$sticker_val =="shappy" | sad$sticker_val =="blur", ]
calculate_lmer(isad_shappy_blur, "satisfaction", "sticker_val", "sports")

### H1c: Sad image with happy sticker mean vs sad image with  opaque
isad_shappy_opaque = sad[sad$sticker_val =="shappy" | sad$sticker_val =="opaque", ]
calculate_lmer(isad_shappy_opaque, "satisfaction", "sticker_val", "sports")

## H2:Happy images with sad stickers lowers satisfaction. (accept)  

### H2a: Happy image with sad sticker mean vs happy image with  null
ihappy_ssad_null = happy[happy$sticker_val =="ssad" | happy$sticker_val =="null", ]
calculate_lmer(ihappy_ssad_null, "satisfaction", "sticker_val", "sports")

### H2b: Happy image with sad sticker mean vs happy image with  blur
ihappy_ssad_blur = happy[happy$sticker_val =="ssad" | happy$sticker_val =="blur", ]
calculate_lmer(ihappy_ssad_blur, "satisfaction", "sticker_val", "sports")

### H2c: Happy image with sad sticker mean vs happy image with  opaque
ihappy_ssad_opaque = happy[happy$sticker_val =="ssad" | happy$sticker_val =="opaque", ]
calculate_lmer(ihappy_ssad_opaque, "satisfaction", "sticker_val", "sports")

