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

mean_dataset <- function(data, nrows, category){
  mean_data = c()
  for (i in 1:nrows){
    data_extract=data[data$ID ==i, ]
    mean_satisfaction = mean(data_extract$satisfaction)
    mean_sufficiency = mean(data_extract$sufficiency)
    mean_appealing = mean(data_extract$appealing)
    mean_values = cbind(i, mean_satisfaction, mean_sufficiency, mean_appealing)
    mean_data = rbind(mean_data, mean_values)
    
  }
  labels= rep(category,nrows)
  mean_data = cbind(mean_data, labels)
  return(mean_data)
} 

nrows = 396
my_data = read.csv("clean_data_gather.csv",  stringsAsFactors=FALSE) [-c(1)]
my_data$sticker_val[my_data$sticker_val == "none"] <- "blur/opaque"

#my_data =my_data[my_data$sticker_val!="blur/opaque",]
my_data$img_val[my_data$img_val == "happy"] <- "ihappy"
my_data$img_val[my_data$img_val == "sad"] <- "isad"

wilcox.test(satisfaction~ img_val, data = my_data, paired = TRUE)

############################### Main effect sticker Type
table(my_data$sticker_type)

cartoon = my_data[my_data$sticker_type=="cartoon",]
blur = my_data[my_data$sticker_type=="blur",]
realistic = my_data[my_data$sticker_type=="realistic",]
mask = my_data[my_data$sticker_type=="mask",]
apaque = my_data[my_data$sticker_type=="apaque",]
control = my_data[my_data$sticker_type=="control",]

cartoon_mean = mean_dataset(cartoon, nrows, "cartoon")
blur_mean = mean_dataset(blur, nrows, "blur")
realistic_mean = mean_dataset(realistic, nrows, "realistic")
mask_mean = mean_dataset(mask, nrows, "mask")
apaque_mean = mean_dataset(apaque, nrows, "apaque")
control_mean = mean_dataset(control, nrows, "control")

sticker_type = rbind(cartoon_mean, blur_mean,  realistic_mean, mask_mean, apaque_mean, control_mean)
colnames(sticker_type) <- c("ID", "satisfaction", "sufficiency", "appealing", "type_sticker")

table(sticker_type$type_sticker)

sticker_type = as.data.frame(sticker_type)
sticker_type$satisfaction = as.numeric(as.character(sticker_type$satisfaction))
sticker_type$sufficiency = as.numeric(as.character(sticker_type$sufficiency))
sticker_type$appealing = as.numeric(as.character(sticker_type$appealing))


friedman.test(satisfaction ~ type_sticker | ID,
              data = sticker_type)

pairwise.wilcox.test(sticker_type$satisfaction, sticker_type$type_sticker, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(sufficiency ~ type_sticker | ID,
              data = sticker_type)
pairwise.wilcox.test(sticker_type$sufficiency, sticker_type$type_sticker, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(appealing ~ type_sticker | ID,
              data = sticker_type)
pairwise.wilcox.test(sticker_type$appealing, sticker_type$type_sticker, paired=TRUE,
                     p.adjust.method = "BH")




############################### Main effect sticker valence
shappy = my_data[my_data$sticker_val=="shappy",]
ssad = my_data[my_data$sticker_val=="ssad",]
none = my_data[my_data$sticker_val=="blur/opaque",]

shappy_mean = mean_dataset(shappy, nrows, "shappy")
ssad_mean = mean_dataset(ssad, nrows, "ssad")
none_mean = mean_dataset(none, nrows, "blur/opaque")

sticker_valence = rbind(shappy_mean, ssad_mean,  none_mean)
colnames(sticker_valence) <- c("ID", "satisfaction", "sufficiency", "appealing", "sticker_val")


sticker_valence = as.data.frame(sticker_valence)
sticker_valence$satisfaction = as.numeric(as.character(sticker_valence$satisfaction))
sticker_valence$sufficiency = as.numeric(as.character(sticker_valence$sufficiency))
sticker_valence$appealing = as.numeric(as.character(sticker_valence$appealing))


friedman.test(satisfaction ~ sticker_val | ID,
              data = sticker_valence)

pairwise.wilcox.test(sticker_valence$satisfaction, sticker_valence$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(sufficiency ~ sticker_val | ID,
              data = sticker_valence)
pairwise.wilcox.test(sticker_valence$sufficiency, sticker_valence$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(appealing ~ sticker_val | ID,
              data = sticker_valence)
pairwise.wilcox.test(sticker_valence$appealing, sticker_valence$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")



############################### Happy image and sticker valence interaction

ihappy_shappy =my_data[my_data$img_val=="ihappy" & my_data$sticker_val=="shappy",]
ihappy_ssad =my_data[my_data$img_val=="ihappy" & my_data$sticker_val=="ssad",]
ihappy_none =my_data[my_data$img_val=="ihappy" & my_data$sticker_val=="blur/opaque",]

ihappy_shappy_mean = mean_dataset(ihappy_shappy, nrows, "shappy")
ihappy_ssad_mean = mean_dataset(ihappy_ssad, nrows, "ssad")
ihappy_none_mean = mean_dataset(ihappy_shappy, nrows, "blur/opaque")

ihappy = rbind(ihappy_shappy_mean, ihappy_ssad_mean,  ihappy_none_mean)
colnames(ihappy) <- c("ID", "satisfaction", "sufficiency", "appealing", "sticker_val")

ihappy = as.data.frame(ihappy)
ihappy$satisfaction = as.numeric(as.character(ihappy$satisfaction))
ihappy$sufficiency = as.numeric(as.character(ihappy$sufficiency))
ihappy$appealing = as.numeric(as.character(ihappy$appealing))


friedman.test(satisfaction ~ sticker_val | ID,
              data = ihappy)

pairwise.wilcox.test(ihappy$satisfaction, ihappy$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(sufficiency ~ sticker_val | ID,
              data = ihappy)
pairwise.wilcox.test(ihappy$sufficiency, ihappy$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(appealing ~ sticker_val | ID,
              data = ihappy)
pairwise.wilcox.test(ihappy$appealing, ihappy$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

table(happy_img$sticker_val)


############################### Sad image and sticker valence interaction
isad_shappy =my_data[my_data$img_val=="isad" & my_data$sticker_val=="shappy",]
isad_ssad =my_data[my_data$img_val=="isad" & my_data$sticker_val=="ssad",]
isad_none =my_data[my_data$img_val=="isad" & my_data$sticker_val=="blur/opaque",]

isad_shappy_mean = mean_dataset(isad_shappy, nrows, "shappy")
isad_ssad_mean = mean_dataset(isad_ssad, nrows, "ssad")
isad_none_mean = mean_dataset(isad_none, nrows, "blur/opaque")

isad = rbind(isad_shappy_mean, isad_ssad_mean,  isad_none_mean)
colnames(isad) <- c("ID", "satisfaction", "sufficiency", "appealing", "sticker_val")

isad = as.data.frame(isad)
isad$satisfaction = as.numeric(as.character(isad$satisfaction))
isad$sufficiency = as.numeric(as.character(isad$sufficiency))
isad$appealing = as.numeric(as.character(isad$appealing))


friedman.test(satisfaction ~ sticker_val | ID,
              data = isad)
pairwise.wilcox.test(isad$satisfaction, isad$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")


friedman.test(sufficiency ~ sticker_val | ID,
              data = isad)
pairwise.wilcox.test(isad$sufficiency, isad$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

friedman.test(appealing ~ sticker_val | ID,
              data = isad)
pairwise.wilcox.test(isad$appealing, isad$sticker_val, paired=TRUE,
                     p.adjust.method = "BH")

