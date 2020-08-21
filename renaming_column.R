library(plyr)

col_extract <- function(quesn) {
  array_name = c()
  for (i in 1:27){
    coln = paste(toString(i),"_", quesn, sep = "" )
    array_name <- c (array_name, coln)
  }
  return(array_name)
}


y=read.csv("pilot_data.csv", header = FALSE) 

y_selected=y[,23:(ncol(y))]

y_selected=y_selected[!apply(y_selected == "", 1, all),] # Removing the empty lines

x = y_selected[1,]

colnames(y_selected) <- unlist(x[1,])
y_selected=y_selected[-c(1,2, 3),]
y_selected=y_selected[y_selected$Q8.1!="",]

write.csv(y_selected, file="pilot_new.csv") # Write in a new csv file

#### ----------------- attention check -------------------------
y_selected=read.csv("pilot_new.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
y_selected=y_selected[y_selected$X5_Q6.5=="2",]
y_selected=y_selected[y_selected$X14_Q6.7=="4",]
y_selected=y_selected[is.na(y_selected$X10_Q6.6)==TRUE,]
  
write.csv(y_selected, file="pilot_new.csv") # Write in a new csv file


#### ----------------- attention check -------------------------
data=read.csv("cartoon_data_new.csv", sep=",", stringsAsFactors=FALSE)[-c(1)]
#data_120=data[data$condition=="120-no-example", ]
#data_360=data[data$condition=="360-no-example", ]
data=data[data$sports_attention=="" & data$movie_attention=="2" & data$politics_attention=="4",]

write.csv(data, "cartoon_data_new.csv")
