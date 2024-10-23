
install.packages(c("Hmisc","ROCR","gridEXtra","pander","reshape2","lazyeval",    "moments","entropy"))

install.packages("C:/Users/User/Downloads/funModeling_1.9.4.tar.gz",repos=NULL,type="source")

install.packages("funModelling")
library(funModeling)

#import packages
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
install.packages("Hmisc")
library(Hmisc)
install.packages("dplyr")
library(dplyr)


#LOAD data
myd<- read.csv("finalstroke.csv", header = TRUE)
#select specific only 
myd<- select(myd, gender, stroke, age, avg_glucose_level, bmi, Residence_type ,smoking_status)
str(myd)


df_status(myd)

#clean data
#row w/o stroke
myd<- myd %>% filter(stroke != 0)
str(myd) #now only data of patient with stroke


df_status(myd)



#delete null value
myd[myd == "N/A" ] <- NA
myd[myd == "Unknown"] <- NA
myd <- na.omit(myd)
str(myd)


#factorize data
myd$bmi <- as.numeric(myd$bmi)


#Univariate 

#create barplot using categorical data for variable "Residence_type"
ggplot(myd, aes(x=Residence_type, fill = Residence_type)) + theme_bw() + geom_bar(color = "brown") + labs(title = "Patient with stroke", subtitle = "By residence type", y="Patient with stroke", x="Residence type",caption = "From stroke prediction dataset") + scale_fill_manual(name="Residence type", values = c("Urban" = "skyblue", "Rural" = "red"))


#Create barplot using continuous data for variable "bmi"
ggplot(myd, aes(x = bmi)) + theme_bw() + geom_histogram(binwidth = 5, fill = "skyblue", color = "brown") + labs(title = "Patient with stroke", subtitle = "By Body Mass Index", y="Patient with stroke", x="Body Mass Index",caption = "From stroke prediction dataset")




#bivariate

#categorical data and continuous data
ggplot(myd,mapping = aes(x = factor(cut_width(age,10)), fill = gender)) + theme_bw() + geom_bar() + labs(title = "Patient with stroke", subtitle = "By gender and age", y="Patient with stroke", x="Age",caption = "From stroke prediction dataset") 

#continuous data and continuous data
ggplot(data = myd, mapping = aes(x = factor(cut_width(age,10)), y = avg_glucose_level)) + theme_bw() + geom_violin(fill = "skyblue", color = "brown") + geom_jitter(width = 0.1, color = "black", size = 1.5) +  labs(title = "Patient with stroke", subtitle = "By Average glucose level and age", y="Average glucose level", x="Age",caption = "From stroke prediction dataset") 




#DATA VISUALIZATION

# AGE-RELATED STROKE INCIDENCE BY SMOKING STATUS AND GENDER
ggplot(data = myd) + geom_histogram(mapping = aes(x = age, fill = gender), binwidth = 10) + scale_fill_manual(name = "Gender", values = c("Male" = "skyblue", "Female" = "pink")) + facet_wrap(~smoking_status) + labs(title = "Patient with stroke", subtitle = "By their age, smoking status and gender", y="Patient with stroke", x="Age",caption = "From stroke prediction dataset") 


#IMPACT OF AVERAGE GLUCOSE LEVELS AND SMOKING STATUS ON STROKE INCIDENCE BY GENDER

ggplot(data = myd) + geom_histogram(mapping = aes(x = avg_glucose_level, fill = gender), binwidth = 10) + scale_fill_manual(name = "Gender", values = c("Male" = "skyblue", "Female" = "pink")) + facet_wrap(~smoking_status) + labs(title = "Patient with stroke", subtitle = "By their average glucose level, smoking status and gender", y="Patient with stroke", x="Average glucose level",caption = "From stroke prediction dataset") 







#basic eda 
basic_eda<- function(myd)
{
  glimpse(myd)
  print(status(myd))
  freq(myd)
  print(profiling_num(myd))
  plot_num(myd)
  describe(myd)
}

basic_eda(myd)
