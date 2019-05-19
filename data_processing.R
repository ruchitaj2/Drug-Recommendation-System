#loading required libraries
library(tidyverse)
library(tidytext)
library(data.table)
library(tm)
library(caret)
library(dplyr)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(lubridate)
#install.packages("proxy")
library(proxy)
library(openssl)


#data <- read.csv("D:/ADS/IST 707 Data Analytics/IST707-Project/drug_train.csv", header = TRUE,  stringsAsFactors = TRUE)
data = read.csv(file = 'D:/ADS/IST 707 Data Analytics/IST707-Project/drugsComTrain_raw.csv', header = TRUE,  stringsAsFactors = TRUE)
#removing dirty data
df <- data[-c(1)]
df<- df[!(df$condition %like% '</span>'),]
df<- df[!(df$condition == ""),]
df %>%
  group_by(drugName) %>%
  filter(n() >= 1000) -> data
data <- droplevels(data)
drugs <- unique(data$drugName)
drugs <- drugs[! drugs %in%  c( "Levonorgestrel","Nexplanon","Ethinyl estradiol / levonorgestrel", "Ethinyl estradiol / norethindrone","Ethinyl estradiol / norgestimate","Ethinyl estradiol / norethindrone")]
data <- data[data$drugName %in% drugs,]
data <- droplevels(data) 
data <- data %>% group_by(drugName)
data$review <- gsub('&#039;', "'", data$review)
data$date <- dmy(data$date)
data['Year'] <- year(data$date)
#nrow(data)

#Find your drugs

drug_table <- function (data, conditions) {
  
  df <- data[-c(5,6)]
  df <- as.data.frame(df[ (df$condition %in% conditions),])
  df <- unique(df)
  df <- df %>%
    group_by(drugName) %>%
    dplyr::summarize(Average_Rating = mean(rating, na.rm=TRUE))
  df <- df[order(-df$Average_Rating),]
  df
  
}

#Wordcloud

groupbyDrug <- function(data) {
  drugViz <- data %>% 
    unnest_tokens(word, review) %>% 
    anti_join(stop_words)
res <- drugViz %>% 
  dplyr::group_by(Year,drugName,word) %>% 
  dplyr::summarise(Freq=n())  

return(res)
}

groupbyDrughist <- function(data) {
  res <- data %>% 
    dplyr::group_by(condition) %>% 
    dplyr::summarise(Freq=n())  
  #res <- res[!res$word=='2']
  #res <- res[!res$word=='3']
  
  return(res)
}

groupbyRating <- function(data) {
  res <- data %>% 
    dplyr::group_by(rating) %>% 
    dplyr::summarise(Freq=n())  
  #res <- res[!res$word=='2']
  #res <- res[!res$word=='3']
  
  return(res)
}


groupbyDrugYear <- function(data) {
  drugViz <- data %>% 
    unnest_tokens(word, review) %>% 
    anti_join(stop_words)
  
  #drugViz$Year <- year(drugViz$date)
  
  res <- drugViz %>% 
    dplyr::group_by(Year, drugName,word) %>% 
    dplyr::summarise(Freq=n())  
  
  return(res)
  
}

##Clustering
drugs_df = read.csv(file = 'D:/ADS/IST 707 Data Analytics/IST707-Project/drugsComTrain_raw.csv')
#removing dirty data
drugs_df <- drugs_df[-c(1)]
drugs_df<- drugs_df[!(drugs_df$condition %like% '</span>'),]
drugs_df<- drugs_df[!(drugs_df$condition == ''),]
top_drugs<-as.data.frame(sort(table(drugs_df$drugName),decreasing=TRUE)[1:15])
colnames(top_drugs) <- c("drugName", "count")
top_drugs <- top_drugs[! (top_drugs$drugName %in%  c( "Levonorgestrel","Nexplanon","Ethinyl estradiol / levonorgestrel", "Ethinyl estradiol / norethindrone","Ethinyl estradiol / norgestimate","Ethinyl estradiol / norethindrone")),]
top_drugs <- droplevels(top_drugs)
drugs_df <- drugs_df[drugs_df$drugName %in% top_drugs$drugName,]
drugs_df <- droplevels(drugs_df) 

set.seed(100)
train_index <- createDataPartition(drugs_df$drugName, p = 0.7, list = FALSE)
drugs_df_train <- drugs_df[train_index, ]
drugs_df_test <- drugs_df[-train_index, ]

drugs_df_train$review <- as.character(drugs_df_train$review)

drugViz <- drugs_df_train %>% 
  unnest_tokens(word, review) %>% 
  anti_join(stop_words)


drugTrain <- drugs_df_train %>% 
  unnest_tokens(word, review) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word))

drugs_df_test$review <- as.character(drugs_df_test$review)


drugTest <- drugs_df_test %>% 
  unnest_tokens(word, review) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>%
  mutate(word = SnowballC::wordStem(word))


drugTrain[,6]<-as.factor(drugTrain[,6])
drugTest[,6]<-as.factor(drugTest[,6])

drug_dtm <- drugTrain %>% 
  dplyr::count(condition, word)%>% cast_dtm(condition, word, n)

##CLASSIFICATION

load(file = "D:/ADS/IST 707 Data Analytics/IST707-Project/model_knn2.rda")
load(file = "D:/ADS/IST 707 Data Analytics/IST707-Project/model_nb.rda")
load(file = "D:/ADS/IST 707 Data Analytics/IST707-Project/model_bag.rda")