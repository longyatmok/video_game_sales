##########################################################
# Create dataset from vgchartz video games ratings
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(RANN)) install.packages("RANN", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(RANN)


# Video Game Sales with Ratings
# https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
ratings <- read.csv("https://raw.githubusercontent.com/longyatmok/video_game_sales/main/Video_Games_Sales_as_at_22_Dec_2016.csv")
summary(ratings)
# Year_of_Release and User_score is character? change to numeric
ratings <- ratings %>% mutate(Year_of_Release = as.numeric(Year_of_Release),
                              User_Score = as.numeric(User_Score))
head(ratings)

##########################################################
# Data visualization and analytics
##########################################################
# Visualization on data
# Year_of_Release, dataset small before 1995
ggplot(ratings) + 
  geom_bar(aes(Year_of_Release))

# Sales by countries
ratings %>% group_by(Year_of_Release) %>% 
  summarise(Sum_NA_Sales=sum(NA_Sales), Sum_EU_Sales=sum(EU_Sales),
            Sum_JP_Sales=sum(JP_Sales), Sum_Other_Sales=sum(Other_Sales)) %>%
  ggplot() +
  geom_line(aes(x = Year_of_Release, y = Sum_NA_Sales, color = "NA"))+
  geom_line(aes(x = Year_of_Release, y = Sum_EU_Sales, color = "EU")) +
  geom_line(aes(x = Year_of_Release, y = Sum_JP_Sales, color = "JP")) +
  geom_line(aes(x = Year_of_Release, y = Sum_Other_Sales, color = "Other")) +
  labs(x = "Year of Release",
       y = "Sum of Sales (in millions of units)",
       color = "Legend") +
  scale_color_manual(values = c("NA" = "blue", "EU" = "red", 
                                "JP" = "green", "Other" = "yellow"))

# Seems NA contribute the most in the dataset

# Average sales per region per year of release
ratings %>% group_by(Year_of_Release) %>% 
  summarise(Mean_NA_Sales=mean(NA_Sales), Mean_EU_Sales=mean(EU_Sales),
            Mean_JP_Sales=mean(JP_Sales), Mean_Other_Sales=mean(Other_Sales)) %>%
  ggplot() +
  geom_line(aes(x = Year_of_Release, y = Mean_NA_Sales, color = "NA"))+
  geom_line(aes(x = Year_of_Release, y = Mean_EU_Sales, color = "EU")) +
  geom_line(aes(x = Year_of_Release, y = Mean_JP_Sales, color = "JP")) +
  geom_line(aes(x = Year_of_Release, y = Mean_Other_Sales, color = "Other")) +
  labs(x = "Year of Release",
       y = "Mean of Sales (in millions of units)",
       color = "Legend") +
  scale_color_manual(values = c("NA" = "blue", "EU" = "red", 
                                "JP" = "green", "Other" = "yellow"))
# Different from sum of sales


# Platform checking
unique(ratings$Platform)
ratings %>% group_by(Platform) %>% 
  summarise(Sales=sum(Global_Sales)) %>% 
  ggplot(aes(x=Platform, y=Sales)) +
  geom_bar(stat="identity")

ratings %>% group_by(Platform) %>% 
  summarise(Sales=sum(Global_Sales)) %>%
  arrange(Sales)

# Seems some platform has nearly no sales, PCFX GG. 3DO, TG16
# Lets group the platforms, reference https://www.kaggle.com/leonardf/releases-and-sales
nintendoplatforms = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sonyplatforms = c("PS","PS2","PSP","PS3","PS4","PSV")
segaplatforms = c("GEN","SCD","DC","GG","SAT")
msplatforms = c("XB","X360", "XOne")
otherplatforms = c("2600","3DO","NG","PCFX","TG16","WS")
pc= c('PC')

ratings$Platform_Group[ratings$Platform %in% nintendoplatforms] <- "Nintendo"
ratings$Platform_Group[ratings$Platform %in% sonyplatforms] <- "Sony"
ratings$Platform_Group[ratings$Platform %in% msplatforms] <- "Microsoft"
ratings$Platform_Group[ratings$Platform %in% segaplatforms] <- "Sega"
ratings$Platform_Group[ratings$Platform %in% pc] <- "PC"
ratings$Platform_Group[ratings$Platform %in% otherplatforms] <- "Other"

ratings %>% group_by(Platform_Group, Year_of_Release) %>%
  summarise(Sales=sum(Global_Sales)) %>%
  ggplot(aes(fill=Platform_Group, y=Sales, x=Year_of_Release)) + 
  geom_bar(position="stack", stat="identity")

# Major platform is Sony (Playstation), Nintendo and Microsoft (Xbox)

# Check by genre with region preference
sales_by_genre <- ratings %>% group_by(Genre) %>% 
  summarise(sum_NA_Sales=sum(NA_Sales), sum_EU_Sales=sum(EU_Sales),
            sum_JP_Sales=sum(JP_Sales), sum_Other_Sales=sum(Other_Sales),
            avg_NA_Sales=mean(NA_Sales), avg_EU_Sales=mean(EU_Sales),
            avg_JP_Sales=mean(JP_Sales), avg_Other_Sales=mean(Other_Sales)) %>%
  mutate(NA_precentage = sum_NA_Sales/sum(ratings$NA_Sales)*100,
         EU_precentage = sum_EU_Sales/sum(ratings$EU_Sales)*100,
         JP_precentage = sum_JP_Sales/sum(ratings$JP_Sales)*100,
         Other_precentage = sum_Other_Sales/sum(ratings$Other_Sales)*100)

melt(select(sales_by_genre, 
            c("Genre", "sum_NA_Sales", "sum_EU_Sales", "sum_JP_Sales", "sum_Other_Sales")), 
     c("Genre")) %>%
  ggplot(aes(fill=variable, y=value, x=Genre))+
  geom_bar(position="dodge", stat="identity")

melt(select(sales_by_genre, 
            c("Genre", "NA_precentage", "EU_precentage", "JP_precentage", "Other_precentage")), 
     c("Genre")) %>%
  ggplot(aes(fill=variable, y=value, x=Genre))+
  geom_bar(position="dodge", stat="identity")

# Region has preference on different Genre
# Need to take care during modelling

unique(ratings$Publisher)
publisher_sales <- ratings %>% group_by(Publisher) %>% 
  summarise(sum_Sales=sum(Global_Sales), avg_Sales=mean(Global_Sales), n=n())
view(arrange(publisher_sales, -sum_Sales))
view(arrange(publisher_sales, -avg_Sales))
ggplot(publisher_sales, aes(x=n, y=avg_Sales)) + geom_point()
#Publisher is also important


##########################################################
# Data cleansing
##########################################################
# NAs introduced by coercion
summary(ratings)
head(ratings)

# NAs, remove Year Na's as 269/16719 not a great problem
ratings <- ratings %>% filter(!is.na(ratings$Year_of_Release))

# Name empty, 2 record
ratings <- ratings %>% filter(ratings$Name != "")

##########################################################
# Modelling
##########################################################
# For modelling, only take the useful columns
# Global sales will be the prediction, may try other sales later
# Developer is too similar with Publisher, so I dropped it first
# Platform is too wide, will be using Platform_Group
d_model <-  ratings[,c('Global_Sales',
                  'Name',
                  'Year_of_Release',
                  'Publisher',
                  'Platform_Group',
                  'Genre',
                  'Critic_Score',
                  'Critic_Count',
                  'User_Score',
                  'User_Count')]
head(d_model)
# Check NA on predictors
colMeans(is.na(d_model))

# over 50% of critic_score and user_score is missing.
# So continue dropping those NA
d_model <- d_model %>% filter(!is.na(d_model$User_Score))
colMeans(is.na(d_model))

# Fill the 7.6% of Critic_Score to median
d_model[is.na(d_model$Critic_Score), "Critic_Score"] <- mean(d_model$Critic_Score, na.rm=TRUE)
d_model[is.na(d_model$Critic_Count), "Critic_Count"] <- mean(d_model$Critic_Count, na.rm=TRUE)
colMeans(is.na(d_model))


# Create dummy vars
dummies <- dummyVars(Global_Sales ~ Platform_Group+Genre, data = d_model)
dummies_model <- predict(dummies, newdata = d_model)
head(dummies_model)

# Create final prediction model to be use
p_model = cbind(d_model, dummies_model) %>% select(-c(Platform_Group, Genre, Name))
p_model$Publisher <- as.factor(p_model$Publisher)

head(p_model)

# Start modeling
set.seed(255)

trainRowNumbers <- createDataPartition(p_model$Global_Sales, p=0.8, list=FALSE)
trainData <- p_model[trainRowNumbers,]
testData <- p_model[-trainRowNumbers,]

# Start training
train_ctr <- trainControl(method="LGOCV", number=3, verboseIter=TRUE)

# Linear regression
M_lm <- train(Global_Sales ~ .,
                weights=exp(trainData$Year_of_Release-min(trainData$Year_of_Release)+1),
                data=trainData,
                trControl=train_ctr,
                method="lm")
train_lm <- predict(M_lm,trainData)
RMSE(train_lm, trainData$Global_Sales)

# Elastic net
glmnetgrid <-expand.grid(alpha=c(0.1,0.55,1),lambda=seq(0,0.5,0.1))
M_glmnet<- train(Global_Sales ~ .,
                 weights=exp(trainData$Year_of_Release-min(trainData$Year_of_Release)+1),
                 data=trainData,
                 trControl=train_ctr,
                 method="glmnet",
                 tuneGrid=glmnetgrid)
train_glmnet <- predict(M_glmnet,trainData)
RMSE(train_glmnet, trainData$Global_Sales)

# SVM
M_svm <- train(Global_Sales ~ .,
               weights=exp(trainData$Year_of_Release-min(trainData$Year_of_Release)+1),
               data=trainData,
               method="svmRadial",
               trControl=train_ctr)
train_svm <- predict(M_svm, trainData)
RMSE(train_svm, trainData$Global_Sales)

# RF
M_rf <- train(Global_Sales~.,
              weights=exp(trainData$Year_of_Release-min(trainData$Year_of_Release)+1),
              data=trainData,
              method="rf",
              tuneLength=2,
              trControl=train_ctr)
train_rf <- predict(M_rf,trainData)
RMSE(train_rf, trainData$Global_Sales)

# It seems Random forest give the best model

# Model testing
test_lm <- predict(M_lm, testData)
RMSE(test_lm, testData$Global_Sales)

test_glmnet <- predict(M_glmnet, testData)
RMSE(test_glmnet, testData$Global_Sales)

test_svm <- predict(M_svm, testData)
RMSE(test_svm, testData$Global_Sales)

test_rf <- predict(M_rf, testData)
RMSE(test_rf, testData$Global_Sales)
