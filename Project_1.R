#Remove all objects stored
rm(list = ls())



#set working directory
setwd("E:/R/Project")


#Load .xls file in R with gdata package
library(gdata)
library(dplyr)
library(tidyverse)


#Loading .xls file by removing first column i.e. ID Column
df <- read.xls("Absenteeism_at_work_Project.xls")[, -1] #Since employee ID column is not useful for our Analysi
str(df)
data.class(df)

              
###############Changing Variable type#########################
#Removing commas(,) from Work.load.Average.day. column
df$Work.load.Average.day. <- as.character(df$Work.load.Average.day.)
df$Work.load.Average.day.<- gsub("\\,", "", df$Work.load.Average.day.)
df$Work.load.Average.day. <- as.integer(df$Work.load.Average.day.)
dim(df)
str(df)

#Converting our original dataset df into tibble just for better understanding
Absenteeism_Data <- as_tibble(df)
data.class(Absenteeism_Data)
#Removing Duplicate rows from  data
Absenteeism_Data <- Absenteeism_Data %>% distinct()
dim(Absenteeism_Data)
str(Absenteeism_Data)
summary(Absenteeism_Data)
View(Absenteeism_Data)


########################################Exploratory Data Analysis######################################################
library(DataExplorer)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

#Checking the dimension of the input dataset and the type of variables
dim(Absenteeism_Data)
plot_str(Absenteeism_Data)

continuous_vars = c("Transportation.expense", "Distance.from.Residence.to.Work", "Service.time", "Age", 'Transportation expense',
                    "Work.load.Average.day.", "Hit.target", "Weight", "Height", "Body.mass.index", "Absenteeism.time.in.hours")

categorical_vars = c("Reason.for.absence","Month.of.absence" ,"Day.of.the.week",
                     "Seasons","Disciplinary.failure", "Education", "Son",
                     "Social.drinker", "Social.smoker","Pet")



            ########################Univariate Analysis########################
                  
                                  ##For Categorical Data##


#Reason.for.absence
 cat_count <- Absenteeism_Data %>% group_by(Reason.for.absence) %>% summarise(counts = n())
  ggplot(cat_count, aes(x = Reason.for.absence, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:28) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
    
#Month.of.absence
  cat_count_1 <- Absenteeism_Data %>% group_by(Month.of.absence) %>% summarise(counts = n())
  ggplot(cat_count_1, aes(x = Month.of.absence, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:12) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  
  #Day.of.the.week
  cat_count_2 <- Absenteeism_Data %>% group_by(Day.of.the.week) %>% summarise(counts = n())
  ggplot(cat_count_2, aes(x = Day.of.the.week, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Seasons
  cat_count_3 <- Absenteeism_Data %>% group_by(Seasons) %>% summarise(counts = n())
  ggplot(cat_count_3, aes(x = Seasons, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Disciplinary.failure
  cat_count_4 <- Absenteeism_Data %>% group_by(Disciplinary.failure) %>% summarise(counts = n())
  ggplot(cat_count_4, aes(x = Disciplinary.failure, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:1) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Education
  cat_count_5 <- Absenteeism_Data %>% group_by(Education) %>% summarise(counts = n())
  ggplot(cat_count_5, aes(x = Education, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Social.drinker
  cat_count_6 <- Absenteeism_Data %>% group_by(Social.drinker) %>% summarise(counts = n())
  ggplot(cat_count_6, aes(x = Social.drinker, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:1) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Social.smoker
  cat_count_7 <- Absenteeism_Data %>% group_by(Social.smoker) %>% summarise(counts = n())
  ggplot(cat_count_7, aes(x = Social.smoker, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:1) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Son
  cat_count_8 <- Absenteeism_Data %>% group_by(Son) %>% summarise(counts = n())
  ggplot(cat_count_8, aes(x = Son, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  #Pet
  cat_count_9 <- Absenteeism_Data %>% group_by(Pet) %>% summarise(counts = n())
  ggplot(cat_count_9, aes(x = Pet, y = counts)) + 
    geom_bar(fill = "#0073C2FF", stat = "identity") + scale_x_continuous(breaks = 0:8) +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_pubclean()
  



##FOR CONTINUOUS VARIABLE
 
 #Transportation.expense
  
  hist(Absenteeism_Data$Transportation.expense,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Transportation expense",
       main = NULL) 
  lines(density(Absenteeism_Data$Transportation.expense, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Distance.from.Residence.to.Work
  hist(Absenteeism_Data$Distance.from.Residence.to.Work,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Distance from Residence to Work",
       main = NULL)
  lines(density(Absenteeism_Data$Distance.from.Residence.to.Work, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Service.time
  hist(Absenteeism_Data$Service.time,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Service time",
       main = NULL)
  lines(density(Absenteeism_Data$Service.time, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Age
  hist(Absenteeism_Data$Age,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Age",
       main = NULL)
  lines(density(Absenteeism_Data$Age, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Work.load.Average.day.
  hist(Absenteeism_Data$Work.load.Average.day.,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Work load Average day",
       main = NULL)
  lines(density(Absenteeism_Data$Work.load.Average.day., na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Hit.target
  DENS_0 <- density(Absenteeism_Data$Hit.target, na.rm = TRUE)
  YMax_0 <- max(DENS_0$y)
  hist(Absenteeism_Data$Hit.target,
       col = "peachpuff",
       border = "black",
       ylim = c(0, YMax_0),
       prob = TRUE,
       xlab = "Hit target",
       main = NULL)
  lines(DENS_0, lwd = 2, col = "chocolate3")
  
  #Weight
  hist(Absenteeism_Data$Weight,
       col = "peachpuff",
       border = "black",
       prob = TRUE,
       xlab = "Weight",
       main = NULL)
  lines(density(Absenteeism_Data$Weight, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Height
  DENS_1 <- density(Absenteeism_Data$Height, na.rm = TRUE)
  YMax_1 <- max(DENS_1$y)
  hist(Absenteeism_Data$Height,
       col = "peachpuff",
       border = "black",
       ylim = c(0, YMax_1),
       prob = TRUE,
       xlab = "Height",
       main = NULL)
  lines(DENS_1, lwd = 2, col = "chocolate3")
  
  #Body.mass.index
  DENS_2 <- density(Absenteeism_Data$Body.mass.index, na.rm = TRUE)
  YMax_2 <- max(DENS_2$y)
  hist(Absenteeism_Data$Body.mass.index,
       col = "peachpuff",
       border = "black",
       ylim = c(0, YMax_2),
       prob = TRUE,
       xlab = "Body mass index",
       main = NULL)
  lines(density(Absenteeism_Data$Body.mass.index, na.rm = TRUE), lwd = 2, col = "chocolate3")
  
  #Absenteeism.time.in.hours
  DENS <- density(Absenteeism_Data$Absenteeism.time.in.hours, na.rm = TRUE)
  YMax <- max(DENS$y)
  hist(Absenteeism_Data$Absenteeism.time.in.hours,
       col = "peachpuff",
       border = "black",
       ylim = c(0, YMax),
       prob = TRUE,
       xlab = "Absenteeism timein hour",
       main = "Target Variable")
  lines(DENS, lwd = 2, col = "chocolate3")

  
              ########################Bivariate Analysis########################

  library(ggcorrplot)
  library(dlookr)
  # Correlation Plot of Numerical Variables
  Absenteeism_Data[ ,c(5:10,17:20)] %>%
    select_if(is.numeric) %>% na.omit() %>%
    cor() %>% 
    ggcorrplot(lab = T)
  
## Compute a correlation matrix
corr_1 <- Absenteeism_Data[, c(1:4, 20)] %>% na.omit() %>% cor()
corr_1
corr_2 <- Absenteeism_Data[, c(5:6, 20)] %>% na.omit() %>% cor()
corr_2
corr_3 <- Absenteeism_Data[, c(7:10, 20)] %>% na.omit() %>% cor()
corr_3
corr_4 <- Absenteeism_Data[, c(11:14, 20)] %>% na.omit() %>% cor()
corr_4
corr_5 <- Absenteeism_Data[, c(15:19, 20)] %>% na.omit() %>% cor()
corr_5


                   #####Grouped Descriptive Statistics#####

                      ##Grouped Numerical Variables##
#Transportation.expense
ggplot(Absenteeism_Data, aes(x=Transportation.expense, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Distance.from.Residence.to.Work
ggplot(Absenteeism_Data, aes(x=Distance.from.Residence.to.Work, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Service.time
ggplot(Absenteeism_Data, aes(x=Service.time, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Age
ggplot(Absenteeism_Data, aes(x=Age, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Work.load.Average.day.
ggplot(Absenteeism_Data, aes(x=Work.load.Average.day., y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Hit.target
ggplot(Absenteeism_Data, aes(x=Hit.target, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Weight
ggplot(Absenteeism_Data, aes(x=Weight, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Height
ggplot(Absenteeism_Data, aes(x=Height, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)
#Body.mass.index
ggplot(Absenteeism_Data, aes(x=Body.mass.index, y=Absenteeism.time.in.hours )) + 
  geom_point(na.rm = T)+
  geom_smooth(method=lm, se=FALSE, na.rm = T)


##Grouped Categorical Variables##
#Reason.for.absence
plot(Absenteeism.time.in.hours~Reason.for.absence, data = Absenteeism_Data, col = colors())

#Month.of.absence
plot(Absenteeism.time.in.hours~Month.of.absence, data = Absenteeism_Data, col = colors())

#Day.of.the.week
plot(Absenteeism.time.in.hours~Day.of.the.week, data = Absenteeism_Data, col = colors())
#Seasons 
plot(Absenteeism.time.in.hours~Seasons, data = Absenteeism_Data, col = colors())
#Disciplinary.failure
plot(Absenteeism.time.in.hours~Disciplinary.failure, data = Absenteeism_Data, col = colors())
#Education
plot(Absenteeism.time.in.hours~Education, data = Absenteeism_Data, col = colors())
#Son
plot(Absenteeism.time.in.hours~Son, data = Absenteeism_Data, col = colors())
#Social.drinker
plot(Absenteeism.time.in.hours~Social.drinker, data = Absenteeism_Data, col = colors())
#Social.smoker
plot(Absenteeism.time.in.hours~Social.smoker, data = Absenteeism_Data, col = colors())
#Pet 
plot(Absenteeism.time.in.hours~Pet, data = Absenteeism_Data, col = colors())


                  ######################Missing Value Treatment######################

#Visualizing Missing Values for each variable
plot_missing(Absenteeism_Data)
#Checking missing values in each columns with their missing percentages
profile_missing(Absenteeism_Data)
#Imputing missing values using MICE package
library(mice)
library(VIM)
imp.Absenteeism_Data <- mice(Absenteeism_Data, m=5, maxit = 50, seed = 500)
summary(imp.Absenteeism_Data)
stripplot(imp.Absenteeism_Data, pch = 20, cex = 1.2)
imp.Absenteeism_Data$imp$Reason.for.absence
#Replacing missing values with imputed values
Absenteeism_Data_complete <- complete(imp.Absenteeism_Data, 1)

###########################################Outlier Analysis#############################################
library(Hmisc)

#Visualizing outlier using boxplots
 #selecting only continuous variable
numeric_data = Absenteeism_Data_complete[ ,c(5:10,17:20)]


cnames = colnames(numeric_data)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(Absenteeism_Data_complete))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
## Plotting plots together #2, 6, 7, 9
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9, gn10, ncol=4)

#Investigating summary of continuous variables to look for outlier
summary(Absenteeism_Data_complete[ ,5:9])
summary(Absenteeism_Data_complete[ ,c(10,17:20)])

#replacing all outliers with NA's
  #Transportation.expense
range_T <- 260 + 1.5*IQR(Absenteeism_Data_complete$Transportation.expense)
Absenteeism_Data_complete$Transportation.expense[Absenteeism_Data_complete$Transportation.expense > range_T] <- NA
  #Service.time
range_ST <- 16 + 1.5*IQR(Absenteeism_Data_complete$Service.time)
Absenteeism_Data_complete$Service.time[Absenteeism_Data_complete$Service.time > range_ST] <- NA
  #Age
range_A <- 40 + 1.5*IQR(Absenteeism_Data_complete$Age)
Absenteeism_Data_complete$Age[Absenteeism_Data_complete$Age > range_A] <- NA
 #Work.load.Average.day.
range_WL <- 294217 + 1.5*IQR(Absenteeism_Data_complete$Work.load.Average.day.)
Absenteeism_Data_complete$Work.load.Average.day.[Absenteeism_Data_complete$Work.load.Average.day. > range_WL] <- NA
  #Month.of.absence has month 0 which is not possible as month ranges from 1-12
Absenteeism_Data_complete$Month.of.absence[Absenteeism_Data_complete$Month.of.absence == 0] <- NA

sum(is.na(Absenteeism_Data_complete))

#Imputing NA's
imp.Absenteeism_Data_complete <- mice(Absenteeism_Data_complete, m=5, maxit = 50, seed = 500)
imp.Absenteeism_Data_complete$imp$Month.of.absence
#stripplot(imp.Absenteeism_Data_complete, pch = 20, cex = 1.2)
Absenteeism_Data_complete_OA <- complete(imp.Absenteeism_Data_complete, 1)

#Visualizing to check for utliers after oulier treatment from some continuous variables
plot_outlier(Absenteeism_Data_complete_OA)

#We have replaced outliers for some variables with NA's then we replaced them with imputed values.
#Apart from that all values which are showing as outlier are accepted for our business purpose in the dataset.
#We are not removing outliers from target variables as we don't know exactly wether these values are outlier or not. 

########################################Feature Selection##########################################
## Correlation Plot 
library(corrgram)
library(VIF)
library(usdm)
corrgram(Absenteeism_Data_complete_OA[ ,c(5:10,17:20)], order = F, 
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Check for multicollinearity using VIF
vifcor(Absenteeism_Data_complete_OA[ ,c(5:10,17:20)])
#Removing Weight variable as a part of our feature selection
Absenteeism_Data_complete_OA_FS = subset(Absenteeism_Data_complete_OA, select = -Weight)
#Checking VIF after removing Weight Column
vifcor(Absenteeism_Data_complete_OA_FS[ ,c(5:10,17:19)])

##################################Feature Scaling################################################

#Scaling Continuous variables by z-score standardization technique
Absenteeism_Data_complete_OA_FS[ ,c(5:10,17:18)] <- as.data.frame(scale(Absenteeism_Data_complete_OA_FS[ ,c(5:10,17:18)]))


#Saving the Complete clean pre-processed data
library(xlsx)
write.xlsx(Absenteeism_Data_complete_OA_FS, file = "Absenteeism_Clean_Data_new.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=F, append=FALSE)





############################################## Model Developement####################################################
emp_abs <- read.xlsx("Absenteeism_Clean_Data_new.xlsx",sheetName="Sheet1")
str(emp_abs)
# Creating dummy variables for categorical variables
library(mlr)
library(dummies)
df_0 = dummy.data.frame(emp_abs, categorical_vars,sep = ".")
#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(df_0), 0.8 * nrow(df_0))
train = df_0[train_index,]
test = df_0[-train_index,]

##################################################Decision Tree#######################################################

#     mae       rmse        mse 
# 5.658996  11.717496 137.299703 

#Load Libraries
library(rpart)
library(DMwR)

# ##rpart for regression
fit = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")
#Predict for new test cases
predictions_dt = predict(fit, test[,-79])
#Error metric evaluation
DMwR::regr.eval(test[ , 79], predictions_dt, stats = c('mae', 'rmse','mse'))
plot(predictions_dt - test[ , 79])

################################################Random Forest####################################################

#   mae       rmse        mse 
# 4.706784  10.819635 117.064493

library(randomForest)
library(inTrees)
set.seed(321)
###Random Forest
RF_model = randomForest(Absenteeism.time.in.hours ~ ., train, importance = TRUE, ntree = 500)
#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)

# #Extract rules
exec = extractRules(treeList, train[,-79])  # R-executable conditions

# #Visualize some rules
exec[1:2,]

# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]

# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-79], train$Absenteeism.time.in.hours)  # get rule metrics

# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-79])
#Error metric evaluation
regr.eval(test[ , 79], RF_Predictions, stats = c('mae', 'rmse','mse'))
plot(RF_Predictions - test[ , 79])

###########################################XG Boost#########################################################
#     mae       rmse        mse 
# 4.815342  10.635997 112.124428

# Fitting model
library(caret)
set.seed(444)
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
##Train the model using training data
model_xboost_1 <- train(Absenteeism.time.in.hours ~ ., data = train, method = "xgbTree", trControl = TrainControl,verbose = FALSE)
#Predict the test cases
predicted_xgb_1 <- predict(model_xboost_1, test[, -79])
regr.eval(test[ , 79], predicted_xgb_1, stats = c('mae', 'rmse', 'mse'))
plot(predicted_xgb_1 - test[ , 79])


###############################################Linear Regression#############################

#   mae      rmse       mse 
# 5.47771  13.38506 179.15995

##Train the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-79])
#Error metric evaluation
regr.eval(test[ , 79], lr_predictions, stats = c('mae', 'rmse', 'mse'))
plot(lr_predictions - test[ , 79])
