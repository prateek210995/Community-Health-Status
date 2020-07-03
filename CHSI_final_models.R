
#setting working directory
setwd("C:/Users/12409/Desktop/FALL-2019/2STAT515/CLASSES/Week-13-14-FINAL PROJECT/0_Submitted")
#intalling packages and libraries as required
install.packages("readxl")
install.packages("randomForest")
install.packages("caret")
install.packages('e1071', dependencies=TRUE) #Error: package e1071 is required

library(tidyverse)
library(corrplot)
library(coefplot)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(GGally)
library(caTools)
library(randomForest)
library(caret) ##This library is used for confusionMatrix()


#loading dataset and doing a Tukey's summary on it  
#pd=merge(summary,dempgraphic,all=TRUE)

projectdata <- read.csv("projectdata.csv", sep = ",")
dim(projectdata)
names(projectdata)
#Tukey's 5 point summary stat information
summary(projectdata)
# Check for NA values
any(is.na(projectdata))
# Check the datatypes of all variables
str(projectdata)

#initial visualizations
#histogram for Health_Status uses ggplot2 library
#1
ggplot(projectdata,aes(x=Health_Status)) +
  geom_histogram(size=1, color = "black",binwidth = 2,fill="lightblue") +
  labs(x="Health Status",y="Frequency",title="Plot1-Histogram for Health Status")
#2
ggplot(projectdata, aes(x=Health_Status, y=ALE))+
  geom_point(shape=21, size=2,fill="red",color="black")+
  stat_smooth(method = 'lm')+
  labs(x="Health Status",y="Average Life Expectancy",
       title="Plot2 Relationship between Life expectancy & Helath status")

#plot a scatter plot between the variables "All_Death" and "ALE". 
#We can observe that as the ALE increases the number of death rates are decreasing. 
#3
ggplot(projectdata, aes(x=Health_Status, y=All_Death))+
  geom_point(shape=21, size=2,fill="red",color="black")+
  stat_smooth()+
  ggtitle("Plot of Death & Haelth Status ")
#4
ggplot(projectdata, aes(x= Poverty, y = Health_Status))+
  geom_point(shape = 21, size = 2, fill = "red", color = "black")+ 
  stat_smooth()

#preparing data for models 
dataset<-projectdata[c(9:24)]
names(dataset)
names(projectdata)
dim(dataset)

#Plotting the correlation plot, ggpairs 
cor0<-cor(dataset)
#using function corrplot from library corrplot
corrplot(cor0,method = "number")

corplot1data<-dataset[c(1:7)]
names(corplot1data)
plot(corplot1data)
cor1<-cor(corplot1data)
corrplot(cor1,method = "number")

corplot2data<-dataset[c(3:7)]
names(corplot2data)
plot(corplot2data)
#using round function 
cor2<-round(cor(corplot2data),2)
cor2
corrplot(cor2,method = "number")
#another plot for correlation
ggcorr(dataset,  method = c("pairwise", "pearson"),nbreaks =7, palette = "RdBu", geom = "tile",
       midpoint = 0, legend.position = "left", label_size = 2, label_alpha = "0", layout.exp = 0)
#another plot for coorelation using corrplot
corrplot::corrplot(cor2, type = "full", order = "hclust", 
                   tl.col = "black")


# using function ggpairs from GGally library
ggpairs(dataset)
ggpairs(corplot2data)
# Splitting the dataset into the Training set and Test set 
# Either by using 0.8 or by using 2/3 as split ratio
set.seed(123)
split = sample.split(dataset$Health_Status, SplitRatio = 2/3) 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#dimensions of the datasets
dim(dataset)
dim(training_set)
dim(test_set)

# Feature Scaling
#training_set = scale(training_set)
#test_set = scale(test_set)
#*************************************1**********************************
# Linear Regression (train the model)
lmodel <- lm(Health_Status~., training_set)
summary(lmodel)
summary(lmodel)$coefficient
par(mfrow=c(2,2))
plot(lmodel)
#dropping the insignificant predictors and rerunning the regression
lmodelnew <- lm(Health_Status~ALE+Unhealthy_Days+Poverty, data=training_set)
summary(lmodelnew)
summary(lmodelnew)$coefficient
#doing backward elimination on previous model lmodel by using step
#which uses AIC internally 
model_bkwrd  = step(lmodel, direction  = 'backward')
summary(model_bkwrd)
plot(model_bkwrd)
#checking the linearity assumptions
plot(lmodelnew)
#now that residual and response vs predictor has been plotted 
#uisng coefplot function from coefplot library 
coefplot(lmodelnew,title="Coefficients for the model")
# Predicting the Test set results using model built
test_model = predict(lmodelnew, newdata = test_set)
test_model

#Checking the Accuracy of the model
RSE <- round(sigma(lmodelnew)/mean(dataset$Health_Status),2)
print(paste0("RSE: ", RSE))  #Residual standard error or sigma
RSS <- round(c(crossprod(lmodelnew$residuals)),2) #Residual sum of squares
print(paste0("RSS: ", RSS))
MSE <- round(RSS / length(lmodelnew$residuals),2) # Mean squared error
print(paste0("MSE: ", MSE))
RMSE <- round(sqrt(MSE),2) # Root MSE
print(paste0("RMSE: ", RMSE))

#Measure performance
table_mat <- table(test_set$Health_Status, test_model)
table_mat 
#Calculate accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
#mannualy calculating
s_value<- predict(lmodelnew,test_set)
s_value
diff<-s_value - test_set$Health_Status
mean(diff^2)
sqrt(mean(diff^2))
#we could state from the accuracy, that it's not a good model.
#*************************************2***************************************
# Fitting Simple Linear Regression to the Training set using just ALE 
regressor= lm(formula = Health_Status ~ ALE ,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

#model_data<-fortify(regressor) ;dim(model_data)
#c<-table(training_set$Health_Status,model_data$Health_Status) ; dim(c) ; view(test_set)

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set$ALE, y = training_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$ALE, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs ALE (Training set)') +
  xlab('Health_Status') +
  ylab('ALE')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$ALE, y = test_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$ALE, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs ALE (Test set)') +
  xlab('ALE') +
  ylab('Health_Status')

plot(regressor)
coefplot(regressor)
summary(regressor)
ggplot(regressor, aes(x=.resid))+geom_histogram()

#*************************************3***************************************
# Fitting Simple Linear Regression to the Training set using All_Death 
regressor = lm(formula = Health_Status ~ All_Death ,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

#model_data<-fortify(regressor) ;dim(model_data)
#c<-table(training_set$Health_Status,model_data$Health_Status) ; dim(c) ; view(test_set)

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set$All_Death, y = training_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$All_Death, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs All_Death (Training set)') +
  xlab('Health_Status') +
  ylab('All_Death')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$All_Death, y = test_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$All_Death, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs All_Death (Test set)') +
  xlab('All_Death') +
  ylab('Health_Status')

plot(regressor)
coefplot(regressor)
summary(regressor)
ggplot(regressor, aes(x=.resid))+geom_histogram()
#*************************************4***************************************
# Fitting Simple Linear Regression to the Training set using Unhealthy_Days
regressor= lm(formula = Health_Status ~ Unhealthy_Days,
              data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

#model_data<-fortify(regressor) ;dim(model_data)
#c<-table(training_set$Health_Status,model_data$Health_Status) ; dim(c) ; view(test_set)

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set$Unhealthy_Days, y = training_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$Unhealthy_Days, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs Unhealthy_Days (Training set)') +
  xlab('Health_Status') +
  ylab('Unhealthy_Days')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$Unhealthy_Days, y = test_set$Health_Status),
             colour = 'red') +
  geom_line(aes(x = training_set$Unhealthy_Days, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Health_Status vs Unhealthy_Days (Test set)') +
  xlab('Unhealthy_Days') +
  ylab('Health_Status')

plot(regressor)
coefplot(regressor)
summary(regressor)
ggplot(regressor, aes(x=.resid))+geom_histogram()

#*************************************5***************************************
#comparing plots 
anova(lmodel,lmodelnew,model_bkwrd)#
AIC(lmodel,lmodelnew,model_bkwrd)#
BIC(lmodel,lmodelnew,model_bkwrd)#
#since other two are model plotted by bckward elimination
#they should have goo disgnificance in all AIC, BIC and Anova
ggplot(lmodelnew, aes(x=.resid))+geom_histogram()

#fitted vs residual
ggplot(aes(x=.fitted, y = .resid), data = lmodelnew)+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_smooth(se=FALSE)+
  labs(x="Fitted Value", y ="Residuals")
#**************** model -2 Random forest ***************
rfdata <- read.csv("projectdata.csv", sep = ",")
rfdata$HealthFlag <- with(rfdata, rfdata$Health_Status >= 25)
rfdata$HealthFlag2 <- as.integer(rfdata$HealthFlag)
rfdata$HealthFlag2 <- as.factor(rfdata$HealthFlag2)

head(rfdata)
table(rfdata$HealthFlag2)

set.seed(123)
indsam <- sample(2, nrow(rfdata), replace = TRUE, prob = c(0.7, 0.3))
train1 <- rfdata[indsam == 1,]
test1 <- rfdata[indsam == 2,]

set.seed(222)
rf <- randomForest(train1$HealthFlag2 ~.-State_FIPS_Code -County_FIPS_Code -CHSI_County_Name -CHSI_State_Name -CHSI_State_Abbr,
                   data = train1, ntree = 300, mtry = 4,
                   importance = TRUE, proximity = TRUE)

print(rf)

p1 <- predict(rf, newdata = test1)

plot(rf)

table(test1$HealthFlag2,p1)

confusionMatrix(p1, test1$HealthFlag2)

#Histogram to print number of nodes for trees
hist(treesize(rf), main = "Number of nodes for the trees", col = "blue")

#Multidimensional scaling plot for proximity
MDSplot(rf, train1$HealthFlag2)

#*********Using ALE as predictor or response variable for linear regression*****
#histogram for Average Life Expentancy
ggplot(projectdata,aes(x=projectdata$ALE)) +
  #geom_histogram(binwidth=2) +
  geom_histogram(size=1, color = "black",binwidth = 2,fill="lightblue") +
  labs(x="Average life expectancy in years",y="Frequency",title="Histogram for Life Expectancy")
#1
ggplot(projectdata, aes(x=ALE, y=All_Death))+
  geom_point(shape=21, size=2,fill="red",color="black")+
  stat_smooth()
#plot a scatter plot between the variables "All_Death" and "ALE". 
#We can observe that as the ALE increases the number of death rates are decreasing. 
#2
ggplot(projectdata, aes(x=All_Death, y=ALE))+
  geom_point(shape=21, size=2,fill="red",color="black")+
  stat_smooth()+
  labs(x="Death rate",y="ALE",title="Plot between ALE and All_Death")
#3 not a good plot 
ggplot(projectdata, aes(x= projectdata$Population_Size, y = projectdata$ALE))+
  geom_point(shape = 21, size = 2, fill = "red", color = "black")+ 
  stat_smooth()+
  labs(x="Population size",y="ALE",title="Plot between ALE and Population Size")

#splitting datastet for ALE as response variable 
set.seed(123)
split = sample.split(dataset$ALE, SplitRatio = 2/3) 
ALE_train = subset(dataset, split == TRUE)
ALE_test = subset(dataset, split == FALSE)

#dimensions of the datasets
dim(dataset)
dim(ALE_train)
dim(ALE_test)

# Feature Scaling
#ALE_train = scale(ALE_train)
#ALE_test = scale(ALE_test)

# Fitting Simple Linear Regression to the Training set using All_Death
ALE_regressor = lm(formula = ALE~All_Death  ,
                   data = ALE_train)
ALE_regressor
# Predicting the Test set results
ALE_pred = predict(ALE_regressor, newdata = ALE_test)
ALE_pred

#table to plot 
table<- data.frame(actual=ALE_test$ALE, ALE_pred)
table$error <- with(table, ALE_pred-actual)
table
# Visualising the Training set results
ggplot() +
  geom_point(aes(x = ALE_train$All_Death, y = ALE_train$ALE),
             colour = 'red') +
  geom_line(aes(x = ALE_train$All_Death, y = predict(ALE_regressor, newdata = ALE_train)),
            colour = 'blue') +
  ggtitle('All_Death vs ALE (Training set)') +
  xlab('All_Death')+
  ylab('ALE') 

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = ALE_test$All_Death, y = ALE_test$ALE),
             colour = 'red') +
  geom_line(aes(x = ALE_train$All_Death, y = predict(ALE_regressor, newdata = ALE_train)),
            colour = 'blue') +
  ggtitle('All_Death vs ALE (Test set)') +
  xlab('All_Death')+
  ylab('ALE') 

plot(ALE_regressor)
coefplot(ALE_regressor)
summary(ALE_regressor)
ggplot(ALE_regressor, aes(x=.resid))+geom_histogram()

ggplot(aes(x=All_Death, y=ALE), data=dataset) +
  geom_point() +
  geom_smooth(se=TRUE) + labs( x= "All_Death", y="ALE")

ggplot(data=ALE_regressor, aes(x=.fitted, y=.resid)) +
  geom_point( ) + 
  geom_hline(yintercept=0) + geom_smooth(se=TRUE) + labs(x="fitted Values", y="Residuals")


#regression linear model using ALE as response
ale1 <- lm(ALE~.,data = corplot2data)
ale2 <- lm(ALE~All_Death,data = corplot2data)
ale3 <- lm(ALE~Unhealthy_Days,data = corplot2data)
ale4 <- lm(ALE~Poverty,data = corplot2data)
ale5 <- lm(ALE~All_Death+Unhealthy_Days,data = corplot2data)
ale6 <- lm(ALE~All_Death+Poverty,data = corplot2data)
ale7 <- lm(ALE~Poverty+Unhealthy_Days,data = corplot2data) 

############# ALE =1 BACKWARD ##########
ale1_bk  = step(ale1, direction  = 'backward')
summary(ale1_bk)
plot(ale1_bk)

RSE <- round(sigma(ale1_bk)/mean((corplot2data$ALE)),2)
RSE
#Mean sqaure error is 
mse<-mean(residuals(ale1_bk)^2)
mse
# Root mean squared error
rmse <- sqrt(mse)
rmse
# Residual sum of squares
rss <- sum(residuals(ale1_bk)^2)
rss
# Residual standard error
rse <- sqrt( sum(residuals(ale1_bk)^2) / ale1_bk$df.residual ) 
rse

#manually calculating value 
s_value<- predict(ale1,corplot1data)
s_value
diff<-s_value - corplot1data$ALE
mean(diff^2)
sqrt(mean(diff^2))
#plot() & summary()
plot(ale6)
summary(ale1)
coefplot(ale1)
ggplot(ale1, aes(x=.resid))+geom_histogram()

#all plots to be compared in one
multiplot(ale1,ale2,ale3,ale4,ale5,ale6,ale7, pointSize = 2)

#comparing plots 
anova(ale1,ale2,ale3,ale4,ale5,ale6,ale7)#anova RSS 1,6
AIC(ale1,ale2,ale3,ale4,ale5,ale6,ale7)#aic 1,6
BIC(ale1,ale2,ale3,ale4,ale5,ale6,ale7)#bic 1,6
#fitted vs residual
ggplot(aes(x=.fitted, y = .resid), data = ale1)+
  geom_point(method ='lm')+
  geom_hline(yintercept = 0)+
  geom_smooth(se=FALSE)+
  labs(x="Fitted Value", y ="Residuals")

#regression model using Backward elimination
alebe0 <- lm(ALE~.,data = dataset)
summary(alebe0)
alebe1 <- lm(ALE~All_Death +Health_Status+Poverty+White+Black+
               Native_American+Asian+Hispanic ,data = dataset)
summary(alebe1)
alebe2 <- lm(ALE~All_Death +Health_Status+Poverty+White+
               Native_American+Asian+Hispanic ,data = dataset)
summary(alebe2)
alebe3 <- lm(ALE~All_Death +Health_Status+Poverty+White+
               Native_American+Asian ,data = dataset)
summary(alebe3)
alebe4 <- lm(ALE~All_Death +Health_Status+White+Native_American+
               Asian+Hispanic,
             data = dataset)
summary(alebe4)
alebe5 <- lm(ALE~All_Death +Health_Status+White+
               Native_American+Asian,
             data = dataset)
summary(alebe5)
alebe6 <- lm(ALE~All_Death +Health_Status+White+
               Native_American+Asian,
             data = dataset)
summary(alebe6)
alebe7 <- lm(ALE~All_Death +Health_Status+White+
               +Asian,
             data = dataset)
summary(alebe7)
alebe8 <- lm(ALE~All_Death +Health_Status+White,
             data = dataset)
summary(alebe8)
#all plots to be compared in one
multiplot(alebe0,alebe1,alebe2,alebe3,alebe4,alebe5,alebe6,alebe7,alebe8, pointSize = 2)

#comparing plots 
anova(alebe0,alebe1,alebe2,alebe3,alebe4,alebe5,alebe6,alebe7,alebe8)#
AIC(alebe0,alebe1,alebe2,alebe3,alebe4,alebe5,alebe6,alebe7,alebe8)#
BIC(alebe0,alebe1,alebe2,alebe3,alebe4,alebe5,alebe6,alebe7,alebe8)#

#Regression tree
set.seed(7900)
model <- rpart(ALE~.,
               method="anova",
               data=corplot2data,
               cp=0.0001)

plotcp(model)
printcp(model)

#change code 
model.1se = prune(model,cp= 0.08)
rpart.plot(model.1se,
           extra=1,
           main="1se regression tree")
printcp(model.1se)

#Logistic regresssion
dataset$HealthFlag <- with(dataset, dataset$Health_Status >= 25)
head(dataset)
lr <- glm(HealthFlag ~ ALE, data = dataset, family = binomial)
summary(lr)
x<- seq(min(dataset$ALE), max(dataset$ALE), 0.01)
y<- predict(lr, list(ALE=x), type = "response")
plot(x,y)

#Decision tree
dim(dataset)
s<-sample(3135,2000)
s
train<- dataset[s,]
test<-dataset[-s,]
dim(train)
dim(test)
Aletree <- rpart(ALE ~ Age_19_Under + Age_19_64 + Age_65_84 + Age_85_and_Over, train)
Aletree
Aletree1 <- rpart(ALE ~ White+ Black+ Native_American+ Asian+ Hispanic, train)
Aletree1
Aletree2 <- rpart(ALE ~ Poverty+ Health_Status+ Unhealthy_Days, train)
Aletree2
Aletree3 <- rpart(ALE ~ Health_Status, train)
Aletree3
rpart.plot(Aletree, type = 4, extra = 101)
rpart.plot(Aletree1, type = 4, extra = 101)
rpart.plot(Aletree2, type = 4, extra = 101)
rpart.plot(Aletree3, type = 4, extra = 101)
#p <- predict(Aletree, test, type = "class")
p <- predict(Aletree, test)
table(test[,11],p)