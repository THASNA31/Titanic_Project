install.packages("dplyr")
install.packages("lmtest")
library(zoo)
library(lmtest)
library(dplyr)


#--------------------------------------------------------
data <- read.csv("train.csv")
model_1 <- glm(Survived ~ Pclass + Name + Sex + Age + SibSp + Parch + Ticket + Fare + Cabin + Embarked, data = data)
summary(model_1)

#--------------------------------------------------------
model_2 <- glm(Survived ~ Age + Sex, data = data)
summary(model_2)

#-------------------------------------------------------
model_3 <- glm(Survived ~ Age, data = data)
summary(model_3)

#-------------------------------------------------------
model_4 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = data)
summary(model_4)

#--------------------------------------------------------
data_corr <- data %>% select(Pclass, Sex, Age, SibSp, Parch, Fare)
replace(data_corr$Sex,c("male","female"),c(1,0))

#convert factor sex variable to numeric with male =1 and female =0

data_corr$Sex<- sapply(as.character(data_corr$Sex),switch, "male"=1,"female"=0, USE.NAMES = F)
data_corr$Pclass <- as.numeric(data_corr$Pclass)
data_corr$Sex <- as.numeric(data_corr$Sex)
data_corr$Age <- as.numeric(data_corr$Age)
data_corr$SibSp <- as.numeric(data_corr$SibSp)
data_corr$Parch <- as.numeric(data_corr$Parch)
data_corr$Fare <- as.numeric(data_corr$Fare)

rcorr(as.matrix(data_corr),type = ("spearman"))

#---------------------------------------------------------------------
model_5 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = data, family = binomial)
summary(model_5)
anova(model_5)
c <- model_5$coefficients

test <- read.csv("test.csv")
attach(test)
test$Sex<- sapply(as.character(test$Sex),switch, "male"=1,"female"=0, USE.NAMES = F)
test$Pclass <- as.numeric(test$Pclass)
test$Sex <- as.numeric(test$Sex)
test$Age <- as.numeric(test$Age)
test$SibSp <- as.numeric(test$SibSp)

test %>% mutate(prob <- c[1] + c[2]*Pclass + c[3]*Sex + c[4]*Age + c[5]*SibSp)



logit <- c[1] + c[2]*test$Pclass + c[3]*test$Sex + c[4]*test$Age + c[5]*test$SibSp
prob <- exp(logit)/(1+exp(logit))
survival<- ifelse(prob >= 0.5,1,0)
check <- ifelse(survival == test$Survived)
