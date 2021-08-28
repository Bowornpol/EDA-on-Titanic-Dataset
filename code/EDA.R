setwd("~/GitHub/EDA-on-Titanic-Dataset")

#Read the dataset#
Titanic_train = read.csv("data/train.csv")

#Check the summary of data#
summary(Titanic_train)

#Check the class of data#
sapply(Titanic_train, class)

#Change the class of data#
Titanic_train$Survived = as.factor(Titanic_train$Survived)
Titanic_train$Sex = as.factor(Titanic_train$Sex)
Titanic_train$SibSp = as.numeric(Titanic_train$SibSp)
Titanic_train$Parch = as.numeric(Titanic_train$Parch)

#Clean the data#
sum(is.na(Titanic_train))
Titanic_train <- Titanic_train[!(is.na(Titanic_train$Age)),]

#Plot#
install.packages("ggplot2")
library(ggplot2)
#Sex
ggplot(data=Titanic_train, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "fill") + 
  xlab("Sex") + scale_fill_discrete(name = "Survived") +
  ylab("%Passengers") +
  ggtitle("Proportion of Passengers by Sex") + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))

#Age
ggplot(data=Titanic_train, aes(Age, fill = factor(Survived))) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  xlab("Age") +
  ylab("Density") +
  labs(fill = "Survived") +
  ggtitle("Age Distribution of Passengers")
ggplot(data=Titanic_train, aes(Survived, Age)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) + 
  geom_jitter()

#Pclass
ggplot(data=Titanic_train, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "fill") + 
  xlab("Pclass") + scale_fill_discrete(name = "Survived") +
  ylab("%Passengers") +
  ggtitle("Proportion of Passengers by Pclass") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))

#SibSp
ggplot(data=Titanic_train, aes(SibSp, fill = factor(Survived))) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  xlab("SibSp") +
  ylab("Density") +
  labs(fill = "Survived") +
  ggtitle("SibSp Distribution of Passengers") +
  
#Parch
ggplot(data=Titanic_train, aes(Parch, fill = factor(Survived))) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  xlab("Parch") +
  ylab("Density") +
  labs(fill = "Survived") +
  ggtitle("Parch Distribution of Passengers")

#SibSp+Parch
ggplot(data=Titanic_train, aes(SibSp+Parch, fill = factor(Survived))) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  xlab("SibSp+Parch") +
  ylab("Density") +
  labs(fill = "Survived") +
  ggtitle("SibSp+Parch Distribution of Passengers")

#Fare
ggplot(data=Titanic_train, aes(Fare, fill = factor(Survived))) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  xlab("Fare") +
  ylab("Density") +
  labs(fill = "Survived") +
  ggtitle("Fare Distribution of Passengers")
ggplot(data=Titanic_train, aes(Survived, Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) + 
  geom_jitter()

#Embarked
ggplot(data=Titanic_train, aes(Embarked, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "dodge") + xlab("Embarked") + 
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Survival by Port of Embarkation")


#The summary statistics#
install.packages("gtsummary")
library(gtsummary)
Titanic_train <- Titanic_train %>% select(Survived, Sex, Age, Pclass, SibSp, Parch, Fare, Embarked)
Titanic_train %>% tbl_summary(by = Survived, type = c(SibSp, Parch) ~ "continuous", 
                              statistic = list(all_continuous() ~ "{mean} ({sd})",
                                               all_categorical() ~ "{n} / {N} ({p}%)"))        