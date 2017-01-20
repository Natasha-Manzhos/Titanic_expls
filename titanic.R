# Set working directory and import datafiles
# setwd("E:/R_work/titanik")
# # rpart for â€œRecursive Partitioning and Regression Treesâ€
# install.packages("rpart")
# #We need for some better graphics for rpart:
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# install.packages("randomForest")
# install.packages('party')
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(readr)
library(rpart)
library(randomForest)
rm(list=ls())
train <- read.csv("train.csv")
test<-read.csv("test.csv")
str(train)

#see statistic of the column of Survived
table(train$Survived)
prop.table(table(train$Survived))

#Command rep  simply repeats something by the number of times we tell it to.
#Since there was no â€œSurvivedâ€ column in the dataframe, 
#it will create one for us and repeat our â€œ0â€ prediction 418 times, 
#the number of rows we have.
test$Survived <- rep(0, 418)

# letâ€™s extract those two columns from the test dataframe, 
#store them in a new container, and then send it to an output file:
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# look at the summary of the variable Sex
summary(train$Sex)

#A two-way comparison on the number of males and females that survived. 
#The proportion of each sex that survived, as separate groups. 
#We need to tell the command 
#to give us proportions in the 1st dimension which stands for the rows 
#(using â€œ2â€ instead would give you column proportions)
prop.table(table(train$Sex, train$Survived),1)

#changing our prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

summary(train$Age)

train$Child <- 0
train$Child[train$Age < 18] <- 1

#Find the number of survivors children
aggregate(Survived~Child+Sex, data=train, FUN=sum)

aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#the fare is a continuous variable. It needs to be reduced 
#to something that can be easily tabulated
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {
  sum(x)/length(x)})

#new prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",control=rpart.control(cp=0.02))

#Draw tree
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
?rpart.control

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
 
#Another set
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=6, cp=0.02))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


#You need to read data sets"test" and "train" again before functions
train$Name[1]
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]

#use the function strsplit, which stands for string split, 
#to break apart our original name

strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}) 
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#We used the function paste to bring two strings together, 
#and told it to separate them with nothing through the sep argument. 
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#Iteractive tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=6, cp=0.02))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

#Random Forest models

sample(1:10, replace = TRUE)
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
Agefit
fancyRpartPlot(Agefit)

summary(combi)
summary(combi$Embarked)

#Çàìåíèì NA â êîëîíêå ñ íàçâàíèåì ïîğòà ïîñàäêè ïàñàæèğîâ ñèìâîëîì S
#Because it’s so few observations and such a large majority boarded in Southampton

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Random Forests in R can only digest factors with up to 32 levels.
#The FamilyID variable had almost double that

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#Split the test and train sets back up
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit, main="Dotchart of variable importance as measured by a Random Forest",
           col="blue", pch=20)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#Let’s try a forest of conditional inference trees. 
#Conditional inference trees are able to handle factors 
#with more levels than Random Forests can
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FamilySize + FamilyID,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
