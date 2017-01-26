if (!require("Amelia")) install.packages("Amelia")
if (!require("vcd")) install.packages("vcd")
if (!require("corrgram")) install.packages("corrgram")
library(Amelia)
library(vcd)
library(corrgram)
library(plyr)
# install.packages("corrplot")
library("corrplot")
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}
Titanic.path <- "https://raw.github.com/OlenaNN/Titanic_expls/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv
train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw   

#Data Mining
#start with a look at missing data in the training set. 
#Use the missmap function from the Amelia package to display these.

missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("wheat","darkred"), legend=FALSE)

# Let's see what can be learned from the data we have. 
#Putting some simple data visualization tools to work.
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="brown")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="darkblue")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

#The following mosaic suggests that traveling class did influence 
#the odds of a passenger's survival.
mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")


require(corrgram)
corrgram.data <- df.train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.conf, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=panel.pie, 
         text.panel=panel.txt,diag.panel=panel.density, main="Titanic Training Data")


corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt,diag.panel=panel.density, main="Titanic Training Data")

#A common approach to this type of situation is to replacing the missings 
#with the average of the available values. In this case, that would mean 
#replacing 177 missing Age values with 29.7

summary(df.train$Age)
names(df.train)


boxplot(df.train$Age ~ df.train$Pclass, 
        main="passenger traveling class",
        xlab="Pclass", ylab="Age")

head(df.train$Name, n=10L)

## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   
df.train$Title <- getTitle(df.train)
unique(df.train$Title)


#To identify the titles which have at least one record with an age missing, 
#I'll use the bystats function from the Hmisc package.

options(digits=2)
require(Hmisc)
bystats(df.train$Age, df.train$Title, 
fun=function(x)c(Mean=mean(x),Median=median(x)))

## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

#then pass that list to the following custom function 
#I created for imputing the missing ages:
  
  imputeMedian <- function(impute.var, filter.var, var.levels) {
    for (v in var.levels) {
      impute.var[ which( filter.var == v)] <- impute(impute.var[ 
        which( filter.var == v)])}
    return (impute.var)}
  df.train$Age[which(df.train$Title=="Dr")]
  
  df.train$Age <- imputeMedian(df.train$Age, df.train$Title, titles.na.train)
  df.train$Age[which(df.train$Title=="Dr")]
  