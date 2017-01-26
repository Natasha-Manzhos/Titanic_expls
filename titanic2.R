if (!require("Amelia")) install.packages("Amelia")
if (!require("vcd")) install.packages("vcd")
if (!require("corrgram")) install.packages("corrgram")
if (!require("caret")) install.packages("caret")
install.packages("ada")
library(caret)
library(Amelia)
library(vcd)
library(corrgram)
library(plyr)
rm(list=ls())
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
str(train.raw)
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
summary(df.train$Fare)

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

#The panel.conf function uses cor.test and calculates pearson correlations. 
#Confidence intervals are not available in cor.test for other methods (kendall, spearman).
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
#use the bystats function from the Hmisc package.
#options-Allow the user to set and examine a variety of global options which 
#affect the way in which R computes and displays its results.
#digits:controls the number of digits to print when printing numeric values. 
#It is a suggestion only. Valid values are 1...22 with default 7. 
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
  
  #apply the impute function from the Hmisc package on a per-title basis 
  #to assign the median of the available ages to the missing age(s). 
  #For example, the single record with a missing Age value and Title="Dr" 
  #will be assigned the median of the ages from the 6 records with Title="Dr" 
  #which do have age data.
  df.train$Age[which(df.train$Title=="Dr")]

  df.train$Age <- imputeMedian(df.train$Age, df.train$Title, titles.na.train)
  df.train$Age[which(df.train$Title=="Dr")]
  
  summary(df.train$Embarked)
  df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'
  summary(df.train$Fare)
  subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                   subset(df.train, Fare < 7)$Pclass), 
                             c("Age", "Title", "Pclass", "Fare")]  
  ## impute missings on Fare feature with median fare by Pclass
  df.train$Fare[ which( df.train$Fare == 0 )] <- NA
  df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                                as.numeric(levels(df.train$Pclass)))
  summary(df.train$Fare)
  
  
  
  df.train$Title <- factor(df.train$Title,
                           c("Capt","Col","Major","Sir","Lady","Rev",
                             "Dr","Don","Jonkheer","the Countess","Mrs",
                             "Ms","Mr","Mme","Mlle","Miss","Master"))
  boxplot(df.train$Age ~ df.train$Title, 
          main="Passenger Age by Title", xlab="Title", ylab="Age")
  
 
  
  #I created and applied a custom function for revaluing the titles, 
  #then reclassified Title to a factor type, as follows:
  
  ## function for assigning a new title value to old title(s) 
  ## function for assigning a new title value to old title(s) 
  changeTitles <- function(data, old.titles, new.title) {
    for (honorific in old.titles) {
      data$Title[ which( data$Title == honorific)] <- new.title
    }
    return (data$Title)
  }
  ## Title consolidation
  df.train$Title <- as.character(df.train$Title)
  df.train$Title <- changeTitles(df.train, 
                                 c("Capt", "Col", "Don", "Dr", 
                                   "Jonkheer", "Lady", "Major", 
                                   "Rev", "Sir"),
                                 "Noble")
  df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                                 "Mrs")
  df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
  df.train$Title <- as.factor(df.train$Title)
  df.train$Title
  
  require(plyr)     # for the revalue function 
  require(stringr)  # for the str_sub function
  
  ## test a character as an EVEN single digit
  isEven <- function(x) x %in% c("0","2","4","6","8") 
  ## test a character as an ODD single digit
  isOdd <- function(x) x %in% c("1","3","5","7","9") 
  ## function to add features to training or test data frames
  featureEngrg <- function(data) {
    ## Using Fate ILO Survived because term is shorter and just sounds good
    data$Fate <- data$Survived
    ## Revaluing Fate factor to ease assessment of confusion matrices later
    data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
    ## Boat.dibs attempts to capture the "women and children first"
    ## policy in one feature.  Assuming all females plus males under 15
    ## got "dibs' on access to a lifeboat
    data$Boat.dibs <- "No"
    data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
    data$Boat.dibs <- as.factor(data$Boat.dibs)
    ## Family consolidates siblings and spouses (SibSp) plus
    ## parents and children (Parch) into one feature
    data$Family <- data$SibSp + data$Parch
    ## Fare.pp attempts to adjust group purchases by size of family
    data$Fare.pp <- data$Fare/(data$Family + 1)
    ## Giving the traveling class feature a new look
    data$Class <- data$Pclass
    data$Class <- revalue(data$Class, 
                          c("1"="First", "2"="Second", "3"="Third"))
    ## First character in Cabin number represents the Deck 
    data$Deck <- substring(data$Cabin, 1, 1)
    data$Deck[ which( is.na(data$Deck ))] <- "UNK"
    data$Deck <- as.factor(data$Deck)
    ## Odd-numbered cabins were reportedly on the port side of the ship
    ## Even-numbered cabins assigned Side="starboard"
    data$cabin.last.digit <- str_sub(data$Cabin, -1)
    data$Side <- "UNK"
    data$Side[which(isEven(data$cabin.last.digit))] <- "port"
    data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
    data$Side <- as.factor(data$Side)
    data$cabin.last.digit <- NULL
    return (data)
  }
  
  ## add remaining features to training data frame
  df.train <- featureEngrg(df.train)
  train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                   "Class", "Deck", "Side", "Fare", "Fare.pp", 
                   "Embarked", "Family")
  df.train.munged <- df.train[train.keeps]
  
  #Fitting a Model
  ## split training data into train batch and test batch. ????:df.train.munged$Survived
  set.seed(23)
  training.rows <- createDataPartition(df.train.munged$Fate, p = 0.8, list = FALSE)
  train.batch <- df.train.munged[training.rows, ]
  test.batch <- df.train.munged[-training.rows, ]
  
  # Logistic regression.By setting the argument family to binomial with a logit link, 
  #I'm asking glm( ) to produce a logistic regression.Family objects provide 
  #a convenient way to specify the details of the models used by functions such as glm. 
  #See the documentation for glm for the details on how such model fitting takes place.
  Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                         data = train.batch, family=binomial("logit"))
  
  # To assess this first model and the various binary logistic regressions that
  # will appear in its wake, we will use the chi-square statistic, which is
  # basically a measure of the goodness of fit of observed values to expected
  # values. The bigger the difference (or deviance) of the observed values from
  # the expected values, the poorer the fit of the model. The null deviance
  # shows how well passenger survival is predicted by a "null" model using only
  # a constant (grand mean). As we adjust the model's formula by adding and/or
  # removing variables, we'll look for those changes which prompt a drop in the
  # residual deviance, indicating an improvement in fit.
  Titanic.logit.1
  # The deviance was reduced by 332.2 (320)! points on 713-705=8 degrees of freedom
  # (DF), a significant reduction...
  951-631
  1 - pchisq(320, df=8)
  
  #Calling anova(), an extractor function, generates the results of the analysis.
  
  anova(Titanic.logit.1, test="Chisq")
  
  Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                         data = train.batch, family=binomial("logit"))
  anova(Titanic.logit.2, test="Chisq")
  
  glm(Fate ~ Sex + Class + Age + Family + Embarked, 
      data = train.batch, family=binomial("logit"))
  ## Define control function to handle optional arguments for train function
  ## Models to be assessed based on largest absolute area under ROC curve
  cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE)
  # 
  # Below is the train function call using the same formula (sans Fare) that we
  # recently passed through glm function. I use the metric argument to tell
  # train to optimize the model by maximizing the area under the ROC curve
  # (AUC). summary(), another extractor function, is called to generate
  # regression coefficients with standard errors and a z-test, plus the residual
  # deviance metric we were watching earlier.
  
  set.seed(35)
  glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                      data = train.batch,
                      method = "glm",
                      metric = "ROC",
                      trControl = cv.ctrl)
  glm.tune.1
  
  summary(glm.tune.1)
  
  # This is as good a time as any to introduce the concept of class compression.
  # Think of it as collapsing particular levels on a categorical variable. One
  # of the earlier bar graphs showed about 70 percent of the Titanic's
  # passengers boarded the ship at Southampton. I'm going to use Embarked and
  # the I() function, which inhibits interpretation & conversion of R objects,
  # to create a new 2-level factor within the model formula. This factor is
  # valued TRUE if a passenger's port of origin was Southampton ("S"), or FALSE
  # otherwise.

  set.seed(35)
  glm.tune.2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked=="S"),
  data = train.batch, method = "glm",
  metric = "ROC", trControl = cv.ctrl)
  summary(glm.tune.2)
  
  # As I discussed earlier, the Title feature addresses more than one theme. For
  # that reason, I believe it has real potential to improve this model. Besides,
  # I put a good chunk of effort into it, so why not give it a go?
  
  set.seed(35)
  glm.tune.3 <- train(Fate ~ Sex + Class + Age + Title + Family + I(Embarked=="S"),
  data = train.batch, method = "glm",
  metric = "ROC", trControl = cv.ctrl)
  summary(glm.tune.3)
  
  set.seed(35)
  glm.tune.4 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                        + Age + Family + I(Embarked=="S"), 
                        data = train.batch, method = "glm",
                        metric = "ROC", trControl = cv.ctrl)
  summary(glm.tune.4)
  
  set.seed(35)
glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                        + Age + Family + I(Embarked=="S") 
                        + I(Title=="Mr"&Class=="Third"), 
                        data = train.batch, 
                        method = "glm", metric = "ROC", 
                        trControl = cv.ctrl)
summary(glm.tune.5)
  
#Other Models

#First up is boosting. I can instruct train to fit a stochastic boosting model
#for the binary response Fate using the adapackage and a range of values for
#each of three tuning parameters. Concretely, when fitting a model using train
#with method=”ada”, one has three levers to tweak: iter (number of boosting
#iterations, default=50), maxdepth (depth of trees), and nu (shrinkage
#parameter, default=1). Create a data frame with these three variables as column
#names and one row per tuning variable combination, and you're good to go. Here
#is just one example of a tuning grid for ada:
## note the dot preceding each variable
ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)
library(ada)
library(rpart)
ada.tune
plot(ada.tune)
