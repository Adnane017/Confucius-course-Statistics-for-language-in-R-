###################################################
# Dative Alternation Dataset Example
#
# File Created on: 29th November, 2017
# Created by: Adnane Ez-zizi
#             a.ez-zizi@sheffield.ac.uk
# 
# Logistic Regression Example in R based on Baayen's book "Analysing Linguistic Data",
# Bresnan et al's (2007) paper, Levshina's book "How to do Linuistics with R" and 
# James et al's book "An introduction to statistical learning"
#
# This file need to be completed using the code provided in the handout, except for 
# the questions, which you need to solve.
#
# Comments are inserted throughout the file to help users know what each
# section of code is designed to do.
###################################################

###########################################
# Loading The Necessary R Libraries
###########################################

### Install the necessary libraries
install.packages("languageR")

### Load the necessary libraries
library(languageR) # contains the dative dataset

###########################################
# Loading datasets
###########################################

### Load the dataset from the "languageR" package 
data(verbs)
head(verbs, 10)

### Load the dataset from a text file  





###########################################
# Data Description
###########################################









###########################################
# Data Manipulation
###########################################









###########################################
# Exploratory Data Analysis
###########################################









###########################################
# Graphical Data Exploration
###########################################

# Load the full dative data: familiarize yourself with the dataset
data("dative")
head(dative, 10)
?dative # Learn more about the data

### Visualisation of one categorical variable




### Visualisation of two categorical variables




########################################################################
### Question: Make a figure similar to Figure 2 (page 76) in the paper 
### Here you need to recode the variables "AccessOfRec" and "AccessOfTheme"
### by combining the categories "accessible" and "new" into a new category
### "nongiven"

### Solution: (need to be completed)




########################################################################


### Visualisation of one quantitative variable


########################################################################
### Question: Compare the histograms of recipient length and theme length 

### Solution: (need to be completed)




########################################################################



### Visualisation of two quantitative variables


########################################################################
# Question: Use Wilcoxon-test to see if you will still get a significance 
# result for the difference between log lengths of theme and recipient.

### Solution: (need to be completed)





########################################################################



###########################################
# Model Fit: Simple Logitic Regression
###########################################

### Modify the dative data to match the data used in Bresnan's paper
## Important the dataset
data(dative)
## Modification1: Keep only spoken dative constructions as in the paper
dative.sp = dative[(dative$Modality=="spoken"), ]
row.names(dative.sp) = 1:nrow(dative.sp)
## Modification2: Combine the categories "accessible" and "new" into"nongiven"
# For AccessOfRec
levels(dative.sp$AccessOfRec)[(levels(dative.sp$AccessOfRec)=="accessible")] = "nongiven"
levels(dative.sp$AccessOfRec)[(levels(dative.sp$AccessOfRec)=="new")] = "nongiven"
# For AccessOfTheme
levels(dative.sp$AccessOfTheme)[(levels(dative.sp$AccessOfTheme)=="accessible")] = "nongiven"
levels(dative.sp$AccessOfTheme)[(levels(dative.sp$AccessOfTheme)=="new")] = "nongiven"
## Modification3: Create a new variable that encodes the difference between the
##length of theme and that of recipient
dative.sp$LogLengthDiff =  log(dative.sp$LengthOfTheme) - log(dative.sp$LengthOfRecipient)

### Simple logistic regression model with one independent variable (length of theme)


## Assessing the goodness of fit of the model: 

# 1) Significance of the coefficients


# 2) Deviance: a measure of the unexplained variation


# 3) Akaike information criterion (AIC): 


# 4) Classification table: comparing the true and predicted outcomes 


###########################################
# Model Fit: Multiple Logitic Regression
###########################################

### Multiple logistic regression model (Model A in the paper, see page 78)




########################################################################
### Question: Write down the formula of the model as was done in Figure 4
### in Bresnan et al's (2007) paper.

### Solution: (need to be completed)



########################################################################


###########################################
# The validation set approach (optional)
###########################################

# At first glance, it appears that the multiple logistic regression model is working
# well. However, this result might be misleading because we trained and tested the 
# model on the same set of 2360 observations. In other words, 100 − 92 = 8% is the
# training error rate, which is often overly optimistic (tends to underestimate 
# the test error rate).
# In order to better assess the accuracy of the logistic regression model in this 
# setting, we can fit the model using part of the data, and then examine how well 
# it predicts the held out data. This will yield a more realistic error rate, in 
# the sense that in practice we will be interested in our model’s performance not 
# on the data that we used to fit the model, but rather on new dative constructions

# Dividing the dataset into a training set and a test set
# Randomly select 2000 training constructions as in the paper (see page 80). 
# The remaining 360 will constitute the test set.
set.seed(1) # set the seed for generating random randoms to make sure to 
             #replicate the same results each time you run the program
# Construct the test dataset
train = sample(2360, 2000)
dative.train = dative.sp[train, ]
# Construct the test dataset
dative.test = dative.sp[-train, ]
# Construct the test response variable
RealOfRec.test = dative.sp$RealizationOfRecipient[-train]

# Model fit (with all predictors)
MLR_valid.fit = glm(RealizationOfRecipient ~ ., data = subset(dative.sp, select=c(-Speaker,
               -Modality, -Verb, -LengthOfTheme, -LengthOfRecipient)), 
               family = binomial, subset = train)
summary(MLR_valid.fit)

# Comparing model predictions with true values
MLR_valid.probs = predict(MLR_valid.fit, dative.test, type="response")
MLR_valid.pred = rep("NP", 360)
MLR_valid.pred[MLR_valid.probs>.5] = "PP"
table(MLR_valid.pred, RealOfRec.test) # Confusion matrix to determine how many obs were incorrectly classified
mean(MLR_valid.pred == RealOfRec.test) # Accuracy rate
mean(MLR_valid.pred != RealOfRec.test) # Error rate

### the k-fold cross-validation approach

# Creating the k (=100) folds
set.seed(1)
K = 100 # Number of folds
folds = sample(1:K, nrow(dative.sp), replace=TRUE) # Creating the k folds

# computing the average accuracy rate on each fold
cv.acc.100 = rep(0, K)
for(k in 1:K){ # This bit might take a few minutes to run
  glm.fit = glm(RealizationOfRecipient ~ ., data = subset(dative.sp[(folds!=k), ], 
            select = c(-Speaker, -Modality, -Verb, -LengthOfTheme, -LengthOfRecipient)), 
            family = binomial)
  # Accuracy rate on the current fold
  glm.probs = predict(glm.fit, dative.sp[(folds==k), ], type="response")
  glm.pred = rep("NP", length(glm.probs))
  glm.pred[glm.probs>.5] = "PP"
  cv.acc.100[k] = mean(glm.pred == dative.sp$RealizationOfRecipient[folds==k])
}
# Grand average accuracy rate on useen data (across the 100 CV simulations) 
mean.cv.acc = mean(cv.acc.100)
mean.cv.acc # Compare it with the value given in the paper

# Another cross-validation error measure (theoretical)
install.packages(boot)
library(boot)
glm.fit = glm(RealizationOfRecipient ~ ., data = subset(dative.sp, 
           select = c(-Speaker, -Modality, -Verb, -LengthOfTheme, -LengthOfRecipient)), 
           family = binomial)
cv.error.10 = cv.glm(data = subset(dative.sp, select = c(-Speaker, -Modality, 
                -Verb, -LengthOfTheme, -LengthOfRecipient)), glm.fit, K=10)$delta[1]
cv.error.10
