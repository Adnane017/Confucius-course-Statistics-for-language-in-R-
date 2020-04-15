###########################################################
#
# Statistics for linguists: tutorial in R (with solutions)
#
# File Created on: 29th November, 2017
# Created by: Adnane Ez-zizi
#             a.ez-zizi@bham.ac.uk
# 
# The examples are based on Bresnan et al's (2007) paper, 
# Baayen's book "Analysing Linguistic Data", Levshina's 
# book "How to do Linuistics with R" and James et al's book 
# "An introduction to statistical learning"
#
# This file contains all the R code that appear in the 
# handout as well as answers to the questions that you 
# needed to complete
#
# Comments are inserted throughout the file to help users 
# know what each section of code is designed to do.
#
###########################################################

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
# First let's save the data frame verbs as a text file
write.table(verbs, "dative.txt")
rm(verbs) # remove the dataset from the workspace
verbs = read.table("dative.txt", header = TRUE) # Important the dataset

### Load the dataset from a csv file (comma-separated spreadsheet data)  
# First let's save the data frame verbs as a text file
write.csv(verbs, "dative.csv", row.names = FALSE)
rm(verbs) # remove the dataset from the workspace
verbs = read.csv("dative.csv", header = TRUE) # Important the dataset

###########################################
# Data Description
###########################################

# Get information about the dataset
?verbs
dim(verbs)
names(verbs)

###########################################
# Data Manipulation
###########################################

### Accessing information in the dataset
# extract the 10th observation
verbs[10, ]
# extract the verb of the 10th observation
verbs[10, 2]
# Alternatively, you can write the name of the variable directly
verbs[10, "Verb"] 
# extract the logarithm of the theme length for the 10th observation
verbs[10, "LengthOfTheme"]
# To get the length 
exp(verbs[10, "LengthOfTheme"])

# To extract information from multiple rows
row.select <- c(10, 97, 433, 901) # Define the vector of row numbers we will use
row.select
verbs[row.select, ] # Display data for the selected rows
verbs[row.select, "LengthOfTheme"] # Display the values of "LengthOfTheme" for the selected rows
verbs[row.select, 5] # same 
verbs[row.select, 1:3] # data for the first 3 variables
verbs[row.select, c("RealizationOfRec", "Verb", "AnimacyOfRec")] # same
# We can also sum the lengths of theme for the selected rows 
sum(verbs[row.select, "LengthOfTheme"])
# Or compute the average length of theme for the selected rows
mean(verbs[row.select, "LengthOfTheme"])

# To extract observations that verify a certain condition
verbs[verbs$AnimacyOfTheme == "animate", ] # select those for which AnimacyOfTheme has the value "animate"
# Alternatively:
subset(verbs, AnimacyOfTheme == "animate")
# You can also use more complex conditions
verbs[(verbs$AnimacyOfTheme=="animate") & (verbs$LengthOfTheme>2), ] 

###########################################
# Exploratory Data Analysis
###########################################

### Summary statistics 
summary(verbs)
# Summary of a qualitative variable
summary(verbs$Verb) 
# Summary of a quantitative variable
summary(verbs$LengthOfTheme) 
# Measure of central tendency
mean(verbs$LengthOfTheme) # average length
median(verbs$LengthOfTheme) # median length (the length value below which we find 50% of the lengths)
# Measure of dispersion
min(verbs$LengthOfTheme) # minimum length
max(verbs$LengthOfTheme) # maximum length
quantile(verbs$LengthOfTheme, 0.25) # first quartile (the length value below which we find 25% of the lengths)
quantile(verbs$LengthOfTheme, 0.75) # third quartile (the length value below which we find 75% of the lengths)
max(verbs$LengthOfTheme) - min(verbs$LengthOfTheme) # range
quantile(verbs$LengthOfTheme, 0.75) - quantile(verbs$LengthOfTheme, 0.25) # Interquartile-range
var(verbs$LengthOfTheme) # variance
sd(verbs$LengthOfTheme) # standard deviation

# Frequency table for the dependent variable
table(verbs$RealizationOfRec)

# Contingency tables
# Setting default number of decimal places when displaying values
options(digits=2)
table(verbs$RealizationOfRec, verbs$AnimacyOfRec)
xtabs(~ RealizationOfRec + AnimacyOfRec, data = verbs) # same
?xtabs # To see all possible options with xtabs
xtabs(~ AnimacyOfRec + AnimacyOfTheme + RealizationOfRec, data = verbs) # with more than two factors
# IF we want to concentrate on inanimate themes as the animate ones are very rare
sub.inanimate = (verbs$AnimacyOfTheme != "animate")
verbs.xtabs = xtabs(~ AnimacyOfRec + RealizationOfRec, 
                    data = verbs,
                    subset = sub.inanimate)
verbs.xtabs # Display the contingency table
# To display the results in term of proportions
verbs.xtabs / sum(verbs.xtabs) # The sum of all the values is 1.
# To display the results in term of percentages
(verbs.xtabs/sum(verbs.xtabs)) * 100
# To display the results in term of relative frequencies with respect to the row totals
verbs.xtabs/rowSums(verbs.xtabs)
# Or similarly
prop.table(verbs.xtabs, 1) # Here the rows sum to one
# We can also produce relative frequencies with respect to the column totals
prop.table(verbs.xtabs, 2) # Here the columns sum to one

###########################################
# Graphical Data Exploration
###########################################

# Upload the full dative data: familiarize yourself with the dataset
data("dative")
head(dative, 10)
?dative # Learn more about the data

## Visualisation of one categorical variable
# Proportion table for the dependent variable
outcome.tab = table(dative$RealizationOfRecipient) # frequenct table 
outcome.percentage = prop.table(outcome.tab) * 100 # Trnasform it into a proportion table
outcome.percentage = round(outcome.percentage , 1) # To display only up to one decimal 
outcome.percentage

# Bar plot 
par(mar=c(5,5,5,5)) # set the margin (useful for other things)
barplot(outcome.percentage, main = "Bar plot of dative alternation category",
        xlab = "dative category", ylab = "Percentage (%)", col = "grey50", 
        ylim = c(0,100), cex.names = 1.2, space=0.25)
grid()
par(mar=c(5.1, 4.1, 4.1, 2.1)) # Return to the default margin

### Visualisation of two categorical variables

# Let's visualize the contingency table for the counts of clauses
# cross-classified by the animacy of the recipient and the realization
# of the recipient (NP versus PP), using a bar plot
dative.xtabs = xtabs(~ AnimacyOfRec + RealizationOfRecipient, 
                    data = dative,
                    subset = (AnimacyOfTheme != "animate"))
dative.xtabs 

# Stacked bar plot
barplot(dative.xtabs, main = "Stacked bar plot of clause counts", ylab = "count", 
        ylim = c(0,2500), legend.text = c("animate", "inanimate"), space=0.25)
grid()

# Side-by-side bar plot
barplot(dative.xtabs, main = "Side by side bar plot of clause counts", ylab = "count", 
        ylim = c(0,2500), legend.text = rownames((dative.xtabs)), beside = T)
grid()

# Put the two bar plots in the same window
windows()
par(mfrow = c(1,2), mar=c(5,5,5,5))
barplot(dative.xtabs, ylab = "count", ylim = c(0,2500), legend.text = c("animate", "inanimate"), 
        args.legend = list(x = "topright", bty="n", inset=c(-0.3,0)))
grid()
barplot(dative.xtabs, ylab = "count", ylim = c(0,2500), legend.text = rownames((dative.xtabs)), 
        beside = T, args.legend = list(x = "topright", bty="n", inset=c(-0.3,0)))
grid()
par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

# As in Figure 1 (page 75), plot the contingency table for the counts of realization of the 
#recipient cross-classified by the accessibility of receiver 
#(i.e., recipient) and entity (i.e., theme)
dative.xtabs = xtabs(~ AccessOfRec + AccessOfTheme+ RealizationOfRecipient, 
                     data = dative)
dative.xtabs
# Create a contingency table for NP NP
xtabs_receiverNP =  rowSums(dative.xtabs[, , 1]) 
xtabs_themeNP =  colSums(dative.xtabs[, , 1]) 
xtabs_NP = cbind(xtabs_receiverNP, xtabs_themeNP)
xtabs_NP = xtabs_NP[c("given", "accessible", "new"), ] # Change the rows order to get to get the same plot as in fig1
colnames(xtabs_NP) = c("Receiver", "Entity")
xtabs_NP

# Create a contingency table for NP PP
xtabs_receiverPP =  rowSums(dative.xtabs[, , 2]) 
xtabs_themePP =  colSums(dative.xtabs[, , 2]) 
xtabs_PP = cbind(xtabs_receiverPP, xtabs_themePP)
xtabs_PP = xtabs_PP[c("given", "accessible", "new"), ] # Change the columns order to get to get the same plot as in fig1
colnames(xtabs_PP) = c("Receiver", "Entity")
xtabs_PP

# Make the bar plot
windows()
par(mfrow = c(1,2), mar=c(5,5,4,5))
barplot(xtabs_NP, beside = T, ylim = c(0,2500), main = "NP NP", ylab = "count",
        legend.text = c("given", "accessible", "new"), 
        args.legend = list(x = "topright", bty="n", inset=c(-0.3,0)))
grid()
barplot(xtabs_PP, beside = T, ylim = c(0,2500), main = "NP PP", ylab = "count", 
        legend.text = c("given", "accessible", "new"), 
        args.legend = list(x = "topright", bty="n", inset=c(-0.3,0)))
grid()
par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

#############
# Question 1
#############

### Make a figure similar to Figure 2 (page 76) in the paper 
### Here you need to recode the variables "AccessOfRec" and "AccessOfTheme"
### by combining the categories "accessible" and "new" into a new category
### "nongiven"

### Solution: 
# For AccessOfRec
levels(dative$AccessOfRec)[(levels(dative$AccessOfRec)=="accessible")] = "nongiven"
levels(dative$AccessOfRec)[(levels(dative$AccessOfRec)=="new")] = "nongiven"
# For AccessOfTheme
levels(dative$AccessOfTheme)[(levels(dative$AccessOfTheme)=="accessible")] = "nongiven"
levels(dative$AccessOfTheme)[(levels(dative$AccessOfTheme)=="new")] = "nongiven"

# Construct the new contingency table
dative.xtabs = xtabs(~ AccessOfRec + AccessOfTheme+ RealizationOfRecipient, 
                     data = dative)
dative.xtabs
# Create a contingency table for NP NP
xtabs_receiverNP =  rowSums(dative.xtabs[, , 1]) 
xtabs_themeNP =  colSums(dative.xtabs[, , 1]) 
xtabs_NP = cbind(xtabs_receiverNP, xtabs_themeNP)
xtabs_NP = xtabs_NP[c("given", "nongiven"), ] # Change the rows order to get to get the same plot as in fig1
colnames(xtabs_NP) = c("Receiver", "Entity")
xtabs_NP
# Create a contingency table for NP PP
xtabs_receiverPP =  rowSums(dative.xtabs[, , 2]) 
xtabs_themePP =  colSums(dative.xtabs[, , 2]) 
xtabs_PP = cbind(xtabs_receiverPP, xtabs_themePP)
xtabs_PP = xtabs_PP[c("given", "nongiven"), ] # Change the rows order to get to get the same plot as in fig1
colnames(xtabs_PP) = c("Receiver", "Entity")
xtabs_PP

# Makine the bar plot
windows()
par(mfrow = c(1,2), mar=c(5,5,4,5))
barplot(xtabs_NP, beside = T, ylim = c(0,2500), main = "NP NP", ylab = "count")
grid()
barplot(xtabs_PP, beside = T, ylim = c(0,2500), main = "NP PP", ylab = "count", 
        legend.text = c("given", "non-given"), 
        args.legend = list(x = "topright", bty="n", inset=c(-0.3,0)))
grid()
par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

#############

### Visualisation of one quantitative variable
# Histogram
hist(dative$LengthOfRecipient, main = "Histogram of recipient length",
     xlab = "Length of recipient")
hist(dative$LengthOfRecipient, main = "Histogram of recipient length",
     xlab = "Length of recipient", breaks = max(dative$LengthOfRecipient))

# Question: Compare the histograms of recipient length and theme length 
# Solution:
par(mfrow=c(1,2), oma = c(0, 0, 2.5, 0)) # oma gives the size of the outer margin area
hist(dative$LengthOfRecipient, main = "", ylim = c(0, 3000),
     xlab = "Length of recipient", breaks = max(dative$LengthOfRecipient))
hist(dative$LengthOfTheme, main = "", ylim = c(0, 3000),
     xlab = "Length of theme", breaks = max(dative$LengthOfTheme))
mtext("Histogram of length of recipient and theme", outer = TRUE, cex = 1.2)
par(mfrow=c(1,1))

# Box plot of theme length
boxplot(log(dative$LengthOfTheme), main = "Box plot of theme lengths", 
        ylab = "log of theme length")
grid()

### Visualisation of two quantitative variables
# Comparing LengthOfTheme and LengthOfRecipient
boxplot(log(dative$LengthOfTheme), log(dative$LengthOfRecipient), 
        main = "Box plot of theme length and recipient length", 
        names = c("Theme length","Recipient length"), ylab = "log length")

# QQ-plot to test the normality assumption
qqnorm(log(dative$LengthOfTheme)-log(dative$LengthOfRecipient))
qqline(log(dative$LengthOfTheme)-log(dative$LengthOfRecipient))

# paired-test
t.test(log(dative$LengthOfTheme), log(dative$LengthOfRecipient), paired = TRUE)

#############
# Question 2
#############

# Use Wilcoxon-test to see if you will still get a significance 
# result for the difference between log lengths of theme and recipient.

# Solution:
wilcox.test(log(dative$LengthOfTheme), log(dative$LengthOfRecipient), 
            paired = TRUE)

#############

# Correlation between LengthOfTheme and LengthOfRecipient
cor(dative$LengthOfTheme, dative$LengthOfRecipient)

# Scatterplot bewteen two variables
plot(dative$LengthOfRecipient, dative$LengthOfTheme, 
     main = "Relationship between length of recipient and of theme", 
     xlab = "Recipient length", ylab = "Theme length")
lines(lowess(dative$LengthOfRecipient, dative$LengthOfTheme), col = "red")

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
# Fit
?glm
logReg.fit = glm(RealizationOfRecipient ~ LogLengthDiff, data = dative.sp, 
               family = binomial)
summary(logReg.fit) # Explain outputs
# Let's look at the coefficients
coef(logReg.fit) # Get only the estimates of the coeffs
### the estimate of the intercept is -1.9, which is the log odds of the outcome 
### (i.e., log[p(outcome=1)/1-p(outcome=1)]) when the predictor "LogLengthDiff"
### is equal to 0 (i.e., when the length of the recipient is equal to the length
### of the theme). 
### "outcome = 0" corresponds to PP and "outcome = 1" corresponds to NP. 
### To check the coding used by the model:
contrasts(dative.sp$RealizationOfRecipient) # Coding for the dummy variable associated with the response variable "RealizationOfRecipient"
# To obtain simple odds, one should exponentiate the coefficient:
exp(coef(logReg.fit))
### Interpretation: 
### 1) Intercept coeff: the chances of "PP" are 0.14 times greater than those of "NP". 
###    In other words, the chances of "PP" are 7.1 (=1/0.14) times lower than those of "NP" 
### 2) predictor coeff: (keeping all other variables constant) the odds of "PP" (outcome=1) 
###    are 12.08 higher when the log of Length difference increases by one unit
# Give an example of probability computation using the coefficients 
#(e.g., P(dative=PP|LogLengthDiff=1))

# Predicting the probability of choosing the prepositional dative (PP)
logReg.probs = predict(logReg.fit, type="response")
logReg.probs[1:10] # estimating the probability of selecting the PP dative for the first 10 constructions

# Ploting the probability of response as a function of the predictor
RealOfRec.coded = rep(0, nrow(dative.sp))
RealOfRec.coded[(dative.sp$RealizationOfRecipient=="PP")] = 1
plot(dative.sp$LogLengthDiff, RealOfRec.coded, 
     xlab="length difference (log scale)", ylab="Probability of prepositional dative") 
xpoints = seq(min(dative.sp$LogLengthDiff), max(dative.sp$LogLengthDiff), length.out = 1000)
ypoints = predict(logReg.fit, data.frame(LogLengthDiff = xpoints), type="resp")
lines(xpoints, ypoints, lwd = 4, lty = 1, type = "l", col="red")
# same plot but with a histogram added (see )
install.packages("popbio")
library(popbio)
logi.hist.plot(dative.sp$LogLengthDiff, RealOfRec.coded, boxp=FALSE, type="hist", 
               col="gray")
?logi.hist.plot # for more details

## Assessing the goodness of fit of the model: 

# 1) Significance of the coefficients
# displaying the coefficients along with p-values
summary(logReg.fit)$coef # Display the coefficients along with the p-values
summary(logReg.fit)$coef[,4] # to get the p-value associated with each coeff
### Both coefficients are significant meaning that we reject the hypotheses that they are equal to 0

# 2) Deviance: a measure of the unexplained variation
logReg.fit$deviance 
### shows how well the response variable is predicted by our model. The lower the 
###value the better the fit
logReg.fit$null.deviance 
### shows how well the response variable is predicted by a model that includes only the intercept (grand mean)
### By comparing the two deviances, we can see that including the predictor log(LengthOfRecipient)
### improved the fit

# 3) Akaike information criterion (AIC): 
logReg.fit$aic 
### another measure of goodness of fit. But this measure is used only to compare
### different models---that is, to be able to say which of several models is the best.
### The lower the AIC value the better the model.

# 4) Classification table: comparing the true and predicted outcomes 
# Predicting the class labels
logReg.pred = rep("NP", 2360)
logReg.pred[logReg.probs >.5 ] = "PP"
logReg.pred[1:10] # Predicted outcomes for the first 10 constructions
logReg.probs[1:10] # probability of selecting the PP dative for the first 10 constructions
table(dative.sp$RealizationOfRecipient, logReg.pred) # Confusion matrix to determine how many obs were incorrectly classified
(1822+182)/2360 # Accuracy rate of classification
mean(logReg.pred == dative.sp$RealizationOfRecipient) # Another way to compute the accuracy rate of classification
# Correct from always guessing NP NP (=0)
mean((dative.sp$RealizationOfRecipient=="NP"))
# Comparing the true and fitted values graphically

###########################################
# Model Fit: Multiple Logitic Regression
###########################################

### Multiple logistic regression model (Model A in the paper, see page 78)
# Fit
MultiLogReg.fit = glm(RealizationOfRecipient ~ ., data = subset(dative.sp, select=c(-Speaker,
                          -Modality, -Verb, -LengthOfTheme, -LengthOfRecipient)), 
                          family = binomial)
summary(MultiLogReg.fit) # Explain outputs
contrasts(dative.sp$SemanticClass)

## Comparison between the simple and multiple logistic regression model

# Compare the deviances
Dev_comp = c(simpleLogReg = logReg.fit$deviance, multipleLogReg = MultiLogReg.fit$deviance)
Dev_comp

# Compare the AICs
AIC_comp = c(simpleLogReg = logReg.fit$aic, multipleLogReg = MultiLogReg.fit$aic)
AIC_comp

# Accuracy
# Predicting the probability of choosing the prepositional dative (PP)
MultiLogReg.probs = predict(MultiLogReg.fit, type="response")
# Predicting the class labels
MultiLogReg.pred = rep("NP", 2360)
MultiLogReg.pred[MultiLogReg.probs >.5 ] = "PP"
# Accuracy table
table(dative.sp$RealizationOfRecipient, MultiLogReg.pred) # Confusion matrix to determine how many obs were incorrectly classified
# Accuracy rate of classification
mean(MultiLogReg.pred == dative.sp$RealizationOfRecipient)
# Correct from always guessing NP NP (=0)
mean((dative.sp$RealizationOfRecipient=="NP"))

#############
# Question 3
#############

# Write down the formula of the model as was done in Figure 4 in 
# Bresnan et al. (2007)'s paper.

# Solution:
# p = 1.40
#   - 1.35{c} + 0.46{f} - 3.44{p} + 1.09{t}
#   - 1.10{accessibility of recipient = given}
#   + 1.21{accessibility of theme = given}
#   - 1.48{pronominality of recipient = pronoun}
#   + 1.65{pronominality of theme = pronoun}
#   - 0.85{definiteness of recipient = indefinite}
#   - 1.16{definiteness of theme = indefinite}
#   - 1.22{animacy of theme = inanimate}
#   + 2.67{animacy of recipient = inanimate}
#   - 0.94.length difference (log scale)

# Note that this is not the same model that Bresnan and colleagues
# used in their paper since they had more variables. They also chose
# different reference levels for some of the categorical variables
# (e.g. the reference level for accessibility of recipient/them was 
# nongiven for them while here we kept it as given)

############################################################################
# The validation set approach (Extra materials not included in the handout)
############################################################################

# At first glance, it appears that the multiple logistic regression model is working
# well. However, this result might be misleading because we trained and tested the 
# model on the same set of 2360 observations. In other words, 100 - 92 = 8% is the
# training error rate, which is often overly optimistic (tends to underestimate 
# the test error rate).In order to better assess the accuracy of the logistic 
# regression model in this setting, we can fit the model using part of the data, then 
# examine how well it predicts the held out data. This will yield a more realistic 
# error rate, in the sense that in practice we will be interested in our model's 
# performance not on the data that we used to fit the model, but rather on new dative 
# constructions

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
