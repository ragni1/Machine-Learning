# Author - Rageeni Sah

# Read data into a dataframe

df <- read.csv('RealEstate.csv')

# Data Exploratory
View(df)
dim(df)
str(df)
colnames(df) # 781 observations with 8 features
# Description about Dataset - It is RealEstate data and the problem is to identify price of a house given
#"MLS"   "Location"    "Price"       "Bedrooms"    "Bathrooms"   "Size"    "Price.SQ.Ft" "Status"   
anyNA(df) # No missing value

#install.packages('caret')
#install.packages('tableplot')
#install.packages('ade4')

library('caret')
library('tableplot')
library('ade4')

# Indentification of near zero variance predictors
nearZeroVar(df, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE,
            names = FALSE, foreach = FALSE, allowParallel = TRUE)
tableplot(df)

# Remove Categorical Variables
df$Location <- NULL
df$Status <- NULL

# Scaling
df$MLS <- scale(df$MLS)
df$Bedrooms <- as.factor(df$Bedrooms)
df$Bathrooms <- as.factor(df$Bathrooms)
df$Size <- scale(df$Size)
df$Price.SQ.Ft <- scale(df$Price.SQ.Ft)

#Partition the data into training and test dataset
set.seed(3456)
trainIndex <-createDataPartition(df$Price, times = 1, p = 0.8, list = FALSE)

train <- df[trainIndex,]
test <- df[-trainIndex,]

# Build a regression model on train data

model1 <- lm(formula = Price~., data = train)
summary(model1)
# Residual standard error: 138100 on 609 degrees of freedom
# Multiple R-squared:  0.8543,	Adjusted R-squared:  0.8502 
# F-statistic:   210 on 17 and 609 DF,  p-value: < 2.2e-16

#Predict the values for test data
pred <- predict.lm(model1, test)

#RMSE should be as low as possible but one should not do overfitting
RMSE(pred, test$Price)


