# Developer - Rageeni Sah
# Topic  - XGBoot - eXtream Gradient Boosting

#install.packages('xgboost')
library('xgboost')

# Loading data for regression 
df <- read.csv('RealEstate.csv')
# Exploratory Analysis
head(df)
anyNA(df)
dim(df)
str(df)

# Drop categorical values for this modeling
df$Location <- NULL
df$Status <- NULL

# Partition data into trainig and testing set
library('caret')
library('data.table')
trainIndex <- createDataPartition(df$Price,p = 0.8, list = FALSE)

train <- df[trainIndex,]
test <- df[-trainIndex,]

## Model Building
# install.packages('xgboost')
library('xgboost')

dtrain <- xgb.DMatrix(data = data.matrix(train[,-2]), label = data.matrix(train[,2]))
xgboostModel <- xgboost(dtrain, nrounds = 10, objective = "reg:linear")
cv <- xgb.cv(data = dtrain, nrounds = 10, objective = "reg:linear", nfold = 10, metrics = 'rmse')
print(cv)

# Top Important Features 
xgb.importance(feature_names = colnames(dtrain), model = xgboostModel)

# View the trees from a model
#install.packages('DiagrammeR')
library('DiagrammeR')
xgb.plot.tree(model = xgboostModel)
# View only the first tree in the XGBoost model
xgb.plot.tree(model = xgboostModel, n_first_tree = 1)


# Prediction on train data and test datasets
predtrain <- predict(xgboostModel, data.matrix(train[,-2]))
predtest <- predict(xgboostModel, data.matrix(test[,-2]))

# Append predicted values to the Dataframe
test$Predicted <- as.integer(predtest)
View(test)


# Model Performance
#install.packages('miscTools')
library('miscTools')
rSquared(train$Price, (predtrain-train$Price))
rSquared(test$Price,(predtest - test$Price))

