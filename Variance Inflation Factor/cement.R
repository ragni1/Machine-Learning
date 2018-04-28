# Developer - Rageeni Sah

## Concept:- Variance Inflation Factor (VIF) gives measure of inflation of variance of an independent variable 
#            in presence of other correlated independent varibales. It is a great tool to check multicollinearity.

df <- read.table('cement.txt', header = TRUE)
head(df)
dim(df)
str(df)
anyNA(df)
df$Pt<-NULL
head(df)
## BP -> Target Variable, Age, Weight, BSA, DUr, pulse ans Stress are responses.

corr <- cor(df[,-1], method = 'pearson')
corr
#install.packages("ggcorrplot")
library('ggcorrplot')
ggcorrplot(corr)
## Correlation matrix depicts following details.
# 1. Highly correlated features: x1-x3, x2-x4
# 2. Moderately correlated features: 
# 3. Lessly correlated features: 

## Modeling
#install.packages('car')
library('car')
vif(lm(y~., data = df))
summary(lm(y~., data = df))
## Summar vif
#x1        x2        x3        x4 
#38.49621 254.42317  46.86839 282.51286  
## vif values of each predictors show they are higly multicorrelated. 
#  vif>5 indicates high multicolinearity
#summmar lm
#lm(formula = y ~ ., data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1750 -1.6709  0.2508  1.3783  3.9254 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  62.4054    70.0710   0.891   0.3991  
# x1            1.5511     0.7448   2.083   0.0708 .
# x2            0.5102     0.7238   0.705   0.5009  
# x3            0.1019     0.7547   0.135   0.8959  
# x4           -0.1441     0.7091  -0.203   0.8441  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.446 on 8 degrees of freedom
# Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9736 
# F-statistic: 111.5 on 4 and 8 DF,  p-value: 4.756e-07


# Modeling without x4
vif(lm(y~x1 + x2 +x3, data = df))
summary(lm(y~x1 + x2 +x3, data = df))
# vif
# x1       x2       x3 
# 3.251068 1.063575 3.142125  ## Just removing x4 from predictor list, vif of other variables have come less than 4
# summary lm
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2543 -1.4726  0.1755  1.5409  3.9711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 48.19363    3.91330  12.315 6.17e-07 ***
#   x1           1.69589    0.20458   8.290 1.66e-05 ***
#   x2           0.65691    0.04423  14.851 1.23e-07 ***
#   x3           0.25002    0.18471   1.354    0.209    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.312 on 9 degrees of freedom
# Multiple R-squared:  0.9823,	Adjusted R-squared:  0.9764 
# F-statistic: 166.3 on 3 and 9 DF,  p-value: 3.367e-08

# Model without x2
vif(lm(y~x1 + x3 +x4, data = df))
summary(lm(y~x1 + x3 +x4, data = df))
# vif
# x1       x3       x4 
# 3.678168 3.459601 1.181000
# > summary lm
# 
# Residuals:
# Min      1Q  Median      3Q     Max 
# -2.9323 -1.8090  0.4806  1.1398  3.7771 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 111.68441    4.56248  24.479 1.52e-09 ***
#   x1            1.05185    0.22368   4.702  0.00112 ** 
#   x3           -0.41004    0.19923  -2.058  0.06969 .  
# x4           -0.64280    0.04454 -14.431 1.58e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.377 on 9 degrees of freedom
# Multiple R-squared:  0.9813,	Adjusted R-squared:  0.975 
# F-statistic: 157.3 on 3 and 9 DF,  p-value: 4.312e-08


