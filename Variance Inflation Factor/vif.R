# Developer - Rageeni Sah

## Concept:- Variance Inflation Factor (VIF) gives measure of inflation of variance of an independent variable 
#            in presence of other correlated independent varibales. It is a great tool to check multicollinearity.

df <- read.table('bloodpressure.txt', header = TRUE)
head(df)
dim(df)
str(df)
anyNA(df)
df$Pt<-NULL
head(df)
## BP -> Target Variable, Age, Weight, BSA, DUr, pulse ans Stress are responses.

corr <- cor(df[,-1], method = 'pearson')
#install.packages("ggcorrplot")
library('ggcorrplot')
ggcorrplot(corr)
## Correlation matrix depicts following details.
# 1. Highly correlated features: Weight-BSA 
# 2. Moderately correlated features: Age-Pulse, Weight-Pulse
# 3. Lessly correlated features: Stress, Duration

## Modeling
#install.packages('car')
library('car')
vif(lm(BP~., data = df))
summary(lm(BP~., data = df))
## Summar vif
# Age   Weight      BSA      Dur    Pulse   Stress 
# 1.762807 8.417035 5.328751 1.237309 4.413575 1.834845 
## Weight has variance inflated by 8.14 times in presence of other correlated features such as BSA. Similarly, Body surface Area (BSA) variance is inflated by 5 times in presence of other correlated features.
#  vif>5 indicates high multicolinearity
#summmar lm
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.93213 -0.11314  0.03064  0.21834  0.48454 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -12.870476   2.556650  -5.034 0.000229 ***
#   Age           0.703259   0.049606  14.177 2.76e-09 ***
#   Weight        0.969920   0.063108  15.369 1.02e-09 ***
#   BSA           3.776491   1.580151   2.390 0.032694 *  
#   Dur           0.068383   0.048441   1.412 0.181534    
# Pulse        -0.084485   0.051609  -1.637 0.125594    
# Stress        0.005572   0.003412   1.633 0.126491    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4072 on 13 degrees of freedom
# Multiple R-squared:  0.9962,	Adjusted R-squared:  0.9944 
# F-statistic: 560.6 on 6 and 13 DF,  p-value: 6.395e-15

# Modeling without Weight
vif(lm(BP~Age+BSA+Dur+Pulse+Stress, data = df))
summary(lm(BP~Age+BSA+Dur+Pulse+Stress, data = df))
# vif
# Age      BSA      Dur    Pulse   Stress 
# 1.703115 1.428349 1.237151 2.360939 1.502936 
# summary lm
# 
# Call:
#   lm(formula = BP ~ Age + BSA + Dur + Pulse + Stress, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3687 -0.9135  0.1546  0.9053  2.8020 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.21215    9.42927   0.659   0.5207    
# Age          0.56297    0.20572   2.737   0.0161 *  
#   BSA         24.55378    3.45160   7.114 5.22e-06 ***
#   Dur          0.07682    0.20437   0.376   0.7126    
# Pulse        0.45644    0.15925   2.866   0.0124 *  
#   Stress      -0.01673    0.01303  -1.284   0.2199    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.718 on 14 degrees of freedom
# Multiple R-squared:  0.9262,	Adjusted R-squared:  0.8998 
# F-statistic: 35.14 on 5 and 14 DF,  p-value: 1.921e-07

# Model without BSA
vif(lm(BP~Age+Weight+Dur+Pulse+Stress, data = df))
summary(lm(BP~Age+Weight+Dur+Pulse+Stress, data = df))
# vif
# Age   Weight      Dur    Pulse   Stress 
# 1.659637 2.256150 1.235620 3.599913 1.739641 
# > summary lm
# 
# Call:
#   lm(formula = BP ~ Age + Weight + Dur + Pulse + Stress, data = df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.02600 -0.18526 -0.00077  0.21934  0.72533 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -15.116781   2.748758  -5.499 7.83e-05 ***
#   Age           0.731940   0.055646  13.154 2.85e-09 ***
#   Weight        1.098958   0.037773  29.093 6.37e-14 ***
#   Dur           0.064105   0.055965   1.145   0.2712    
# Pulse        -0.137444   0.053885  -2.551   0.0231 *  
#   Stress        0.007429   0.003841   1.934   0.0736 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4708 on 14 degrees of freedom
# Multiple R-squared:  0.9945,	Adjusted R-squared:  0.9925 
# F-statistic: 502.5 on 5 and 14 DF,  p-value: 2.835e-15

## By excluding BSA, vif of other variables are less than 5 thus, free of high multicollinearity while retaining 
   # R-squared as high as 99.45%.
