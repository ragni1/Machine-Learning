# Developer - Rageeni Sah
# Read the data file
data <- read.csv('HeightWeight.csv', header = TRUE)
View(data)
# Target -> Approved

# Check if any missing values in the dataset
anyNA(data)

# Start Preprocessing
attach(data)
dim(data) # Dimensions (3 features and 200 observations)

# scatter plot
plot(weight, height, main = 'Before Scaling', xlab = 'Weight in Kg', ylab = 'Height in cm')

# Scaling - because the range of height and weight are not same
data$weight <- scale(data$weight)
data$height <- scale(data$height)

# scatter plot 
plot(weight, height, main = 'After Scaling',xlab = 'Weight', ylab = 'Height')

# Boxplot
boxplot(height, weight,col="red") # outliers present in weight

# Visualise outliers range
qqplot(data$weight, data$height)

# remove outliers
data <- data[-which(data$weight > 2.5),] 
#dim(data)

# Boxplot
boxplot(height, weight,col="green") 
qqplot(data$weight, data$height)

# Correlation Check
#cor(data)
cor(data[,-1])
