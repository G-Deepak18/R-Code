#-----------------------------------------------------------------------------------
#Dataset 1 - birthwt
library(MASS)
library(psych)
#Data overview
help(birthwt) #Description about dataset
str(birthwt) #structure birthwt

#summary statistics - birth weight
summary(birthwt$bwt)
sd(birthwt$bwt) #Standard Deviation
skew(birthwt$bwt) #Skewness
kurtosi(birthwt$bwt) #Kurtosis

#Visualization - birth weight

hist(birthwt$bwt, main="Histogram of birth Weight (bwt)", xlab="Weight", col="lightblue", border="black")

boxplot(birthwt$bwt, main="Boxplot of birth Weight (bwt)", ylab="Weight",col="lightblue", border="black") #Boxplot

#Categorical Variable Analysis - race

race_counts <- table(birthwt$race)
barplot(race_counts, 
        main = "Bar Plot of Mother's Race",
        col = "lightblue", 
        border = "black",
        names.arg = c("White", "Black", "Other"),  # Custom labels
        xlab = "Mother's Race",
        ylab = "Frequency",
        ylim = c(0, 110))

#Correlation analysis
numerical_data_birthwt <- birthwt[, c("age", "lwt", "ptl", "ftv","bwt")]
cor_matrix_birthwt <- cor(numerical_data_birthwt)
print(cor_matrix_birthwt)

#correlation plot
library(corrplot)
corrplot(cor_matrix_birthwt, 
         method = "circle", # Circle method for visualization
         type = "upper",
         order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", # Add correlation coefficients in the plot
         tl.col = "black", 
         tl.srt = 45)

# Scatter plot for bwt vs ptl (birth weight vs number of premature labors)
plot(birthwt$bwt, birthwt$ptl, 
     main = "Scatter Plot of Birth Weight vs Number of Premature Labors", 
     xlab = "Birth Weight (bwt)", 
     ylab = "Number of Premature Labors (ptl)", 
     pch = 19,  
     col = "blue")  
model_bwt_ptl_line <- lm(ptl ~ bwt, data = birthwt) # Fitting a linear model predicting ptl from bwt
abline(model_bwt_ptl_line, col = "red", lwd = 2)  # Add the fitted line to the scatter plot

#Multi regression
  model_birthwt <- glm(bwt ~ lwt + ptl, data = birthwt)
summary(model_birthwt)

#Model Diagnostics
plot(model_birthwt, which = 1)#Residuals vs Fitted plot

plot(model_birthwt, which = 2)#QQ plot of residuals

#PCA
pca_birthwt <- prcomp(birthwt[, c("age", "lwt", "ptl","ftv","bwt")], scale. = TRUE)
pca_summary_birthwt<- summary(pca_birthwt)
print(pca_summary_birthwt)

# Plot the explained variance (scree plot)
screeplot(pca_birthwt,type= "lines" ,main = "Scree Plot of Principal Components")

#Biplot
biplot(pca_birthwt,main = "Biplot", col = c("lightblue", "blue")) #biplot

#Plot the cumulative variance vs principal components
eigenvalues <- pca_summary_birthwt$importance[2, ]  # Proportion of variance
cumulative_variance <- pca_summary_birthwt$importance[3, ]  # Cumulative variance


variance_df <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalues = eigenvalues,
  CumulativeVariance = cumulative_variance
)# Create a data frame for plotting

library(ggplot2)

ggplot(variance_df, aes(x = Component)) +
  geom_line(aes(y = CumulativeVariance), color = "black", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "black", size = 2) +
  # Add axis lines
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  # Customize labels and themes
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Cumulative Variance"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)
#----------------------------------------------------------------------------------

#Dataset-2
library(rattle)
#Data overview
help(wine)
str(wine)# Display structure

#Summary statistics - Numeric Variable - alcohol
summary(wine$Alcohol)
sd(wine$Alcohol)#Standard Deviation
skew(wine$Alcohol) #Skewness
kurtosi(wine$Alcohol) #Kurtosis

# Histogram and Boxplot
hist(wine$Alcohol, main = "Histogram of Alcohol", xlab = "Alcohol", col = "skyblue", breaks = 15)

#Boxplot
boxplot(wine$Alcohol, main = "Boxplot of Alcohol", ylab = "Alcohol", col = "skyblue", horizontal = TRUE)

#Categorical Variable analysis
barplot(table(wine$Type), 
        main = "Distribution of Wine Types", 
        xlab = "Type", 
        col = c("violet", "blue", "green"), 
        names.arg = c("Class 1", "Class 2", "Class 3"))

#correlation Matrix
numerical_data_wine <- wine[, -1]
cor_matrix_wine <- cor(numerical_data_wine)
print(cor_matrix_wine)

#correlation plot
library(corrplot)
corrplot(cor_matrix_wine, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         title = "Correlation Plot", mar = c(0, 0, 1, 0))

# Scatter plot for phenols vs flavanoids
plot(wine$Phenols, wine$Flavanoids, 
     main = "Scatter Plot of Phenols vs Flavanoids", 
     xlab = "Phenols", 
     ylab = "Flavanoids", 
     pch = 19,  
     col = "blue")  

model_wine_line <- lm(wine$Flavanoids ~ wine$Phenols, data = wine)# Fit a linear model
abline(model_wine_line, col = "red", lwd = 2)# Add the fitted line to the scatter plot

#Multi Regression
model_wine <- lm(Alcohol ~ Proline + Malic, data = wine)# Fitting a linear model predicting Alcohol using Proline and Malic_Acid
summary(model_wine)

#Model Diagnostics

plot(model_wine,which=1)# Residuals vs fitted plot

plot(model_wine,which=2)#QQ plot of residuals

#PCA
wine_pca <- prcomp(wine[, -1], center = TRUE, scale. = TRUE)  # Exclude Type
pca_summary_wine <- summary(wine_pca)
print(pca_summary_wine)

# Scree plot
explained_variance <- wine_pca$sdev^2 / sum(wine_pca$sdev^2)
plot(1:13, explained_variance, type = "b", pch = 19, col = "black",
     xlab = "Principal Components", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")
# Biplot
biplot(wine_pca, main = "PCA Biplot", col = c("lightblue", "blue"), xlabs = rep("", 178),xlim = c(-0.30, 0.30))


#Cummulative Variance vs Principal components

eigenvalues <- pca_summary_wine$importance[2, ]  # Proportion of variance
cumulative_variance <- pca_summary_wine$importance[3, ]  # Cumulative variance


variance_df <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalues = eigenvalues,
  CumulativeVariance = cumulative_variance
) # Create a data frame for plotting

library(ggplot2)

ggplot(variance_df, aes(x = Component)) +
  geom_line(aes(y = CumulativeVariance), color = "black", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Cumulative Variance"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq_along(eigenvalues))

  

#----------------------------------------------------------------------------------
#Dataset 3 - pima.te

library(ggplot2)
#Data overview
help(Pima.te)
str(Pima.te)#Structure of data

#Summary statistics - numeric variable Glucose
glucose <- Pima.te$glu
summary(glucose)
sd(glucose)#standard deviation
skew(glucose) #Skewness
kurtosi(glucose) #Kurtosis

#Visualization

# Histogram for glucose
hist(glucose, main = "Histogram of Glucose Levels", xlab = "Glucose", col = "skyblue", breaks = 10)

# Boxplot for glucose
boxplot(glucose, main = "Boxplot of Glucose Levels", xlab = "Glucose", col = "skyblue")

#categorical Variable Analysis

# Bar plot for diabetes variable
barplot(table(Pima.te$type), main = "Distribution of Diabetes", xlab = "Diabetes (0 = No, 1 = Yes)", ylab = "Count", col = "skyblue")

#Correlation Matrix
cor_matrix_pima <- cor(Pima.te[, 1:7])  # Exclude the target variable (type)

# Display correlation matrix
corrplot(cor_matrix_pima, 
         method = "circle", # Circle method for visualization
         type = "upper", 
         order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45,title = "Correlation Plot")

#scatter plot of npreg vs age

plot(Pima.te$npreg, Pima.te$age, 
     main = "Scatter Plot of npreg vs age", 
     xlab = "Number of Pregnancies (npreg)", 
     ylab = "Age", 
     pch = 19,  
     col = "blue")  
model <- lm(age ~ npreg, data = Pima.te)
abline(model, col = "red", lwd = 2)  # Adding the fitted line to the scatter plot

#Multiple regression

model_pima <- lm(glucose ~ bp + bmi, data=Pima.te)
summary(model_pima)

#Model Diagnostics
plot(model_pima,which=1)# residuals vs fitted plots

plot(model_pima,which=2)#QQ plot of residuals

#PCA

pca_data_Pima <- Pima.te[, c("npreg", "glu", "bp", "skin","bmi", "ped", "age")]# Select numeric columns for PCA

# Perform PCA
pca_Pima <- prcomp(pca_data_Pima, scale. = TRUE)
pca_summary_pima <- summary(pca_Pima)

# Plot explained variance
screeplot(pca_Pima,type = "lines", main="Scree Plot of PCA")

# Biplot of PCA
biplot(pca_Pima, main="PCA Biplot", col = c("lightblue", "blue"))

#Plot cumulative variance vs principal component
eigenvalues <- pca_summary_pima$importance[2, ]  # Proportion of variance
cumulative_variance <- pca_summary_pima$importance[3, ]  # Cumulative variance

variance_df <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalues = eigenvalues,
  CumulativeVariance = cumulative_variance
) # Create a data frame for plotting

library(ggplot2)

ggplot(variance_df, aes(x = Component)) +
  geom_line(aes(y = CumulativeVariance), color = "black", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Cumulative Variance"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq_along(eigenvalues)) 

#-------------------------------------------------------------------------------

# Dataset -4 - Diamonds

library(ggplot2)
#Data Overview
help("diamonds")
str(diamonds)

#Summary Statistics for Numeric Variable - price
summary(diamonds$price)
sd(diamonds$price) #Standard deviation
skew(diamonds$price) #Skewness
kurtosi(diamonds$price) #Kurtosis

#Vizualization
# Histogram
hist(diamonds$price, main="Histogram of Diamond Prices", xlab="Price", col="lightblue", breaks=50)

# Boxplot
boxplot(diamonds$price, main="Boxplot of Diamond Prices", ylab="Price", col="lightblue")

#Categorical Variable analysis - cut

ggplot(diamonds, aes(x = cut)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Barplot of Diamond Cut Quality", x = "Cut Quality", y = "Count") +
  theme_minimal()

#Multivariate Analysis

#Correlation Analysis

numerical_data_diamonds <- diamonds[, c("price", "carat", "depth", "table", "x", "y", "z")] # Select only the numerical variables (ignoring categorical variables like 'cut', 'color', 'clarity')
cor_matrix_diamonds <- cor(numerical_data_diamonds)
print(cor_matrix_diamonds) # Print the correlation matrix

corrplot(cor_matrix_diamonds, 
         method = "circle", 
         type = "upper", 
         order = "hclust", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45,title = "Correlation Plot")

# Scatter plot for price vs carat
plot(diamonds$carat, diamonds$price, 
     main = "Scatter Plot of Price vs Carat", 
     xlab = "Carat", 
     ylab = "Price", 
     pch = 19,  
     col = "blue")  
model_diamonds_line <- lm(price ~ carat, data = diamonds)# Fitting a linear model predicting price from carat
abline(model_diamonds_line, col = "red", lwd = 2)  # Adding the fitted line to the scatter plot

# Multi regression model
model_diamonds <- lm(price ~ carat + depth, data = diamonds)
summary(model_diamonds)

#Model Diagnostics
#Residual vs fitted plot
plot(model_diamonds, which =1)

#QQ plot of residuals
plot(model_diamonds, which =2)

#Principle component analysis
diamonds_data <- diamonds[, c("carat", "depth", "table", "x", "y", "z")] # Standardize the data (numeric features only)
pca_diamonds <- prcomp(diamonds_data, center = TRUE, scale. = TRUE)
pca_summary_diamonds <- summary(pca_diamonds)
print(pca_summary_diamonds)# Summary of PCA 

# Scree plot
screeplot(pca_diamonds,type= "lines" ,main = "Scree Plot of Principal Components")

# Biplot
biplot(pca_diamonds, main = "PCA Biplot",col = c("lightblue", "blue"))

#cumulative variance vs eigen values plot

eigenvalues <- pca_summary_diamonds$importance[2, ]  # Proportion of variance
cumulative_variance <- pca_summary_diamonds$importance[3, ]  # Cumulative variance


variance_df <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalues = eigenvalues,
  CumulativeVariance = cumulative_variance
   ) # Create a data frame for plotting

library(ggplot2)

ggplot(variance_df, aes(x = Component)) +
  geom_line(aes(y = CumulativeVariance), color = "black", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "black", size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Cumulative Variance"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq_along(variance_df$Component))





