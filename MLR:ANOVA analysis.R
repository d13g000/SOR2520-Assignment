# Load necessary libraries
library(car)        # For Variance Inflation Factor (VIF), condition indices, etc.
library(MASS)       # For Durbin-Watson test
library(lmtest)     # For Breusch-Pagan test
library(ggplot2)    # For visualization (scatterplot matrix, histograms, etc.)
library(corrplot)   # For correlation matrix
library(Metrics)    # For additional diagnostic tests
library(DescTools)  # For Shapiro-Wilk test


# Dependent variable: cognitive_function_scores
# Independent variables: systolic_blood_pressure, diastolic_blood_pressure, cholesterol_levels, blood_glucose_levels


# Step 1: Create a correlation matrix of continuous variables

# Correlation Hypothesis:
# Null: There is no significant correlation between the continuous variables.
# Alternative: There is a significant correlation between at least two continuous variables.

correlation_matrix <- cor(data[, c("cognitive_function_scores", "systolic_blood_pressure", 
                                    "diastolic_blood_pressure", "cholesterol_levels", 
                                    "blood_glucose_levels")], use = "complete.obs")
# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle")


# Step 2: Create a scatterplot matrix of all continuous variables
# This helps to visually examine relationships between the variables.

pairs(data[, c("cognitive_function_scores", "systolic_blood_pressure", 
               "diastolic_blood_pressure", "cholesterol_levels", "blood_glucose_levels")])


# Step 3: Analyze multicollinearity
## 3.1 Bivariate normality tests and Spearman correlation
# Test bivariate normality (using shapiro.test for individual variables or combinations)

# Multivariate Normality Hypothesis:
# Null hypothesis: Data is normally distributed
# Alternative hypothesis: Data is not normally distributed

# Shapiro-Wilk test for normality (you can run this for each variable individually)

shapiro.test(data$cognitive_function_scores)
shapiro.test(data$systolic_blood_pressure)
shapiro.test(data$diastolic_blood_pressure)
shapiro.test(data$cholesterol_levels)
shapiro.test(data$blood_glucose_levels)

# Spearman correlation for non-parametric correlation between continuous variables

spearman_corr <- cor(data[, c("systolic_blood_pressure", "diastolic_blood_pressure", 
                              "cholesterol_levels", "blood_glucose_levels")], 
                     method = "spearman")
print(spearman_corr)

## 3.2 Condition indices and variance proportions
# Condition indices help identify multicollinearity

vif_model <- lm(cognitive_function_scores ~ systolic_blood_pressure + diastolic_blood_pressure + 
                  cholesterol_levels + blood_glucose_levels, data = data)
vif(vif_model)

# Variance proportions: Look for high values (greater than 0.9 for serious multicollinearity)
# Look at the VIF output and examine variance proportions.

## 3.3 Variance Inflation Factors (VIFs)
# Variance Inflation Factor (VIF) tells us if a variable is highly correlated with other predictors.

vif(vif_model)


# Step 4: Run MLR and ANOVA
# Linear regression (Multiple Linear Regression)

# MLR Hypothesis:
# Null hypothesis: Systolic/diastolic blood pressure levels, cholesterol levels, and blood glucose levels do not significantly predict cognitive function
# Alternative hypothesis: At least one of the predictors (systolic/diastolic blood pressure levels, cholesterol levels, and blood glucose levels) significantly predicts cognitive function


mlr_model <- lm(cognitive_function_scores ~ systolic_blood_pressure + diastolic_blood_pressure + 
                  cholesterol_levels + blood_glucose_levels, data = data)
summary(mlr_model)

# ANOVA Hypothesis:
# Null hypothesis: Model with only a constant term (intercept only model) is a good fit for the data
# Alternative hypothesis: Model fitted (which includes covariates) fits better than the model with only the intercept term

anova(mlr_model)

# Step 5: Analyze independence of residuals using Durbin-Watson test

# Durbin-Watson Hypothesis:
# Null hypothesis: Residuals are not correlated (no autocorrelation)
# Alternative hypothesis: Residuals are correlated (there is autocorrelation)

dwtest(mlr_model)


# Step 6: Detect outliers using:
## 6.1 Leverage values
# High leverage points can influence the regression model. Look for points with high leverage.

hatvalues <- hatvalues(mlr_model)
plot(hatvalues, main = "Leverage values")
abline(h = 2 * mean(hatvalues), col = "red")

## 6.2 Cook's distance
# Cook’s distance helps identify influential data points. Points with large values should be examined.

cooks_distance <- cooks.distance(mlr_model)
plot(cooks_distance, main = "Cook's Distance")
abline(h = 4 / nrow(data), col = "red")

## 6.3 Studentized residuals
# Studentized residuals indicate outliers that have a large influence on the regression.

studentized_residuals <- rstudent(mlr_model)
plot(studentized_residuals, main = "Studentized Residuals")
abline(h = 2, col = "red")
abline(h = -2, col = "red")


# Step 7: Check the distribution of residuals
## 7.1 Shapiro-Wilk test for normality

# Shapiro-Wilk Hypothesis:
# Null hypothesis: Residuals are normally distributed
# Alternative hypothesis: Residuals are not normally distributed

shapiro.test(residuals(mlr_model))

## 7.2 Histogram of residuals

hist(residuals(mlr_model), main = "Histogram of Residuals", xlab = "Residuals", col = "blue")

## 7.3 Q-Q plot for residuals

qqnorm(residuals(mlr_model))
qqline(residuals(mlr_model), col = "red")


# Step 8: Check homoscedasticity using:
## 8.1 Scatterplot of fitted vs residual values

# Homoscedasticity Hypothesis:
# Null hypothesis: Homoscedasticity (constant variance of residuals)
# Alternative hypothesis: Heteroscedasticity (non-constant variance of residuals)

fitted_values <- fitted(mlr_model)
plot(fitted_values, residuals(mlr_model), main = "Fitted vs Residuals", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

## 8.2 Breusch-Pagan test

# Breusch-Pagan Hypothesis:
# Null hypothesis: Homoscedasticity (constant variance of residuals)
# Alternative hypothesis: Heteroscedasticity (non-constant variance of residuals)

bptest(mlr_model)

# End of analysis
