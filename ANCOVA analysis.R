# Load necessary libraries
library(car)        # For VIF, condition indices, etc.
library(lmtest)     # For Durbin-Watson and Breusch-Pagan tests
library(MASS)       # For diagnostics
library(ggplot2)    # For visualizations
library(corrplot)   # For correlation matrix

# Dependent variable: cognitive_function_scores
# Factors: education (categorical) and income_level (categorical)
# Covariates: systolic_blood_pressure, cholesterol_levels, blood_glucose_levels


# Step 1: Create boxplots of levels in factor variables
# Visualize relationships between cognitive function scores and factors

# Boxplot for education levels
ggplot(data, aes(x = education, y = cognitive_function_scores)) + 
  geom_boxplot(fill = "skyblue") +
  ggtitle("Boxplot: Education Levels vs Cognitive Function Scores") +
  xlab("Education Levels") + ylab("Cognitive Function Scores")

# Boxplot for income levels
ggplot(data, aes(x = income_level, y = cognitive_function_scores)) + 
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot: Income Levels vs Cognitive Function Scores") +
  xlab("Income Levels") + ylab("Cognitive Function Scores")


# Step 2: Create scatterplot matrix of cognitive function vs covariates
# Visualize relationships between cognitive function scores and covariates

pairs(data[, c("cognitive_function_scores", "systolic_blood_pressure", 
               "cholesterol_levels", "blood_glucose_levels")], 
      main = "Scatterplot Matrix of Cognitive Function Scores vs Covariates")


# Step 3: Analyze multicollinearity
## 3.1 Spearman correlation of factors and covariates

# Spearman Hypothesis:
# Null: There is no correlation between covariates and factors.
# Alternative: There is significant correlation between at least one pair of variables.

correlation_matrix <- cor(data[, c("systolic_blood_pressure", "cholesterol_levels", 
                                   "blood_glucose_levels")], method = "spearman")
corrplot(correlation_matrix, method = "circle", title = "Spearman Correlation Matrix")

## 3.2 Condition indices and variance proportions
# Fit initial model for multicollinearity diagnostics

ancova_model <- lm(cognitive_function_scores ~ education + income_level + 
                     systolic_blood_pressure + cholesterol_levels + 
                     blood_glucose_levels, data = data)

# Check condition indices and variance proportions

vif(ancova_model)

## 3.3 Variance Inflation Factors (VIFs)

# VIF Hypothesis:
# Null hypothesis: VIF is within acceptable range (VIF < 5-10).
# Alternative: High VIF indicates multicollinearity.

vif_values <- vif(ancova_model)
print(vif_values)


# Step 4: Run ANCOVA

# ANCOVA Hypothesis:
# Null: There is no difference in cognitive function scores between factor levels after accounting for covariates.
# Alternative: At least one factor level has a significant effect on cognitive function scores after accounting for covariates.

summary(ancova_model) # View results of ANCOVA
anova(ancova_model)   # View ANOVA table for the model


# Step 5: Analyze independence of residuals using Durbin-Watson test

# Durbin-Watson Hypothesis:
# Null: Residuals are independent (no autocorrelation).
# Alternative: Residuals are autocorrelated.

dwtest(ancova_model)


# Step 6: Detect outliers
## 6.1 Leverage values
# High leverage points can influence the model significantly.

hatvalues <- hatvalues(ancova_model)
plot(hatvalues, main = "Leverage Values", xlab = "Observation", ylab = "Leverage")
abline(h = 2 * mean(hatvalues), col = "red")

## 6.2 Cook's distance
# Cook's distance identifies influential points. Threshold is 4/n, where n is the number of observations.

cooks_distance <- cooks.distance(ancova_model)
plot(cooks_distance, main = "Cook's Distance", xlab = "Observation", ylab = "Cook's Distance")
abline(h = 4 / nrow(data), col = "red")

## 6.3 Studentized residuals
# Residuals greater than ±2 are considered outliers.

studentized_residuals <- rstudent(ancova_model)
plot(studentized_residuals, main = "Studentized Residuals", xlab = "Observation", ylab = "Residuals")
abline(h = c(-2, 2), col = "red")


# Step 7: Check distribution of residuals
## 7.1 Shapiro-Wilk test

# Shapiro-Wilk Hypothesis:
# Null: Residuals are normally distributed.
# Alternative: Residuals are not normally distributed.

shapiro.test(residuals(ancova_model))

## 7.2 Histogram of residuals

hist(residuals(ancova_model), main = "Histogram of Residuals", 
     xlab = "Residuals", col = "blue", breaks = 20)

## 7.3 Q-Q plot of residuals

qqnorm(residuals(ancova_model))
qqline(residuals(ancova_model), col = "red")


# Step 8: Check homoscedasticity
## 8.1 Scatterplot of fitted vs residual values

# Homoscedasticity Hypothesis:
# Null: Homoscedasticity (constant variance of residuals).
# Alternative: Heteroscedasticity (non-constant variance of residuals).

fitted_values <- fitted(ancova_model)
plot(fitted_values, residuals(ancova_model), main = "Fitted vs Residuals",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

## 8.2 Breusch-Pagan test

# Breusch-Pagan Hypothesis:
# Null: Homoscedasticity (constant variance of residuals).
# Alternative: Heteroscedasticity (non-constant variance of residuals).

bptest(ancova_model)

# End of Analysis
