# Stats-R-Assignment
R code scripted to run MLR and ANCOVA models

In the assumption analyses phase, the evaluation will initially use a scatterplot matrix (covariates in MLR and ANCOVA models) and/or box plots (factors in ANCOVA model) including all the involved variables to visually assess whether a linear relationship exists. 

Next, multicollinearity will be examined using Pairwise Correlations (Spearman), Condition indices (CIs) (with thresholds: 5 < CI <10 indicating weak dependencies, CI > 30 indicating moderate/strong dependencies, and CI >> 30 indicating serious multicollinearity), Variance Proportions (where a variance proportion > 0.5 indicates dependency), and the Variance Inflation Factor (VIF) (where VIF = 1 indicates no correlation, 1 < VIF < 5 suggests moderate correlation, and VIF > 5 indicates high correlation). 

The Durbin-Watson statistic will be used to determine the independence of residuals. 

Outliers will be detected using Leverage Values (values greater than 2 p/n , where p is the number of parameters in the model and n is the number of observations, will be flagged as potentially influential), Cook’s Distance (distance > 1 indicates a potential outlier), and Studentised Residuals (residuals > ± 2 will be considered outliers). 

Lastly, the distribution of residuals will be evaluated using the Shapiro-Wilk test, while their homoscedasticity will be assessed via a scatterplot of residuals versus fitted values.
