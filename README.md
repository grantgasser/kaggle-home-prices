# Predicting Home Prices (Kaggle Competition)

## Prelim Report:

### Main:
This project was done jointly with [Matt Suski](https://www.linkedin.com/in/matt-suski/) for [Dr. Patrick's](https://www.baylor.edu/statistics/index.php?id=941853) Regression 3386 course. 

### Methodology
With a dataset of 79 features, we used a combination of regsubsets and Lasso to perform model selection. Model with variables selected by Lasso (glmnet in R) performed better with a RMSE = .16044. We examined outliers & influential points. We also tested the following assumptions of regression models:
1. Normality of residuals
2. Independence of residuals
3. Constant variance of residuals

