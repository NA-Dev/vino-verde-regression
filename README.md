# Vino Verde Regression

Using quality and composition data on wines from the Vino Verde region of Portugal, we were able to perform simple linear regression to see which factors were significant predictors of wine quality and type. Prediction was significantly improved when Red and White were modeled separately, which makes sense as their flavor profiles differ significantly. Predicting wine type was much more accurate (99%) than wine quality (75%) in a 70-30 test-train split. Alcohol content, acidity, pH, free sulfur dioxide, and sugar levels were the most significant features.

Final Report: https://github.com/NA-Dev/vino-verde-regression/blob/main/report.pdf

Data Source: https://archive.ics.uci.edu/ml/datasets/Wine+Quality

Code: https://github.com/NA-Dev/vino-verde-regression/blob/main/regression_code.R