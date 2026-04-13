# Mortality Prediction Using Logistic Regression
[![R Version](https://img.shields.io/badge/R-4.0+-blue.svg)](https://r-project.org)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
## Overview
This project performs binary logistic regression analysis to predict mortality (`death`) using health, lifestyle, and demographic data.
The analysis includes comprehensive model diagnostics, variable selection, and validation techniques to identify key predictors of mortality.
## Key Features
- **Binary Logistic Regression** for mortality prediction
- **Multicollinearity testing** using Variance Inflation Factor (VIF)
- **Heteroskedasticity detection** via Lipsitz test
- **Robust standard errors** using White's correction (HC0)
- **Autocorrelation testing** with Durbin-Watson statistic
- **Quadratic transformations** for non-linear relationships
- **Composite variable creation** (smoking exposure = smokeyrs/age)
- **Model diagnostics** including ROC/AUC, calibration plots, and residual analysis
### Final Model Predictors
- `sbp` - Systolic blood pressure (quadratic transformation)
- `age` - Age (quadratic transformation)
- `income` - Income level
- `wt82_71` - Weight at follow-up
- `smokeyrs` - Years of smoking (quadratic transformation)
- `chroniccough` - Chronic cough indicator
- `hbpmed` - High blood pressure medication indicator
### Outcome Variable
- `death` - Mortality status (binary)
## Prerequisites
```r
# Required R packages
install.packages(c(
  "dplyr",
  "car",
  "stargazer",
  "lmtest",
  "sandwich",
  "ggplot2",
  "pROC",
  "vip"
))
