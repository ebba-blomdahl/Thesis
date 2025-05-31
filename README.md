## All code used for the thesis for program Master of Statistics and Data science in June 2025

## Effect of Higher-Order Variables on Prediction Model Heterogeneity --- A Case Study: Ovarian Cancer

This project investigates how higher-order variables—such as center type and disease prevalence—affect the heterogeneity of clinical prediction models. The study uses ovarian cancer as a case example and evaluates how model performance varies across healthcare centers.

### Authours: Ebba Blomdahl and Linwei 

---

## 1. Data Generation

This module defines the process for generating training and test datasets across multiple scenarios:

- Inclusion/exclusion of higher-order variables:
  - `Center Type` (e.g., oncology vs non-oncology center as 1 and 0)
  - `Prevalence` (disease rate per center)
- Corresponding adjustments to regression-based imputation for missing values.

---

## 2. Data Exploration

Center-specific exploratory analyses include:

- Descriptive statistics
- Distribution plots
- Missing data summaries

The `metric` file contains extraction scripts for model evaluation metrics (AUC and O:E ratio) based on predicted probabilities.

---

## 3. Model Evaluation

Performance is assessed at the center level using:

- **AUC**: Area Under the Receiver Operating Characteristic Curve  
- **O:E Ratio**: Observed-to-Expected outcome ratio  
- **Net Benefit**: Derived from Decision Curve Analysis  

These metrics are grouped across 21 centers to quantify heterogeneity.

---

## 4. Meta-Analysis

- `meta_analysis`: Executes meta-analysis for a defined scenario using custom functions.  
- `auc_meta`: Aggregates meta-analytic results across multiple scenarios.  
  - Focuses on tau-squared (τ²) values for AUC and O:E ratio to capture between-center heterogeneity.

---

## 5. Models

Four predictive models are implemented under 24 unique scenarios:

- Logistic Regression  
- Multilevel Logistic Regression  
- Random Forest  
- XGBoost  

All models depend on files in the **Data** module for dynamic training/test dataset generation.

---

## 6. Visualization

Contains all plots used in the thesis:

- Introduction to evaluation metrics  
- Calibration curves  
- Plots of τ² values for AUC and O:E ratio across scenarios  



