All code used for the thesis for program Master of Statistics and Data science in June 2025:

Author: Ebba Blomdahl and Linwei He

Topic: Effect of Higher-Order Variables on Prediction Model---Heterogeneity using Ovarian Cancer as a Case Study


Data: the mechanism to generate training and test dataset for different scenarions with/without higher-order variables as center type and prevalence, with accordingly change of regression imputation of missing value. 

Data exploration: Explorating analysis of basic description in center-specific way, including the distribution of data and missing value description. Metric file records the extraction of auc and oe value from the results of the model which includes the probabitlity.

Evaluation: Including three metrics, auc, o:e ratio and net benefit grouped by 21 centers.

Meta_analysis: meta_analysis records one scenario with defined functions, and auc_meta compilesn of all meta analysis of tau square in terms of AUC and O:E ratio

Models: Four models in 24 scenarios, needs to run with files in Date to activate the function of generate the training and test dataset.

Visualization: Plots related in the thesis,in terms of metrics introduction, calibration curves, and tau square of metrics. 
