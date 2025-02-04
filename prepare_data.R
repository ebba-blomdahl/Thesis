#### script to prepare the data ####

# libraries 
library(data.table) #rbindlist
library(DescTools) #Desc
library(dplyr) # Data wrangling

# load data
load("/Users/ebbablomdahl/Thesis/Code/IOTAsynthpop_data3.RData")

# Create type of center variable where 1 is when the center is an oncology center
AGRsyn3$center = "AGR"
AGRsyn3$oncocenter = 1
BCHsyn3$center = "BCH"
BCHsyn3$oncocenter = 1
BFRsyn3$center = "BFR"
BFRsyn3$oncocenter = 0
BI0syn3$center = "BI0"
BI0syn3$oncocenter = 1
BI1syn3$center = "BI1"
BI1syn3$oncocenter = 1
CITsyn3$center = "CIT"
CITsyn3$oncocenter = 1
GBEsyn3$center = "GBE"
GBEsyn3$oncocenter = 0
KUKsyn3$center = "KUK"
KUKsyn3$oncocenter = 1
LBEsyn3$center = "LBE"
LBEsyn3$oncocenter = 1
LPOsyn3$center = "LPO"
LPOsyn3$oncocenter =1
LSWsyn3$center = "LSW"
LSWsyn3$oncocenter = 1
MFRsyn3$center = "MFR"
MFRsyn3$oncocenter = 0
MITsyn3$center = "MIT" 
MITsyn3$oncocenter = 0
MSWsyn3$center = "MSW"
MSWsyn3$oncocenter = 0
NCIsyn3$center = "NCI"
NCIsyn3$oncocenter = 1
NITsyn3$center = "NIT"
NITsyn3$oncocenter = 0
OITsyn3$center = "OIT"
OITsyn3$oncocenter = 1
PCRsyn3$center = "PCR"
PCRsyn3$oncocenter = 1
RITsyn3$center = "RIT"
RITsyn3$oncocenter = 1
SITsyn3$center = "SIT"
SITsyn3$oncocenter = 0
SSWsyn3$center = "SSW"
SSWsyn3$oncocenter = 1

# This file contains all the synthetic data with a column indicating 
# type of center and a column indicating the center ID
all_data <- rbindlist(list(AGRsyn3, BCHsyn3, BFRsyn3, BI0syn3, BI1syn3, CITsyn3, GBEsyn3, KUKsyn3, LBEsyn3, LPOsyn3, LSWsyn3, MFRsyn3, MITsyn3, MSWsyn3, NCIsyn3, NITsyn3, OITsyn3, PCRsyn3, RITsyn3, SITsyn3, SSWsyn3), fill=TRUE)

# Save the processed dataset
save(all_data, file = "/Users/ebbablomdahl/Thesis/Code/IOTAsynthpop_data3_prepared.RData")
