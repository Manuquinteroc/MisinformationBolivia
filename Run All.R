# Replication file for Can We Shield Citizens Against Misinformation
#     Through Digital Literacy Training and Fact-checks?
#     Last updated: July 1st 2022

# ------------------------------------------------------------------------------
# Set working directory
rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install and load require packages
source('Scripts/RequiredPackages.R')

# Load auxiliary functions 
source('Scripts/Functions.R')
source('Scripts/General Scripts/ICW_function.R')

# Raw data manipulation for alfabetizacion
source('Scripts/Data Manipulation/Data Manipulation Alf.R')

# Raw data manipulation for WhatsApp
source('Scripts/Data Manipulation/Data Manipulation WhatsApp.R')

# Read raw data sets and create a single data frame with ICW indexes
source('Scripts/Data Manipulation/MergeData_ICW.R')
source('Scripts/Data Manipulation/MergeData_ICW_Attrition.R')

# First Stage Course
source('Scripts/First Stage Course.R')

# WhatsApp interaction survey 
source('Scripts/WhatsApp Interaction.R')

# Response rate
source('Scripts/Response Rate.R')

# Attrition
source('Scripts/Attrition.R')

# Balance Tables
source('Scripts/Balance Summary.R')
source('Scripts/Balance.R')

# Wording tables of main indexes
source('Scripts/Wording Indexes.R')

# Main Results
source('Scripts/Main Results Indexes.R')

# Disaggregated Abalysis 
source('Scripts/Disaggregated Analysis.R')

# Robust check - Main results of union of variables (Obsolete)
# source('Scripts/Main Results Indexes Union Surveys.R')

# Generate Post-stratification weights and Sample comparison
source('Scripts/Sample Camparison.R')

# Reweighting Analysis
source('Scripts/Main Indexes Reweighting.R')

# Analysis on social media data
source('Scripts/Analysis Social Media.R') # Attrition and data wrangling
source('Scripts/Social Media Treatment Effects.R')

