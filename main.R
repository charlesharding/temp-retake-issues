## Scripts were run in 2020 with R version 3.5.2 on 64-bit Windows 10

## Load packages

library(data.table)
library(bit64)
library(ggplot2)
library(ggthemes)
library(scales)
library(hrbrthemes)
library(blandr)
library(flextable)
library(officer)


## Set parameters

caliper <- 15 # Maximum time between matched measurements (minutes)


## Paths

# Scripts
functions_path <- file.path('.', 'scripts', 'functions.R')
formatting_path <- file.path('.', 'scripts', 'formatting.R')
prepare_nurse_charting_path <- file.path(
  '.', 'scripts', 'prepare-nurse-charting.R'
)
prepare_patient_path <- file.path('.', 'scripts', 'prepare-patient.R')
prepare_matched_path <- file.path('.', 'scripts', 'prepare-matched.R')
analysis_path <- file.path('.', 'scripts', 'analysis.R')

# Data files
chart_path <- file.path('.', 'raw data', 'nurseCharting.csv')
clean_chart_path <- file.path('.', 'clean data', 'chart-data.csv')
patient_data_path <- file.path('.', 'raw data', 'patient.csv')
apache_res_data_path <- file.path('.', 'raw data', 'apachePatientResult.csv')
apache_vars_data_path <- file.path('.', 'raw data', 'apachePredVar.csv')
clean_patient_path <- file.path('.', 'clean data', 'patient-data.csv')
clean_matched_path <- file.path(
  '.', 'clean data', paste0('matched-data-', caliper, '-min.RData')
)
char_table_path <- file.path(
  '.', 'results', paste0('char-table-', caliper, '-min.docx')
)


## Load functions

source(functions_path)
source(formatting_path)


## Clean data IF cleaned files not already present

if(!file.exists(clean_matched_path)){

  if(!file.exists(clean_chart_path)){
    source(prepare_nurse_charting_path)
  } else {
    chart_data <- read.csv(clean_chart_path, stringsAsFactors = FALSE)
  }
  if(!file.exists(clean_patient_path)){
    source(prepare_patient_path)
  } else {
    patient_data <- read.csv(clean_patient_path, stringsAsFactors = FALSE)
  }

  source(prepare_matched_path)
  
} else {

  matched_data <- get(load(clean_matched_path))
    
}


## Analyze data

source(analysis_path)
