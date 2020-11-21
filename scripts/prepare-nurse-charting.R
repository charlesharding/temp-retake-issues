## Load data

# File too large for read.csv
# chart_data <- read.csv(chart_path, stringsAsFactors = FALSE)

# Use fread from data.table instead
chart_data <- fread(chart_path, header = TRUE, sep = ',')

# Limit to temperatures
chart_data <- chart_data[
    chart_data$nursingchartcelltypevallabel %in% 'Temperature',
]

#dim(chart_data)
# [1] 16548343        8

# Garbage collection
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()

# Separate Celsius, Fahrenheit, and measurement location records
celsius_data <- chart_data[
  chart_data$nursingchartcelltypevalname %in% 'Temperature (C)',
]  
fahrenheit_data <- chart_data[
  chart_data$nursingchartcelltypevalname %in% 'Temperature (F)',
]  
location_data <- chart_data[
  chart_data$nursingchartcelltypevalname %in% 'Temperature Location',
]  

celsius_data <- celsius_data[ , 
  c('nursingchartid', 'patientunitstayid', 
  'nursingchartoffset', 'nursingchartentryoffset',
  'nursingchartvalue')
]
names(celsius_data)[c(1,4:5)] <- c(
  'nursingchartidc', 'nursingchartentryoffsetc', 'temperaturec'
)

fahrenheit_data <- fahrenheit_data[ , 
  c('nursingchartid', 'patientunitstayid', 
  'nursingchartoffset', 'nursingchartentryoffset',
  'nursingchartvalue')
]
names(fahrenheit_data)[c(1,4:5)] <- c(
  'nursingchartidf', 'nursingchartentryoffsetf', 'temperaturef'
)

location_data <- location_data[ , 
  c('patientunitstayid', 
  'nursingchartoffset', 'nursingchartentryoffset',
  'nursingchartvalue')
]
names(location_data)[3:4] <- c('nursingchartentryoffsetsite', 'chartsite')

# Dimensions
# dim(celsius_data)
# [1] 6267541       5
# dim(fahrenheit_data)
# [1] 6267330       5
# dim(location_data)
# [1] 4013472       5

# Garbage collection
rm(chart_data)
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()

# Set keys
setkey(celsius_data, patientunitstayid, nursingchartoffset)
setkey(fahrenheit_data, patientunitstayid, nursingchartoffset)
setkey(location_data, patientunitstayid, nursingchartoffset)


# celsius_data FULL OUTER JOIN fahrenheit_data
chart_data <- merge(celsius_data, fahrenheit_data, all = TRUE)
# dim(chart_data)
# [1] 6267909       7

# Garbage collection
rm(celsius_data, fahrenheit_data)
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()
Sys.sleep(0.1)
gc()

# Essentially no differences in listed entry times for C and F measurements
# offset_diff <- abs(
#  chart_data$nursingchartentryoffsetc - chart_data$nursingchartentryoffsetf
# )
# table(offset_diff)
#      0       9 
# 6267004       2 

chart_data$nursingchartentryoffset <- ifelse(
  is.finite(chart_data$nursingchartentryoffsetc),
  chart_data$nursingchartentryoffsetc,
  chart_data$nursingchartentryoffsetf
)
chart_data <- chart_data[, 
  c('nursingchartentryoffsetc', 'nursingchartentryoffsetf'):=NULL
]

chart_data$temperaturec <- as.numeric(chart_data$temperaturec)
chart_data$temperaturef <- as.numeric(chart_data$temperaturef)

# Few differences between C and F measurements, except for some
# apparent entry errors
# measurement_diff <- abs(
#   fToC(chart_data$temperaturef) - chart_data$temperaturec
# )
# table(round(measurement_diff, 1))

# Flag temperatures for exclusion
chart_data$exclusionflag <- 'None'
chart_data$exclusionflag <- ifelse(
  !is.finite(chart_data$temperaturef) | !is.finite(chart_data$temperaturec),
  chart_data$exclusionflag,
  ifelse(abs(fToC(chart_data$temperaturef) - chart_data$temperaturec) > 0.2,
    'F and C values differ',
    chart_data$exclusionflag
  )
)

# Add indicator for whether measurements are available in both F and C,
# or only one
chart_data$unitavailabilityflag <- ifelse(
  is.finite(chart_data$temperaturef) & is.finite(chart_data$temperaturec),
  'Both',
  ifelse(
    is.finite(chart_data$temperaturef), 
    'F only',
    ifelse(
      is.finite(chart_data$temperaturec), 
      'C only',
      'Neither'
    )
  )
)

# Fill in missing F and C measurements
chart_data$temperaturef <- ifelse(
  is.finite(chart_data$temperaturef),
  chart_data$temperaturef,
  cToF(chart_data$temperaturec)
)
chart_data$temperaturec <- ifelse(
  is.finite(chart_data$temperaturec),
  chart_data$temperaturec,
  fToC(chart_data$temperaturef)
)

# Flag more temperatures for exclusion: available in C but not F.
# Reason for exclusion: C and F values for the same measurement can be recorded 
# at different times. Treating them as separate measurements could affect 
# repeatability estimates for quickly retaken temperatures.
chart_data$exclusionflag <- ifelse(
  chart_data$unitavailabilityflag %in% 'C only',
  'Unclear if separate measurement',
  chart_data$exclusionflag
)

# Flag more temperatures for exclusion: implausible temperatures
chart_data$exclusionflag <- ifelse(
  chart_data$temperaturef < 50 | chart_data$temperaturef > 120 |
  chart_data$temperaturec < fToC(50) | fToC(chart_data$temperaturec) > 120,
  'Implausible temperature',
  chart_data$exclusionflag
)

# dim(chart_data)
# [1] 6267909       

# Remove duplicates
chart_data <- chart_data[
  !duplicated(chart_data[, c('patientunitstayid', 'nursingchartoffset')]), 
]
# dim(chart_data)
# [1] 6267876       7


# Clean location (measurement site) data

# Recode site synonyms, referencing table(location_data$chartsite)
# Enteries are recoded for all sites appearing at least 10 times:
#   table(location_data$chartsite)[table(location_data$chartsite) >= 10]
# which accounts for 99.98% of all measurements.
# Notes: PO = Per Os = By Mouth. Swan = Swan-Ganz catheter = Pulmonary artery.
# Foley = Foley catheter = Urinary bladder. PR = Per rectum = Rectal
# Core is ambiguous, so is kept separate here.
# Some unknown/other entries are not explicitly recoded, since everything not
# recoded elsewhere is recoded to unknown/other.

syn_ta <- c('Temporal', ',ta', ',TA', '.ta', '.Ta', '.TA', 'forhead', 
  'Forehead', 'ta', 'TA', 'temp art', 'Temp art', 'TEMP ART', 'Temp Artery',
  'Tempor', 'temporal', 'TEMPORAL', 'Temporal Artery', 'TEMPORAL ARTERY',
  'Temporal artery scan', 'Temporal Artery Scan', 'temporal scan', 
  'Temporal scan', 'TEMPOROL', 'Temprl', 'Tpl')
syn_ax <- c('Axillary', '.ax', '.AX', 'ax', 'Ax', 'AX', 'axil', 'axill', 
  'axilla', 'Axilla', 'axillary', 'Axillary', 'AXILLARY', 'Axllry')
syn_or <- c('Oral', '.or', '.OR', '.oral', '.po', '.PO', 'O.', 'or', 'OR', 
  'oral', 'Oral', 'ORAL', 'orally', 'po', 'PO')
syn_cor <- c('Core', '.core', 'core', 'Core', 'CORE', 'Core Temperature')
syn_eso <- c('Esophageal', 'eso', 'Eso', 'esoph', 'esophageal', 'ESOPHAGEAL',
  'Esophageal probe', 'Esophageal Probe', 'esophogeal', 'esophogeal probe',
  'Transesoph Core') 
syn_rec <- c('Rectal', '.rec', '.REC', '.rectal', 'core/rectal', 'PR', 'R', 
  'R.', 'rec', 'Rec', 'REC', 'rect', 'rectal', 'Rectal', 'RECTAL')
syn_pa <- c('Pulmonary artery', '.swam', '.swan', '.SWAN', 'Core central line',
  'Intravascular (swan)', 'pa cath', 'PA cath', 'PA CATHETER')
syn_bld <- c('Bladder', 'badder', 'baldder', 'bladder', 'BLADDER', 'blader',
  'Bladr', 'Bldr', 'Core urinary catheter', 'core/bladder', 'foley', 'Foley', 
  'FOLEY', 'temp foley', 'Temperature Sensing Urinary Catheter')
syn_tym <- c('Tympanic', 'tym', 'Tym', 'tymp', 'tympanic', 'Tympanic', 
  'TYMPANIC')
syn_groin <- c('Groin', 'gr', 'groin')
syn_skin <- c('Skin', 'skin', 'SKIN', 'Skin probe', 'Skin Sensor', 
  'Skin Temp Probe', 'ST')
syn_unk <- c('Unknown and other', 'a', 'Blood', 'catheter', 'cooling blanket', 
  'Documentation undone', 'EP', 'Gastric Tube, Oral', 'IC', 'o', 'O', 'O.', 
  'Oth', 'Other', 'OTHER (COMMENT)', 'Other (See Comment)', 'T.', 'temp', 'TEMP', 
  'undocumented', 'unknown', 'vaginal', 'x', 'zoll')

site_list <- list(
    syn_ta,
    syn_ax,
    syn_or,
    syn_cor,
    syn_eso,
    syn_rec,
    syn_pa,
    syn_bld, 
    syn_tym,
    syn_groin,
    syn_skin,
    syn_unk
)

location_data$site <- 'Unknown and other'
for(i in site_list){
    location_data[location_data$chartsite %in% i, 'site'] <- i[[1]]
}

# dim(location_data)
# 4013472

# Remove duplicates
location_data <- location_data[
  !duplicated(location_data[, c('patientunitstayid', 'nursingchartoffset')]),
]

#dim(location_data)
# 4013465     

# Combine chart and site data:
# chart_data LEFT OUTER JOIN location_data
chart_data <- location_data[chart_data]


# Recode NA to 'Unknown and other'
chart_data$site <- ifelse(
  is.na(chart_data$site),
  'Unknown and other',
  chart_data$site
)
  
# table(chart_data$site, useNA = 'always')
#         Axillary           Bladder              Core 
#           623616            173959            337979 
#       Esophageal             Groin              Oral 
#            16777              5120           1912068 
# Pulmonary artery            Rectal              Skin 
#            22441            112905              6363 
#         Temporal          Tympanic Unknown and other 
#           412914            176421           2467313 
#             <NA> 
#                0 

# Save data           
write.csv(chart_data, file = clean_chart_path)
