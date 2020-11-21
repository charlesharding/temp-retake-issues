## Prepare patient data

# Load patient data
patient_data <- read.csv(patient_data_path, stringsAsFactors = FALSE)

# Load Apache results
apache_res_data <- read.csv(apache_res_data_path, stringsAsFactors = FALSE)

# Limit to Apache IV scoring
apache_res_data <- apache_res_data[apache_res_data$apacheversion %in% 'IV', ]

# Load Apache variables
apache_vars_data <- read.csv(apache_vars_data_path, stringsAsFactors = FALSE)

# Merge
apache_vars_data <- merge(
  apache_vars_data, apache_res_data,
  by = 'patientunitstayid',
  all.x = TRUE, all.y = TRUE
)

patient_data <- merge(
  patient_data, apache_vars_data, 
  by = 'patientunitstayid', 
  all.x = TRUE, all.y = FALSE,
  suffixes = c('', '.apache')
)

# Keep some variables
keep_vars <- c('patientunitstayid', 'uniquepid', 'gender', 'age', 'ethnicity', 
  'hospitalid', 'wardid',
  'apacheadmissiondx', 'admissionheight',  'admissionweight',
  'hospitaladmitoffset', 'hospitaladmitsource', 'unittype', 'unitadmittime24', 
  'unitadmitsource', 'unitvisitnumber', 
  'unitdischargeoffset', 'hospitaldischargestatus', 'unitdischargestatus',
  'acutephysiologyscore', 'predictedicumortality', 'predictedhospitalmortality',
  'actualicumortality', 'actualhospitalmortality', 'actualiculos', 'preopmi',
  'unabridgedactualventdays',
  'readmit', 'diabetes', 'electivesurgery', 'immunosuppression', 'midur', 
  'admitdiagnosis'
)

patient_data <- patient_data[ , keep_vars ]


# Save data          
write.csv(patient_data, file = clean_patient_path)

