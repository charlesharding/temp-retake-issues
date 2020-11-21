## Combine patient and chart data, and finish exclusions

# Merge patient and chart data
data <- merge(
  patient_data,
  chart_data,
  by = 'patientunitstayid',
  all.x = FALSE,
  all.y = TRUE
)

# Exclusion flag for thermometer sites not studied
data$anystudiedsite <- !(
  is.na(data$site) | data$site %in% c('Groin', 'Skin', 'Unknown and other')
)
data$exclusionflag <- ifelse(
  !data$anystudiedsite, 
  'Thermometer site not studied',
  data$exclusionflag
)

# Exclusion flag for patients age <18
data$exclusionflag <- ifelse(
  !(data$age %in% 18:110),
  'Patient not adult',
  data$exclusionflag
)

# Exclusion flag for hospitals that never or rarely have the studied thermometer
# sites listed on patient charts (<50 times)
hospitals_w_site <- as.data.frame(
  aggregate(anystudiedsite ~ hospitalid, data, sum)
)
# dim(hospitals_w_site) # 205 hospitals
hospitals_w_site <- hospitals_w_site[hospitals_w_site[,2]>=50, 1]
# length(hospitals_w_site) # 154 hospitals
data$exclusionflag <- ifelse(
  !(data$hospitalid %in% hospitals_w_site), 
  'Studied thermometers not listed by hospital',
  data$exclusionflag
)

# Count inclusions and exclusions

# length(unique(patient_data$hospitalid)) # 208
# length(unique(patient_data$uniquepid)) # 139367
# length(unique(patient_data$patientunitstayid)) # 200859

# length(hospitals_w_site) # 154
# length(unique(
  # patient_data[patient_data$hospitalid %in% hospitals_w_site, ]$uniquepid
# )) # 115253
# length(unique(
  # patient_data[patient_data$hospitalid %in% hospitals_w_site, 
    # ]$patientunitstayid
# )) # 167113

# unique(data$exclusionflag)

# count_chars <- function(data){
  # data.frame(
    # hospitals = length(unique(data$hospitalid)),
    # patients = length(unique(data$uniquepid)),
    # stays = length(unique(data$patientunitstayid)),
    # temperatures = dim(data)[[1]]
  # )
# }

# count_chars(data)
# count_chars(
  # data[
    # !(data$exclusionflag %in% 
      # c('Studied thermometers not listed by hospital', 
        # 'Patient not adult')
    # ), 
  # ]
# )

# sum(data$exclusionflag %in% 'Thermometer site not studied') # 1617897
# sum(data$exclusionflag %in% 'Implausible temperature') # 16449
# sum(data$exclusionflag %in% 'Unclear if separate measurement') # 104
# sum(data$exclusionflag %in% 'F and C values differ') # 3
# total: 1634453

# count_chars(data[data$exclusionflag %in% 'None', ])

# sum(data$exclusionflag %in% 'None' & data$site %in% 'Pulmonary artery') # 22371


## Limit to included temperatures and summarize characteristics

inc_data <- data[data$exclusionflag %in% 'None', ]

# mean(inc_data[inc_data$site %in% central_sites, ]$temperaturef < 95) # 0.0474
# mean(inc_data[inc_data$site %in% central_sites, ]$temperaturef < 92) # 0.02170%

inc_data_table_col <- summary_table_col(inc_data)


## Match quickly retaken temperatures, save, and summarize characteristics

matched_data <- as.data.table(inc_data)  
matched_data <- split(matched_data, by = 'patientunitstayid')

matched_data <- caliper_time_match(
  matched_data, 
  'nursingchartoffset', 
  caliper = caliper
)
        
save(matched_data, file = clean_matched_path)

m_index_df <- do.call(rbind, lapply(matched_data, function(x) {
  do.call(rbind, lapply(x, function(y) y$index))
}))
m_index_df$recordtype = 'index'
m_match_df <- do.call(rbind, lapply(matched_data, function(x) {
  do.call(rbind, lapply(x, function(y) y$match))
}))
m_match_df$recordtype = 'match'
m_df <- rbind(m_index_df, m_match_df)

# length(unique(m_df$hospitalid)) # 139
# length(unique(m_df$uniquepid)) # 24765
# length(unique(m_df$patientunitstayid)) # 27709
# nrow(m_df) # 160130

#        Axillary          Bladder             Core 
#           20191            18150            37626 
#      Esophageal             Oral Pulmonary artery 
#            2005            46777             2012 
#          Rectal         Temporal         Tympanic 
#           12897            16304             4168 

matched_data_table_col <- summary_table_col(m_df)

## Create table draft

table_df <- as.data.frame(cbind(
  names(inc_data_table_col), 
  inc_data_table_col, 
  matched_data_table_col
))

format_and_save_table(
  table_df, 
  char_table_path,
  caption = paste('Characteristics for all recorded temperatures meeting the',
    'study inclusion criteria, and for temperatures that were remeasured',
    'within 15 minutes.'),
  footnotes = paste('Values are n (%) or median (interquartile range).\n',
    'BMI: body mass index. MI: myocardial infarction.', 
    'For admission diagnosis, includes both acute MI and > 24 h after',
    'ischemia onset. CHF: congestive heart failure. Stroke-Hem:',
    'cerebrovascular accident, subarachnoid hemorrhage, intracranial',
    'hemorrhage and hematoma, and related surgeries. Trauma: Trauma',
    'and surgery for trauma. ICU: intensive care unit.\n',
    'Admission diagnoses are recoded from APACHE IV scoring categories.',
    'Probability of in-ICU death is obtained from APACHE IV scores.\n',
    'For patients with multiple admissions, characteristics from the first',
    'admission are shown.'
  )
)


## Clear memory
rm(data)
rm(inc_data)
rm(chart_data)
rm(patient_data)






