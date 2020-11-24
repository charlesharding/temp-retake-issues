data <- matched_data

## Recode sites

central_sites <- c('Bladder', 'Core', 'Esophageal', 'Pulmonary artery', 'Rectal')

data <- lapply(
  data,
  function(x) {
    lapply(
      x, 
      function(y) {
        y$index$site <- ifelse(
          y$index$site %in% central_sites, 'Central', y$index$site
        )
        y$match$site <- ifelse(
          y$match$site %in% central_sites, 'Central', y$match$site
        )
        y
      }
    )
  }
)


## Reformat matched data for ease of Bland-Altman analysis

data <- lapply(
  data,
  function(x) {
    lapply(
      x,
      function(y) {
        a <- y$index
        b <- y$match
        z <- data.frame(
          hospitalid = a$hospitalid,
          uniquepid = a$uniquepid,
          patientunitstayid = a$patientunitstayid,
          unitadmittime24 = a$unitadmittime24
        )
        if(a$site < b$site){
          z$offset.1 <- a$nursingchartoffset
          z$offset.2 <- b$nursingchartoffset
          z$site.1 <- a$site
          z$site.2 <- b$site
          z$temperature.1 <- a$temperaturef
          z$temperature.2 <- b$temperaturef                     
        } else {
          z$offset.1 <- b$nursingchartoffset
          z$offset.2 <- a$nursingchartoffset
          z$site.1 <- b$site
          z$site.2 <- a$site
          z$temperature.1 <- b$temperaturef
          z$temperature.2 <- a$temperaturef    
        }
        z
      }
    )
  }
)

data <- do.call(
    rbind, 
    lapply(
        data,
        function(x) {
            do.call(rbind, x)
        }
    )
)

# mean(data[data$site.1 %in% 'Central', ]$temperature.1 <= 95) # 0.1048582
# mean(data[data$site.2 %in% 'Central', ]$temperature.2 <= 95) # 0.09883867
# mean(data[data$site.1 %in% 'Central', ]$temperature.1 <= 92) # 0.04211185
# mean(data[data$site.2 %in% 'Central', ]$temperature.2 <= 92) #  0.04272147

# data$dummy <- 1
# quantile(aggregate(dummy ~ uniquepid, data, sum)[,2], c(0.05, 0.25, 0.5, 0.75, 0.95))
# 5% 25% 50% 75% 95% 
#  1   1   1   3  10 

# table(data[, c('site.1', 'site.2')])
#             site.2
# site.1     Axillary Central  Oral Temporal Tympanic
#   Axillary     8610     411  2215      258       87
#   Central         0   35496   865      252      170
#   Oral            0       0 21294      865      244
#   Temporal        0       0     0     7311      307
#   Tympanic        0       0     0        0     1680

# Time between measurements 
# mean(abs(data$offset.1 - data$offset.2)) # 11.7
# quantile(abs(data$offset.1 - data$offset.2))
#  0%  25%  50%  75% 100% 
#  1    9   15   15   15 

# Retaken temperatures with a central measurement
vs_central_data <- data[data$site.1 %in% 'Central' | data$site.2 %in% 'Central', ]
vs_central_data <- reorder_sites(
  vs_central_data, function(x) (x$site.1 == 'Central')
)

# Temperatures retaken at the same site
vs_same_data <- data[data$site.1 == data$site.2, ]
vs_same_data <- reorder_sites(
  vs_same_data, function(x) (x$offset.1 < x$offset.2)
)


## Run Bland-Altman analyses

vs_central_bland_data <- bland_res_boot(vs_central_data)

for(methods_comparison in c(FALSE, TRUE)){
  bland_plot(
    vs_central_data[!(vs_central_data$site.1 %in% 'Central'), ], 
    vs_central_bland_data[!(vs_central_bland_data$site.1 %in% 'Central'), ],
    y_lim = c(-7.5, 7.5),
    x_lim = c(89, 105.5),
    plot_title = paste('Bland-Altman comparison of central temperatures',
      'and temperatures at other sites\nthat',
      'were measured within', caliper , 'minutes'),
    y_axis_title = 'Difference of measurements\n(central - other)',
    methods_comparison = methods_comparison
  )
  ggsave(
    file.path('.', 'results', paste0('sites-vs-central-', caliper, '-min-',
      file_name_time(), '.png')),
    width = 8, height = 2.75
  )
  ggsave(
    file.path('.', 'results', paste0('sites-vs-central-', caliper, '-min-', 
     file_name_time(), '.svg')),
    width = 8, height = 2.75
  )
  Sys.sleep(1)
}

vs_same_bland_data <- bland_res_boot(vs_same_data)

for(methods_comparison in c(FALSE, TRUE)){
  bland_plot(
    vs_same_data, 
    vs_same_bland_data,
    y_lim = c(-8, 8),
    plot_title = paste('Bland-Altman comparison of temperatures measured',
      'twice at the same site within', caliper, 'minutes'),
    y_axis_title = 'Difference of measurements',
    methods_comparison = methods_comparison
  )
  ggsave(
    file.path('.', 'results', 
      paste0('sites-vs-self-', caliper, '-min-',  
        file_name_time(), '.png')),
    width = 8, height = 2.75
  )
  ggsave(
    file.path('.', 'results', 
      paste0('sites-vs-self-', caliper, '-min-', 
        file_name_time(), '.svg')),
    width = 8, height = 2.75
  )
  Sys.sleep(1)
}

