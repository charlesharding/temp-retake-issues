## Temperature conversion functions

# Fahrenheit to Celsius conversion
fToC <- function(x){5/9 * (x - 32)}

# Celsius to Fahrenheit conversion
cToF <- function(x){9/5 * x + 32}


## File naming functions

# System time as formatted to make it consistent with file names
file_name_time <- function(x) gsub("[[:punct:]]", ".", Sys.time())


## Record matching functions

caliper_time_match_single_admission <- function(
  x, # Data frame or data table with records for one admission
  time_variable, # Name of the time variable for matching, as character
  caliper # Maximum time difference allowed when making matches, as numeric
) {
  # For a single admission, the function returns a list of pairs of records 
  # taken no more than caliper time units apart.
  # Each measurement may be an index or match, but not both.
  
  x <- as.data.frame(x)
  
  y <- list()
  if(dim(x)[[1]] > 0){
  
      x <- x[order(x[, time_variable]), , drop = FALSE]
      i <- 1
      
      while(i < dim(x)[[1]]){
      
        m <- list(index = x[i, ], matches = list())
        j <- i + 1
        
        while(
          (j <= dim(x)[[1]]) &
          ((x[j, time_variable] - x[i, time_variable]) <= caliper)
        ) {
           m$matches[[length(m$matches) + 1]] <- x[j, ]
           j <- j + 1
        }
        
        i <- j
        
        if(length(m$matches) > 0){
            y[[length(y) + 1]] <- m
        }
        
    }
  }
  y
}

select_first_match <- function(
  matches # List of matches
) {
  # Select the first (closest) match for each index measurement in the list of
  # matches for a single admission
  lapply(
    matches,
    function(y) {
      if(length(y) > 0){
        list(index = y$index, match = y$matches[[1]])
      } else {
        NULL
      }
    }
  )
}

caliper_time_match <- function(
  data, # List, each element of which is records for a single admission
  time_variable, # Name of the time variable for matching, as character
  caliper # Maximum time difference allowed when making matches, as numeric
) {
  # For each admission in data, the function returns a list of pairs of records 
  # taken no more than caliper time units apart.
  # Each measurement may be an index or match, but not both.
  
  res <- lapply(
    data,
    function(x) {
      select_first_match(
        caliper_time_match_single_admission(
          x, time_variable, caliper
        )
      )
    }
  )
  
  # Drop any admissions with no matches found
  res <- res[lapply(res, length) > 0]  
  res
}


## Table making functions

summary_table_col <- function(
  data
) {
  # Prepare a column for summary table (Table 1)

  rnd <- function(x) round(x, 1)
  miqr <- function(x, d = 1) {
    paste0(
      round(median(x, na.rm = TRUE), d), 
      ' (', round(quantile(x, 0.25, na.rm = TRUE), d), ', ', 
        round(quantile(x, 0.75, na.rm = TRUE), d), ')'
    ) 
  }
  npc <- function(n, n_tot, d = 1) {
    paste0(
      n, ' (', round(100*n/n_tot, d), '%)'
    )
  }
  cnp <- function(name, values, d = 1) {
    y <- table(values)
    p <- round(prop.table(y) * 100, d)
    z <- paste0(y, ' (', p, '%)')
    z <- c('', z)
    names(z) <- c(name, paste0('  ', names(y)))
    z
  }
  
  # Basic data cleaning
  data$age <- as.numeric(data$age)
  data$gender <- ifelse(
    data$gender %in% "",
    "Unknown",
    data$gender
  )
  data$ethnicity <- ifelse(
    data$ethnicity %in% c("", "Other/Unknown"),
    "Other and Unknown",
    data$ethnicity
  )
  data$bmi <- data$admissionweight / (data$admissionheight/100)^2
  
  data$predictedicumortality <- ifelse(
    data$predictedicumortality >= 0 & data$predictedicumortality <= 1,
    data$predictedicumortality,
    NA
  )
  
  # Analyzed temperatures
  data$dummy <- ifelse(
    is.finite(data$temperaturef) & (data$exclusionflag %in% 'None'),
    1,
    0
  )
  tpp <- aggregate(dummy ~ uniquepid, data, FUN = sum, na.rm = TRUE)[, 2]
  nt <- sum(tpp)
  

  data$table.sepsis <- ifelse(data$apacheadmissiondx %in% 
    c('Sepsis, gynecologic', 'Sepsis, pulmonary', 'Sepsis, renal/UTI (including bladder)',
      'Sepsis, GI', 'Sepsis, unknown', 'Sepsis, cutaneous/soft tissue',
      'Sepsis, other'),
     1, 0)
  data$table.pneumonia <- ifelse(data$apacheadmissiondx %in% 
    c('Pneumonia, fungal', 'Pneumonia, viral', 
      'Pneumonia, parasitic (i.e., Pneumocystic pneumonia)', 'Pneumonia, other',
      'Pneumonia, aspiration', 'Pneumonia, bacterial'),
    1, 0)
  data$table.mi <- ifelse(data$apacheadmissiondx %in% 
    # MI: yocardial infarction, acuta and > 24 h after ischemia onset
    c('Infarction, acute myocardial (MI)', 
    'MI admitted > 24 hrs after onset of ischemia'
    ),
    1, 0)
  data$table.chf <- ifelse(data$apacheadmissiondx %in% 
    # CHF: Congestive heart failure
    c('CHF, congestive heart failure'),
    1, 0)
  data$table.stroke <- ifelse(data$apacheadmissiondx %in% 
    # Stroke-Hem: cerebrovascular accident, subarachnoid hemorrhage,
    # intracranial hemorrhage and hematoma, and related surgeries.
    c('CVA, cerebrovascular accident/stroke',
    'Subarachnoid hemorrhage/arteriovenous malformation', 
    'Subarachnoid hemorrhage/intracranial aneurysm, surgery for',
    'Subarachnoid hemorrhage/intracranial aneurysm',
    'Hemorrhage/hematoma, intracranial', 
    'Hemorrhage/hematoma-intracranial, surgery for'),
    1, 0) 
  data$table.trauma <- ifelse(data$apacheadmissiondx %in% 
    # Trauma: Trauma and surgery for
    c('Pelvis/face trauma', 'Abdomen/face trauma', 'Pelvis/spinal trauma',
    'Abdomen/spinal trauma', 'Chest/pelvis trauma', 'Chest/thorax only trauma',
    'Head/pelvis trauma', 'Abdomen/pelvis trauma', 'Extremity/face trauma',
    'Chest/face trauma', 'Head/abdomen trauma', 'Pelvis/multiple trauma',
    'Pelvis/extremity trauma', 'Spinal/face trauma', 'Face/multiple trauma',
    'Extremity/multiple trauma', 'Abdomen/extremity trauma', 'Trauma medical, other',
    'Spinal/extremity trauma', 'Face only trauma', 'Chest/spinal trauma', 
    'Chest/extremity trauma', 'Spinal/multiple trauma', 'Abdomen/multiple trauma',
    'Abdomen only trauma', 'Pelvis/hip trauma', 'Chest/abdomen trauma', 
    'Head/spinal trauma', 'Head/chest trauma', 'Extremity only trauma',
    'Chest thorax only trauma', 'Spinal cord only trauma', 'Head/extremity trauma',
    'Chest/multiple trauma', 'Head/face trauma', 'Head/multiple trauma', 
    'Head only trauma', 
    'Extremity/face trauma, surgery for', 'Face/multiple trauma, surgery for',
    'Trauma surgery, other', 'Face only trauma, surgery for', 
    'Extremity/multiple trauma, surgery for', 'Extremity only trauma, surgery for'
    ),
     1, 0)

  # Limit to 1 record per patient
  pdata <- data[order(data$uniquepid, data$patientunitstayid), ]
  r <- !duplicated(pdata[, c('uniquepid')])
  pdata <- pdata[r, ] 
  np <- dim(pdata)[[1]]
  
  res <- list(
    Hospitals = length(unique(data$hospitalid)),
    `Temperatures` = nt,
    `Temperatures\nper patient*` = miqr(tpp),
    Patients = np,
    Gender = '',
    `  Female` = npc(sum(pdata$gender %in% 'Female'), np),
    `  Male` = npc(sum(pdata$gender %in% 'Male'), np),
    `  Other` = npc(sum(pdata$gender %in% 'Other'), np),
    `  Unknown` = npc(sum(pdata$gender %in% 'Unknown'), np),
    `Age, years` = miqr(pdata$age),
    Ethnicity = '',
    `  African American` = npc(sum(pdata$ethnicity %in% 'African American'), np),
    `  Asian` = npc(sum(pdata$ethnicity %in% 'Asian'), np),
    `  Caucasian` = npc(sum(pdata$ethnicity %in% 'Caucasian'), np),
    `  Hispanic` = npc(sum(pdata$ethnicity %in% 'Hispanic'), np),
    `  Native American` = npc(sum(pdata$ethnicity %in% 'Native American'), np),
    `  Other and unknown` = npc(sum(pdata$ethnicity %in% 'Other and Unknown'), np),
    `BMI, kg/m^2` = miqr(pdata$bmi),
    `  Unknown` = npc(sum(!is.finite(pdata$bmi)), np),
    `Diabetes` = '',
    `  Yes` = npc(sum(pdata$diabetes %in% 1), np),
    `  No` = npc(sum(pdata$diabetes %in% 0), np),
    `  Unknown` = npc(sum(is.na(pdata$diabetes)), np),
    `MI in last 6 mo.` = '',
    `  Yes` = npc(sum(pdata$midur %in% 1), np),
    `  No` = npc(sum(pdata$midur %in% 0), np),
    `  Unknown` = npc(sum(is.na(pdata$midur)), np),
    `Admission diagnosis` = '',
    `  Sepsis` = npc(sum(pdata$table.sepsis %in% 1), np),
    `  Pneumonia` = npc(sum(pdata$table.pneumonia %in% 1), np),
    `  MI` = npc(sum(pdata$table.mi %in% 1), np),
    `  CHF` = npc(sum(pdata$table.chf %in% 1), np),
    `  Stroke-Hem` = npc(sum(pdata$table.stroke %in% 1), np),
    `  Trauma` = npc(sum(pdata$table.trauma %in% 1), np),
    `Unit discharge status` = '',
    `  Alive` = npc(sum(pdata$unitdischargestatus %in% 'Alive'), np),
    `  Expired` = npc(sum(pdata$unitdischargestatus %in% 'Expired'), np),
    `  Unknown` = npc(sum(pdata$unitdischargestatus %in% ''), np),
    `In-ICU death probability` = miqr(pdata$predictedicumortality, d = 2),
    `  Unknown or inapplicable` = npc(
      sum(!is.finite(pdata$predictedicumortality)), np
    )
  )
  res
}

format_and_save_table <- function(
  data, # Data frame
  path, # Path to save at
  caption = NULL, # Table caption
  footnotes = NULL
) {
  ft <- flextable(data)
  if(!is.null(caption)){
    ft <- set_caption(ft, caption = caption)
  }
  if(!is.null(footnotes)){
    ft <- footnote(
      ft, i = 1, j = seq_along(footnotes),
      value = as_paragraph(footnotes),
      ref_symbols = rep('', length(footnotes)),
      part = "header"
    )
  }
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = 'center', part = 'all')
  ft <- align(ft, j = 1, align = 'left', part = 'all')
  ft <- autofit(ft)
  save_as_docx("Table" = ft, path = path)
}


## Bland-Altman analysis functions

reorder_sites <- function(data, reorder_function){
  # Reorder the matched measurements data frame, for consistency in the
  # Bland-Altman analyses.

  n <- names(data)
  n_1 <- n[substr(n, nchar(n) - 1, nchar(n)) == '.1']
  n_2 <- n[substr(n, nchar(n) - 1, nchar(n)) == '.2']
  
  data <- lapply(
    seq_len(dim(data)[[1]]),
    function(i) {
      x <- data[i, ]
      if(reorder_function(x)){
        y <- x
        y[, n_1] <- x[, n_2]
        y[, n_2] <- x[, n_1]
        x <- y
      }
      x
    }
  )
  data <- do.call(rbind, data)
  data
  
}

bland_res <- function(data) {
  # Obtain Bland-Altman results in a convenient format, with 
  # temperature.2 - temperature.1
  res <- lapply(
    sort(unique(data$site.1)),
    function(i) {
      y <- data[data$site.1 %in% i, ]
      z <- blandr::blandr.statistics(
        y$temperature.2,
        y$temperature.1,
        sig.level = 0.95
      )
      data.frame(
        n = dim(y)[[1]],
        site.1 = y$site.1[[1]],
        site.2 = y$site.2[[1]],
        bias = z$bias,
        bias.uci = z$biasUpperCI,
        bias.lci = z$biasLowerCI,
        loa.upper = z$upperLOA,
        loa.upper.uci = z$upperLOA_upperCI,
        loa.upper.lci = z$upperLOA_lowerCI,
        loa.lower = z$lowerLOA,
        loa.lower.uci = z$lowerLOA_upperCI,
        loa.lower.lci = z$lowerLOA_lowerCI
      )
    }
  )
  do.call(rbind, res)
}

bland_res_boot <- function(
  data, 
  reps = 20000, # Replicates
  two_stage = FALSE # One- or two-stage clustered bootstrap? 
) {
  # Obtain Bland-Altman results in a convenient format, with 
  # temperature.2 - temperature.1 as the difference.
  
  # This version of the function uses bootstrapping that approximately 
  # accounts for multiple measurement pairs per patient.
  
  # Let n be the number of patient. In each replicate of the one-stage 
  # bootstrap, a vector of length n is generated by resampling patients with
  # replacement. Bland-Altman parameters are calculated for the measurement 
  # pairs associated with the patient resamples. 
  #
  # In each replicate of the two-stage bootstrap, a vector of length n is
  # generated in the same way. However, for each entry of the vector, the
  # measurement pairs from the patient are also resampled with replacement. The
  # Bland-Altman parameters are calculated for the resampled measurement pairs,
  # thereby including two stages/levels of resampling in the analysis.
  #
  # One- and two-stage bootstrap returned very similar results for this dataset.
  # The one-stage bootstrap is the usual cluster bootstrap and is used for the 
  # main analyses. For discussion of one- vs. two-stage bootstrap, see
  #   * Davison AC, Hinkley DV. Bootstrap methods and their application. 1997.
  #   * Field CA, Welsh AH. Bootstrapping clustered data. J R Statist Soc B 
  #        2007;69(3):369-90.
  #   * Ren S et al. Nonparametric bootstrapping for hierarchical data. J Appl 
  #       Stat 2010;37(9) 


  data$dif <- data$temperature.2 - data$temperature.1

  res <- lapply(
    sort(unique(data$site.1)),
    function(i) {
      w <- data[data$site.1 %in% i, ]
      w <- w[order(w$uniquepid, w$offset.1, w$offset.2), ]
      site_1 <- w$site.1[[1]]
      site_2 <- w$site.2[[1]]
      w$uniquepid <- as.character(w$uniquepid)
      w <- w[ , c('uniquepid', 'dif')]
      dif <- w$dif
      
      # Instead of using nested loop, make look-up table for bootstrapping: 
      # A data frame with a row for each PID that has the number of measurements
      # and their starting row in w
      p <- as.data.frame(table(w$uniquepid)) 
      names(p) <- c('uniquepid', 'records')
      p$starting.row <- Reduce(sum, p$records, accumulate = TRUE) - 
        p$records + 1
      row.names(p) <- NULL
      
      records <- p$records
      starting_row <- p$starting.row

      r <- sample(
        seq_len(dim(p)[[1]]), 
        reps * dim(p)[[1]],
        replace = TRUE
      )
      r <- matrix(r, nrow = reps)
      
      g <- function(i){
        if(two_stage){ 
          x <- lapply(i, function(j) {
            k <- sample.int(records[[j]], replace = TRUE)
            dif[starting_row[[j]] + k - 1]
          })
        } else {
          x <- lapply(i, function(j) {
            k <- seq_len(records[[j]])
            dif[starting_row[[j]] + k - 1]
          })
        }
        
        # x_single: only 1 mesurement pair per patient.
        if(two_stage){
          x_single <- lapply(x, `[[`, 1)
        } else {
          x_single <- lapply(x, function(d) d[sample.int(length(d), 1)])
        }
        
        x <- do.call(c, x) 
        x_single <- do.call(c, x_single)
        
        data.frame(
          bias = mean(x),
          loa.lower = mean(x) - 2 * sd(x),
          loa.upper = mean(x) + 2 * sd(x),
          bias.single = mean(x_single),
          loa.single.lower  = mean(x_single) - 2 * sd(x_single),
          loa.single.upper = mean(x_single) + 2 * sd(x_single),
          loa.q.lower = quantile(x, 0.025),
          loa.q.upper = quantile(x, 0.975),
          loa.q.single.lower = quantile(x_single, 0.025),
          loa.q.single.upper = quantile(x_single, 0.975)
        )
      }
      
      z <- apply(r, 1, g)
      z <- do.call(rbind, z)
      
      data.frame(
        n = length(dif),
        site.1 = site_1,
        site.2 = site_2,
        bias = mean(dif),
        bias.lci = quantile(z$bias, 0.025),
        bias.uci = quantile(z$bias, 0.975),
        loa.lower = mean(dif) - 2 * sd(dif),
        loa.lower.lci = quantile(z$loa.lower, 0.025),
        loa.lower.uci = quantile(z$loa.lower, 0.975),
        loa.upper = mean(dif) + 2 * sd(dif),
        loa.upper.lci = quantile(z$loa.upper, 0.025),
        loa.upper.uci = quantile(z$loa.upper, 0.975),
        bias.single = median(z$bias.single),
        bias.single.lci = quantile(z$bias.single, 0.025),
        bias.single.uci = quantile(z$bias.single, 0.975),
        loa.single.lower = median(z$loa.single.lower),
        loa.single.lower.lci = quantile(z$loa.single.lower, 0.025),
        loa.single.lower.uci = quantile(z$loa.single.lower, 0.975),
        loa.single.upper = median(z$loa.single.upper),
        loa.single.upper.lci = quantile(z$loa.single.upper, 0.025),
        loa.single.upper.uci = quantile(z$loa.single.upper, 0.975),
        loa.q.lower = quantile(dif, 0.025),
        loa.q.lower.lci = quantile(z$loa.q.lower, 0.025),
        loa.q.lower.uci = quantile(z$loa.q.lower, 0.975),
        loa.q.upper = quantile(dif, 0.975),
        loa.q.upper.lci = quantile(z$loa.q.upper, 0.025),
        loa.q.upper.uci = quantile(z$loa.q.upper, 0.975),
        loa.q.single.lower = median(z$loa.q.single.lower),
        loa.q.single.lower.lci = quantile(z$loa.q.single.lower, 0.025),
        loa.q.single.lower.uci = quantile(z$loa.q.single.lower, 0.975),
        loa.q.single.upper = median(z$loa.q.single.upper),
        loa.q.single.upper.lci = quantile(z$loa.q.single.upper, 0.025),
        loa.q.single.upper.uci = quantile(z$loa.q.single.upper, 0.975)
      )
    }
  )
  do.call(rbind, res)
}

bland_plot <- function(
  point_data,
  bland_res,
  plot_title = 'Bland-Altman comparison',
  y_axis_title = 'Difference of measurements\n(measurement 2 - measurement 1',
  x_lim = c(86, 106),
  y_lim = c(-10, 10),
  methods_comparison = FALSE
) {

  point_data$temperature.mean <- 1/2 *
    (point_data$temperature.1 + point_data$temperature.2)

  point_data$temperature.diff <-
    (point_data$temperature.2 - point_data$temperature.1)
    
  text_data <- bland_res
  text_data$y.bias <- max(y_lim) - 0.5
  text_data$y.loa.upper <- min(y_lim) + 2
  text_data$y.loa.lower <- min(y_lim) + 0.5
  text_data$x <- max(x_lim) - 1/10
  text_data$bias.text <- paste0(
    round(text_data$bias, 1), '\u00b0F (', 
    round(text_data$bias.lci, 1), ', ', round(text_data$bias.uci, 1), ')'
  )
  text_data$loa.upper.text <- paste0(
    round(text_data$loa.upper, 1), '\u00b0F (',
    round(text_data$loa.upper.lci, 1), ', ', round(text_data$loa.upper.uci, 1),
    ')'
  )
  text_data$loa.lower.text <- paste0(
    round(text_data$loa.lower, 1), '\u00b0F (',
    round(text_data$loa.lower.lci, 1), ', ', round(text_data$loa.lower.uci, 1),
    ')'
  )

  color_bias <- '#e41a1c'
  color_loa <- '#2166bc'
  
  figure <- ggplot(point_data) +
    geom_hline(yintercept = 0, size = 0.75) +
    geom_point(
        aes(x = temperature.mean, y = temperature.diff),
        size = 0.3
    ) +
    geom_segment(
        data = bland_res,
        aes(
            y = bias,
            yend = bias,
            x = -100, xend = 200
        ), 
        linetype = 1, 
        size = 0.5,
        color = color_bias
    ) +
    geom_segment(
        data = bland_res,
        aes(
            y = loa.lower,
            yend = loa.lower,
            x = -100, xend = 200
        ), 
        linetype = 5, 
        size = 0.5,
        color = color_loa
    ) +
    geom_segment(
        data = bland_res,
        aes(
            y = loa.upper,
            yend = loa.upper,
            x = -100, xend = 200
        ), 
        linetype = 5, 
        size = 0.5,
        color = color_loa
    ) +
    geom_errorbar(
        data = bland_res,
        aes(
            ymin = loa.lower.lci,
            ymax = loa.lower.uci,
            x = min(x_lim) + 1
        ), 
        linetype = 1, 
        size = 0.5,
        color = color_loa
    ) +
    geom_errorbar(
        data = bland_res,
        aes(
            ymin = loa.upper.lci,
            ymax = loa.upper.uci,
            x = min(x_lim) + 1
        ), 
        linetype = 1, 
        size = 0.5,
        color = color_loa
    ) +
    geom_errorbar(
        data = bland_res,
        aes(
            ymin = bias.lci,
            ymax = bias.uci,
            x = min(x_lim) + 1
        ), 
        linetype = 1, 
        size = 0.5,
        color = color_bias
    ) +
    geom_text(
      data = text_data,
      aes(
        x = x,
        y = y.bias,
        label = bias.text
      ),
      size = 2.9,
      hjust = 1,
      color = color_bias,
      family = "Roboto Condensed"
    ) +
    geom_text(
      data = text_data,
      aes(
        x = x,
        y = y.loa.upper,
        label = loa.upper.text
      ),
      size = 2.9,
      hjust = 1,
      color = color_loa,
      family = "Roboto Condensed"
    ) +
    geom_text(
      data = text_data,
      aes(
        x = x,
        y = y.loa.lower,
        label = loa.lower.text
      ),
      size = 2.9,
      hjust = 1,
      color = color_loa,
      family = "Roboto Condensed"
    ) +
    coord_fixed(xlim = x_lim, ylim = y_lim) +
    scale_y_continuous(
        y_axis_title,
        breaks = -100:100*4, 
        labels = dFS(-100:100*4) #,
        #sec.axis = sec_axis(
        #    ~ (.)*5/9, name = NULL, 
        #    breaks = -100:100, labels = dCS(-100:100)
        #)
    ) +
    scale_x_continuous(
        paste('Mean of measurements'),
        breaks = -100:100*4, 
        labels = dFS(-100:100*4) #,
        #sec.axis = sec_axis(
        #    ~ (. -32)*5/9, name = NULL, 
        #    breaks = -100:100, labels = dC(-100:100)
        # )
    ) +
    facet_wrap(~ site.1, nrow = 1) +
    theme_pub_ext +
    labs(subtitle = plot_title)

  if(methods_comparison){
    color_bias_single <- 'gray'
    color_loa_single <- 'gray'
    
    color_bias_q <- '#8856a7'
    color_loa_q <- '#8856a7'
    
    color_bias_q_single <- '#8c6bb1'
    color_loa_q_single <- '#8c6bb1'
  
    figure <- figure +
      geom_segment(
          data = bland_res,
          aes(
              y = bias.single,
              yend = bias.single,
              x = -100, xend = 200
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_bias_single
      ) +
      geom_segment(
          data = bland_res,
          aes(
              y = loa.single.lower,
              yend = loa.single.lower,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_single
      ) +
      geom_segment(
          data = bland_res,
          aes(
              y = loa.single.upper,
              yend = loa.single.upper,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_single
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.single.lower.lci,
              ymax = loa.single.lower.uci,
              x = min(x_lim) + 1.5
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_single
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.single.upper.lci,
              ymax = loa.single.upper.uci,
              x = min(x_lim) + 1.5
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_single
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = bias.single.lci,
              ymax = bias.single.uci,
              x = min(x_lim) + 1.5
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_bias_single
      ) + 
      geom_segment(
          data = bland_res,
          aes(
              y = loa.q.lower,
              yend = loa.q.lower,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_q
      ) +
      geom_segment(
          data = bland_res,
          aes(
              y = loa.q.upper,
              yend = loa.q.upper,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_q
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.q.lower.lci,
              ymax = loa.q.lower.uci,
              x = min(x_lim) + 2
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_q
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.q.upper.lci,
              ymax = loa.q.upper.uci,
              x = min(x_lim) + 2
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_q
      ) +
      geom_segment(
          data = bland_res,
          aes(
              y = loa.q.single.lower,
              yend = loa.q.single.lower,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_q_single
      ) +
      geom_segment(
          data = bland_res,
          aes(
              y = loa.q.single.upper,
              yend = loa.q.single.upper,
              x = -100, xend = 200
          ), 
          linetype = 5, 
          size = 0.5,
          color = color_loa_q_single
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.q.single.lower.lci,
              ymax = loa.q.single.lower.uci,
              x = min(x_lim) + 2.5
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_q_single
      ) +
      geom_errorbar(
          data = bland_res,
          aes(
              ymin = loa.q.single.upper.lci,
              ymax = loa.q.single.upper.uci,
              x = min(x_lim) + 2.5
          ), 
          linetype = 1, 
          size = 0.5,
          color = color_loa_q_single
      ) 
  }
  print(figure)
}

