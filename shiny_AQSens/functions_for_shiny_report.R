
## Functions for shiny_DB_sensors.Rmd

library(dplyr)

#######################################################
# compute numbers of records by pollutants and model ##
#######################################################
Table_1_func <- function(MODEL      = MODEL,
                         POLLUTANT  = POLLUTANT,
                         aggregator = aggregator) {

  if (aggregator == TRUE) {
    
    # do not aggregate counts of records by pollutants
    
    filter_pollutant_sensor <- DB %>%
      dplyr::filter(model     == MODEL) %>%
                    # pollutant == POLLUTANT) 
      dplyr::group_by(type_sensor,
                      field___lab) %>%
      summarise(records    = length(pollutant),
                references = paste(unique(new_ref), collapse=", "),
                link       = paste(unique(paste0("[",id_ref, "] ", link)), collapse=", ")) %>%
      mutate(pollutant = POLLUTANT)
    filter_pollutant_sensor <- as.data.frame(filter_pollutant_sensor)
    
  }
  
  if (aggregator == FALSE) {
    # aggregate counts of records by pollutants
    filter_pollutant_sensor <- DB %>%
      dplyr::filter( model == MODEL) %>%
      #              pollutant == POLLUTANT) %>%
      dplyr::group_by(type_sensor,
                      pollutant,
                      field___lab,
                      model) %>%
      summarise(records = length(pollutant),
                references = paste(unique(new_ref), collapse=", "),
                link       = paste(unique(paste0("[",id_ref, "] ", link)), collapse=", "))
    filter_pollutant_sensor <- as.data.frame(filter_pollutant_sensor)
  }
  
  # add column for the Model type
  filter_pollutant_sensor$model <- MODEL
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM2.5$","$PM_{2.5}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM10$","$PM_{10}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM1$","$PM_{1}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^NO2$","$NO_{2}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^O3$","$O_{3}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^CO$","$CO$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^NO$","$NO$", levels(filter_pollutant_sensor$pollutant))
  
  filter_pollutant_sensor <- filter_pollutant_sensor %>%
    dplyr::select(model,
           pollutant,
           type_sensor,
           field___lab,
           records,
           references,
           link)
  return(filter_pollutant_sensor)
}

##################################
# calibration records table ######
##################################

# pollutant_sensor <- DB %>%
#   dplyr::filter(model     == MODEL)
# POLLUTANT <- unique(pollutant_sensor$pollutant)

Table_2_func <- function(MODEL      = MODEL,
                        POLLUTANT  = POLLUTANT) {

    # do not aggregate counts of records by pollutants
    
    filter_pollutant_sensor <- DB %>%
      dplyr::filter(model     == MODEL,
                    pollutant == POLLUTANT) %>%
      dplyr::group_by(pollutant) %>%
      mutate(references = paste(unique(new_ref), collapse=", "),
            link        = paste(unique(paste0("[",id_ref, "] ", link)), collapse="\n")) 
       # mutate(Pollutant = unique(filter_pollutant_sensor$pollutant))
    filter_pollutant_sensor <- as.data.frame(filter_pollutant_sensor)
  
   
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM2.5$","$PM_{2.5}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM10$","$PM_{10}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^PM1$","$PM_{1}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^NO2$","$NO_{2}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^O3$","$O_{3}$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^CO$","$CO$", levels(filter_pollutant_sensor$pollutant))
  # levels(filter_pollutant_sensor$pollutant) <- gsub("^NO$","$NO$", levels(filter_pollutant_sensor$pollutant))
 
  # change names FIELD and LAB
  levels(filter_pollutant_sensor$field___lab) <- gsub("^FIELD$","Field Test", levels(filter_pollutant_sensor$field___lab))
  levels(filter_pollutant_sensor$field___lab) <- gsub("^LAB$","Laboratory Test", levels(filter_pollutant_sensor$field___lab))
  
  # calibration models
  DB_calibration <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$r2_calib)), ]
  n_DB_calibration <- nrow(DB_calibration)
  
  DB_calibration_LAB <- DB_calibration %>%
    filter(field___lab == "Laboratory Test")
  n_DB_calibration_LAB <- nrow(DB_calibration_LAB)
 
  
  DB_calibration_FIELD <- DB_calibration %>%
    filter(field___lab == "Field Test")
  n_DB_calibration_FIELD <- nrow(DB_calibration_FIELD)


  # comparison (regression lines)
  DB_comparison <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$r2)), ]
  n_DB_comparison_r2 <- nrow(DB_comparison)
  
 
  DB_comparison_LAB <- DB_comparison %>%
    filter(field___lab == "Laboratory Test")
  n_DB_comparison_LAB <- nrow(DB_comparison_LAB)


  DB_comparison_FIELD <- DB_comparison %>%
    filter(field___lab == "Field Test") 
  n_DB_comparison_FIELD <- nrow(DB_comparison_FIELD)
  

  # total number of comparison tests reporting the slope
  DB_comparison_slope <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$slope)), ]
  n_DB_comparison_slope <- nrow(DB_comparison_slope)
  
  # total number of comparison tests reporting the intercept
  DB_comparison_intercept <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$intercept)), ]
  n_DB_comparison_intercept <- nrow(DB_comparison_intercept)
  
 
  DB_comparison_slope_LAB <- DB_comparison_slope %>%
    filter(field___lab == "Laboratory Test") 
  n_DB_comparison_slope_LAB <- nrow(DB_comparison_slope_LAB)
  

  DB_comparison_intercept_LAB <- DB_comparison_intercept %>%
    filter(field___lab == "Laboratory Test") 
  n_DB_comparison_intercept_LAB <- nrow(DB_comparison_intercept_LAB)
  

  DB_comparison_slope_FIELD <- DB_comparison_slope %>%
    filter(field___lab == "Field Test")
  n_DB_comparison_slope_FIELD <- nrow(DB_comparison_slope_FIELD)
  
  DB_comparison_intercept_FIELD <- DB_comparison_intercept %>%
    filter(field___lab == "Field Test")
  n_DB_comparison_intercept_FIELD <- nrow(DB_comparison_intercept_FIELD)
  

  # total number of comparison tests reporting the RMSE
  DB_comparison_RMSE <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$rmse)), ]
  n_DB_comparison_RMSE <- nrow(DB_comparison_RMSE)
  
  DB_comparison_RMSE_LAB <- DB_comparison_RMSE %>%
    filter(field___lab == "Laboratory Test")
  n_DB_comparison_RMSE_LAB <- nrow(DB_comparison_RMSE_LAB)

  DB_comparison_RMSE_FIELD <- DB_comparison_RMSE %>%
    filter(field___lab == "Field Test") 
  n_DB_comparison_RMSE_FIELD <- nrow(DB_comparison_RMSE_FIELD)
  
  # total number of comparison tests reporting the Uncertainty (U)
  DB_comparison_U <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$u)), ]
  n_DB_comparison_U <- nrow(DB_comparison_U)
  
  DB_comparison_U_LAB <- DB_comparison_U %>%
    filter(field___lab == "Laboratory Test")
  n_DB_comparison_U_LAB <- nrow(DB_comparison_U_LAB)
  
  DB_comparison_U_FIELD <- DB_comparison_U %>%
    filter(field___lab == "Field Test")
  n_DB_comparison_U_FIELD <- nrow(DB_comparison_U_FIELD)
  
  
  n_FIELD_tests <- c(# as.numeric(field_tests),
    as.numeric(n_DB_calibration_FIELD),
    as.numeric(n_DB_comparison_FIELD),
    as.numeric(n_DB_comparison_slope_FIELD),
    as.numeric(n_DB_comparison_intercept_FIELD),
    as.numeric(n_DB_comparison_RMSE_FIELD),
    as.numeric(n_DB_comparison_U_FIELD))
  
  n_LAB_tests <- c(# as.numeric(lab_tests),
    as.numeric(n_DB_calibration_LAB),
    as.numeric(n_DB_comparison_LAB),
    as.numeric(n_DB_comparison_slope_LAB),
    as.numeric(n_DB_comparison_intercept_LAB),
    as.numeric(n_DB_comparison_RMSE_LAB),
    as.numeric(n_DB_comparison_U_LAB))
  
  
  # remove NA values
  DB_prices <- filter_pollutant_sensor[!(is.na(filter_pollutant_sensor$price)), ]
  
  count_DB_prices <- DB_prices[!duplicated(DB_prices[c("model")]),]
  n_price <- nrow(count_DB_prices)
  
  df_metrics <- cbind(n_FIELD_tests,
                      n_LAB_tests)
  df_metrics <- as.data.frame(df_metrics)
  
  rownames(df_metrics) <- c("$R^{2}$ from calibrations", "$R^{2}$ from comparisons", 
                            "slope of reg. line", "intercept", 
                            "RMSE", "Uncertainity (U)")
  df_metrics <- data.frame(names = row.names(df_metrics), df_metrics)
  rownames(df_metrics) <- NULL
  colnames(df_metrics) <- c("metrics", "n. Field Tests", "n. Laboratory Tests")
  
  levels(df_metrics$metrics) <- gsub("^R2 calibration$","$R^{2} calibration$", levels(df_metrics$metrics))
  levels(df_metrics$metrics) <- gsub("^R2 comparison$","$R^{2} comparison$", levels(df_metrics$metrics))
  # add column for the Model type
  
  df_metrics$model <- MODEL
  df_metrics$pollutant = POLLUTANT
  
  df_metrics <- df_metrics %>%
    dplyr::select(metrics,
                  model,
                 pollutant,
                  `n. Field Tests`,
                  `n. Laboratory Tests`)
  return(df_metrics)
}

#################################
# calibration models table ######
#################################

Table_3_func <- function(MODEL      = MODEL,
                         POLLUTANT  = POLLUTANT) {

n_calibration_model <- DB %>%
  dplyr::filter(model     == MODEL,
                pollutant == POLLUTANT) %>%
  group_by(pollutant,
           model_type_calibration) %>%
  summarise(counts = length(model_type_calibration),
            references = paste(unique(new_ref), collapse=", "),
            median_r2_calib = format(median(r2_calib, na.rm = T), digits = 2),  
            median_r2 = format(median(r2, na.rm = T), digits = 2),
            link        = paste(unique(paste0("[",id_ref, "] ", link)), collapse="\n"))
n_calibration_model <- as.data.frame(n_calibration_model)


# make all references as superscript
# n_calibration_model$references <- paste0("$^{", format(unlist(n_calibration_model$references)),"}$")

n_calibration_model <- n_calibration_model[!is.na(n_calibration_model$model_type_calibration),]
names(n_calibration_model)[names(n_calibration_model)=="counts"] <- "n. records"
names(n_calibration_model)[names(n_calibration_model)=="model_type_calibration"] <- "calibration model"
total_n_calibration_records <- sum(n_calibration_model$`n. records`, na.rm = T)
return(n_calibration_model)
}

####################################################
#### calibration reference plot ####################
####################################################

Figure_3_func <- function(MODEL      = MODEL,
                          POLLUTANT  = POLLUTANT) {
  
  DB_calibration <- DB[!(is.na(DB$r2_calib)), ]
  DB_calibration <- DB_calibration %>%
    dplyr::filter(model     == MODEL,
                  pollutant == POLLUTANT)
  
  # summarize by reference
  DB_calibration <- DB_calibration %>%
    group_by(new_ref_author,
             model) %>%
    filter(OEM_system == "SS") %>%
    summarise(mean_r2_calib = mean(r2_calib, na.rm = T))
  
  DB_calibration <- DB_calibration %>%
    filter(mean_r2_calib > 0.25)
  
  # create a new referecne with number (ID) of the reference and first author name
  DB_calibration$id_ref_new <- NULL
  # DB_calibration$id_ref_new <- paste0("[", DB_calibration$id_ref, "]")
  # DB_calibration$id_ref_new <- DB_calibration$id_ref
  DB_calibration$id_ref_new <- DB_calibration$new_ref_author
  
  #======== Calibration plots for all pollutants (R2)
  
  
  plot <- ggplot(DB_calibration, aes(mean_r2_calib, model)) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(colour="grey", size = 1, linetype="dashed"),
          panel.grid.major.x = element_line(colour="grey", size = 1)) +
    geom_point(alpha=1, color="black", size = 4) + 
    # facet_grid(model ~ .) + 
    geom_line(size = 2) +
    geom_text_repel(aes(label=new_ref_author), size = 6.5, show.legend = FALSE) +
    ylab("Sensor System") +
    scale_y_discrete(expand=c(0.01, 0.6)) +
    xlim(0.25, 1.02) +
    xlab(expression(paste(R^2),size=40)) + 
    # xlab(paste0("R","\u00B2")) +
    geom_vline(xintercept=1.0, linetype="dashed", color = "red", size = 1.2) +
    theme(axis.title.y = element_text(face="bold", colour="black", size=22, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=22, colour = "black")) +
    theme(axis.title.x = element_text(colour="black", size=22),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black")) +
    # ggtitle(expression(paste("Mean value of ", R^2, " from the CALIBRATION of sensor systems"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 18, hjust = 0.5)) 
  # plot <- ggplotly(plot) 
}


####################################################
#### plot references comparison ####################
####################################################

Figure_4_func <- function(MODEL      = MODEL,
                          POLLUTANT  = POLLUTANT) {
  
  DB_comparison <- DB[!(is.na(DB$r2)), ]
  DB_comparison <- DB_comparison %>%
    dplyr::filter(model     == MODEL,
                  pollutant == POLLUTANT)
  
  # summarize by reference
  DB_comparison <- DB_comparison %>%
    group_by(new_ref_author,
             model) %>%
    filter(OEM_system == "SS") %>%
    summarise(mean_r2_comparison = mean(r2, na.rm = T))
  
  DB_comparison <- DB_comparison %>%
    filter(mean_r2_comparison > 0.25)
  
  # create a new referecne with number (ID) of the reference and first author name
  DB_comparison$id_ref_new <- NULL
  # DB_comparison$id_ref_new <- paste0("[", DB_comparison$id_ref, "]")
  DB_comparison$id_ref_new <- DB_comparison$new_ref_author
  
  #======== Calibration plots for all pollutants (R2)
  
  
  plot <- ggplot(DB_comparison, aes(mean_r2_comparison, model)) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(colour="grey", size = 1, linetype="dashed"),
          panel.grid.major.x = element_line(colour="grey", size = 1)) +
    geom_point(alpha=1, color="blue", size = 4) + 
    geom_line(size = 1) +
    geom_text_repel(aes(label=id_ref_new), size = 5.7, show.legend = FALSE) +
    ylab("Sensor Model") +
    scale_y_discrete(expand=c(0.01, 0.6)) +
    xlim(0.25, 1.1) +
    xlab(expression(paste(R^2),size=40)) + 
    geom_vline(xintercept=1.0, linetype="dashed", color = "red", size = 1.5) +
    theme(axis.title.y = element_text(face="bold", colour="black", size=22, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=18, colour = "black")) +
    theme(axis.title.x = element_text(colour="black", size=22),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=22, colour = "black")) +
    # ggtitle(expression(paste("Mean value of ", R^2, " from the COMPARISON of sensor systems with reference measurements"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 15, hjust = 0.5)) 
  
}

####################################################
#### prices of sensors #############################
####################################################


Table_4_func <- function(MODEL      = MODEL,
                          POLLUTANT  = POLLUTANT) {
  
levels(DB_prices$time_avg) <- gsub("^hourly$","1 hour", levels(DB_prices$time_avg))
levels(DB_prices$time_avg) <- gsub("^daily$","24 hour", levels(DB_prices$time_avg))

# filter data with AVERAGING TIME  == 1 hour
SS_price_pollutants <- DB_prices %>%
  filter(time_avg == "1 hour")

# identify pollutants measured by each sensor

SS_price_pollutants <- as.data.frame(SS_price_pollutants[,1])
names(SS_price_pollutants) <- "model" 

all_DB_prices_1h <- all_DB_prices %>%
  filter(time_avg == "1 hour")

SS_price_pollutants <- SS_price_pollutants %>%
  left_join(all_DB_prices_1h, c("model"))

# filter sensors with R2 > 0.85 and 0.8< slope< 1.2
SS_price_pollutants <- SS_price_pollutants %>%
  filter(r2 > 0.85 & new_slope > 0.8 & new_slope < 1.2)

# make all new prices in EUR
for (i in 1:nrow(SS_price_pollutants)) {
  if  (SS_price_pollutants$currency[i] == "USD") {
    SS_price_pollutants$new_price_EUR[i] = round(0.88*SS_price_pollutants$price[i], digits = 0)
  } else SS_price_pollutants$new_price_EUR[i] = round(SS_price_pollutants$price[i], digits = 0)
}


SS_price_pollutants <- SS_price_pollutants %>%
  select(-price)
# rename "new_price_EUR"" into "price""
names(SS_price_pollutants)[names(SS_price_pollutants)=="new_price_EUR"] <- "price"


means <- SS_price_pollutants %>%
  group_by(model) %>%
  summarise(mean_r2 = format(mean(r2, na.rm = T), digits = 2),
            mean_slope = format(mean(new_slope, na.rm = T), digits = 2),
            mean_intercept = format(mean(abs(new_intercept), na.rm = T), digits = 2))


SS_price_pollutants <- SS_price_pollutants %>%
  group_by(model,
           open_close,
           living,
           commercial,
           pollutant,
           price) %>%
  summarise(mean_r2 = format(mean(r2, na.rm = T), digits = 2),
            mean_slope = format(mean(new_slope, na.rm = T), digits = 2),
            mean_intercept = format(mean(new_intercept, na.rm = T), digits = 2))


SS_price_pollutants <- SS_price_pollutants %>%
  filter(!pollutant == "PM10-2.5",
         !pollutant == "PM2.5-0.5",
         !pollutant == "PM2",
         !pollutant == "PM3")


# select unique model and pollutant per model of sensor
SS_price_pollutants <- SS_price_pollutants[!duplicated(SS_price_pollutants[c("model", "price", "pollutant")]),]

SS_price_pollutants <- SS_price_pollutants %>%
  select(model,
         open_close,
         living,
         commercial,
         pollutant,
         price,
         mean_r2,
         mean_slope,
         mean_intercept)


# change names
levels(SS_price_pollutants$commercial) <- gsub("^NC$","non commercial", levels(SS_price_pollutants$commercial))
levels(SS_price_pollutants$commercial) <- gsub("^$","commercial", levels(SS_price_pollutants$commercial))
# levels(SS_price_pollutants$pollutant) <- gsub("^PM2.5$","$PM_{2.5}$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^PM10$","$PM_{10}$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^PM1$","$PM_{1}$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^NO2$","$NO_{2}$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^O3$","$O_{3}$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^CO$","$CO$", levels(SS_price_pollutants$pollutant))
# levels(SS_price_pollutants$pollutant) <- gsub("^NO$","$NO$", levels(SS_price_pollutants$pollutant))

#only PM2.5
SS_price_pollutants_PM25 <- SS_price_pollutants %>%
  filter(pollutant == "PM2.5")

#only PM10
SS_price_pollutants_PM10 <- SS_price_pollutants %>%
  filter(pollutant == "PM10")

#only PM1
SS_price_pollutants_PM1 <- SS_price_pollutants %>%
  filter(pollutant == "PM1")

#only NO2
SS_price_pollutants_NO2 <- SS_price_pollutants %>%
  filter(pollutant == "NO2")

#only O3
SS_price_pollutants_O3 <- SS_price_pollutants %>%
  filter(pollutant == "O3")

#only CO
SS_price_pollutants_CO <- SS_price_pollutants %>%
  filter(pollutant == "CO")

#only NO
SS_price_pollutants_NO <- SS_price_pollutants %>%
  filter(pollutant == "NO")

# AAA <- SS_price_pollutants_PM25 %>%
#   full_join(SS_price_pollutants_PM10, c("model")) 
# 
# BBB <- AAA %>%
#   full_join(SS_price_pollutants_PM1, c("model")) 
# 
# CCC <- BBB %>%
#   full_join(SS_price_pollutants_NO2, c("model")) 
# 
# DDD <- CCC %>%
#   full_join(SS_price_pollutants_O3, c("model")) 
# 
# EEE <- DDD %>%
#   full_join(SS_price_pollutants_CO, c("model")) 
# 
# FFF <- EEE %>%
#   full_join(SS_price_pollutants_NO, c("model"))
# 
# 
# FFF <- as.data.frame(FFF)
# 
# FFF <- FFF %>%
#   select(-price,
#          -price.x,
#          -price.x.x,
#          -price.x.x.x,
#          -price.y,
#          -price.y.y,
#          -price.y.y.y,
#          -open_close,
#          - open_close.x,
#          - open_close.x.x,
#          - open_close.x.x.x,
#          - open_close.y,
#          - open_close.y.y,
#          - open_close.y.y.y,
#          - living,
#          - living.x,
#          - living.x.x,
#          - living.x.x.x,
#          - living.y,
#          - living.y.y,
#          - living.y.y.y,
#          - commercial,
#          - commercial.x,
#          - commercial.x.x,
#          - commercial.x.x.x,
#          - commercial.y,
#          - commercial.y.y,
#          - commercial.y.y.y,
#          - mean_r2,
#          - mean_r2.x,
#          - mean_r2.x.x,
#          - mean_r2.x.x.x,
#          - mean_r2.y,
#          - mean_r2.y.y,
#          - mean_r2.y.y.y,
#          - mean_slope,
#          - mean_slope.x,
#          - mean_slope.x.x,
#          - mean_slope.x.x.x,
#          - mean_slope.y,
#          - mean_slope.y.y,
#          - mean_slope.y.y.y,
#          - mean_intercept,
#          - mean_intercept.x,
#          - mean_intercept.x.x,
#          - mean_intercept.x.x.x,
#          - mean_intercept.y,
#          - mean_intercept.y.y,
#          - mean_intercept.y.y.y)
# 
# # aggregate all pollutants into one column
# # df$variable_7 <- apply(df, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ", "))
# # FFF$pollutant <- apply(FFF[2:8], 1, function(x) paste(x[!is.na(x)], collapse = ", "))
# FFF <- FFF %>%
#   select(model,
#          pollutant)

# merge with  price
# SS_price_pollutants <- SS_price_pollutants[!duplicated(SS_price_pollutants[c("model", "price", "open_close", "living", "commercial")]),]

# FFF <- FFF[order(FFF$model),]
# SS_price_pollutants <- cbind(FFF, 
#                              means$mean_r2, 
#                              means$mean_slope, 
#                              means$mean_intercept, 
#                              SS_price_pollutants$open_close, 
#                              SS_price_pollutants$living, SS_price_pollutants$commercial, 
#                              SS_price_pollutants$price)

# colnames(SS_price_pollutants) <- c("model", "pollutant", "mean $R^{2}$", "mean slope", "mean intercept", "open/close", "living", "commercial", "price (EUR)")
# sort by price
# SS_price_pollutants <- SS_price_pollutants[order(SS_price_pollutants$price),]

# rownames(SS_price_pollutants) <- NULL

SS_price_pollutants <- SS_price_pollutants %>%
  dplyr::filter(model     == MODEL,
                pollutant == POLLUTANT)
return(SS_price_pollutants)
}


######################################
#### plot R2 and slope  ##############
######################################

Figure_8_func <- function(MODEL      = MODEL,
                          POLLUTANT  = POLLUTANT) {

  levels(DB$field___lab) <- gsub("^FIELD$","Feld Test", levels(DB$field___lab))
  levels(DB$field___lab) <- gsub("^LAB$","Laboratory Test", levels(DB$field___lab))
  DB_taylor <- DB
  
  DB_taylor <- DB_taylor %>%
    dplyr::filter(model     == MODEL,
                  pollutant == POLLUTANT)
  
  # select only FIELD TESTS tests
  DB_taylor <- DB_taylor %>%
    filter(!field___lab == "Laboratory Test",
           sensor_result_unit %in% c("µg/m3", "ppb", "ppm", "#", "mg/m3"))
  
  # remove records with empty slope
  DB_taylor <- DB_taylor[!(is.na(DB_taylor$slope)), ]
  
  # remove records with empty r2
  DB_taylor <- DB_taylor[!(is.na(DB_taylor$r2)), ]
  
  DB_taylor$new_slope <- DB_taylor$slope
  
  for (i in 1:nrow(DB_taylor)) {
    if  (DB_taylor$y[i] == "Sensor") {
      DB_taylor$new_slope[i] = 1/DB_taylor$slope[i]
    } 
  }
  
  
  # filter sensors with R2 > 0.7 and 0.5 < slope < 1.5
  DB_taylor <- DB_taylor %>%
    filter(r2 > 0.75 & new_slope > 0.5 & new_slope < 1.2)
  
  DB_taylor <- DB_taylor %>%
    group_by(model,
             open_close,
             OEM_system,
             living) %>%
    filter(time_avg == "1 hour") %>%
    summarise(mean_r2 = mean(r2),
              mean_slope = mean(new_slope))
  
  DB_taylor$pollutant <- POLLUTANT
  
  # rename "updated" with "y" (living)
  levels(DB_taylor$living) <- gsub("^updated","Y", levels(DB_taylor$living))
  
  # DB_taylor_SS <- DB_taylor %>%
  #   filter(OEM_system == "SS")
  
  
  ##########################################################
  ###### function to shif axis #############################
  
  shift_axis_x <- function(p, x=0){
    g <- ggplotGrob(p)
    dummy <- data.frame(x=x)
    ax <- g[["grobs"]][g$layout$name == "axis-l"][[1]]
    p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(x=1, width = sum(ax$height))), 
                          xmax=x, xmin=x) +
      geom_vline(aes(xintercept=x), data = dummy) +
      theme(axis.text.y = element_blank(), 
            axis.ticks.y=element_blank(),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            # panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(colour = "black"))
  }
  
  ###########################################################
  ###########################################################
  
  x <- c(0.5, 1, 1.5)
  y <- c(1, 0.75, 1) 
  TRIANGLE <- data.frame(x,y)
  
  
  plot <- ggplot(data = DB_taylor, aes(x = mean_slope, y = mean_r2)) + 
    theme_bw() +
    # theme_classic() +
    geom_point(size = 3, aes(shape = open_close, color = open_close), show.legend = FALSE) +
    geom_polygon(data = TRIANGLE, aes(x=x, y=y), fill = "skyblue2", alpha=0.4) +
    scale_shape_manual(values=c(16, 1)) +
    geom_text_repel(aes(label=model,color = living), size = 4, show.legend = FALSE) +
    scale_color_manual(values=c("black", "blue", "red", "black")) +  # "living", "no-living", "open source", "black box"
    #  scale_color_manual(values=c("blue", "black")) +
    xlab("mean slope") +
    xlim(c(0.5,1.5)) +
    ylab(expression(paste("mean ", R^2),size=12)) +
    theme(strip.text = element_text(size = 15, face="bold")) +
    theme(strip.text.y = element_text(angle = 0)) +
    theme(axis.title.x = element_text(colour="black", size=15),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=13, colour = "red")) +
    theme(axis.title.y = element_text(colour="black", size=15),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13, colour = "red")) +
    # ggtitle(expression(paste("Relation between mean ", R^2, " and mean slope for Sensor Systems (1 hour avg. time)"))) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 15, hjust = 0.5)) 
  
  plot <- shift_axis_x(plot, 1)
  return(plot)
  
  }
