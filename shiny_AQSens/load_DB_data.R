
####################################################################

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(tools)
library(ggplot2)
library(ggpmisc)
library(xtable)
library(pander)
library(formattable)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(forcats)
library(bookdown)
library(ggrepel)
library(httr)
library(scales)

# only run this part when @ the JRC
PROXY = TRUE; URL      = "10.168.209.72"; PORT     = 8012; LOGIN    = NULL; PASSWORD = NULL
# no login and no password on our proxy
      if (PROXY) {
          # checking that we have the httr package to use function Set_Config()
          library("httr")
          cat("[CONFIG] INFO Package httr loaded\n")
          # implement PROXY
          set_config(use_proxy(url=URL, port=PORT, username = LOGIN, password = PASSWORD))
      } else reset_config()


# setwd("L:/ERLAP/Diffusion/AQSens/Deliverables/2.1 review")
# setwd("C:/JRC_CA/AA AQSens")

# import DB data

### read from GoogleDrive ###############################

myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSsQGWrp2KAqEDdqM6usR8B3-iLCWDHHLGODOHIvDZGunaeBExnf3JNRIpEwFlFLtoWgJ8dezp6MIPg/pub?output=xlsx"


GET(myurl, write_disk(tf <- tempfile(fileext = ".xlsx")))
DB <- readxl::read_excel(tf, sheet = 1)

# read bibliography sheet from the DB
DB_bib <- readxl::read_excel(tf, sheet = 2)
########################################################
########################################################

# DB <- readxl::read_excel("DB_sensor_types.xlsx",
#                                 sheet = 1)
#
# # read bibliography sheet from the DB
# DB_bib <- readxl::read_excel("DB_sensor_types.xlsx",
#                                 sheet = 2)


# order in alphabetic order
DB_bib <- DB_bib[order(DB_bib$reference),]

# assign an ID to references (mantain the same order as in the DB)
# DB$ID_reference <- as.numeric(as.factor(DB$reference))
DB_bib$ID_reference <- as.numeric(factor(DB_bib$reference, levels = unique(DB_bib$reference)))


# UNIQUE LIST of REFERENCES TO BE INSERTED IN THE SAME ORDER!!!!!
list_DB_bib <-  DB_bib[!duplicated(DB_bib[c("ID_reference")]),]

write.csv(list_DB_bib, "list_DB_bib.csv")

# merge DB with DB_bib
DB$ID_ref <- data.frame(DB_bib[match(DB$reference, table = DB_bib$reference), "ID_reference"])

DB <- DB %>%
  mutate(time_AVG = paste(`Averaging time`, `units (avg_time)`))

DB <- as.matrix(DB)
DB <- as.data.frame(DB)

# data cleaning
headers <- names(DB)
headers <- str_replace_all(headers, "\\.|\\(|\\)", "")
headers <- make.names(headers, unique = TRUE)
headers <- str_replace_all(headers, "\\.", "_")
headers <- str_to_lower(headers)

# Give table headers
names(DB) <- headers

write.csv(DB, "DB.csv")
DB <- read.csv("DB.csv", header = TRUE)

# remove rows with NA
DB <- DB[!(is.na(DB$model)), ]

# remove columns with NA from the first column
# DB <- DB[colSums(!is.na(DB)) > 0]


names(DB)[names(DB)=="oem___sensor_system1"] <- "OEM_system"
names(DB)[names(DB)=="typeopc__nephelometer__electrochemical__metal_oxide"] <- "type_sensor"
names(DB)[names(DB)=="open_source_correction_or_black_box_"] <- "open_close"


# create a new referecne with number (ID) of the reference and first author name
DB$new_ref <-NA
DB$new_ref_author <-NA
ref <- strsplit(x = as.character(DB$reference), "\\, |\\_| ")  # space "" and "_"

for (i in 1:length(ref)) {
  ref[[i]][1]
  DB$new_ref[i] <- paste0(ref[[i]][1], "[", DB$id_ref[i], "]")
}

for (i in 1:length(ref)) {
  ref[[i]][1]
  DB$new_ref_author[i] <- ref[[i]][1]
}

# select only few field of interest
DB <- DB %>%
  select(manufacturer,
         project,
         model,
         pollutant,
         sensor_result_unit,
         OEM_system,
         open_close,
         field___lab,
         site,
         time_avg,
         r2,
         r2_calib,
         rmse,
         u,
         intercept,
         slope,
         reference,
         year_ref,
         type_sensor,
         model_type_calibration,
         x, y,
         price,
         living,
         commercial,
         currency,
         id_ref,
         new_ref,
         new_ref_author,
         link)

# filter out model KUNAKAIR and Bettair for now...
DB <- DB %>%
  filter(!model %in% c("KUNAKAIR P10 V2", "KUNAKAIR A10 V2", "Bettair"))


# for now filter out the pollutant "NO"
# DB <- DB %>%
#   filter(!pollutant == "NO")


# replace "sensor system" with SS
levels(DB$OEM_system) <- gsub("^sensor system$","SS", levels(DB$OEM_system))

#rename N in "commercial" with "NC"
levels(DB$commercial) <- gsub("^N$","NC", levels(DB$commercial))
levels(DB$commercial) <- gsub("^Y$","", levels(DB$commercial))

# n. records
n_DB_length <- nrow(DB) # n.records

# n. OEM records
DB_length_OEM <- DB %>%
  filter(OEM_system == "OEM")
n_DB_length_OEM <- nrow(DB_length_OEM)

# n. SS records
DB_length_SS <- DB %>%
  filter(OEM_system == "SS")
n_DB_length_SS <- nrow(DB_length_SS)


# n.records "living"
n_DB_length_records_living <- DB %>%
  filter(living %in% c("Y", "updated"))
n_DB_length_records_living <- nrow(n_DB_length_records_living)

# n.records "non-living"
n_DB_length_records_non_living <- DB %>%
  filter(living %in% c("N"))
n_DB_length_records_non_living <- nrow(n_DB_length_records_non_living)


# n.records "living" OEM
n_DB_length_records_living_OEM <- DB %>%
  filter(living %in% c("Y", "updated"),
         OEM_system == "OEM")
n_DB_length_records_living_OEM <- nrow(n_DB_length_records_living_OEM)


# n.records "living" SS
n_DB_length_records_living_SS <- DB %>%
  filter(living %in% c("Y", "updated"),
         OEM_system == "SS")
n_DB_length_records_living_SS <- nrow(n_DB_length_records_living_SS)


# n.records "non-living" OEM
n_DB_length_records_non_living_OEM <- DB %>%
  filter(living %in% c("N"),
         OEM_system == "OEM")
n_DB_length_records_non_living_OEM <- nrow(n_DB_length_records_non_living_OEM)


# n.records "non-living" SS
n_DB_length_records_non_living_SS <- DB %>%
  filter(living %in% c("N"),
         OEM_system == "SS")
n_DB_length_records_non_living_SS <- nrow(n_DB_length_records_non_living_SS)



# manufacturers
DB_manufacturer <- DB[!duplicated(DB[c("manufacturer")]),] # manufacturers

# manufacturers - OEM
DB_manufacturers_OEM <- DB_manufacturer %>%
  filter(OEM_system == "OEM")
n_DB_manufacturers_OEM <- nrow(DB_manufacturers_OEM)

# manufacturers -SS
DB_manufacturers_SS <- DB_manufacturer %>%
  filter(OEM_system == "SS")
n_DB_manufacturers_SS <- nrow(DB_manufacturers_SS)


n_manufacturer_count <- count(DB[!duplicated(DB[c("manufacturer")]),]) # n. manufacturers
n_projects_count <- count(DB[!duplicated(DB[c("project")]),]) # n. projects
n_model_count <- count(DB[!duplicated(DB[c("model")]),])
n_references <- count(DB[!duplicated(DB[c("reference")]),]) # n. sources
n_system <- count(DB[!duplicated(DB[c("OEM_system")]),])
n_UNCERTAINITY_count <- count(DB[!duplicated(DB[c("rmse", "u")]),]) # n. records reporting uncertainties (RMSE or U)

# references reporting RMSE
rmse_references <- DB[!(is.na(DB$rmse)), ]
rmse_references <- count(rmse_references[!duplicated(rmse_references[c("reference")]),]) # n. references reporting RMSE
# references reporting Uncertainty U (mostly standard deviation)
u_references <- DB[!(is.na(DB$u)), ]
u_references <- count(u_references[!duplicated(u_references[c("reference")]),]) # n. references reporting U
uncertainty_references <- rmse_references + u_references


# check how many records for different time averages (CALIBRATION)
time_AVG_calib <- DB %>%
  group_by(time_avg,
           OEM_system) %>%
  filter(!is.na(r2_calib)) %>%
  summarise(counts = length(time_avg))
AVG_1_hour_calib_OEM <- time_AVG_calib$counts[1]
AVG_1_hour_calib_SS <- time_AVG_calib$counts[2]
AVG_1_hour_calib <- AVG_1_hour_calib_OEM + AVG_1_hour_calib_SS
AVG_1_min_calib_OEM <- time_AVG_calib$counts[3]
AVG_1_min_calib_SS <- time_AVG_calib$counts[4]
AVG_1_min_calib <- AVG_1_min_calib_OEM + AVG_1_min_calib_SS
AVG_calib_OEM <- AVG_1_hour_calib_OEM + AVG_1_min_calib_OEM
AVG_calib_SS <- AVG_1_hour_calib_SS + AVG_1_min_calib_SS


# check how many records for different time averages (COMPARISON)
time_AVG_comp <- DB %>%
  group_by(time_avg,
           OEM_system) %>%
  filter(!is.na(r2)) %>%
  summarise(counts = length(time_avg))
AVG_1_hour_comp_OEM <- time_AVG_comp$counts[1]
AVG_1_hour_comp_SS <- time_AVG_comp$counts[2]
AVG_1_hour_comp <- AVG_1_hour_comp_OEM + AVG_1_hour_comp_SS
AVG_1_min_comp_OEM <- time_AVG_comp$counts[3]
AVG_1_min_comp_SS <- time_AVG_comp$counts[4]
AVG_1_min_comp <- AVG_1_min_comp_OEM + AVG_1_min_comp_SS
AVG_comp_OEM <- AVG_1_hour_comp_OEM + AVG_1_min_comp_OEM
AVG_comp_SS <- AVG_1_hour_comp_SS + AVG_1_min_comp_SS

# select only sensors that have not been discontinued
DB_unique_model <- DB[!duplicated(DB[c("model")]),]
DB_living <- DB_unique_model %>%
  filter(living %in% c("Y", "updated"))
n_DB_length_living <- nrow(DB_living) # n.records of living sensors

DB_living_OEM <- DB_unique_model %>%
  filter(living %in% c("Y", "updated"),
         OEM_system == "OEM")
n_DB_length_living_OEM <- nrow(DB_living_OEM) # n.records of living sensors OEM

DB_living_SS <- DB_unique_model %>%
  filter(living %in% c("Y", "updated"),
         OEM_system == "SS")
n_DB_length_living_SS <- nrow(DB_living_SS) # n.records of living sensors SS


# number of non living sensors
DB_non_living <- DB_unique_model %>%
  filter(living %in% c("N"))
n_DB_length_non_living <- nrow(DB_non_living) # n.records of living sensors

DB_non_living_OEM <- DB_unique_model %>%
  filter(living %in% c("N"),
         OEM_system == "OEM")
n_DB_length_non_living_OEM <- nrow(DB_non_living_OEM) # n.records of living sensors OEM

DB_non_living_SS <- DB_unique_model %>%
  filter(living %in% c("N"),
         OEM_system =="SS")
n_DB_length_non_living_SS <- nrow(DB_non_living_SS) # n.records of living sensors SS


count_OEM_systems <- DB %>%
  group_by(OEM_system) %>%
  summarise(counts = length(OEM_system),
            references = paste(unique(id_ref), collapse=","))


# counts Open Source or black box RECORDS
DB_open_model <- DB[!(is.na(DB$model)), ]

count_open_source_systems <- DB %>%
  group_by(open_close,
           OEM_system) %>%
  summarise(counts = length(open_close))


open_source_SS <- count_open_source_systems$counts[4]
black_box_SS <- count_open_source_systems$counts[2]

open_source_OEM <- count_open_source_systems$counts[3]
black_box_OEM <- count_open_source_systems$counts[1]

open_source <- count_open_source_systems$counts[4] + count_open_source_systems$counts[3]
black_box <- count_open_source_systems$counts[2] + count_open_source_systems$counts[1]


###### OPEN SOURCE / BLACK BOX models  #########################
################################################################

DB_open_source_model <- DB %>%
  filter(open_close == "open source")
n_open_source <- count(DB_open_source_model[!duplicated(DB_open_source_model[c("model")]),]) # n. open source models

DB_black_box_model <- DB %>%
  filter(open_close == "black box")
n_black_box <- count(DB_black_box_model[!duplicated(DB_black_box_model[c("model")]),]) # n. black box model

###### COMMERCIAL sensors
DB_commercial_OEM <- DB %>%
  filter(commercial == "",
         OEM_system == "OEM")
n_DB_commercial_OEM <- nrow(DB_commercial_OEM)
DB_commercial_OEM <- count(DB_commercial_OEM[!duplicated(DB_commercial_OEM[c("model")]),])

DB_commercial_SS <- DB %>%
  filter(commercial == "",
         OEM_system == "SS")
n_DB_commercial_SS <- nrow(DB_commercial_SS)
DB_commercial_SS <- count(DB_commercial_SS[!duplicated(DB_commercial_SS[c("model")]),])

DB_commercial <- DB_commercial_SS + DB_commercial_OEM


###### COMMERCIAL / non-COMMERICAL sensors
DB_non_commercial_OEM <- DB %>%
  filter(commercial == "NC",
         OEM_system == "OEM")
n_DB_non_commercial_OEM <- nrow(DB_non_commercial_OEM)
DB_non_commercial_OEM <- count(DB_non_commercial_OEM[!duplicated(DB_non_commercial_OEM[c("model")]),])

DB_non_commercial_SS <- DB %>%
  filter(commercial == "NC",
         OEM_system == "SS")
n_DB_non_commercial_SS <- nrow(DB_non_commercial_SS)
DB_non_commercial_SS <- count(DB_non_commercial_SS[!duplicated(DB_non_commercial_SS[c("model")]),])

DB_non_commercial <- DB_non_commercial_SS + DB_non_commercial_OEM


########################################
##### Open source / black box ##########
########################################

# Open source OEM MODELS
DB_open_source_model_OEM <- DB %>%
  filter(open_close == "open source",
         OEM_system == "OEM")
n_open_source_OEM <- count(DB_open_source_model_OEM[!duplicated(DB_open_source_model_OEM[c("model")]),]) # n. open source models OEM

# Open source SS MODELS
DB_open_source_model_SS <- DB %>%
  filter(open_close == "open source",
         OEM_system == "SS")
n_open_source_SS <- count(DB_open_source_model_SS[!duplicated(DB_open_source_model_SS[c("model")]),]) # n. open source models SS

n_open_source <- n_open_source_OEM + n_open_source_SS



# Black box OEM MODELS
DB_black_box_model_OEM <- DB %>%
  filter(open_close == "black box",
         OEM_system == "OEM")
n_black_box_OEM <- count(DB_black_box_model_OEM[!duplicated(DB_black_box_model_OEM[c("model")]),]) # n. black box model OEM


# Black box SS MODELS
DB_black_box_model_SS <- DB %>%
  filter(open_close == "black box",
         OEM_system == "SS")
n_black_box_SS <- count(DB_black_box_model_SS[!duplicated(DB_black_box_model_SS[c("model")]),]) # n. black box model SS

n_black_box <- n_black_box_OEM + n_black_box_SS


#######################################################################
#######################################################################
# only select open source sensors
# DB <- DB %>%
#   filter(open_close == "open source")

OEM <- count_OEM_systems$counts[1]
SS <- count_OEM_systems$counts[2]

DB_OEM <- DB %>%
  filter(OEM_system == "OEM")
n_OEM_count <- count(DB_OEM[!duplicated(DB_OEM[c("model")]),]) # n. OEMs

DB_SS <- DB %>%
  filter(OEM_system == "SS")
n_SS_count <- count(DB_SS[!duplicated(DB_SS[c("model")]),]) # n. n. SS


## counts LAB/FIELD tests
count_tests <- DB %>%
  group_by(field___lab) %>%
  summarise(counts = length(field___lab))

field_tests <- count_tests$counts[1]
lab_tests <- count_tests$counts[2]

###########################
## Particulate Matter #####
###########################

# count sensor types
PM_sensors_counts <- DB %>%
  filter(pollutant %in% c("PM10", "PM2.5", "PM1", "PM10-2.5", "PM2.5-0.5", "PM3", "PM2", "PM")) %>%
  group_by(type_sensor) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(id_ref), collapse=","))

####################################################################
# find pollutants for each type of sensor for Particulate Matter ###
####################################################################

poll_PM_sensors_counts <- DB %>%
  group_by(type_sensor,
           pollutant) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(id_ref), collapse=",")) %>%
  filter(pollutant %in% c("PM10", "PM2.5", "PM1", "PM10-2.5", "PM2.5-0.5", "PM3", "PM2", "PM"))

# rename pollutants
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM2.5-0.5$","PM2.5", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM2$","PM2.5", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM3$","PM2.5", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM10-2.5$","PM10", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM$","PM2.5", levels(poll_PM_sensors_counts$pollutant))

levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM2.5$","$PM_{2.5}$", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant) <- gsub("^PM10$","$PM_{10}$", levels(poll_PM_sensors_counts$pollutant))
levels(poll_PM_sensors_counts$pollutant)<- gsub("^PM1$","$PM_{1}$", levels(poll_PM_sensors_counts$pollutant))

# .....group again...
poll_PM_sensors_counts <- poll_PM_sensors_counts %>%
  group_by(type_sensor,
           pollutant) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(references), collapse=","))

pollutants_PM_sensor_type <- poll_PM_sensors_counts %>%
  group_by(type_sensor) %>%
  summarise(pollutant = paste(unique(pollutant), collapse=","))

#####################################################################
#####################################################################

sum_PM_counts <- sum(PM_sensors_counts$counts)
OPC <- PM_sensors_counts$counts[3]
neph <- PM_sensors_counts$counts[2]

Optical_tests <- PM_sensors_counts$counts[2] + PM_sensors_counts$counts[3]

PM_sensors_counts <- cbind(PM_sensors_counts, pollutants_PM_sensor_type[,2])
names(PM_sensors_counts)[names(PM_sensors_counts)=="counts"] <- "n. records"
PM_sensors_counts <- PM_sensors_counts %>%
  select(type_sensor,
         pollutant,
         `n. records`,
         references)

##############
# Gases ######
##############

GAS_sensors_counts <- DB %>%
  filter(pollutant %in% c("CO", "NO", "NO2", "O3", "NO2-O3")) %>%
  group_by(type_sensor) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(id_ref), collapse=","))

GAS_sensors_counts_electrochemical <- GAS_sensors_counts[1,2]
GAS_sensors_counts_MOS <- GAS_sensors_counts[2,2]

####################################################################
# find pollutants for each type of sensor for Gases ################
####################################################################

poll_GAS_sensors_counts <- DB %>%
  group_by(type_sensor,
           pollutant) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(id_ref), collapse=",")) %>%
  filter(pollutant %in% c("CO", "NO", "NO2", "O3", "NO2-O3"))

levels(poll_GAS_sensors_counts$pollutant) <- gsub("^NO2$","$NO_{2}$", levels(poll_GAS_sensors_counts$pollutant))
levels(poll_GAS_sensors_counts$pollutant) <- gsub("^O3$","$O_{3}$", levels(poll_GAS_sensors_counts$pollutant))
levels(poll_GAS_sensors_counts$pollutant) <- gsub("^CO$","$CO$", levels(poll_GAS_sensors_counts$pollutant))
levels(poll_GAS_sensors_counts$pollutant) <- gsub("^NO$","$NO$", levels(poll_GAS_sensors_counts$pollutant))

# .....group again...
poll_GAS_sensors_counts <- poll_GAS_sensors_counts %>%
  group_by(type_sensor,
           pollutant) %>%
  summarise(counts = length(type_sensor),
            references = paste(unique(references), collapse=","))

pollutants_GAS_sensor_type <- poll_GAS_sensors_counts %>%
  group_by(type_sensor) %>%
  summarise(pollutant = paste(unique(pollutant), collapse=","))

#####################################################################
#####################################################################

GAS_sensors_counts <- cbind(GAS_sensors_counts, pollutants_GAS_sensor_type[,2])
names(GAS_sensors_counts)[names(GAS_sensors_counts)=="counts"] <- "n. records"
GAS_sensors_counts <- GAS_sensors_counts %>%
  select(type_sensor,
         pollutant,
         `n. records`,
         references)

# bind Particulate Matter with Gases ###

PM_GAS_sensors_counts <- rbind(PM_sensors_counts,
                               GAS_sensors_counts)
names(PM_GAS_sensors_counts)[names(PM_GAS_sensors_counts)=="type_sensor"] <- "type"

# make all references as superscript
PM_GAS_sensors_counts$references <- paste0("$@^{", format(unlist(PM_GAS_sensors_counts$references)),"}$")



# statistics of the MEDIAN of R2 for COMPARISON and CALIBRATION

DB_R2 <- DB[!is.na(DB$r2 & DB$r2_calib), ]
DB_R2 <- DB_R2 %>%
  filter(r2 > 0,
         r2_calib > 0) %>%
  summarise(median_R2_CALIB = median(r2_calib, na.rm = T),
            median_R2       = median(r2, na.rm = T),
            mean_R2_CALIB = mean(r2_calib, na.rm = T),
            mean_R2       = mean(r2, na.rm = T))

#######################################
########## prices data ################
#######################################

DB_prices <- DB[!is.na(DB$price & DB$slope & DB$intercept), ]

# only 1 hour averaged data
DB_prices <- DB_prices %>%
  filter(time_avg %in% c("1 hour", "24 hour"))


DB_prices$new_slope     <- DB_prices$slope
DB_prices$new_intercept <- DB_prices$intercept

Inversion.row <- which(DB_prices$y == "Sensor")

if(length(Inversion.row) > 0) {
  DB_prices[Inversion.row,"new_slope"] <-  1/DB_prices[Inversion.row,"slope"]
  DB_prices[Inversion.row,"new_intercept"] <- - DB_prices[Inversion.row, "intercept"]/DB_prices[Inversion.row,"slope"]
}


# for (i in 1:nrow(DB_prices)) {
# if  (DB_prices$y[i] == "Sensor") {
#   DB_prices$new_slope[i] = 1/DB_prices$slope[i]
# } 
#   }

# make all new prices in EUR
for (i in 1:nrow(DB_prices)) {
  if  (DB_prices$currency[i] == "USD") {
    DB_prices$new_price_EUR[i] = 0.88*DB_prices$price[i]
  } else DB_prices$new_price_EUR[i] = round(DB_prices$price[i], digits = 0)
}

count_DB_prices <- DB_prices[!duplicated(DB_prices[c("model", "pollutant", "time_avg" )]),]
count_DB_prices <- count_DB_prices %>%
  group_by(model,
           open_close,
           living,
           commercial,
           new_price_EUR,
           time_avg) %>%
  summarise(counts = length(pollutant))

n_price <- count_DB_prices[!is.na(count_DB_prices$new_price_EUR),]


# filter sensors with R2 > 0.7 and 0.5< slope< 1.5
DB_prices <- DB_prices %>%
  filter(r2 > 0.85 & new_slope > 0.8 & new_slope < 1.2)


# only select sensor systems
# DB_prices <- DB_prices %>%
#   filter(OEM_system == "SS")


# select prices <= 2500 EUR
DB_prices <- DB_prices %>%
  filter(# new_price_EUR < 2500,
    # !living == "N",
    !pollutant == "PM10-2.5",
    !pollutant == "PM2.5-0.5",
    !pollutant == "PM2",
    !pollutant == "PM3",
    new_price_EUR > 0)
# order in alphabetic order
DB_prices <- DB_prices[order(DB_prices$model),]

all_DB_prices <- DB_prices


# select unique PRICE and POLLUTANT per model of sensor
DB_prices <- DB_prices[!duplicated(DB_prices[c("model", "price", "time_avg")]),]
count_DB_prices <- count_DB_prices[!duplicated(count_DB_prices[c("model", "time_avg")]),]
count_DB_prices <- as.data.frame(count_DB_prices)

DB_prices <- DB_prices %>%
  left_join(count_DB_prices, c("model" , "time_avg"))

DB_prices <- DB_prices %>%
  select(model,
         open_close.x,
         living.x,
         commercial.x,
         counts,
         new_price_EUR.x,
         time_avg)

color_living <- DB_prices %>%
  group_by(model,
           living.x) %>%
  summarize(counts = length(count))

color_living <- ifelse(color_living$living.x %in% c("Y", "updated"), "black", "red")


# only select sensor systems with  (all possible pollutants)
names <- count_DB_prices[count_DB_prices$counts >= 0, ]$model
names <- as.character(names)
DB_prices <- DB_prices[DB_prices$model %in% names,]


levels(DB_prices$time_avg) <- gsub("^1 hour$","hourly", levels(DB_prices$time_avg))
levels(DB_prices$time_avg) <- gsub("^24 hour$","daily", levels(DB_prices$time_avg))

models = unique(DB_prices$model)
List.averaging_hours <- sapply(models, function(i) paste(unique(DB_prices$time_avg[DB_prices$model == i]), collapse = ", "))
List.averaging_counts <- sapply(models, function(i) max(DB_prices$counts[DB_prices$model == i], na.rm = T))

# select unique model and price per model of sensor
DB_prices <- DB_prices[!duplicated(DB_prices[c("model")]),]
DB_prices$averaging_hours <- List.averaging_hours
DB_prices$averaging_counts <- List.averaging_counts

##################################################################
##################################################################
