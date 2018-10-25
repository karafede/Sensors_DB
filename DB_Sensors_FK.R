
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(tools)
library(ggplot2)

# Set global options
options(stringsAsFactors = TRUE)
# no scientific format
options(scipen=999)

setwd("L:/ERLAP/Diffusion/AQSens/Deliverables/2.1 review")
setwd("C:/Users/karaf/Desktop/AA AQSens")

getwd()

list.files()

# import DB data

DB <- readxl::read_excel("DB_sensor_types.xlsx",
                                sheet = 1)

# data cleaning
headers <- DB[1, ]
headers <- str_replace_all(headers, "\\.|\\(|\\)", "")
headers <- make.names(headers, unique = TRUE)
headers <- str_replace_all(headers, "\\.", "_")
headers <- str_to_lower(headers)

# Give table headers
DB <- DB[-1, ]
names(DB) <- headers

write.csv(DB, "DB.csv")
DB <- read.csv("DB.csv", header = TRUE)
# delete .csv temporary file
if (file.exists("DB.csv")) file.remove("DB.csv")  

# remove rows with NA
DB <- DB[!(is.na(DB$model)), ]

# remove columns with NA from the first column
DB <- DB[colSums(!is.na(DB)) > 0]

# select only few field of interest
DB <- DB %>%
  select(manufacturer,
         model,
         pollutant,
         field___lab,
         r2,
         r2_calib,
         intercept,
         slope)

  # ## subset only PM2.5
  # grep("^PM0.5-2.5$", DB$pollutant)
  # grep("^PM$", DB$pollutant)
  # toMatch <- c("PM0.5-2.5", "PM2.5")
  # toMatch <- c("PM") # all PM
  # toMatch <- c("^PM$") # only PM name
  # grep(paste(toMatch,collapse="|"), DB$pollutant)

names(DB)[names(DB)=="field___lab"] <- "type"

DB_comparison <- DB[!(is.na(DB$r2)), ]
DB_calibration <- DB[!(is.na(DB$r2_calib)), ]
str(DB_comparison)


DB_comparison_PM2.5 <- DB_comparison %>%
  filter(pollutant %in% c("PM2.5", "PM2.5-0.5"))

DB_calibration_PM2.5 <- DB_calibration %>%
  filter(pollutant %in% c("PM2.5", "PM2.5-0.5"))

# compute frequency of records for each model of sensor
count_DB_comparison_PM2.5 <- DB_comparison_PM2.5 %>%
  group_by(model,
           type) %>%
  summarise(counts = length(model))

## quick box plot

plot <- ggplot(DB_comparison_PM2.5, aes(model, r2, fill = model)) +
  theme_bw() +
  geom_point(alpha=1, color="tomato", position = "jitter") +
  geom_boxplot(alpha=0) +
  #  facet_grid(. ~ year) +
  facet_grid(type ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 1) +
  geom_text(data = count_DB_comparison_PM2.5, aes(x = model, y = 0.8, label = counts))+
  theme( strip.text = element_text(size = 15)) + 
  xlab("Sensor Model") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(R^2),size=24)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  # geom_hline(yintercept=150, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of ", R^2, " for all ", PM[2.5], " sensors: COMPARISON with REFERENCE"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 

plot

# save plot
png("R2_Comparison_REF_PM2.5_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()




plot <- ggplot(DB_calibration_PM2.5, aes(model, r2_calib, fill = model)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(type ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 1) +
  theme( strip.text = element_text(size = 15)) + 
  xlab("Sensor Model") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(R^2),size=24)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  # geom_hline(yintercept=150, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of ", R^2, " for all ", PM[2.5], " sensors: CALIBRATION"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 

plot

# save plot
png("R2_CALIBRATION_PM2.5_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

##############################################################
##############################################################


