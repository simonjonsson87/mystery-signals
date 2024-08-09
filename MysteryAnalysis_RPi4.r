library(dplyr) # anti_join
library(stringr) # str_extract

#===============================================================================
# Import and organise data
#===============================================================================
#
# Getting a list of all csv files with 'withX' in the name from out path
#
path <- "/Users/simonjonsson/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/SDR/MysterySignals/data/RPi4_data/tmp"
files <- list.files(path = path, pattern = ".+?RPi4.+?csv$", full.names = TRUE)

#
# Import the data from eac csv file and put the result in raw.
#
raw_433 <- data.frame()
raw_868 <- data.frame()

for (file in files) {
  temp_df <- read.csv(file)
  
  if (nrow(temp_df) > 0) {
    # While we import the data, we'll take the location part of the filename 
    # and put it in it's own column in the data.frame
    split_filename <- strsplit(basename(file), split = "_")[[1]]
    location <- split_filename[4]
    temp_df$location <- location
    freq <- split_filename[3]
    if (freq == '433') {
      raw_433 <- rbind(raw_433, temp_df)
    } else {
      raw_868 <- rbind(raw_868, temp_df)
    }
  }
}
rm(temp_df)
#
# First we extract the mystery signals
#
df_868 <- data.frame(time=as.POSIXct(raw_868$time, format="%Y-%m-%d %H:%M:%S"), codes=raw_868$codes, rssi=as.numeric(raw_868$rssi), model=raw_868$model, location=raw_868$location)
df_868 <- subset(df_868, model=='mystery')
df_868$payload <- str_extract(toupper(df_868$codes[df_868$model=='mystery']), "(?<=AAAAAAAAAAAAAAAA)([0-9A-Fa-f]*)") 
df_868 <- subset(df_868, payload!= 'NA')
df_868 <- subset(df_868, substr(payload, 1, 4)=='2DD4')
df_868$id <- substr(df_868$payload, 5, 8)
df_868 <- subset(df_868, substr(payload, 9, 9)=='0')
df_868$value <- strtoi(substr(df_868$payload, 15, 18), base = 16) # We pick out the value from the payload and convert it to decimal
df_868$glbId <- paste(df_868$model, '-', df_868$id, sep='')
df_868$value <- df_868$value * 0.1
#
# We extract the known sensor values
#
red <- raw_433

tmp <- red[red$model=='Nexus-TH' & red$id=='33',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, '-temp', sep='')
df_433 <- data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location)
tmp$glbId <- paste(tmp$model, '-', tmp$id, '-hum', sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$humidity, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Acurite-Tower' & red$id=='794',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, '-temp', sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
tmp$glbId <- paste(tmp$model, '-', tmp$id, '-hum', sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$humidity, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Schrader',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Abarth 124 Spider',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Rubicson-Temperature',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Citroen',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))

tmp <- red[red$model=='Renault',]
tmp$glbId <- paste(tmp$model, '-', tmp$id, sep='')
df_433 <- rbind(df_433, data.frame(time=as.POSIXct(tmp$time, format="%Y-%m-%d %H:%M:%S"), glbId=tmp$glbId, value=tmp$temperature_C, rssi=as.numeric(tmp$rssi), location=tmp$location))
red <- anti_join(red, tmp, by=c("time"))


df_433$time <- as.POSIXct(df_433$time, format="%Y-%m-%d %H:%M:%S")
rm(red, tmp)

df <- rbind(data.frame(time=df_433$time, glbId=df_433$glbId, value=df_433$value), data.frame(time=df_868$time, glbId=df_868$glbId, value=df_868$value))
#===============================================================================
# Graphs
#===============================================================================
summary(as.factor(df$glbId))
plot(df$value ~ df$time, col=df$glbId)

plot1 <- function(df, sensor) {
  tmp <- df[df$glbId==sensor | df$glbId=='mystery-075B' | df$glbId=='mystery-DCCC' | df$glbId=='mystery-EEA0',]
  ggplot(tmp, aes(x = time, y = value, color = glbId)) +
    geom_point() +
    theme_minimal() +
    ylim(c(0,105)) 
}

plot1(df, 'Acurite-Tower-794-hum')
plot1(df, 'Acurite-Tower-794-temp')
plot1(df, 'Nexus-TH-33-hum')
plot1(df, 'Nexus-TH-33-temp')

#
# Mystery signals and known temperature sensors.
# All data
#
tmp <- df[df$glbId=='Nexus-TH-33-temp' | df$glbId=='Acurite-Tower-794-temp' | df$glbId=='mystery-075B' | df$glbId=='mystery-DCCC' | df$glbId=='mystery-EEA0',]
ggplot(tmp, aes(x = time, y = value, color = glbId)) +
  geom_point() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  scale_y_continuous(breaks = seq(10, 37, by = 1), limits = c(10, 37))
#
# Mystery signals and known temperature sensors.
# Just a few days
#
tmp <- df[df$glbId=='Nexus-TH-33-temp' | df$glbId=='Acurite-Tower-794-temp' | df$glbId=='mystery-075B' | df$glbId=='mystery-DCCC' | df$glbId=='mystery-EEA0',]
tmp <- tmp[tmp$time > as.POSIXct("2024-08-01") & tmp$time < as.POSIXct("2024-08-05"),]
ggplot(tmp, aes(x = time, y = value, color = glbId)) +
  geom_point() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  scale_y_continuous(breaks = seq(10, 37, by = 1), limits = c(10, 37))





