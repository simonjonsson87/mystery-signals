library(dplyr)
library(tidyverse)
library(sjPlot)
library(lubridate) #floor_date
library(knitr)

#
# Getting a list of all csv files with 'withX' in the name from out path
#
path <- "/Users/simonjonsson/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/SDR/MysterySignals/data/initial_data/"
files <- list.files(path = path, pattern = ".+?withX.+?csv$", full.names = TRUE)

#
# Import the data from eac csv file and put the result in raw.
#
raw <- data.frame()
for (file in files) {
  temp_df <- read.csv(file)
  
  # While we import the data, we'll take the location part of the filename 
  # and put it in it's own column in the data.frame
  location <- str_extract(file, "(?<=withX_)(.*)(?=_)")
  temp_df$location <- location
  
  raw <- rbind(raw, temp_df)
}

#
# Create seperate columns with sections of the hex message.
#
df <- data.frame(time=as.POSIXct(raw$time, format="%Y-%m-%d %H:%M:%S"), codes=raw$codes, rssi=as.numeric(raw$rssi), model=raw$model, hex_data=raw$hex_data, binary_data=raw$binary_data, lengthBits=raw$lengthBits, rowCount=as.numeric(raw$rowCount), location=raw$location)
df$payload[df$model=='Unknown1'] <- str_extract(toupper(df$hex_data[df$model=='Unknown1']), "(?<=AAAAAAAAAAAAAAAA)([0-9A-Fa-f]*)")
df$payload[df$model!='Unknown1'] <- str_extract(toupper(df$codes[df$model!='Unknown1']), "(?<=AAAAAAAAAAAAAAAA)([0-9A-Fa-f]*)") 
df <- subset(df, payload!= 'NA')
df$first4bytes <- substr(df$payload, 1, 4)
df$second4bytes <- substr(df$payload, 5, 8)
df$third4bytes <- substr(df$payload, 9, 10)
df$nibble5to8 <- substr(df$payload, 5, 8) # byte 11, 12. Suspected id of device
df$nibble5to9 <- substr(df$payload, 5, 9) 
df$nibble9to9 <- substr(df$payload, 9, 9) # Either 0 or 6, which seem to determine package type.
df$nibble5to10 <- substr(df$payload, 5, 10)
df$nibble9to10 <- substr(df$payload, 9, 10) # byte 13
df$nibble11to12 <- substr(df$payload, 11, 12) # byte 14
df$nibble13to14 <- substr(df$payload, 13, 14) # byte 15
df$nibble15to16 <- substr(df$payload, 15, 16) # byte 16
df$nibble17to18 <- substr(df$payload, 17, 18) # byte 17
df$nibble19to20 <- substr(df$payload, 19, 20) # byte 18
df$nibble21toend <- substr(df$payload, 21, 100)
df$nibble13toend <- substr(df$payload, 13, 100)
df$nibble15to18 <- substr(df$payload, 15, 18) # byte 16-17

#
# We'll remove any records where the rowCount is 2 or above because these measures are unreliable.
#
print(paste("Will remove ", nrow(df[df$rowCount>1 & !is.na(df$rowCount),]), " rows where rowCount is more than 1."))
df <- subset(df, rowCount==1 | is.na(rowCount))

#
# We'll remove any records where the payload is longer than 40. We know that this 
# just happens when two messages have ended up in the same record
#
print(paste("Will remove ", nrow(df[nchar(df$payload)>=40,]), " rows where payload length is over 40."))
df <- subset(df, nchar(df$payload)<40)

#
# Tranforming hex strings into decimal numbers
#
df$nibble15to18_dec <- strtoi(df$nibble15to18, base = 16)
df$nibble15to16_dec <- strtoi(df$nibble15to16, base = 16)

#
# Is the inital 2DDF universal?
#
print(paste(round(sum(df$first4bytes=='2DD4')/length(df$first4bytes)*100, 2), '% of our', length(df$first4bytes), 'messages start with 2DDF after the preamble.' ))
summary(as.factor(df$first4bytes))
View(df[df$first4bytes!='2DD4',])
df <- subset(df, df$first4bytes=='2DD4')

#
# What's the next bytes after 2DDF?
#
dev_freq <- as.factor(df$second4bytes) %>%
  enframe(name = NULL) %>%  # Convert to tibble
  count(value) %>%          # Count the frequency of each level
  arrange(desc(n)) 
dev_freq$prop <- dev_freq$n / sum(dev_freq$n)
print(paste("The id's 'DCCC', '075B', and 'EEA0' make up ", round(dev_freq$prop[dev_freq$value=='DCCC']*100,2), "%, ", round(dev_freq$prop[dev_freq$value=='075B']*100,2), "%, and ", round(dev_freq$prop[dev_freq$value=='EEA0']*100,2), "%, respectively.", sep=""))

#
# What about the less frequent ids?
#
View(df[df$second4bytes=='B40D',])
View(df[df$second4bytes=='0EB6',])
View(df[df$second4bytes=='03AD',])
View(df[df$second4bytes=='ED40',])
View(df[df$second4bytes=='DD40',])
View(df[df$second4bytes=='056C',])

#
# Comparing signal strength (rssi) at different times and for different  potential 'ids'
#
summary_function <- function(x) {
  c(mean = mean(x, na.rm = TRUE), 
    sd = sd(x, na.rm = TRUE), 
    median = median(x, na.rm = TRUE), 
    count = length(x))
}

summary_table <- aggregate(rssi ~ nibble5to8, data = df[df$location=='livingroom',], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table


summary_table <- aggregate(rssi ~ nibble5to8, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-14 07:00:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-14 08:00:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to8, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-15 08:00:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-15 09:00:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to8, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-20 18:12:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-20 19:12:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to8, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-20 18:12:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-20 19:12:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to9, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-14 07:00:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-14 08:00:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to9, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-15 08:00:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-15 09:00:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to9, data = df[df$location=='livingroom' & df$time>as.POSIXct("2024-06-20 18:12:00", format="%Y-%m-%d %H:%M:%S") & df$time<as.POSIXct("2024-06-20 19:12:00", format="%Y-%m-%d %H:%M:%S") ,], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to9, data = df[df$location=='livingroom',], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble5to10, data = df[df$location=='livingroom',], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

summary_table <- aggregate(rssi ~ nibble9to10, data = df[df$location=='livingroom',], FUN = summary_function)
sorted_summary_table <- summary_table[order(-summary_table['rssi']$rssi[,'count']), ]
sorted_summary_table

#
# Check entropy for each letter in payload
#
tmp <- lapply(seq(1,30), function (i) { substr(df$payload, i, i) })
entropy <- function (x) {
  char_counts <- table(x)
  total_chars <- sum(char_counts)
  char_probs <- char_counts / total_chars
  entropy_value <- -sum(char_probs * log2(char_probs))
  entropy_value
}
lapply(tmp, entropy)

#
# What are the thrid round of bytes?
#
as.factor(df$third4bytes) %>%
  enframe(name = NULL) %>%  # Convert to tibble
  count(value) %>%          # Count the frequency of each level
  arrange(desc(n)) 

#
# Creating a table with all distinct messages and their frequency
#
red <- data.frame(time=df$time, first=df$first4bytes, bytes11to12=df$second4bytes, byte13=df$third4bytes, byte14=df$nibble11to12, byte15=df$nibble13to14, byte16=df$nibble15to16, byte17=df$nibble17to18, byte18=df$nibble19to20, byte19toend=df$nibble21toend)
red <- red[red$bytes11to12=='DCCC' | red$bytes11to12=='EEA0' | red$bytes11to12=='075B',]
red <- subset(red, byte14!='')

tmp <- red[red$second=='DCCC',]
summary(as.factor(tmp$third))
summary(as.factor(tmp$fourth[tmp$third=='05']))
summary(as.factor(tmp$rest[tmp$third=='05']))
sort(unique(tmp$rest[tmp$third=='05']))
tmp[tmp$third=='05',]
as.factor(tmp$fourth) %>% enframe(name = NULL) %>% count(value) %>% arrange(desc(n)) 

result <- red %>%
  group_by(bytes11to12, byte13, byte14, byte15, byte16, byte17, byte18, byte19toend) %>%
  summarise(row_count = n()) %>%
  arrange(bytes11to12, byte13, byte14, byte15, byte16, byte17, byte18, byte19toend) 

tab_df(result, file='/Users/simonjonsson/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/SDR/MysterySignals/messages.html')

#
# Bytes 14 and 15
#
tmp <- red
kable(summary(as.factor(tmp$byte14)))
kable(summary(as.factor(tmp$byte15)))
tmp <- red[red$bytes11to12=='DCCC',]
kable(summary(as.factor(tmp$byte14)))
kable(summary(as.factor(tmp$byte15)))


#
# Bytes 16 and 17
#
tmp <- summary(as.factor(red$byte16[red$byte13=='05']))
print (paste("Byte 16 is '00' ",round(tmp['00']/sum(tmp)*100, 2), "% of the time, '01' ", round(tmp['03']/sum(tmp)*100, 2), "%, and '03' ", round(tmp['02']/sum(tmp)*100, 10), "%. n=", sum(tmp), sep='' ))
summary(as.factor(red$byte17[red$byte13=='05']))

#
# Bytes 16 and 17 graphs
#
red <- df[df$nibble9to9=='0',]
red <- red[red$nibble5to8=='DCCC' | red$nibble5to8=='EEA0' | red$nibble5to8=='075B',]

midnight_intercept <- floor_date(df$time, "day")
noon_intercept <- midnight_intercept + hours(12)

ggplot(red, aes(x = time, y = nibble15to18_dec, color = nibble5to8)) +
  geom_point() +
  theme_minimal() +
  ylim(c(180,300)) +
  xlim(as.POSIXct("2024-06-20", tz = "UTC"), as.POSIXct("2024-06-25", tz = "UTC"))
