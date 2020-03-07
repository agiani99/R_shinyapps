library(tidyverse)

setwd("Y:/BMBF/ZUKUNFTCluster/colorectal_100")

conditions <- read_csv("csv/2019_11_12T10_21_59Z/conditions.csv", 
                       col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                        STOP = col_date(format = "%Y-%m-%d")))

medications <- read_csv("csv/2019_11_12T10_21_59Z/medications.csv", 
                        col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                         STOP = col_date(format = "%Y-%m-%d")))

names(medications)[3] <- "Id"
names(conditions)[3] <- "Id"

patients <- read_csv("csv/2019_11_12T10_21_59Z/patients.csv", 
                     col_types = cols(BIRTHDATE = col_date(format = "%Y-%m-%d"), 
                                      DEATHDATE = col_date(format = "%Y-%m-%d")))
patients <- patients %>% 
  mutate(Name = paste(FIRST, LAST, sep = "")) %>% select(Name, everything()) %>% 
  select(-c(CITY, COUNTY, ZIP, STATE, LAT, LON, ADDRESS, BIRTHPLACE, SSN, PASSPORT, DRIVERS, MAIDEN))
patients

observations <- read_csv("csv/2019_11_12T10_21_59Z/observations.csv", 
                         col_types = cols(DATE = col_date(format = "%Y-%m-%d"), 
                                          ENCOUNTER = col_skip(), TYPE = col_skip()))


observations
names(observations)[2] <- "Id"


sum(is.na(df$ID))

table(df$Name, df$ID)

length(unique(df$ID))
length(unique(df$Name))


##%######################################################%##
#                                                          #
####           reading in output from synthea           ####
#                                                          #
##%######################################################%##

library(dplyr)
library(stringr)

#text <- readLines("output_colorectal_cancer_sel.txt" )


fileName <- '../colorectal_100/output_colorectal_cancer_100.csv'
text <- readChar(fileName, file.info(fileName)$size)


df <- text %>%               #assumes your text is a single character string
  str_split("-----") %>%     #split at -----
unlist() %>%               #unlist
  str_trim() %>%             #trim spaces
  enframe(name = NULL) %>%   #convert to dataframe (tibble) - one column called value
  mutate(ID = as.numeric(str_extract(value, "^\\d+")),                          #first digits
         Name = str_trim(str_match(value, "-- (.+?) \\(")[,2]),
         Name = gsub(" ","",Name),#between -- and (
         Age = as.numeric(str_match(value, "\\((\\d+)")[,2]),       #digits after (
         #Gender = str_match(value, "(M|F)\\)")[,2], #MF before )
         Location = str_match(value, "\\) (.+?)\\n")[,2]) %>% #after ) to end line
  mutate(Creatinine = as.numeric(str_match(value, "Ratio =\\s+([\\.0-9]+)")[,2]),
         Glucose = as.numeric(str_match(value, "Glucose =\\s+([\\.0-9]+)")[,2]),
         Height = as.numeric(str_match(value, "Height =\\s+([\\.0-9]+)")[,2]),
         BMI = as.numeric(str_match(value, "BMI =\\s+([\\.0-9]+)")[,2]),
         Calcium = as.numeric(str_match(value, "Calcium =\\s+([\\.0-9]+)")[,2]),
         Carbon_dioxide = as.numeric(str_match(value, "Carbon Dioxide =\\s+([\\.0-9]+)")[,2]),
         Cholesterol_tot = as.numeric(str_match(value, "Total Cholesterol =\\s+([\\.0-9]+)")[,2]),
         LDL = as.numeric(str_match(value, "LDL =\\s+([\\.0-9]+)")[,2]),
         HDL = as.numeric(str_match(value, "HDL =\\s+([\\.0-9]+)")[,2]),
         Dia_Pressure = as.numeric(str_match(value, "Diastolic Blood Pressure =\\s+([\\.0-9]+)")[,2]),
         Sodium = as.numeric(str_match(value, "Sodium =\\s+([\\.0-9]+)")[,2]),
         EGFR = as.numeric(str_match(value, "EGFR =\\s+([\\.0-9]+)")[,2]),
         Urea_N = as.numeric(str_match(value, "Urea Nitrogen =\\s+([\\.0-9]+)")[,2]),
         Triglycerides = as.numeric(str_match(value, "Triglycerides =\\s+([\\.0-9]+)")[,2]),
         Sys_Pressure = as.numeric(str_match(value, "Systolic Blood Pressure =\\s+([\\.0-9]+)")[,2]),
         Weight = as.numeric(str_match(value, "Weight =\\s+([\\.0-9]+)")[,2]),
         Potassium = as.numeric(str_match(value, "Potassium =\\s+([\\.0-9]+)")[,2]),
         Smoker = str_match(value, "smoker = false")[,1]) %>% 
         #DALY = str_match(value, "DALY =\\s+(.+?)\\n")[,2],
         #QALY = str_match(value, "QALY =\\s+(.+?)\\n")[,2],
         #QOL = str_match(value, "QOL =\\s+(.+?)\\n")[,2]) %>%
  filter(!is.na(ID)) %>%     #remove final blank row
  select(-value) %>%         #remove original text
  mutate(Smoker = gsub("smoker = ", "", Smoker)) %>% 
  arrange(ID) 
  
#df <- df[-c(1,102),]

location <- (c("Hamburg", "Aachen", "Giessen", "Schmallenberg", "Frankfurt", "Berlinghofen"))
df$Location <- sample(location, 100, replace = T)
df$Status <- rep("Living", 100)
df$Status[19] <- "Deceased"
df$Smoker[is.na(df$Smoker)] <- "true"

summary(df)
table(df$Status)
table(df$Location)
table(df$Smoker)

medications
conditions


patient2 <- inner_join(patients, df, by = "Name") # without Medication
table(patient2$GENDER)

patients3 <- full_join(medications, patient2, by = "Id") # with Medication

patients4 <- full_join(conditions, patient2, by = "Id") # with Conditions

patients5 <- full_join(observations,patient2, by = "Id")

write_excel_csv(patient2,"VC_colorectal_patients2.csv", delim = ";")
write_excel_csv(patients3,"VC_colorectal_patients3.csv", delim = ";")
write_excel_csv(patients4,"VC_colorectal_patients4.csv", delim = ";")

patients5 %>% group_by(Id) %>% filter(CODE == "QOL") %>% filter(Age < 60) %>% filter(VALUE <100) %>%   
  select(VALUE, DATE, Name, GENDER, RACE) %>% 
  ggplot() +
  geom_line(aes(x=DATE, y = jitter(VALUE, factor = 0.2), group = Name, color = RACE)) +
  ylab("QOL Value")

##%######################################################%##
#                                                          #
####                   Readin results 50                ####
#                                                          #
##%######################################################%##

patients2 <- read_delim("Y:/BMBF/ZUKUNFTCluster/Colorectal/VC_colorectal_patients2.csv", 
                        "\t", escape_double = FALSE, col_types = cols(BIRTHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      DEATHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      SUFFIX = col_skip(), Urea_N = col_skip()), 
                        trim_ws = TRUE)

patients3 <- read_delim("Y:/BMBF/ZUKUNFTCluster/Colorectal/VC_colorectal_patients3.csv", 
                        "\t", escape_double = FALSE, col_types = cols(BIRTHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      DEATHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      SUFFIX = col_skip(), Urea_N = col_skip()), 
                        trim_ws = TRUE)

patients4 <- read_delim("Y:/BMBF/ZUKUNFTCluster/Colorectal/VC_colorectal_patients4.csv", 
                        "\t", escape_double = FALSE, col_types = cols(BIRTHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      DEATHDATE = col_date(format = "%d.%m.%Y"), 
                                                                      SUFFIX = col_skip(), Urea_N = col_skip()), 
                        trim_ws = TRUE)


##%######################################################%##
#                                                          #
####               read in colorectal 100               ####
#                                                          #
##%######################################################%##

setwd("Y:/BMBF/ZUKUNFTCluster/colorectal_100/csv/2019_11_12T10_21_59Z")

load("Y:/BMBF/ZUKUNFTCluster/colorectal_100/reference_100_colorectal.RData")

conditions <- read_csv("conditions.csv", 
                       col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                        STOP = col_date(format = "%Y-%m-%d")))

medications <- read_csv("medications.csv", 
                        col_types = cols(START = col_date(format = "%Y-%m-%d"), 
                                         STOP = col_date(format = "%Y-%m-%d")))

names(medications)[3] <- "Id"
names(conditions)[3] <- "Id"

patients <- read_csv("patients.csv", 
                     col_types = cols(BIRTHDATE = col_date(format = "%Y-%m-%d"), 
                                      DEATHDATE = col_date(format = "%Y-%m-%d")))
patients <- patients %>% 
  mutate(Name = paste(FIRST, LAST, sep = "")) %>% select(Name, everything()) %>% 
  select(-c(CITY, COUNTY, ZIP, STATE, LAT, LON, ADDRESS, BIRTHPLACE, SSN, PASSPORT, DRIVERS, MAIDEN))
patients

observations <- read_csv("observations.csv", 
                         col_types = cols(DATE = col_date(format = "%Y-%m-%d"), 
                                          ENCOUNTER = col_skip(), TYPE = col_skip()))


observations
names(observations)[2] <- "Id"

library(dplyr)
library(stringr)
library(ggrepel)

#text <- readLines("output_colorectal_cancer_sel.txt" )

fileName <- '../../output_colorectal_cancer_100.csv'
text <- readChar(fileName, file.info(fileName)$size)


df <- text %>%               #assumes your text is a single character string
  str_split("-----") %>%     #split at -----
unlist() %>%               #unlist
  str_trim() %>%             #trim spaces
  enframe(name = NULL) %>%   #convert to dataframe (tibble) - one column called value
  mutate(ID = as.numeric(str_extract(value, "^\\d+")),                          #first digits
         Name = str_trim(str_match(value, "-- (.+?) \\(")[,2]),
         Name = gsub(" ","",Name),#between -- and (
         Age = as.numeric(str_match(value, "\\((\\d+)")[,2]),       #digits after (
         #Gender = str_match(value, "(M|F)\\)")[,2], #MF before )
         Location = str_match(value, "\\) (.+?)\\n")[,2]) %>% #after ) to end line
  mutate(Creatinine = as.numeric(str_match(value, "Ratio =\\s+([\\.0-9]+)")[,2]),
         Glucose = as.numeric(str_match(value, "Glucose =\\s+([\\.0-9]+)")[,2]),
         Height = as.numeric(str_match(value, "Height =\\s+([\\.0-9]+)")[,2]),
         BMI = as.numeric(str_match(value, "BMI =\\s+([\\.0-9]+)")[,2]),
         Calcium = as.numeric(str_match(value, "Calcium =\\s+([\\.0-9]+)")[,2]),
         Carbon_dioxide = as.numeric(str_match(value, "Carbon Dioxide =\\s+([\\.0-9]+)")[,2]),
         Cholesterol_tot = as.numeric(str_match(value, "Total Cholesterol =\\s+([\\.0-9]+)")[,2]),
         LDL = as.numeric(str_match(value, "LDL =\\s+([\\.0-9]+)")[,2]),
         HDL = as.numeric(str_match(value, "HDL =\\s+([\\.0-9]+)")[,2]),
         Dia_Pressure = as.numeric(str_match(value, "Diastolic Blood Pressure =\\s+([\\.0-9]+)")[,2]),
         Sodium = as.numeric(str_match(value, "Sodium =\\s+([\\.0-9]+)")[,2]),
         EGFR = as.numeric(str_match(value, "EGFR =\\s+([\\.0-9]+)")[,2]),
         Urea_N = as.numeric(str_match(value, "Urea Nitrogen =\\s+([\\.0-9]+)")[,2]),
         Triglycerides = as.numeric(str_match(value, "Triglycerides =\\s+([\\.0-9]+)")[,2]),
         Sys_Pressure = as.numeric(str_match(value, "Systolic Blood Pressure =\\s+([\\.0-9]+)")[,2]),
         Weight = as.numeric(str_match(value, "Weight =\\s+([\\.0-9]+)")[,2]),
         Potassium = as.numeric(str_match(value, "Potassium =\\s+([\\.0-9]+)")[,2]),
         Smoker = str_match(value, "smoker = false")[,1]) %>% 
  #DALY = str_match(value, "DALY =\\s+(.+?)\\n")[,2],
  #QALY = str_match(value, "QALY =\\s+(.+?)\\n")[,2],
  #QOL = str_match(value, "QOL =\\s+(.+?)\\n")[,2]) %>%
  filter(!is.na(ID)) %>%     #remove final blank row
  select(-value) %>%         #remove original text
  mutate(Smoker = gsub("smoker = ", "", Smoker)) %>% 
  arrange(ID) 

#df <- df[-c(1,102),]

location <- (c("Hamburg", "Aachen", "Giessen", "Schmallenberg", "Frankfurt", "Berlinghofen"))
df$Location <- sample(location, 100, replace = T)
df$Status <- rep("Living", 100)
df$Status[19] <- "Deceased"
df$Smoker[is.na(df$Smoker)] <- "true"

summary(df)
table(df$Status)
table(df$Location)
table(df$Smoker)

medications
conditions


patient2 <- inner_join(patients, df, by = "Name") # without Medication
table(patient2$GENDER)

patients3 <- full_join(medications, patient2, by = "Id") # with Medication

patients4 <- full_join(conditions, patient2, by = "Id") # with Conditions

patients5 <- full_join(observations,patient2, by = "Id")

write_excel_csv(patient2,"VC_colorectal_patients2.csv", delim = ";")
write_excel_csv(patients3,"VC_colorectal_patients3.csv", delim = ";")
write_excel_csv(patients4,"VC_colorectal_patients4.csv", delim = ";")

patients5 %>% group_by(Id) %>% filter(CODE == "QOL") %>% filter(Age < 70) %>% filter(VALUE <100) %>%   
  select(VALUE, DATE, Name, GENDER, RACE, Id, Status) %>% 
  ggplot() +
  geom_line(aes(x=DATE, y = jitter(VALUE, factor = 0.2), group = Name, color = Status)) +
  ylab("QOL Value")

table(patients5$VALUE, patients5$Age)

patients5 %>% filter(DESCRIPTION != "QALY" ) %>% filter(DESCRIPTION != "DALY" ) %>% filter(DESCRIPTION != "QOL" ) %>% 
  filter(CODE == "57905-2") %>% 
  select(VALUE, DATE, Name, GENDER, RACE, Id, Status, Age) %>% 
  ggplot() +
  geom_point(aes(x=Age, y = VALUE, group = Name, color = GENDER, shape = Status, size = 2)) +
  ylab("Haemoglobin in faecis value (ng/mL)") +
  guides(size=FALSE, shape = F) +
  geom_hline(aes(yintercept=(median(VALUE, na.rm = T) + sd(VALUE, na.rm = T))), size = 1, color = "red", linetype="dashed") +
  geom_text(aes(x = 58, y = (median(VALUE, na.rm = T) + sd(VALUE, na.rm = T) + 5)), label = "Median + SD", color = "red") +
  geom_text(aes(x=Age, y = VALUE, label=ifelse(VALUE>100,as.character(Name),'')),hjust=1,vjust=1)

patients5 %>% filter(DESCRIPTION != "QALY" ) %>% filter(DESCRIPTION != "DALY" ) %>% filter(DESCRIPTION != "QOL" ) %>% 
  filter(CODE == "33756-8") %>% 
  select(VALUE, DATE, Name, GENDER, RACE, Id, Status, Age) %>% 
  ggplot() +
  geom_point(aes(x=Age, y = VALUE, group = Name, color = GENDER, shape = Status, size = 2)) +
  ylab("Polyp size greatest dimension by CAP cancer protocols (mm)") +
  guides(size=FALSE, shape = F) +
  geom_hline(aes(yintercept=(median(VALUE, na.rm = T) + sd(VALUE, na.rm = T))), size = 1, color = "red", linetype="dashed") +
  geom_text(aes(x = 58, y = (median(VALUE, na.rm = T) + sd(VALUE, na.rm = T) + 0.5)), label = "Median + SD", color = "red") +
  geom_text(aes(x=Age, y = VALUE, label=ifelse(VALUE>15,as.character(Name),'')),hjust=1,vjust=1)

save.image("Y:/BMBF/ZUKUNFTCluster/colorectal_100/reference_100_colorectal.RData")

