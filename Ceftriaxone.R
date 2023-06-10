# CEFTRIAXONE

# Questions for Matthias
#####
# ID 12 and 18: Can you confirm correct interpretation of the dosing and sampling times?
# ID 17: Did this patient receive q12h dosing from the start? Can you provide the correct complete dosing history?
# ID 31: Sampling times in 'Algemeen' and 'Relatieve uren' tabs do not match. Can you provide correct sampling times?
# ID 31: Cell E186 and E187 (dates): Can you confirm year should be 2015 instead of 2014?
# ID 24, 28, 33, 39: 'AUC bloed' concentration needs to be excluded, correct?
# ID 30: Sampling times in 'Algemeen' and 'Relatieve uren' tabs do not match. Can you provide correct sampling times?
# ID 36: Do we have reason to believe that the concentration/timing of the sample with concentration not 7.09 (cell G226 in tab 'Algemeen') is not correct?
# Answers from Matthias (email 26 Aug 2020)
# # ID 12 and 18: Can you confirm correct interpretation of the dosing and sampling times?
# Yes, correct. 
# # ID 17: Did this patient receive q12h dosing from the start? Can you provide the correct complete dosing history?
# Yes, 2x2g from the start because of possible CNS involvement (indication for HD CEF
# # ID 31: Sampling times in 'Algemeen' and 'Relatieve uren' tabs do not match. Can you provide correct sampling times?
# ???Algemeen??? had been drafted first based on ???theoretical??? times. After checking the files and database, this was changed.
#  So ???Relatieve uren??? gives the correct sampling times
# , as was used in the CEF plasma tab.
# # ID 31: Cell E186 and E187 (dates): Can you confirm year should be 2015 instead of 2014?
# Yes, this is a lapsus. Should be 2015
# # ID 24, 28, 33, 39: 'AUC bloed' concentration needs to be excluded, correct?
# Correct, ???AUC??? samples represent the plasma concentrations at the time of BAL sampling.
# # ID 30: Sampling times in 'Algemeen' and 'Relatieve uren' tabs do not match. Can you provide correct sampling times?
# Same situation as for ID 31.
# # ID 36: Do we have reason to believe that the concentration/timing of the sample with concentration not 7.09 (cell G226 in tab 'Algemeen') is not correct?
# As far as I can check, there is no apparent reason for this. Of course, there is always an inherent risk of sampling error. Many samples (as this one) have been taken by the nurse. It might well be (it seems) that ceftriaxone had not yet been administered at the timing of this sample.
#####

# Load packages
library(readxl)       # Data assembly
library(dplyr)        # Data assembly
library(tidyr)        # Data assembly
library(lubridate)    # Data assembly
library(reshape2)     # Data assembly
library(ggplot2)      # Visualisation
library(ggforce)      # Statistics
library(nlme)
library(pROC)
library(binom)
library(ggpubr)
library(truncnorm)

# Set seed for reproducible research
set.seed(081212)

# Remove all the variables from the workspace
rm(list = ls())

# Set theme
theme_set(theme_bw())

# Command+F -> search: AZERTY = load dataset immediately

# Set working directory
getwd()
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data") # mac
setwd("C:/Users/u0064069/Dropbox/Ceftriaxone/Data") # dell
dir(pattern="*csv")
dir(pattern="*xlsx")

### Data assembly
####################################################################################################################################################################################################
Tab1 <- read_excel(path="PK ceftriaxone_werkversie_20200513_fu berekening.xlsx", sheet = "Algemeen", skip = 0, na="")
Tab2 <- read_excel(path="PK ceftriaxone_werkversie_20200513_fu berekening.xlsx", sheet = "CEF plasma", skip = 0, na="")
Tab3 <- read_excel(path="PK ceftriaxone_werkversie_20200513_fu berekening.xlsx", sheet = "Fu voorspeld vs gemeten", skip = 0, na="")
Tab4 <- read_excel(path="PK ceftriaxone_werkversie_20200513_fu berekening.xlsx", sheet = "Relatieve uren", skip = 0, na="")

# Tab1
colnames(Tab1)
Tab1 <- Tab1 %>% select(`Pati<U+00EB>nt`,
                        "Dag",
                        `Matrix\r\nCave 4 x BAL (groene invulling)`,
                        "Datum",
                        `Theoretisch uur \r\nstaalname`,
                        `Concentratie Ceftriaxone\r\nTotaal`,
                        `Concentratie Ceftriaxone\r\nOngebonden fractie (%)`) %>%
  rename(ID   = `Pati<U+00EB>nt`,
         DAY  = "Dag",
         CMT  = `Matrix\r\nCave 4 x BAL (groene invulling)`,
         DATE = "Datum",
         TIME = `Theoretisch uur \r\nstaalname`,
         DV   = `Concentratie Ceftriaxone\r\nTotaal`, # in mg/L
         FU   = `Concentratie Ceftriaxone\r\nOngebonden fractie (%)`) %>%
  filter(!is.na(DAY)) %>%
  mutate(ID = substr(as.character(ID), 2, 9999), # remove the R in the patient IDs
         DAY = substr(as.character(DAY), 2, 2), # remove the D in the day numbers
         ID = as.numeric(ID),
         DAY = as.numeric(DAY),
         CMT = ifelse(CMT == "plasma", 1, 2), # Plasma = CMT 1, BAL = CMT 2
         DATE = as.character(DATE),
         TIME = substr(as.character(TIME), 12, 16),
         DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME),
         DV = round(as.numeric(DV), digits = 1),
         FU = round(as.numeric(FU), digits = 1)) %>%
  filter(!is.na(DV)) %>%
  select(ID, DATETIME, DATE, TIME, DAY, CMT, DV, FU) %>%
  mutate(DVID = 1, # DVID=1: total ceftriaxone concentration
         MDV = 0,
         EVID = 0)

# Tab2
colnames(Tab2)
Tab2 <- Tab2 %>% select(PatientID,
                        D,
                        "Dosis per keer (mg)",
                        "Aantal giften per dag",
                        "TBW bij opname (kg)")

# Tab4
colnames(Tab4)
Tab4 <- Tab4 %>% select(PatientID,
                        D,
                        "T toediening")

# Tab24
Tab24 <- full_join(Tab2, Tab4, by = c("PatientID", "D"))
Tab24 <- Tab24 %>% rename(ID = PatientID,
                          DAY = D,
                          NDOSE = "Aantal giften per dag",
                          AMT = "Dosis per keer (mg)",
                          BM = "TBW bij opname (kg)",
                          TIME = "T toediening") %>%
  mutate(ECMO = ifelse(ID == "R17_ECMO" | ID == "R17" | ID == "R27_ECMO" | ID == "R27",
                       1,
                       0),
         ID = as.numeric(substr(ID, 2, 3)),
         DAY = as.numeric(substr(DAY, 2, 2)),
         TIME = substr(as.character(TIME), 12, 16))

# There are problems with eight patients. Look into them and solve one by one:
Tab24 <- Tab24 %>% arrange(ID, DAY) %>%
  mutate(TIME = ifelse(ID == 17, "10:00:00", TIME)) %>% # ID 17
  mutate(TIME = ifelse(ID == 19, "16:00:00", TIME)) %>% # ID 19 --- cf. Tab Algemeen, column I: "eigenlijk D4"
  mutate(TIME = ifelse(ID == 21, "18:00:00", TIME)) %>% # ID 21 --- 
  mutate(TIME = ifelse(ID == 27, "18:00:00", TIME)) %>% # ID 27 --- 
  mutate(TIME = ifelse(ID == 33 & DAY == 3, "10:00:00", TIME)) %>% # ID 33 --- 
  mutate(TIME = ifelse(ID == 33 & DAY == 6, "10:00:00", TIME)) %>% # ID 33 --- 
  mutate(TIME = ifelse(ID == 40, "02:00:00", TIME)) %>% # ID 40 --- 
  mutate(TIME = ifelse(ID == 41, "10:00:00", TIME)) %>% # ID 41 --- 
  filter(!is.na(AMT))

# Make the same corrections in Tab1
Tab1 <- Tab1 %>%
  mutate(DAY = ifelse(ID == 13, 3, DAY)) %>%
  mutate(DAY = ifelse(ID == 19, 4, DAY)) %>%
  mutate(DAY = ifelse(ID == 21, 3, DAY)) %>%
  mutate(DAY = ifelse(ID == 27, 3, DAY)) %>%
  mutate(DAY = ifelse(ID == 33 & DAY == 2, 3, DAY)) %>%
  mutate(DAY = ifelse(ID == 33 & DAY == 5, 6, DAY)) %>%
  mutate(DAY = ifelse(ID == 35, 3, DAY)) %>%
  mutate(DAY = ifelse(ID == 40, 4, DAY)) %>%
  mutate(DAY = ifelse(ID == 41, 3, DAY))

# Row bind Tab1 and Tab24
colnames(Tab1)
colnames(Tab24)
Tab24 <- Tab24 %>% mutate(DATETIME = NA,
                          DATE = NA,
                          CMT = 1, # because doses are administered in the central compartment
                          DV = 0,
                          FU = 0,
                          DVID = NA,
                          MDV = 1,
                          EVID = 1) %>%
  select(ID, DATETIME, DATE, TIME, DAY, CMT, DV, FU, DVID, MDV, EVID, AMT, NDOSE, BM, ECMO)
Tab1 <- Tab1 %>% mutate(AMT = 0,
                        NDOSE = NA,
                        BM = NA,
                        ECMO = NA) %>%
  select(ID, DATETIME, DATE, TIME, DAY, CMT, DV, FU, DVID, MDV, EVID, AMT, NDOSE, BM, ECMO)
colnames(Tab1)
colnames(Tab24)
Tab124 <- rbind(Tab1, Tab24)
Tab124 <- Tab124 %>% arrange(ID, DATE, TIME)

# Manually fill in the NA DATES in the administration data records
Tab124 <- Tab124 %>%
  mutate(DATE = ifelse(ID == 7 & is.na(DATE), "2014-01-30", DATE)) %>%
  mutate(DATE = ifelse(ID == 8 & is.na(DATE) & DAY == 2, "2014-02-27", DATE)) %>%
  mutate(DATE = ifelse(ID == 8 & is.na(DATE) & DAY == 5, "2014-03-02", DATE)) %>%
  mutate(DATE = ifelse(ID == 9 & is.na(DATE) & DAY == 2, "2014-03-03", DATE)) %>%
  mutate(DATE = ifelse(ID == 9 & is.na(DATE) & DAY == 5, "2014-03-06", DATE)) %>%
  mutate(DATE = ifelse(ID == 10 & is.na(DATE), "2014-03-01", DATE)) %>%
  mutate(DATE = ifelse(ID == 13 & is.na(DATE), "2014-03-17", DATE)) %>%
  mutate(DATE = ifelse(ID == 14 & is.na(DATE) & DAY == 2, "2014-03-18", DATE)) %>%
  mutate(DATE = ifelse(ID == 14 & is.na(DATE) & DAY == 5, "2014-03-21", DATE)) %>%
  mutate(DATE = ifelse(ID == 15 & is.na(DATE) & DAY == 2, "2014-03-20", DATE)) %>%
  mutate(DATE = ifelse(ID == 15 & is.na(DATE) & DAY == 5, "2014-03-23", DATE)) %>%
  mutate(DATE = ifelse(ID == 16 & is.na(DATE), "2014-04-01", DATE)) %>%
  mutate(DATE = ifelse(ID == 17 & is.na(DATE), "2014-04-01", DATE)) %>%
   mutate(ID = ifelse(ID == 18 & DAY == 5, 12, ID)) %>% # R18 DAY 5 is R12 DAY 5 !!!
   mutate(DATE = ifelse(ID == 18 & is.na(DATE) & DAY == 2, "2014-04-22", DATE)) %>%
   mutate(DATE = ifelse(ID == 12 & is.na(DATE) & DAY == 5, "2014-04-08", DATE)) %>%
  mutate(DATE = ifelse(ID == 19 & is.na(DATE), "2014-08-05", DATE)) %>%
  mutate(DATE = ifelse(ID == 21 & is.na(DATE), "2015-02-26", DATE)) %>%
  mutate(DATE = ifelse(ID == 23 & is.na(DATE), "2015-03-05", DATE)) %>%
  mutate(DATE = ifelse(ID == 24 & is.na(DATE), "2015-03-10", DATE)) %>%
  mutate(DATE = ifelse(ID == 25 & is.na(DATE), "2015-03-10", DATE)) %>%
  mutate(DATE = ifelse(ID == 26 & is.na(DATE), "2015-03-18", DATE)) %>%
  mutate(DATE = ifelse(ID == 27 & is.na(DATE), "2015-03-23", DATE)) %>%
  mutate(DATE = ifelse(ID == 28 & is.na(DATE), "2015-04-20", DATE)) %>%
  mutate(DATE = ifelse(ID == 29 & is.na(DATE), "2015-05-22", DATE)) %>%
  mutate(DATE = ifelse(ID == 30 & is.na(DATE), "2015-07-07", DATE)) %>%
  mutate(DATE = ifelse(ID == 31 & is.na(DATE), "2014-07-30", DATE)) %>%
  mutate(DATE = ifelse(ID == 32 & is.na(DATE), "2015-08-11", DATE)) %>%
  mutate(DATE = ifelse(ID == 33 & is.na(DATE) & DAY == 3, "2015-10-21", DATE)) %>%
  mutate(DATE = ifelse(ID == 33 & is.na(DATE) & DAY == 6, "2015-10-24", DATE)) %>%
  mutate(DATE = ifelse(ID == 34 & is.na(DATE), "2016-01-28", DATE)) %>%
  mutate(DATE = ifelse(ID == 35 & is.na(DATE), "2016-02-04", DATE)) %>%
  mutate(DATE = ifelse(ID == 36 & is.na(DATE), "2016-05-13", DATE)) %>%
  mutate(DATE = ifelse(ID == 37 & is.na(DATE), "2016-06-17", DATE)) %>%
  mutate(DATE = ifelse(ID == 38 & is.na(DATE), "2016-07-14", DATE)) %>%
  mutate(DATE = ifelse(ID == 39 & is.na(DATE), "2017-09-28", DATE)) %>%
  mutate(DATE = ifelse(ID == 40 & is.na(DATE), "2017-10-05", DATE)) %>%
  mutate(DATE = ifelse(ID == 41 & is.na(DATE), "2017-10-17", DATE)) %>%
  mutate(DATE = ifelse(ID == 42 & is.na(DATE), "2018-03-01", DATE))

# Correct an error in year in ID 31
Tab124 <- Tab124 %>% mutate(DATE = ifelse(ID == 31 & DATE == "2014-07-30", "2015-07-30", DATE),
                            DATE = ifelse(ID == 31 & DATE == "2014-07-31", "2015-07-31", DATE),
                            DATETIME = paste(DATE, TIME, sep = " "),
                            DATETIME = as.POSIXct(DATETIME))
Tab124 <- Tab124 %>% mutate(TIME = substr(TIME, 1, 5),
                            DATETIME = paste(DATE, TIME, sep = " "),
                            DATETIME = as.POSIXct(DATETIME)) %>%
  arrange(ID, DATETIME)

# Tab3
colnames(Tab3)
Tab3 <- Tab3 %>% select(PatientID,
                        "CEFt1\r\nmg/L",
                        "CEFt2\r\nmg/L",
                        "CEFu1\r\nmg/L",
                        "CEFu2\r\nmg/L",
                        "serum albumine (g/L)") %>%
  rename(ID = PatientID,
         CEFT1 = "CEFt1\r\nmg/L",
         CEFT2 = "CEFt2\r\nmg/L",
         CEFU1 = "CEFu1\r\nmg/L",
         CEFU2 = "CEFu2\r\nmg/L",
         ALB = "serum albumine (g/L)") %>%
  mutate(ID = substr(ID, 2, 3)) %>%
  filter(!is.na(ID)) %>%
  mutate(CEFT1 = round(CEFT1, digits = 1),
         CEFT2 = round(CEFT2, digits = 1),
         CEFU1 = round(CEFU1, digits = 3),
         CEFU2 = round(CEFU2, digits = 3))
Tab3a <- Tab3 %>% select(ID, CEFT1, CEFU1, ALB) %>% rename(DV = CEFT1, CEFU = CEFU1)
Tab3b <- Tab3 %>% select(ID, CEFT2, CEFU2, ALB) %>% rename(DV = CEFT2, CEFU = CEFU2)
Tab3 <- rbind(Tab3a, Tab3b)
Tab3 <- Tab3 %>% mutate(ID = as.numeric(ID))

# Tab1234
Tab1234 <- full_join(Tab124, Tab3, by = c("ID", "DV"))
colnames(Tab1234)
Tab1234 <- Tab1234 %>% mutate(CEFU = ifelse(ID == 18 & DATE == "2014-04-22" & TIME == "15:45", NA, CEFU),
                              ALB  = ifelse(ID == 18 & DATE == "2014-04-22" & TIME == "15:45", NA, ALB)) # ID 18: predose and trough DV are both 6.4! Remove predose CEFu and ALB because they are not correct

# FU column can go because it is calculated based on CEFtotal and CEFunbound
Tab1234 <- Tab1234 %>% select(ID, DATETIME, DATE, TIME, DAY, EVID, MDV, AMT, NDOSE, CMT, DVID, DV, CEFU, BM, ALB, ECMO) %>%
  arrange(ID, DATETIME)
Tab1234a <- Tab1234 %>% select(ID, DATETIME, DATE, TIME, DAY, EVID, MDV, AMT, NDOSE, CMT, DVID, DV  , BM, ALB, ECMO)
Tab1234b <- Tab1234 %>% select(ID, DATETIME, DATE, TIME, DAY, EVID, MDV, AMT, NDOSE, CMT, DVID, CEFU, BM, ALB, ECMO) %>%
  filter(!is.na(CEFU)) %>%
  rename(DV = CEFU) %>%
  mutate(DVID = 2) # DVID=2: unbound ceftriaxone concentration
Tab1234 <- rbind(Tab1234a,
                 Tab1234b)
Tab1234 <- Tab1234 %>% arrange(ID, DATETIME) %>%
  group_by(ID, DAY) %>%
  fill(c(BM, ALB, ECMO), .direction = "downup")

# ID 17 received a second infusion!! cf. CEF 2g 2dd, dus reeds nieuwe gift om 22u, dus eigenlijk te zien als dal (CEF 2g 2dd), gebruiken ipv andere dal owv ongebonden fractie voor deze conc beschikbaar
add <- Tab1234 %>% filter(ID == 17 & TIME == "22:00") %>%
  mutate(DATETIME = DATETIME + 60,
         TIME = "22:01",
         EVID = 1,
         MDV = 1,
         AMT = 2000,
         NDOSE = 2,
         CMT = 1,
         DVID = NA,
         DV = 0.000)
Tab1234 <- rbind(Tab1234, add)
Tab1234 <- Tab1234 %>% arrange(ID, DATETIME) %>%
  group_by(ID) %>%
  fill(c(NDOSE), .direction = "downup")

# Add, patient by patient, the previous dosing history
add1 <- Tab1234 %>% filter(EVID == 1) %>% filter(TIME != "22:01") # Now I have a dataset with 38 data records, one for each patient. Now start adjusting it.
add1 <- add1 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA)
add2 <- add1 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA) %>%
  filter(DAY > 0)
add3 <- add2 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA) %>%
  filter(DAY > 0)
add4 <- add3 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA) %>%
  filter(DAY > 0)
add5 <- add4 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA) %>%
  filter(DAY > 0)
add6 <- add5 %>% mutate(DATETIME = DATETIME - 1*60*60*24,
                        DATE = substr(DATETIME, 1, 10),
                        DAY = DAY - 1,
                        ALB = NA) %>%
  filter(DAY > 0)
add <- rbind(add1, add2, add3, add4, add5, add6)
add <- add %>% unique()

Tab1234 <- rbind(Tab1234, add)
Tab1234 <- Tab1234 %>% arrange(ID, DATETIME) %>%
  group_by(ID, DAY) %>%
  fill(c(ALB), .direction = "downup") %>%
  unique()

# Matthias confirmed that ID 17 received q12h dosing from the beginning: add these doses
add <- Tab1234 %>% filter(ID == 17 & (DATE == "2014-03-28" | DATE == "2014-03-29" | DATE == "2014-03-30" | DATE == "2014-03-31")) %>%
  mutate(TIME = "22:00")
Tab1234 <- rbind(Tab1234, add)
Tab1234 <- Tab1234 %>% arrange(ID, DATETIME)

# Correct an error in time in ID 13
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 13 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 14
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 14 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 21
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 21 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in sampling in ID 24
Tab1234 <- Tab1234 %>% filter(!(ID == 24 & DV == 107.000)) %>%
  mutate(DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 27
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 27 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in sampling in ID 28
Tab1234 <- Tab1234 %>% filter(!(ID == 28 & DV == 132.000)) %>%
  mutate(DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 29
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 29 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 30
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 30 & TIME == "10:30" & DV == 147.000, "11:20", TIME),
                              TIME = ifelse(ID == 30 & TIME == "11:00" & DV == 141.000, "11:50", TIME),
                              TIME = ifelse(ID == 30 & TIME == "11:00" & DV == 22.560 , "11:50", TIME), # also do it for DVID == 2 !!
                              TIME = ifelse(ID == 30 & TIME == "12:00" & DV == 142.000, "12:50", TIME),
                              TIME = ifelse(ID == 30 & TIME == "16:00" & DV == 116.000, "16:50", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 31
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 31 & TIME == "18:30" & DV == 306.000, "19:00", TIME),
                              TIME = ifelse(ID == 31 & TIME == "19:00" & DV == 242.000, "19:30", TIME),
                              TIME = ifelse(ID == 31 & TIME == "20:00" & DV == 194.000, "20:30", TIME),
                              TIME = ifelse(ID == 31 & TIME == "00:00" & DV == 191.000, "00:30", TIME),
                              DATE = ifelse(ID == 31 & TIME == "00:30" & DV == 191.000, "2015-07-31", DATE),
                              TIME = ifelse(ID == 31 & TIME == "06:00" & DV == 114.000, "06:30", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 37
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 37 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in sampling in ID 39
Tab1234 <- Tab1234 %>% filter(!(ID == 39 & DV == 59.100)) %>%
  mutate(DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME))

# Correct an error in time in ID 42
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 42 & TIME == "00:00", "23:59", TIME),
                              DATETIME = paste(DATE, TIME, sep = " "),
                              DATETIME = as.POSIXct(DATETIME))

# Correct an error in sampling in ID 33
Tab1234 <- Tab1234 %>% filter(!(ID == 33 & DV == 51.000)) %>%
  mutate(DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME))

# Make time relative and rearrange dataset
Tab1234 <- Tab1234 %>%
  mutate(DATETIME = paste(DATE, TIME, sep = " "),
         DATETIME = as.POSIXct(DATETIME)) %>%
  arrange(ID, DATETIME) %>%
  group_by(ID) %>%
  mutate(INTERVAL = DATETIME - lag(DATETIME),
         INTERVAL = INTERVAL/(60*60), # in hours
         INTERVAL = ifelse(is.na(INTERVAL),
                           0,
                           INTERVAL),
         REL_TIME = round(cumsum(INTERVAL), digits = 3),
         OCC = cumsum(EVID))

Tab1234 <- Tab1234 %>%
  group_by(ID, OCC) %>%
  mutate(INTERVAL = DATETIME - lag(DATETIME),
         INTERVAL = INTERVAL/(60*60), # in hours
         INTERVAL = ifelse(is.na(INTERVAL),
                           0,
                           INTERVAL),
         TAD = cumsum(INTERVAL)) %>%
  replace(is.na(.), 0) %>%
  mutate(RATE = ifelse(EVID == 1, AMT*2, 0)) %>% # 2000 mg over 30 minutes = 4000 mg/h
  select(ID,
         DATETIME,
         DATE,
         TIME,
         REL_TIME,
         TAD,
         OCC,
         EVID,
         MDV,
         AMT,
         RATE,
         CMT,
         DV,
         DVID,
         BM,
         ALB,
         ECMO,
         DAY) %>%
  arrange(ID, DATETIME)

Tab1234 <- Tab1234 %>% mutate(ALB = ifelse(ALB == 0, NA, ALB))

# 26 Aug 2020: Check whether sampling times are correct (Relatieve uren = correct)
check1 <- Tab1234 %>% filter(DVID == 1) %>%
  ungroup() %>%
  select(ID, TIME, DV)
check2 <- read_excel(path="PK ceftriaxone_werkversie_20200513_fu berekening.xlsx",
                   sheet = "Relatieve uren",
                   skip = 0,
                   na="",
                   col_types = c("text", "date", "date", "date", "date", "date", "date",
                                 "date", "date", "date", "date", "date", "date", "date", 
                                 "date", "date", "date"))
check2 <- check2 %>% select(PatientID, T1, T2, T3, T4, T5, T6, T7) %>%
  reshape2::melt(id = c("PatientID")) %>%
  mutate(value = substr(value, 12, 16),
         PatientID = as.numeric(substr(PatientID, 2, 999))) %>%
  rename(ID = PatientID,
         TIME = value) %>%
  mutate(ID = as.numeric(ID),
         DV = "") %>%
  select(-variable)
check12 <- rbind(check1, check2)
check12 <- check12 %>% arrange(ID, TIME)
# Make the necessary corrections
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 23 & (DV == 63.000 | DV == 8.820), "13:00", TIME))
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 29 & TIME == "12:30", "12:50", TIME))
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 29 & TIME == "13:00", "13:20", TIME))
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 29 & TIME == "13:00", "13:20", TIME))
Tab1234 <- Tab1234 %>% mutate(TIME = ifelse(ID == 29 & TIME == "14:00", "14:20", TIME))

# Add covariate data
Tab5 <- read_excel(path="PK ceftriaxone_werkversie_20200605_formula.xlsx",
                   sheet = "Formula",
                   skip = 0,
                   na="")
colnames(Tab5)
Tab5 <- Tab5 %>% rename(ID = Patient,
                        DAY = D,
                        AGE = Age,
                        BILITOT = Bili_tot,
                        CRCLCG = CrCl_CG) %>%
  select(ID, DAY, AGE, BILITOT, CRCLCG) %>%
  unique() %>%
  mutate(ID = as.numeric(substr(ID, 2, 3)),
         DAY = as.numeric(substr(DAY, 2, 2)))

# Summarise
x <- Tab5 %>% select(AGE) %>% filter(!is.na(AGE))
y <- Tab5 %>% select(BILITOT) %>% filter(!is.na(BILITOT))
z <- Tab5 %>% select(CRCLCG) %>% filter(!is.na(CRCLCG))
quantile(x, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = T)
quantile(y, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = T)
quantile(z, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = T)

# Tab12345
Tab12345 <- full_join(Tab1234, Tab5, by = c("ID", "DAY"))

# Add new albumin data --- cf. email Matthias 11-Sep-2020
Tab6 <- read_excel(path="Databank Ceftriaxone_20200911_Hb&Alb.xlsx",
                   sheet = "Hb & Alb dagelijks",
                   skip = 0,
                   na="NA")
Tab6 <- Tab6 %>% select(Nummer,
                        D1, D2, D3, D4, D5, D6, D7,
                        Alb1, Alb2, Alb3, Alb4, Alb5, Alb6, Alb7,
                        Hb1, Hb2, Hb3, Hb4, Hb5, Hb6, Hb7,
                        Hct1, Hct2, Hct3, Hct4, Hct5, Hct6, Hct7) %>%
  rename(ID = Nummer) %>%
  mutate(ID = ifelse(ID == "ECMO 17", 17,
                     ifelse(ID == "ECMO 27", 27,
                            ID)),
         ID = as.numeric(ID))
Tab6DATE <- Tab6 %>% select(ID, D1, D2, D3, D4, D5, D6, D7) %>%
  reshape2::melt(id = c("ID")) %>%
  rename(DATE = value,
         D = variable) %>%
  mutate(D = substr(as.character(D), 2,999),
         D = as.numeric(D),
         DATE = substr(as.character(DATE), 1, 10))
Tab6ALB <- Tab6 %>% select(ID, Alb1, Alb2, Alb3, Alb4, Alb5, Alb6, Alb7) %>%
  reshape2::melt(id = c("ID")) %>%
  rename(ALB = value,
         D = variable) %>%
  mutate(D = substr(as.character(D), 4,999),
         D = as.numeric(D))
Tab6HB <- Tab6 %>% select(ID, Hb1, Hb2, Hb3, Hb4, Hb5, Hb6, Hb7) %>%
  reshape2::melt(id = c("ID")) %>%
  rename(HB = value,
         D = variable) %>%
  mutate(D = substr(as.character(D), 3,999),
         D = as.numeric(D))
Tab6HT <- Tab6 %>% select(ID, Hct1, Hct2, Hct3, Hct4, Hct5, Hct6, Hct7) %>%
  reshape2::melt(id = c("ID")) %>%
  rename(HT = value,
         D = variable) %>%
  mutate(D = substr(as.character(D), 4,999),
         D = as.numeric(D))
Tab6DATEALB <- full_join(Tab6DATE, Tab6ALB, by = c("ID", "D"))
Tab6DATEALBHB <- full_join(Tab6DATEALB, Tab6HB, by = c("ID", "D"))
Tab6DATEALBHBHT <- full_join(Tab6DATEALBHB, Tab6HT, by = c("ID", "D"))
Tab6 <- Tab6DATEALBHBHT %>% select(-D)

# Tab123456
Tab123456 <- full_join(Tab12345, Tab6, by = c("ID", "DATE"))
Tab123456 <- Tab123456 %>% ungroup() %>%
  mutate(ID = ifelse(is.na(DATE), NA, ID)) %>%
  filter_all(any_vars(complete.cases(.))) %>%
  arrange(ID, DATETIME)

# Make corrections in ID 10, 37, 38
Tab123456 <- Tab123456 %>% mutate(ALB.y = ifelse(ID == 10 & DATE == "2014-03-01", 31.9  , ALB.y), # LOCB (only option)
                                  HB    = ifelse(ID == 10 & DATE == "2014-03-01",  9.7  , HB), # LOCB (only option)
                                  HT    = ifelse(ID == 10 & DATE == "2014-03-01",  0.313, HT), # LOCB (only option)
                                  ALB.y = ifelse(ID == 38 & (DATE == "2016-07-14" | DATE == "2016-07-15"), 34.9 , ALB.y)) %>% #, LOCB (only option)
                                  # HB    = ifelse(ID == 38 & (DATE == "2016-07-14" | DATE == "2016-07-15"), 10.30 , HB),
                                  # HT    = ifelse(ID == 38 & (DATE == "2016-07-14" | DATE == "2016-07-15"),  0.297, HT))
  filter(!is.na(DATETIME)) %>%
  arrange(ID, REL_TIME, DVID) %>%
  mutate(ALB_check = ifelse(!is.na(ALB.x) & (ALB.x != ALB.y), "x", ""))
# There are so many mismatches between the ALB values in the original dataset and the new one. Mostly in terms of dates, so there is often one day of difference. Sometimes very strange things like ID 42???
# I consider the newly received data as correct!!!
Tab123456 <- Tab123456 %>% select(-ALB.x, -ALB_check) %>% rename(ALB = ALB.y)

# Use LOCF to impute missing values
# I also mark these imputed data records with xxx_IMPUTE = 1
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
check <- Tab123456 %>% group_by(ID) %>% summarise(sdAGE = sd(AGE, na.rm = T))
check # age within patient all the same --> impute with mean
Tab123456 <- Tab123456 %>%
  arrange(ID, REL_TIME, DVID) %>%
  group_by(ID) %>%
  mutate(AGE_IMPUTE = ifelse(is.na(AGE), 1, 0),
         BILITOT_IMPUTE = ifelse(is.na(BILITOT), 1, 0),
         CRCLCG_IMPUTE = ifelse(is.na(CRCLCG), 1, 0),
         ALB_IMPUTE = ifelse(is.na(ALB), 1, 0),
         HB_IMPUTE = ifelse(is.na(HB), 1, 0),
         HT_IMPUTE = ifelse(is.na(HT), 1, 0),
         AGE = ifelse(ID == 17 | ID == 27, 72, AGE), # median
         BILITOT = ifelse(ID == 17 | ID == 27, 0.38, BILITOT), # median
         CRCLCG = ifelse(ID == 17 | ID == 27, 72.8, CRCLCG), # median
         AGE = impute.mean(AGE),
         # first implement locf, then nocb
         BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lag(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT), BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT),BILITOT = ifelse(is.na(BILITOT), lead(BILITOT), BILITOT),
         CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lag(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG), CRCLCG = ifelse(is.na(CRCLCG), lead(CRCLCG), CRCLCG),
         ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lag(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB), ALB = ifelse(is.na(ALB), lead(ALB), ALB),
         HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lag(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB), HB = ifelse(is.na(HB), lead(HB), HB),
         HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lag(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT), HT = ifelse(is.na(HT), lead(HT), HT))

# ID 42 has no value in new dataset??? Impute with median
Tab123456 <- Tab123456 %>% mutate(ALB = ifelse(ID == 42, 20.6, ALB))
any(is.na(Tab123456)) # no NAs in dataset anymore :)

# Add ALB in DV column
tryALB <- Tab123456 %>% filter(DVID >0) %>% mutate(DV = ALB,
                                                   DVID = 3,
                                                   MDV = 0)
Tab123456 <- rbind(Tab123456, tryALB)
Tab123456 <- Tab123456 %>% arrange(ID, REL_TIME, DVID)

# Mark imputed ALB values on DVID=3 data records only
Tab123456 <- Tab123456 %>% mutate(ALB_IMPUTE_DVID3 = ifelse(DVID == 3 & ALB_IMPUTE == 1, 1, 0))

# Remove duplicate lines
Tab123456 <- Tab123456 %>% unique()

# Age, total bilirubin, and creatinine clearance of patients 17 and 27 are not reported in the dataset.
# But now I see that EMD numbers are reported in "Databank Ceftriaxone_20200911_Hb&Alb.xlsx" sheet "Pati??ntgegevens":
# ID 17: 520810M070 --> 10 Aug 1952 /// 28 Mar 2014 --->
dates <- c("10.08.1952", "28.03.2014")
dates <- strptime(dates, format = "%d.%m.%Y")
diff(as.numeric(dates))/(60 * 60 * 24 * 365) # difference in years
Tab123456 <- Tab123456 %>% mutate(AGE = ifelse(ID == 17, 61, AGE),
                                  AGE_IMPUTE = ifelse(ID == 17, 0, AGE_IMPUTE))
# ID 27: 630614V180 --> 14 Jun 1963 /// 21 Mar 2015 --->
dates <- c("14.06.1963", "21.03.2015")
dates <- strptime(dates, format = "%d.%m.%Y")
diff(as.numeric(dates))/(60 * 60 * 24 * 365) # difference in years
Tab123456 <- Tab123456 %>% mutate(AGE = ifelse(ID == 27, 51, AGE),
                                  AGE_IMPUTE = ifelse(ID == 27, 0, AGE_IMPUTE))
# Also, I see that CRCLCG is present in "Databank Ceftriaxone_20200911_Hb&Alb.xlsx" sheet "Parameters staalname":
# ID 17: day 2: NA          /// day 5: 135 mL/min
# ID 27: day 2: 49.5 mL/min /// day 5: NA
Tab123456 <- Tab123456 %>% mutate(CRCLCG = ifelse(ID == 17, 135, CRCLCG), #  & DAY == 5 (locf, locb)
                                  CRCLCG_IMPUTE = ifelse(ID == 17 & DAY == 5, 0, CRCLCG_IMPUTE)) # (locf, locb)
Tab123456 <- Tab123456 %>% mutate(CRCLCG = ifelse(ID == 27, 49.5, CRCLCG), #  & DAY == 3 (locf, locb)
                                  CRCLCG_IMPUTE = ifelse(ID == 27 & DAY == 3, 0, CRCLCG_IMPUTE)) # (locf, locb)
# Same for BILITOT
# ID 17: day 2: NA   /// day 5: 0.74
# ID 27: day 2: 1.39 /// day 5: NA
Tab123456 <- Tab123456 %>% mutate(BILITOT = ifelse(ID == 17, 0.74, BILITOT), #  & DAY == 5 (locf, locb)
                                  BILITOT_IMPUTE = ifelse(ID == 17 & DAY == 5, 0, BILITOT_IMPUTE)) # (locf, locb)
Tab123456 <- Tab123456 %>% mutate(BILITOT = ifelse(ID == 27, 1.39, BILITOT), #  & DAY == 3 (locf, locb)
                                  BILITOT_IMPUTE = ifelse(ID == 27 & DAY == 3, 0, BILITOT_IMPUTE)) # (locf, locb)

# Add sex
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data")
sex <- read_excel(path="Databank Ceftriaxone_20200911_Hb&Alb.xlsx",
                   sheet = 1,
                   skip = 1,
                   na="")
sex <- sex %>% select(Nummer, "Geslacht (m/v)") %>% filter(!is.na(Nummer)) %>%
  rename(ID = Nummer, SEX = "Geslacht (m/v)") %>%
  mutate(ID = ifelse(ID =="ECMO 17" , 17,
                     ifelse(ID == "ECMO 27", 27,
                            ID)),
         ID = as.numeric(ID),
         SEX = ifelse(SEX == "V", 0,
                      ifelse(SEX == "M", 1, "x")),
         SEX = as.numeric(SEX))
Tab123456 <- full_join(Tab123456, sex, by = c("ID"))

# Add L2 column
Tab123456 <- Tab123456 %>% mutate(L2 = REL_TIME)

##### Add CKD-EPI, SOFA, and ARCpred - Mail Matthias 18 Sept 2020
Tab7 <- read_excel(path="CKD-EPI_SOFA_ARCpred.xlsx",
                   sheet = "Blad1",
                   skip = 0,
                   na="NA")
Tab7 <- Tab7 %>% filter(!is.na(Nummer)) %>% select(Nummer,
                                                   `CKD-EPI_D2`, `CKD-EPI_D5`,
                                                   SOFA_D2, SOFA_D5,
                                                   ARCprediction_D2, ARCprediction_D5,
                                                   SCr_prevadmday_D2, SCr_prevadmday_D5)
Tab7a <- Tab7 %>% select(Nummer,
                         `CKD-EPI_D2`,
                         SOFA_D2,
                         ARCprediction_D2,
                         SCr_prevadmday_D2) %>% mutate(D = "early") %>% rename(ID = Nummer, CKDEPI = 'CKD-EPI_D2', SOFA = SOFA_D2, ARCPRED = ARCprediction_D2, SCR = SCr_prevadmday_D2)
Tab7b <- Tab7 %>% select(Nummer,
                         `CKD-EPI_D5`,
                         SOFA_D5,
                         ARCprediction_D5,
                         SCr_prevadmday_D5) %>% mutate(D = "late") %>% rename(ID = Nummer, CKDEPI = 'CKD-EPI_D5', SOFA = SOFA_D5, ARCPRED = ARCprediction_D5, SCR = SCr_prevadmday_D5)
Tab7 <- rbind(Tab7a, Tab7b)
Tab7 <- Tab7 %>% filter(!is.na(CKDEPI))
Tab123456 <- Tab123456 %>% mutate(D = ifelse(OCC <=3, "early", "late"))

# Tab1234567
Tab1234567 <- full_join(Tab123456, Tab7, by = c("ID", "D"))

# Redefine L2
Tab1234567 <- Tab1234567 %>%  mutate(L2 = REL_TIME)

# Organise dataset
Tab1234567 <- Tab1234567 %>% arrange(ID, DATETIME) %>%
  group_by(ID, OCC) %>%
  mutate(INTERVAL = DATETIME - lag(DATETIME),
         INTERVAL = INTERVAL/(60*60), # in hours
         INTERVAL = ifelse(is.na(INTERVAL),
                           0,
                           INTERVAL),
         TAD = cumsum(INTERVAL)) %>%
  mutate(-INTERVAL)
  



# Write dataset for popPK
print <- Tab1234567 %>% select(-DATETIME, -DATE, -TIME, -DAY, -AGE_IMPUTE, -BILITOT_IMPUTE, -CRCLCG_IMPUTE, -ALB_IMPUTE, -HB_IMPUTE, -HT_IMPUTE, -ALB_IMPUTE_DVID3) %>% as.data.frame() %>%
  select(ID, REL_TIME, TAD, OCC, CMT, EVID, MDV, AMT, RATE, DV, DVID, D, SEX, BM, AGE, ECMO, SOFA, ALB, HB, HT, BILITOT, SCR, CRCLCG, CKDEPI, ARCPRED, L2)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses")
write.csv(print, file="Ceftriaxone.csv", quote=F, na="-99", row.names = F)

####################################################################################################################################################################################################

# Load dataset - AZERTY
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses")
setwd("C:/Users/u0064069/Dropbox/Ceftriaxone/Analyses") # dell
DS <- read.csv(file="Ceftriaxone.csv", na="-99", header = T)
DS <- DS %>% mutate(ID = as.numeric(ID),
                    OCC = as.numeric(OCC),
                    CMT = as.numeric(CMT),
                    EVID = as.numeric(EVID),
                    MDV = as.numeric(MDV),
                    AMT = as.numeric(AMT),
                    RATE = as.numeric(RATE),
                    DVID = as.numeric(DVID),
                    SEX = as.numeric(SEX),
                    AGE = as.numeric(AGE),
                    ECMO = as.numeric(ECMO),
                    SOFA = as.numeric(SOFA),
                    CKDEPI = as.numeric(CKDEPI))

# # 2021 02 01 _24hCLu_pred
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data/20210128_Yuan predictions")
# PRED <- read.csv(file="Merged_byED_manually.csv", na="NA", header = T, colClasses = "numeric")
# PRED <- PRED %>% select(-OCC)
# DS2  <- full_join(DS, PRED, by = c("ID", "ARCPRED", "CKDEPI"))
# DS2 <- DS2 %>% filter(!is.na(REL_TIME)) %>%
#   mutate(CRCL24H = ifelse(is.na(ARCPRED), NA, CRCL24H))
# DS2 %>% summarise(med_YuanPred = quantile(CRCL24H, probs = c(0.5), na.rm = T))
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
# write.csv(DS2, file="Ceftriaxone_24hCLu_pred.csv", quote=F, na="-99", row.names = F)

### Exploratory data analysis
###################################################################################################################################################################################################
## Descriptive statistics
#####
# Number of patients
length(unique(DS$ID)) # 33 patients
# Sex
DS %>% filter(REL_TIME == 0) %>% ungroup() %>% summarise(NR_MALE = sum(SEX))
# Age
DS %>% group_by(ID) %>% summarise(meanAGE = mean(AGE), sdAGE = sd(AGE)) # AGE is at baseline --- time constant
quantile(x = DS$AGE[DS$REL_TIME == 0], probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
# Body mass
DS %>% group_by(ID) %>% summarise(meanWGT = mean(BM), sdWGT = sd(BM)) # BM is at baseline --- time constant
quantile(x = DS$BM[DS$REL_TIME == 0], probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
# ECMO
DS %>% filter(REL_TIME == 0) %>% ungroup() %>% summarise(NR_ECMO = sum(ECMO))
# SOFA
DS %>% group_by(ID) %>% summarise(meanSOFA = mean(SOFA), sdSOFA = sd(SOFA)) # SOFA is time-varying!!!
DS %>% select(ID, SOFA, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(SOFA, probs = c(0.5), na.rm = T),
                                                                                          Q1 = quantile(SOFA, probs = c(0.25), na.rm = T),
                                                                                          Q3 = quantile(SOFA, probs = c(0.75), na.rm = T),
                                                                                          n = n())
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data")
SOFA_bl <- read_excel(path="Databank Ceftriaxone_20200911_Hb&Alb.xlsx", sheet = "Pati??ntgegevens", skip = 1, na="NA")
SOFA_bl <- SOFA_bl %>% select(Nummer, `SOFA score bij opname (zie PDMS)`) %>% rename(ID = Nummer, SOFA_bl = 'SOFA score bij opname (zie PDMS)') %>% filter(!is.na(ID)) %>% mutate(ID = ifelse(ID == "ECMO 17", 17, ifelse(ID == "ECMO 27", 27, ID)))
quantile(x = SOFA_bl$SOFA_bl, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
DS %>% select(ID, SOFA, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(SOFA) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(SOFA, probs = c(0.5), na.rm = T),
            Q1 = quantile(SOFA, probs = c(0.25), na.rm = T),
            Q3 = quantile(SOFA, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, SOFA, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(SOFA) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(SOFA, probs = c(0.5), na.rm = T),
            Q1 = quantile(SOFA, probs = c(0.25), na.rm = T),
            Q3 = quantile(SOFA, probs = c(0.75), na.rm = T),
            n = n())
# CRCLCG
DS %>% group_by(ID) %>% summarise(meanCRCLCG = mean(CRCLCG), sdCRCLCG = sd(CRCLCG)) # CRCLCG is time-varying!!!
DS %>% select(ID, CRCLCG, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(CRCLCG, probs = c(0.5), na.rm = T),
                                                                                          Q1 = quantile(CRCLCG, probs = c(0.25), na.rm = T),
                                                                                          Q3 = quantile(CRCLCG, probs = c(0.75), na.rm = T),
                                                                                          n = n())
DS %>% select(ID, CRCLCG, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(CRCLCG) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CRCLCG <= 130, 0, 1)) %>%
  summarise(Q2 = quantile(CRCLCG, probs = c(0.5), na.rm = T),
            Q1 = quantile(CRCLCG, probs = c(0.25), na.rm = T),
            Q3 = quantile(CRCLCG, probs = c(0.75), na.rm = T),
            n = n(),
            nARC = sum(ARC))
DS %>% select(ID, CRCLCG, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(CRCLCG) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CRCLCG <= 130, 0, 1)) %>%
  summarise(Q2 = quantile(CRCLCG, probs = c(0.5), na.rm = T),
            Q1 = quantile(CRCLCG, probs = c(0.25), na.rm = T),
            Q3 = quantile(CRCLCG, probs = c(0.75), na.rm = T),
            n = n(),
            nARC = sum(ARC))
x <- DS %>% select(ID, CRCLCG, D, EVID) %>% filter(EVID == 0) %>%
  group_by(ID, D) %>% count(CRCLCG) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CRCLCG <= 130, 0, 1))
# CKDEPI
DS %>% group_by(ID) %>% summarise(meanCKDEPI = mean(CKDEPI), sdCKDEPI = sd(CKDEPI)) # CKDEPI is time-varying!!!
DS %>% select(ID, CKDEPI, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(CKDEPI, probs = c(0.5), na.rm = T),
                                                                                          Q1 = quantile(CKDEPI, probs = c(0.25), na.rm = T),
                                                                                          Q3 = quantile(CKDEPI, probs = c(0.75), na.rm = T),
                                                                                          n = n())
DS %>% select(ID, CKDEPI, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(CKDEPI) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CKDEPI <= 96.5, 0, 1)) %>%
  summarise(Q2 = quantile(CKDEPI, probs = c(0.5), na.rm = T),
            Q1 = quantile(CKDEPI, probs = c(0.25), na.rm = T),
            Q3 = quantile(CKDEPI, probs = c(0.75), na.rm = T),
            n = n(),
            nARC = sum(ARC))
DS %>% select(ID, CKDEPI, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(CKDEPI) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CKDEPI <= 96.5, 0, 1)) %>%
  summarise(Q2 = quantile(CKDEPI, probs = c(0.5), na.rm = T),
            Q1 = quantile(CKDEPI, probs = c(0.25), na.rm = T),
            Q3 = quantile(CKDEPI, probs = c(0.75), na.rm = T),
            n = n(),
            nARC = sum(ARC, na.rm = T))
y <- DS %>% select(ID, CKDEPI, D, EVID) %>% filter(EVID == 0) %>%
  group_by(ID, D) %>% count(CKDEPI) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARC = ifelse(CKDEPI <= 96.5, 0, 1))
# ARCPRED
DS %>% group_by(ID) %>% summarise(meanARCPRED = mean(ARCPRED), sdARCPRED = sd(ARCPRED)) # ARCPRED is time-varying!!!
DS %>% select(ID, ARCPRED, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(ARCPRED, probs = c(0.5), na.rm = T),
                                                                                          Q1 = quantile(ARCPRED, probs = c(0.25), na.rm = T),
                                                                                          Q3 = quantile(ARCPRED, probs = c(0.75), na.rm = T),
                                                                                          n = n())
DS %>% select(ID, ARCPRED, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(ARCPRED) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(ARCPRED, probs = c(0.5), na.rm = T),
            Q1 = quantile(ARCPRED, probs = c(0.25), na.rm = T),
            Q3 = quantile(ARCPRED, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, ARCPRED, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(ARCPRED) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(ARCPRED, probs = c(0.5), na.rm = T),
            Q1 = quantile(ARCPRED, probs = c(0.25), na.rm = T),
            Q3 = quantile(ARCPRED, probs = c(0.75), na.rm = T),
            n = n())
# ALB
DS %>% group_by(ID) %>% summarise(meanALB = mean(ALB), sdALB = sd(ALB)) # ALB is time-varying!!!
DS %>% select(ID, ALB, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(ALB, probs = c(0.5), na.rm = T),
                                                                                       Q1 = quantile(ALB, probs = c(0.25), na.rm = T),
                                                                                       Q3 = quantile(ALB, probs = c(0.75), na.rm = T),
                                                                                       n = n())
DS %>% select(ID, ALB, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(ALB) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(ALB, probs = c(0.5), na.rm = T),
            Q1 = quantile(ALB, probs = c(0.25), na.rm = T),
            Q3 = quantile(ALB, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, ALB, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(ALB) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(ALB, probs = c(0.5), na.rm = T),
            Q1 = quantile(ALB, probs = c(0.25), na.rm = T),
            Q3 = quantile(ALB, probs = c(0.75), na.rm = T),
            n = n())
z <- DS %>% select(ALB) %>% unique()
# BILITOT
DS %>% group_by(ID) %>% summarise(meanBILITOT = mean(BILITOT), sdBILITOT = sd(BILITOT)) # BILITOT is time-varying!!!
DS %>% select(ID, BILITOT, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(BILITOT, probs = c(0.5), na.rm = T),
                                                                                       Q1 = quantile(BILITOT, probs = c(0.25), na.rm = T),
                                                                                       Q3 = quantile(BILITOT, probs = c(0.75), na.rm = T),
                                                                                       n = n())
DS %>% select(ID, BILITOT, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(BILITOT) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(BILITOT, probs = c(0.5), na.rm = T),
            Q1 = quantile(BILITOT, probs = c(0.25), na.rm = T),
            Q3 = quantile(BILITOT, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, BILITOT, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(BILITOT) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(BILITOT, probs = c(0.5), na.rm = T),
            Q1 = quantile(BILITOT, probs = c(0.25), na.rm = T),
            Q3 = quantile(BILITOT, probs = c(0.75), na.rm = T),
            n = n())
# HT
DS %>% group_by(ID) %>% summarise(meanHT = mean(HT), sdHT = sd(HT)) # HT is time-varying!!!
DS %>% select(ID, HT, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(HT, probs = c(0.5), na.rm = T),
                                                                                           Q1 = quantile(HT, probs = c(0.25), na.rm = T),
                                                                                           Q3 = quantile(HT, probs = c(0.75), na.rm = T),
                                                                                           n = n())
DS %>% select(ID, HT, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(HT) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(HT, probs = c(0.5), na.rm = T),
            Q1 = quantile(HT, probs = c(0.25), na.rm = T),
            Q3 = quantile(HT, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, HT, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(HT) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(HT, probs = c(0.5), na.rm = T),
            Q1 = quantile(HT, probs = c(0.25), na.rm = T),
            Q3 = quantile(HT, probs = c(0.75), na.rm = T),
            n = n())
# HB
DS %>% group_by(ID) %>% summarise(meanHB = mean(HB), sdHB = sd(HB)) # HB is time-varying!!!
DS %>% select(ID, HB, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(HB, probs = c(0.5), na.rm = T),
                                                                                           Q1 = quantile(HB, probs = c(0.25), na.rm = T),
                                                                                           Q3 = quantile(HB, probs = c(0.75), na.rm = T),
                                                                                           n = n())
DS %>% select(ID, HB, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(HB) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(HB, probs = c(0.5), na.rm = T),
            Q1 = quantile(HB, probs = c(0.25), na.rm = T),
            Q3 = quantile(HB, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, HB, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(HB) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(HB, probs = c(0.5), na.rm = T),
            Q1 = quantile(HB, probs = c(0.25), na.rm = T),
            Q3 = quantile(HB, probs = c(0.75), na.rm = T),
            n = n())
# SCR
DS %>% group_by(ID) %>% summarise(meanSCR = mean(SCR), sdSCR = sd(SCR)) # SCR is time-varying!!!
DS %>% select(ID, SCR, REL_TIME) %>% filter(REL_TIME == 0)  %>% unique() %>% summarise(Q2 = quantile(SCR, probs = c(0.5), na.rm = T),
                                                                                           Q1 = quantile(SCR, probs = c(0.25), na.rm = T),
                                                                                           Q3 = quantile(SCR, probs = c(0.75), na.rm = T),
                                                                                           n = n())
DS %>% select(ID, SCR, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(SCR) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(SCR, probs = c(0.5), na.rm = T),
            Q1 = quantile(SCR, probs = c(0.25), na.rm = T),
            Q3 = quantile(SCR, probs = c(0.75), na.rm = T),
            n = n())
DS %>% select(ID, SCR, D, EVID) %>% filter(D == "late"  & EVID == 0) %>%
  group_by(ID) %>% count(SCR) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  summarise(Q2 = quantile(SCR, probs = c(0.5), na.rm = T),
            Q1 = quantile(SCR, probs = c(0.25), na.rm = T),
            Q3 = quantile(SCR, probs = c(0.75), na.rm = T),
            n = n())
# Total CEF
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 1 & CMT == 1) %>% unique() %>% dim() # total
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 1 & CMT == 1 & D == "early") %>% unique() %>% dim() # total
counts <- DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 1 & CMT == 1 & D == "early") %>% group_by(ID) %>% summarise(number = n())
quantile(x = counts$number, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 1 & CMT == 1 & D == "late" ) %>% unique() %>% dim() # total
counts <- DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 1 & CMT == 1 & D == "late" ) %>% group_by(ID) %>% summarise(number = n())
quantile(x = counts$number, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
# Unbound CEF
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 2 & CMT == 1) %>% unique() %>% dim() # total
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 2 & CMT == 1 & D == "early") %>% unique() %>% dim() # total
counts <- DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 2 & CMT == 1 & D == "early") %>% group_by(ID) %>% summarise(number = n())
quantile(x = counts$number, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
          DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 2 & CMT == 1 & D == "late" ) %>% unique() %>% dim() # total
counts <- DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter(DVID == 2 & CMT == 1 & D == "late" ) %>% group_by(ID) %>% summarise(number = n())
quantile(x = counts$number, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = F)
DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter((DVID == 1 | DVID == 2) & CMT == 1) %>% unique() %>% dim() # total
a <- DS %>% select(ID, REL_TIME, DV, DVID, CMT, D) %>% filter((DVID == 1 | DVID == 2) & CMT == 1) %>% unique() # total
#####

## Covariate correlations
#####
a <- DS %>% select(ID, SCR, CRCLCG, CKDEPI, ARCPRED, D, EVID) %>% filter(D == "early"  & EVID == 0) %>%
  group_by(ID) %>% count(SCR, CRCLCG, CKDEPI, ARCPRED) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARCcg = ifelse(CRCLCG <= 130, 0, 1),
         ARCckdepi = ifelse(CKDEPI <= 96.5, 0, 1),
         D = "early")
b <- DS %>% select(ID, SCR, CRCLCG, CKDEPI, ARCPRED, D, EVID) %>% filter(D == "late"   & EVID == 0) %>%
  group_by(ID) %>% count(SCR, CRCLCG, CKDEPI, ARCPRED) %>% mutate(prop = prop.table(n)) %>% slice(which.max(n)) %>% filter(n >4) %>%
  ungroup() %>%
  mutate(ARCcg = ifelse(CRCLCG <= 130, 0, 1),
         ARCckdepi = ifelse(CKDEPI <= 96.5, 0, 1),
         D = "late")
ab <- rbind(a, b)

ggplot(data = ab, mapping = aes(x = ARCPRED,
                                # y = ARCcg,
                                y = ARCckdepi,
                                colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("ARC") +
  xlab("Predicted probability of ARC") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))
ggplot(data = ab, mapping = aes(x = CRCLCG,
                                # y = ARCcg,
                                y = ARCckdepi,
                                colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("ARC") +
  xlab("eGFR(CG)") +
  geom_vline(xintercept = 130, linetype = "dashed", colour = "red") +
  theme(legend.position = "none") +
  scale_x_log10(breaks = seq(0, 10000, by = 250)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))
ggplot(data = ab, mapping = aes(x = ARCPRED, y = CRCLCG, colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("eGFR(CG)") +
  xlab("Predicted probability of ARC") +
  theme(legend.position = "none") +
  scale_y_log10(breaks = seq(0, 10000, by = 250)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))
ggplot(data = ab, mapping = aes(x = CKDEPI, y = CRCLCG, colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("eGFR(CG)") +
  xlab("CKD-EPI") +
  theme(legend.position = "none") +
  scale_y_log10(breaks = seq(0, 10000, by = 250)) +
  scale_x_continuous(breaks = seq(0, 1000, by = 10))
ggplot(data = ab, mapping = aes(x = ARCPRED, y = CKDEPI, colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("CKD-EPI") +
  xlab("Predicted probability of ARC") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10000, by = 10)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))
ggplot(data = ab, mapping = aes(x = CKDEPI,
                                # y = ARCcg,
                                y = ARCckdepi,
                                colour = D)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("ARC") +
  xlab("CKD-EPI") +
  geom_vline(xintercept = 96.5, linetype = "dashed", colour = "red") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, 1000, by = 10))

ggplot(data = DS2, mapping = aes(x = CRCL24H, y = CKDEPI)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab(expression(eGFR["CKD-EPI"] * " (mL/min/1.73 " * m^2 * ")")) +
  xlab("Predicted creatinine clearance (mL/min)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10000, by = 10)) +
  scale_x_continuous(breaks = seq(0, 10000, by = 10))
ggplot(data = DS2, mapping = aes(x = CRCL24H, y = ARCPRED)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("Predicted probability of ARC") +
  xlab("Predicted creatinine clearance (mL/min)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10000, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, 10000, by = 10))
ggplot(data = DS2 %>% mutate(ARCPRED = log(ARCPRED/(1-ARCPRED))), mapping = aes(x = CRCL24H, y = ARCPRED)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  ylab("Logit(Predicted probability of ARC)") +
  xlab("Predicted creatinine clearance (mL/min)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(-10000, 10000, by = 1)) +
  scale_x_continuous(breaks = seq(0, 10000, by = 10))

# ROC analysis ARC vs prediction
rocobjCKDEPI <- auc(ab$ARCckdepi, ab$ARCPRED, smooth=FALSE, plot=TRUE)
summary(rocobjCKDEPI)
ci.auc(rocobjCKDEPI)
rocobjCKDEPI <- roc(ab$ARCckdepi, ab$ARCPRED, smooth=FALSE, plot=TRUE)
ci.sp.obj <- ci.sp(ab$ARCckdepi, ab$ARCPRED, sensitivities=seq(0, 1, .01), boot.n=1000)
plot(ci.sp.obj, type="shape", col="grey88")
# plot(rocobjCKDEPI, print.thres="best", print.thres.best.method="youden", add=TRUE)
plot(rocobjCKDEPI, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
round(coords(rocobjCKDEPI, "b", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 3)
round(coords(rocobjCKDEPI, "b", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 3)

rocobjCG <- auc(ab$ARCcg, ab$ARCPRED, smooth=FALSE, plot=TRUE)
summary(rocobjCG)
ci.auc(rocobjCG)
rocobjCG <- roc(ab$ARCcg, ab$ARCPRED, smooth=FALSE, plot=TRUE)
ci.sp.obj <- ci.sp(ab$ARCcg, ab$ARCPRED, sensitivities=seq(0, 1, .01), boot.n=1000)
plot(ci.sp.obj, type="shape", col="grey88")
plot(rocobjCG, print.thres="best", print.thres.best.method="youden", add=TRUE)
plot(rocobjCG, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
round(coords(rocobjCG, "b", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 1)

rocplot <- plot(roc(ab$ARCcg    , ab$ARCPRED), print.auc = T, col = "cyan3"         , print.thres="best", print.thres.best.method="closest.topleft", print.auc.y = .2)
rocplot <- plot(roc(ab$ARCckdepi, ab$ARCPRED), print.auc = T, col = "darkgoldenrod3", print.thres="best", print.thres.best.method="closest.topleft", print.auc.y = .3, add = T)
roc.test(rocobjCKDEPI, rocobjCG, boot.stratified = F)

#####

# Analyse the relationship between CEFu and CEFt
#####
CEFt <- DS %>% ungroup() %>% filter(DVID == 1 & CMT == 1) %>% select(ID, REL_TIME, DV, BM, ALB, ECMO) %>% rename(CEFt = DV)
CEFu <- DS %>% ungroup() %>% filter(DVID == 2 & CMT == 1) %>% select(ID, REL_TIME, DV, BM, ALB, ECMO) %>% rename(CEFu = DV)
CEFtu <- full_join(CEFt, CEFu, by = c("ID", "REL_TIME", "BM", "ALB", "ECMO"))
DS2 <- CEFtu %>% filter(!is.na(CEFu)) %>% mutate(FU = CEFu/CEFt)

#####

## Plots
#####
# Individual concentration-time plots
# n_pages <- ceiling(length(unique(DS$ID))/33)
n_pages <- ceiling(length(unique(DS$ID))/9)
n_pages
for (i in seq_len(n_pages)) {
  ggplot()+
    # geom_point(data = DS[DS$EVID == 0 & DS$DVID == 1 & DS$CMT == 1, ], mapping = aes(x = REL_TIME, y = DV), color = "blue") +
    # geom_line( data = DS[DS$EVID == 0 & DS$DVID == 1 & DS$CMT == 1, ], mapping = aes(x = REL_TIME, y = DV), color = "blue", alpha = I(1/2)) +
    geom_point(data = DS[DS$EVID == 0 & DS$DVID == 2 & DS$CMT == 1, ], mapping = aes(x = REL_TIME, y = DV), color = "green") +
    geom_line( data = DS[DS$EVID == 0 & DS$DVID == 2 & DS$CMT == 1, ], mapping = aes(x = REL_TIME, y = DV), color = "green", alpha = I(1/2)) +
    geom_vline(data = DS[DS$EVID == 1, ], mapping = aes(xintercept = REL_TIME), alpha = I(1/2), color = "red") +
    geom_hline(yintercept = 1, alpha=I(1/2), color="grey", linetype="dashed") +
    geom_hline(yintercept = 4, alpha=I(1/2), color="grey", linetype="dashed") +
    xlab("Time since first dose (hours)") +
    ylab("Ceftriaxone concentration (mg/L)") +
    facet_wrap_paginate(~ID, nrow = 3, ncol = 3, scales = "free", as.table = TRUE, page = i, labeller = label_bquote(cols = .(ID)))+
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_x_continuous(breaks = seq(0,(24*6)+1,by=24)) +
    scale_y_continuous(breaks = seq(0,330,by=50)) +
    ggsave(paste("Individual plots", i, "png", sep = "."), device = "png", path = "/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/Individual plots", units = "cm", width = 35, height = 15, dpi = 600)
}

# Pooled concentration-TAD plot
ggplot() +
  geom_point(data = DS %>% filter(EVID == 0 & CMT == 1 & DVID == 1), mapping = aes(x = TAD, y = DV), colour = "blue", alpha = 0.5) +
  geom_point(data = DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2), mapping = aes(x = TAD, y = DV), colour = "green", alpha = 0.5) +
  xlab("Time since last dose (hours)") +
  ylab("Ceftriaxone concentration (mg/L)") +
  # geom_smooth(data = DS %>% filter(EVID == 0 & CMT == 1 & DVID == 1), mapping = aes(x = TAD, y = DV), colour = "blue" , alpha = 0.5, method = "loess", se = F, size = 0.5) +
  # geom_smooth(data = DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2), mapping = aes(x = TAD, y = DV), colour = "green", alpha = 0.5, method = "loess", se = F, size = 0.5) +
  scale_x_continuous(breaks = seq(0, 25, 2)) +
  # scale_y_continuous(breaks = seq(0, 350, 40)) +
  scale_y_log10(breaks = c(0, 1, 2, 10, 50, 100, 150, 200, 300)) +
  geom_hline(yintercept = 2, alpha=I(1/2), color="grey", linetype="dashed") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 4, linetype = "dashed", colour = "red")
DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2 & TAD > 22) %>% # CEFu at trough
  mutate(THER = ifelse(DV > 2, 1, 0)) %>%
  group_by(D) %>%
  summarise(Therap  = sum(THER),
            Total   = n(),
            Highest = max(DV))
b <- DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2 & TAD > 22) %>% # CEFu at trough
  mutate(THER = ifelse(DV > 2, 1, 0)) %>%
  group_by(D)
DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2 & TAD > 22) %>% # CEFu at trough
  mutate(THER = ifelse(DV > 8, 1, 0)) %>%
  group_by(D) %>%
  summarise(Therap  = sum(THER),
            Total   = n(),
            Highest = max(DV))
c <- DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2 & TAD > 22) %>% # CEFu at trough
  mutate(THER = ifelse(DV > 8, 1, 0)) %>%
  group_by(D)
x <- DS %>% filter(EVID == 0 & CMT == 1 & DVID == 2 & TAD > 18) %>% select(ID, REL_TIME, DV)
  
# # mg/L --> mmol/L
# DS <- DS %>% mutate(CEFu = CEFu/661.6,
#                     CEFt = CEFt/661.6)

# Plot data
# CEFu versus CEFt
plot <- ggplot(data = DS2, mapping = aes(x = CEFu, y = CEFt, color = as.factor(ID)), group = ID) +
  geom_point(alpha = 0.8) +
  geom_line(size = 1/4, alpha = 0.5) +
  geom_smooth(method = loess, se = F, color = "blue", size = 1, linetype = "solid") +
  # scale_x_log10() +
  # scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 9999, by = 20)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 50)) +
  ylab("Total ceftriaxone concentration (mg/L)") +
  xlab("Unbound ceftriaxone concentration (mg/L)") +
  theme(legend.position = "none")
plot
# plot <- ggplot(data = DS2, mapping = aes(x = CEFu, y = CEFt)) +
#   geom_point(color = "blue", alpha = I(1/3)) +
#   geom_smooth(method = loess, se = F, color = "blue", size = 1/2) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   theme(legend.position="none")
# plot
# plot <- ggplot(data = DS2, mapping = aes(x = CEFt, y = CEFu), group = ID) +
#   geom_point(color = "blue", alpha = I(1/3)) +
#   facet_wrap(~ ID) +
#   geom_line(color = "blue", size = 1/4) +
#   theme(legend.position="none") +
#   xlab("CEFu") +
#   ylab("CEFt")
# plot
## FU versus CEFt
plot <- ggplot(data = DS2, mapping = aes(x = CEFt, y = FU*100, color = as.factor(ID)), group = ID) +
  geom_point(alpha = 0.8) +
  geom_line(size = 1/4, alpha = 0.5) +
  geom_smooth(method = loess, se = F, color = "blue", size = 1, linetype = "solid") +
  # scale_x_log10() +
  # scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 99999, by = 50)) +
  scale_y_continuous(limits = c(0,50)) +
  ylab("Unbound fraction (%)") +
  xlab("Total ceftriaxone concentration (mg/L)") +
  theme(legend.position = "none")
plot

# # CEFu versus ALB
# plot <- ggplot(data = DS2, mapping = aes(x = ALB, y = CEFu), group = ID) +
#   geom_point(mapping = aes(color = as.factor(ID)), alpha = 0.5) +
#   geom_line(mapping = aes(color = as.factor(ID)), size = 1/4, alpha = 0.5) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   geom_smooth(method = "loess", se = F, mapping = aes(x = ALB, y = CEFu)) +
#   theme(legend.position = "none")
# plot
# FU versus ALB
plot <- ggplot(data = DS2 %>% mutate(TA = ifelse(CEFu >= 2, 1, 0)), mapping = aes(x = ALB, y = FU*100, color = as.factor(ID)), group = ID) +
  geom_point(alpha = 0.8, mapping = aes(shape = as.factor(TA), size = 4)) +
  geom_line(size = 1/4, alpha = 0.5) +
  geom_smooth(method = loess, se = F, color = "blue", size = 1, linetype = "solid") +
  # scale_x_log10() +
  # scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 99999, by = 5)) +
  # scale_y_continuous(limits = c(0,50)) +
  ylab("Unbound fraction (%)") +
  xlab("Serum albumin concentration (g/L)") +
  theme(legend.position = "none")
plot
# Correlation between ALB and BM
# plot <- ggplot(data = DS2, mapping = aes(x = BM, y = ALB)) +
#   geom_point() +
#   geom_smooth()
# plot
# cor(DS$BM, DS$ALB)
# ALB over time
ggplot(data = DS %>% filter(DVID == 3), mapping = aes(x = REL_TIME, y = DV)) +
  geom_point(mapping = aes(colour = as.factor(ID)), alpha = 0.8) +
  geom_line( mapping = aes(colour = as.factor(ID)), alpha = 0.2) +
  # geom_smooth(method = "loess", se = F) +
  theme(legend.position = "none") +
  facet_wrap(~ID, nrow = 4, ncol = 9, scales = "fixed")
ggplot(data = DS %>% filter(DVID == 3), mapping = aes(x = REL_TIME, y = DV)) +
  geom_point(mapping = aes(colour = as.factor(ID)), alpha = 0.8) +
  geom_line( mapping = aes(colour = as.factor(ID)), alpha = 0.2) +
  geom_smooth(method = "loess", se = F) +
  theme(legend.position = "none")
#####

# Albumin-binding models
#####
      # ## model_1: linear model - IIV on b
      # model_1 <- nlme(CEFt ~ CEFu + CEFu * b,
      #                 data = DS,
      #                 start = c(b = 4), # starting value, can be anything
      #                 fixed = list(b ~ 1),
      #                 random = b ~ 1,
      #                 groups = ~ ID,
      #                 method = "ML")
      # print(model_1) # residual variability close to assay variability?
      # summary(model_1)
      # plot(model_1, resid(., type = "p") ~ fitted(.), abline = 0)
      # plot(model_1, CEFt ~ fitted(.), abline = c(0,1))
      # plot <- ggplot(DS, aes(x = CEFu, y = CEFt)) +
      #   geom_point(size = 2) +
      #   facet_wrap(~ ID) +
      #   geom_line(aes(y = predict(model_1)))
      # plot
      # test <- function(x) {x + x * 3.747257} # model parameters of Bmax and B50. typical patient with ALB=44
      # plot <- ggplot(data = DS) +
      #   geom_point(aes(x = CEFu, y = CEFt), size = 2, color = "blue", alpha = I(1/3)) +
      #   stat_function(fun = test, color = "blue", size = 1/2, linetype = "dashed")
      # plot
      # plot(model_1)
      # resid(model_1)
      # coef(model_1)
      # fitted(model_1)
      # fixed.effects(model_1)
      # random.effects(model_1)
      # 
      # ## model_2: Emax model - IIV on B50
      # median(DS$ALB) # 0.44325 <> Bos et al. 2018 J Antimicrob Chemother: 0.42
      # model_2 <- nlme(CEFt ~ CEFu + CEFu * ( Bmax * ((ALB/30)^h) / (B50+CEFu) ),
      #                 data = DS,
      #                 start = c(Bmax = 0.12, h = 1.3, B50 = 0.0092), # initial values taken from Bos et al. 2018 J Antimicrob Chemother
      #                 fixed = list(Bmax ~ 1, h ~ 1, B50 ~ 1),
      #                 random = B50 ~ 1,
      #                 groups = ~ ID,  # only IIV on B50
      #                 method = "ML") # model_1 and model_2 are not nested models. LRT can only be used to compare nested models. --> use IC! Cf. infra
      # print(model_2)
      # summary(model_2)
      # plot(model_2, resid(., type = "p") ~ fitted(.), abline = 0)
      # plot(model_2, CEFt ~ fitted(.), abline = c(0,1))
      # plot <- ggplot(DS, aes(x = CEFu, y = CEFt)) +
      #   geom_point(size = 2) +
      #   facet_wrap(~ ID) +
      #   geom_line(aes(y = predict(model_2)))
      # plot
      # test <- function(x) {x + x * ( 221.95713/(27.70658+x) )} # model parameters of Bmax and B50. typical patient with ALB=44
      # plot <- ggplot(data = DS) +
      #   geom_point(aes(x = CEFu, y = CEFt), size = 2, color = "blue", alpha = I(1/3)) +
      #   stat_function(fun = test, color = "blue", size = 1/2, linetype = "dashed")
      # plot
      # plot(model_2)
      # resid(model_2)
      # coef(model_2)
      # fitted(model_2)
      # fixed.effects(model_2)
      # 
      ## model_3: Emax model - IIV on Bmax
      median(DS2$ALB) # 29.6
      model_3 <- nlme(CEFt ~ CEFu + CEFu * ( Bmax * ((ALB/30)^h) / (B50+CEFu) ),
                      data = DS2,
                      start = c(Bmax = 239, h = 0.797, B50 = 31), # initial values taken from Bos et al. 2018 J Antimicrob Chemother
                      fixed = list(Bmax ~ 1, h ~ 1, B50 ~ 1),
                      random = Bmax ~ 1,
                      groups = ~ ID,
                      method = "ML") # only IIV on Bmax
      print(model_3)
      summary(model_3) # The hill function is not statistically significantly different from 0, meaning there is no evidence for the albumin effect. In the Bos model it is: cf. Table 2: 1.30 %RSE 41% --> standard error = 0.41*1.30 = 0.315 --> 95%CI lower boundary at 1.3 - 1.96*0.315=0.682 >0)
      # Bmax 0.3603369 0.03211018 39 11.221889  0.0000
      # h    0.3193540 0.27122785 39  1.177438  0.2462
      # B50  0.0419167 0.00669174 39  6.263945  0.0000
      plot(model_3, resid(., type = "p") ~ fitted(.), abline = 0)
      plot(model_3, CEFt ~ fitted(.), abline = c(0,1))
      # plot <- ggplot(DS2, aes(x = CEFu, y = CEFt)) +
      #   geom_point(size = 2, color = "blue", alpha = I(1/3)) +
      #   facet_wrap(~ ID) +
      #   geom_line(aes(y = predict(model_3)), color = "blue", size = 1/2, linetype = "solid")
      # plot
      test <- function(x) {x + x * ( 209.82931/(25.00697+x) )} # model parameters of Bmax and B50. typical patient with ALB=44
      plot <- ggplot(data = DS2, mapping = aes(x = CEFu, y = CEFt, color = as.factor(ID)), group = ID) +
        geom_point(alpha = 0.8) +
        geom_line(size = 1/4, alpha = 0.5) +
        # geom_smooth(method = loess, se = F, color = "blue", size = 1, linetype = "solid") +
        # scale_x_log10() +
        # scale_y_log10() +
        stat_function(fun = test, color = "blue", size = 1, linetype = "solid") +
        scale_x_continuous(breaks = seq(0, 9999, by = 20)) +
        scale_y_continuous(breaks = seq(0, 9999, by = 50)) +
        ylab("Total ceftriaxone concentration (mg/L)") +
        xlab("Unbound ceftriaxone concentration (mg/L)") +
        theme(legend.position = "none")
      plot
      plot(model_3)
      resid(model_3)
      coef(model_3)
      fitted(model_3)
      fixed.effects(model_3)
      # 
      # ## model_4: Emax model - IIV on Bmax and h
      # model_4 <- nlme(CEFt ~ CEFu + CEFu * (Bmax*(1+h*(ALB-30))/(B50+CEFu)),
      #                 data = DS,
      #                 start = c(Bmax = 0.12, h = 1.3, B50 = 0.0092), # initial values taken from Bos et al. 2018 J Antimicrob Chemother
      #                 fixed = list(Bmax ~ 1, h ~ 1, B50 ~ 1),
      #                 random = Bmax ~ 1,
      #                 # random = pdDiag(Bmax + h ~ 1), # only IIV on Bmax and h
      #                 groups = ~ ID)
      # print(model_4)
      # summary(model_4)
      # plot(model_4, resid(., type = "p") ~ fitted(.), abline = 0)
      # plot(model_4, CEFt ~ fitted(.), abline = c(0,1))
      # plot <- ggplot(DS, aes(x = CEFu, y = CEFt)) +
      #   geom_point(size = 2, size = 2, color = "blue", alpha = I(1/3)) +
      #   facet_wrap(~ ID) +
      #   geom_line(aes(y = predict(model_4)), color = "blue", size = 1/2, linetype = "solid")
      # plot
      # test <- function(x) {x + x * (220.46908/(27.65724+x) )} # model parameters of Bmax and B50. typical patient with ALB=44
      # plot <- ggplot(data = DS) +
      #   geom_point(aes(x = CEFu, y = CEFt), size = 2, color = "blue", alpha = I(1/3)) +
      #   stat_function(fun = test, color = "blue", size = 1/2, linetype = "dashed")
      # plot
      # plot(model_4)
      # resid(model_4)
      # coef(model_4)
      # fitted(model_4)
      # fixed.effects(model_4)
      # 
                    # # Export simple dataset for popPK ###
                    # test <- Tab1234 %>% ungroup() %>%
                    #   arrange(ID, REL_TIME) %>%
                    #   # filter(CMT != 2 & DVID != 2) %>%
                    #   select(-DATETIME, -DATE, -TIME, -DAY)
                    # setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
                    # write.csv(test, file="Ceftriaxone.csv", quote=F, na="-99", row.names = F)
                    # 
                    # ### CeftrianxeL2.csv dataset with L2 data item ###
                    # setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
                    # CefPK <- read.csv(file="Ceftriaxone.csv", na="-99", header = T)
                    # CefPK <- CefPK %>% group_by(ID) %>%
                    #   # mutate(L2 = match(REL_TIME, unique(REL_TIME)), L2 = L2-1)
                    #   mutate(L2 = REL_TIME)
                    # write.csv(CefPK, file="CeftriaxoneL2.csv", quote=F, na="-99", row.names = F)
                    # 
                    # ### CeftrianxeL2ALB.csv dataset with ALB in DV data item ###
                    # setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
                    # CefPKL2 <- read.csv(file="CeftriaxoneL2.csv", na="-99", header = T)
                    # CefALBL2 <- CefPKL2 %>% filter(DVID >0) %>%
                    #   mutate(DV = ALB,
                    #          DVID = 3)
                    # CefPKL2ALB <- rbind(CefPKL2, CefALBL2)
                    # CefPKL2ALB <- CefPKL2ALB %>% arrange(ID, REL_TIME, DVID)
                    # write.csv(CefPKL2ALB, file="CeftriaxoneL2ALB_2.csv", quote=F, na="-99", row.names = F)
#####

# Additional data exploration
#####
check <- DS %>% filter(DVID == 2) %>% select(ID, TAD, DV, ALB) %>% mutate(TA = ifelse(DV >= 4, 1, 0))
check
ggplot(data = check, mapping = aes(x = ALB, y = DV)) + geom_point(mapping = aes(colour = as.factor(TA)))
ggplot(data = check, mapping = aes(x = TAD, y = DV)) + geom_point(mapping = aes(colour = as.factor(TA))) + scale_y_log10()
#####

###################################################################################################################################################################################################

### Model diagnostics
###################################################################################################################################################################################################
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
setwd("C:/Users/u0064069/Dropbox/Ceftriaxone/Analyses/PopPK") # dell
# dat <- read.table (file='sdtab005', skip=1, header=T) # base model
# dat <- read.table (file='sdtab005_BMallo', skip=1, header=T) # base model
# dat <- read.table (file='sdtab005_BMallo_CKDEPI', skip=1, header=T) # final model
dat <- read.table (file='sdtab005_BMallo_ARCpred', skip=1, header=T) # final model
# dat <- read.table (file='sdtab005_BMallo_CRCL24Hpred', skip=1, header=T) # final model
dat <- subset (dat, EVID==0)

test <- function(x) {x + x * ((234)/(30.1+x))} # model parameters of Bmax and B50. typical patient with ALB=44
plot <- ggplot(data = DS2, mapping = aes(x = CEFu, y = CEFt, color = as.factor(ID)), group = ID) +
  geom_point(alpha = 0.8) +
  geom_line(size = 1/4, alpha = 0.5) +
  # geom_smooth(method = loess, se = F, color = "blue", size = 1, linetype = "solid") +
  # scale_x_log10() +
  # scale_y_log10() +
  stat_function(fun = test, color = "blue", size = 1, linetype = "solid") +
  scale_x_continuous(breaks = seq(0, 9999, by = 20)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 50)) +
  ylab("Total ceftriaxone concentration (mg/L)") +
  xlab("Unbound ceftriaxone concentration (mg/L)") +
  theme(legend.position = "none")
plot

linear <- function(x) {1+0.0237*(x-30)} # linear model
power <- function(x) {(x/30)^0.7} # power model
ggplot(data.frame(x = c(0, 60)), aes(x)) +
  stat_function(fun = linear, color = "orange", size = 1, linetype = "solid") +
  stat_function(fun = power, color = "blue", size = 1, linetype = "solid") +
  geom_vline(xintercept = 19.6, linetype = "dashed", colour = "grey") +
  geom_vline(xintercept = 45.4, linetype = "dashed", colour = "grey")
  
ggplot (data = dat, aes(x=PRED, y=CWRES)) +
  #choose:
  # geom_point(colour='dodgerblue', shape=1, size=3) +
  geom_point(aes(colour = as.character(DVID)), shape=1, size=3) +
  xlab('Population predicted ceftriaxone concentration (mg/L)') +
  ylab('Conditional weighted residual error') +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dashed") +  
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 9999, by = 25)) +
  scale_y_continuous(limits = c(-4.1, 4.1), breaks = seq(-9999, 9999, by = 1)) +
  geom_smooth(se = F, aes(colour = "red"), method = "loess") +
  theme(legend.position="none") +
  theme(text = element_text(size = 18))

ggplot (data = dat, aes(x=TAD, y=CWRES)) +
  #choose:
  # geom_point(colour='dodgerblue', shape=1, size=3) +
  geom_point(aes(colour = as.character(DVID)), shape=1, size=3) +
  xlab('Time since last dose (hours)') +
  ylab('Conditional weighted residual error') +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -2, linetype = "dashed") +  
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_smooth(se = F, aes(colour = "red"), method = "loess") +
  scale_x_continuous(breaks = seq(0, 9999, by = 2)) +
  scale_y_continuous(limits = c(-4.1, 4.1), breaks = seq(-9999, 9999, by = 1)) +
  theme(legend.position="none") +
  theme(text = element_text(size = 18))

ggplot (data = dat, aes(x=PRED, y=DV)) +
  #choose:
  # geom_point(colour='dodgerblue', shape=1, size=3) +
  geom_point(aes(colour = as.character(DVID)), shape=1, size=3) +
  xlab('Population predicted concentration (mg/L)') +
  ylab('Observed concentration (mg/L)') +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,350), breaks = seq(0, 999, by = 50)) +
  scale_y_continuous(limits = c(0,350), breaks = seq(0, 999, by = 50)) +
  geom_smooth(se = F, aes(colour = "red"), method = "loess") +
  theme(legend.position="none") +
  theme(text = element_text(size = 18))

ggplot (data = dat, aes(x=IPRED, y=DV)) +
  #choose:
  # geom_point(colour='dodgerblue', shape=1, size=3) +
  geom_point(aes(colour = as.character(DVID)), shape=1, size=3) +
  xlab('Individual predicted concentration (mg/L)') +
  ylab('Observed concentration (mg/L)') +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,350), breaks = seq(0, 999, by = 50)) +
  scale_y_continuous(limits = c(0,350), breaks = seq(0, 999, by = 50)) +
  geom_smooth(se = F, aes(colour = "red"), method = "loess") +
  theme(legend.position="none") +
  theme(text = element_text(size = 18))

setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
dat <- read.table (file='sdtab005_BMallo_CKDEPI', skip=1, header=T) # final model
n_pages <- ceiling(length(unique(dat$ID))/6)
n_pages
for (i in seq_len(n_pages)) {
  ggplot()+
    #choose:
    #option 1: only total concentrations
    # geom_point(data = dat[dat$EVID == 0, ], mapping = aes(x = TIME, y = DV), color = "grey") +
    # geom_line( data = dat[dat$EVID == 0, ], mapping = aes(x = TIME, y = DV), color = "grey") +
    # geom_point(data = dat[dat$EVID == 0, ], mapping = aes(x = TIME, y = IPRED), color = "blue", alpha = I(1/2)) +
    # geom_line( data = dat[dat$EVID == 0, ], mapping = aes(x = TIME, y = IPRED), color = "blue", alpha = I(1/2)) +
    #option 2: total and unbound concentrations
    geom_point(data = dat[dat$EVID == 0 & dat$DVID == 1, ], mapping = aes(x = TIME, y = DV), color = "blue") +
    geom_line( data = dat[dat$EVID == 0 & dat$DVID == 1, ], mapping = aes(x = TIME, y = DV), color = "blue") +
    # geom_point(data = dat[dat$EVID == 0 & dat$DVID == 1, ], mapping = aes(x = TIME, y = IPRED), color = "blue", alpha = I(1/2)) +
    # geom_line( data = dat[dat$EVID == 0 & dat$DVID == 1, ], mapping = aes(x = TIME, y = IPRED), color = "blue", alpha = I(1/2)) +
    geom_point(data = dat[dat$EVID == 0 & dat$DVID == 2, ], mapping = aes(x = TIME, y = DV), color = "green") +
    geom_line( data = dat[dat$EVID == 0 & dat$DVID == 2, ], mapping = aes(x = TIME, y = DV), color = "green") +
    # geom_point(data = dat[dat$EVID == 0 & dat$DVID == 2, ], mapping = aes(x = TIME, y = IPRED), color = "green", alpha = I(1/2)) +
    # geom_line( data = dat[dat$EVID == 0 & dat$DVID == 2, ], mapping = aes(x = TIME, y = IPRED), color = "green", alpha = I(1/2)) +
    #end choose
    geom_vline(data = dat[dat$EVID == 1, ], mapping = aes(xintercept = TIME), alpha = I(1/2), color = "red") +
    geom_hline(yintercept = 4, alpha=I(1/2), color="grey", linetype="dashed") +
    xlab("Time since first dose (hours)") +
    ylab("Ceftriaxone concentration (mg/L)") +
    facet_wrap_paginate(~ID, nrow = 2, ncol = 3, scales = "fixed", as.table = TRUE, page = i, labeller = label_bquote(cols = .(ID)))+
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    scale_x_continuous(breaks = seq(0,(24*6)+1,by=12)) +
    # scale_y_continuous(breaks = seq(0,330,by=50)) +
    scale_y_log10() +
    ggsave(paste("Individual plots", i, "png", sep = "."), device = "png", path = "/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/IPREDvsOBS_2", units = "cm", width = 35, height = 15, dpi = 600)
}
###################################################################################################################################################################################################

### External model validation Leegwater
###################################################################################################################################################################################################
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
dat <- read.table (file='run_leegwater_001.data', skip=1, header=T)
dat <- dat %>% filter(EVID == 0) %>% select(ID, DV, IPRED, PRED, CWRES, TIME, OCC, TAD, FREE) %>%
  mutate(rBias = (  (IPRED-DV)     / ((IPRED+DV)/2)    ),
         rRMSE = ( ((IPRED-DV)^2) / (((IPRED+DV)/2)^2) )
  )

# Overall
CefT_Accur   <- dat %>% filter(FREE == 1) %>% select(rBias, rRMSE) %>% # CefT
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100)
CefT_Accur
CefU_Accur <- dat %>% filter(FREE == 2) %>% select(rBias, rRMSE) %>% # CefU
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100)
CefU_Accur

# Per sampling time point
dat <- dat %>% mutate(TIMEPOINT = ifelse(TAD <0.75, "0.5",
                                         ifelse(TAD >=0.75 & TAD <1.75, "1.0",
                                                ifelse(TAD >=1.75 & TAD <3.75, "2.0",
                                                       ifelse(TAD >=3.75 & TAD <8, "6.0",
                                                              ifelse(TAD >10 & TAD <16, "12.0",
                                                                     "24.0"))))))
by_timepoint <- dat %>% group_by(TIMEPOINT)
CefT_Accur   <- by_timepoint %>% filter(FREE == 1) %>% # CefT
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100,
            n = n())
CefT_Accur
CefU_Accur  <- by_timepoint %>% filter(FREE == 2) %>% # CefU
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100,
            n = n())
CefU_Accur

ggplot(data = dat) +
  geom_point(mapping = aes(x = TIME, y = IPRED-DV, colour = as.character(FREE)), shape=1, size=3) +
  geom_smooth(mapping = aes(x = TIME, y = IPRED-DV, colour = "red"), method = "loess", se = F, size = 0.5, linetype = "solid") +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dotted", colour = "darkgrey") +
  ylab("Prediction error") +
  xlab("Time (hours)") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 9999, by = 25)) +
  scale_y_continuous(limits = c(-120, 120), breaks = seq(-120, 120, by = 20)) +
  theme(legend.position="none", text = element_text(size = 18))
ggplot(data = dat) +
  geom_point(mapping = aes(x = TAD, y = IPRED-DV, colour = as.character(FREE)), shape=1, size=3) +
  geom_smooth(mapping = aes(x = TAD, y = IPRED-DV, colour = "red"), method = "loess", se = F, size = 0.5, linetype = "solid") +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dotted", colour = "darkgrey") +
  ylab("Prediction error") +
  xlab("Time since last dose (hours)") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 9999, by = 6)) +
  scale_y_continuous(limits = c(-120, 120), breaks = seq(-120, 120, by = 20)) +
  theme(legend.position="none", text = element_text(size = 18))
###################################################################################################################################################################################################

### Predictive performance assessment
###################################################################################################################################################################################################
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/popPK")
dat <- read.table (file='sdtab005_BMallo_CKDEPI', skip=1, header=T)  # best fit   model
dat <- read.table (file='sdtab005_BMallo_ARCpred', skip=1, header=T) # predictive model
dat <- read.table (file='run_leegwater_001.data', skip=1, header=T) # predictive model
dat <- dat %>% filter(EVID == 0) %>% select(ID, DV, IPRED, PRED, CWRES, TIME, OCC, TAD, FREE) %>% # DVID = FREE (in Leegwater model)
  mutate(rBias = (  (IPRED-DV)     / ((IPRED+DV)/2)    ),
         rRMSE = ( ((IPRED-DV)^2) / (((IPRED+DV)/2)^2) )
  )
dat <- dat %>% rename(DVID = FREE)
# Overall
CefT_Accur   <- dat %>% filter(DVID == 1) %>% select(rBias, rRMSE) %>% # CefT
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100)
CefT_Accur
CefU_Accur <- dat %>% filter(DVID == 2) %>% select(rBias, rRMSE) %>% # CefU
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100)
CefU_Accur

# Per sampling time point
dat <- dat %>% mutate(TIMEPOINT = ifelse(TAD <0.75, "0.5",
                                         ifelse(TAD >=0.75 & TAD <1.75, "1.0",
                                                ifelse(TAD >=1.75 & TAD <3.75, "2.0",
                                                       ifelse(TAD >=3.75 & TAD <8, "6.0",
                                                              ifelse(TAD >10 & TAD <16, "12.0",
                                                                     "24.0"))))))
by_timepoint <- dat %>% group_by(TIMEPOINT)
CefT_Accur   <- by_timepoint %>% filter(DVID == 1) %>% # CefT
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100,
            n = n())
CefT_Accur
CefU_Accur  <- by_timepoint %>% filter(DVID == 2) %>% # CefU
  summarise(avg_rBias = mean(rBias)*100,
            avg_rRMSE = sqrt(mean(rRMSE))*100,
            n = n())
CefU_Accur

ggplot(data = dat) +
  geom_point(mapping = aes(x = TIME, y = IPRED-DV, colour = as.character(DVID)), shape=1, size=3) +
  geom_smooth(mapping = aes(x = TIME, y = IPRED-DV, colour = "red"), method = "loess", se = F, size = 0.5, linetype = "solid") +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dotted", colour = "darkgrey") +
  ylab("Prediction error") +
  xlab("Time (hours)") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 9999, by = 25)) +
  scale_y_continuous(limits = c(-120, 120), breaks = seq(-120, 120, by = 20)) +
  theme(legend.position="none", text = element_text(size = 18))
ggplot(data = dat) +
  geom_point(mapping = aes(x = TAD, y = IPRED-DV, colour = as.character(DVID)), shape=1, size=3) +
  geom_smooth(mapping = aes(x = TAD, y = IPRED-DV, colour = "red"), method = "loess", se = F, size = 0.5, linetype = "solid") +
  geom_hline(mapping = aes(yintercept = 0), linetype = "dotted", colour = "darkgrey") +
  ylab("Prediction error") +
  xlab("Time since last dose (hours)") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 9999, by = 6)) +
  scale_y_continuous(limits = c(-120, 120), breaks = seq(-120, 120, by = 20)) +
  theme(legend.position="none", text = element_text(size = 18))

check <- dat %>% filter(DVID == 2) %>% select(ID, TAD, DV, IPRED) %>% mutate(TA_DV    = ifelse(DV >= 1, 1, 0),
                                                                             TA_IPRED = ifelse(IPRED >= 1, 1, 0),
                                                                             TA_DISCR = ifelse(TA_DV != TA_IPRED, "discrepancy", "ok"))
check <- dat %>% filter(DVID == 2) %>% select(ID, TAD, DV, IPRED) %>% mutate(TA_DV    = ifelse(DV >= 4, 1, 0),
                                                                             TA_IPRED = ifelse(IPRED >= 4, 1, 0),
                                                                             TA_DISCR = ifelse(TA_DV != TA_IPRED, "discrepancy", "ok"))

#### Precision dosing scenarios (make numeric, otherwise nonmem complains)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PPA")

inputfile <- read.csv (file='Ceftriaxone_MIPD.csv', skip=0, sep=",", header=T)
inputfile <- inputfile %>% mutate_if(is.integer,as.numeric)
write.csv(inputfile, file="Ceftriaxone_MIPD.csv", quote = F, na="-99", row.names = F)

### All + MIPD:
## Final model

### CKDEPI ###
# dat <- read.table (file='Ceftriaxone_MIPD1_CKDEPI.csv', skip=1, header=T)   # All samples
# dat <- read.table (file='Ceftriaxone_MIPD2_CKDEPI.csv', skip=1, header=T)   # CEFt predose previous day
# dat <- read.table (file='Ceftriaxone_MIPD3_CKDEPI.csv', skip=1, header=T)   # CEFt predose previous day + CEFt+u 1h postdose today
# dat <- read.table (file='Ceftriaxone_MIPD4_CKDEPI.csv', skip=1, header=T)   # CEFt predose previous day + CEF  u 1h postdose today
# dat <- read.table (file='Ceftriaxone_MIPD5_CKDEPI.csv', skip=1, header=T)   # all samples except CEFu predose today
dat <- read.table (file='Ceftriaxone_MIPD6_CKDEPI.csv', skip=1, header=T)   # No  samples

### PARC ###
# dat <- read.table (file='Ceftriaxone_MIPD1_PARC.csv', skip=1, header=T)   # All samples
# dat <- read.table (file='Ceftriaxone_MIPD2_PARC.csv', skip=1, header=T)   # CEFt predose previous day
# dat <- read.table (file='Ceftriaxone_MIPD3_PARC.csv', skip=1, header=T)   # CEFt predose previous day + CEFt+u 1h postdose today
# dat <- read.table (file='Ceftriaxone_MIPD4_PARC.csv', skip=1, header=T)   # CEFt predose previous day + CEF  u 1h postdose today
# dat <- read.table (file='Ceftriaxone_MIPD5_PARC.csv', skip=1, header=T)   # all samples except CEFu predose today
# dat <- read.table (file='Ceftriaxone_MIPD6_PARC.csv', skip=1, header=T)   # No  samples

dat <- dat %>% filter(DVID == 2 & TAD >19 & CONC != 0) %>% select(ID, CONC, IPRED) %>% mutate(ACCUR = ifelse(IPRED >= 1 & CONC < 1, "overpred",
                                                                                                             ifelse(IPRED < 1 & CONC >= 1, "underpred",
                                                                                                                    ifelse(IPRED >= 1 & CONC >= 1, "correct above",
                                                                                                                           ifelse(IPRED <1 & CONC <1, "correct below",
                                                                                                                                  "ERROR")))),
                                                                                              rBias = (  (IPRED-CONC)     / ((IPRED+CONC)/2)    ),
                                                                                              rRMSE = ( ((IPRED-CONC)^2) / (((IPRED+CONC)/2)^2) ))
dat %>% group_by(ACCUR) %>% summarise(n = n())

dat <- dat %>% filter(DVID == 2 & TAD >19 & CONC != 0) %>% select(ID, CONC, IPRED) %>% mutate(ACCUR = ifelse(IPRED >= 4 & CONC < 4, "overpred",
                                                                                                             ifelse(IPRED < 4 & CONC >= 4, "underpred",
                                                                                                                    ifelse(IPRED >= 4 & CONC >= 4, "correct above",
                                                                                                                           ifelse(IPRED <4 & CONC <4, "correct below",
                                                                                                                                  "ERROR")))),
                                                                                              rBias = (  (IPRED-CONC)     / ((IPRED+CONC)/2)    ),
                                                                                              rRMSE = ( ((IPRED-CONC)^2) / (((IPRED+CONC)/2)^2) ))
dat %>% group_by(ACCUR) %>% summarise(n = n())

dat %>% summarise(avg_rBias = mean(rBias)*100,
                  avg_rRMSE = sqrt(mean(rRMSE))*100)
###################################################################################################################################################################################################

### Simulations
###################################################################################################################################################################################################
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PKsim")

## Make dataset
##########
SIM <- read.csv(file = "Ceftriaxone_sim_blank.csv", na="", header = T, colClasses = "numeric") # 2g q12h vs q24h
# What are the unique sets of WGT, ALB, CKDEPI, and PARC in the original dataset?
COV <- DS %>% ungroup() %>% select(BM, ALB, CKDEPI, ARCPRED) %>% unique()
      ggplot(data = COV, mapping = aes(x = BM, y = ALB)) +
        geom_point() +
        geom_smooth(se = F)
      ggplot(data = COV, mapping = aes(x = BM, y = CKDEPI)) +
        geom_point() +
        scale_y_log10() +
        geom_smooth(se = F)
      ggplot(data = COV, mapping = aes(x = ALB, y = CKDEPI)) +
        geom_point() +
        scale_y_log10() +
        geom_smooth(se = F)
      ggplot(data = COV, mapping = aes(x = BM, y = ARCPRED)) +
        geom_point() +
        scale_y_log10() +
        geom_smooth(se = F)
      ggplot(data = COV, mapping = aes(x = ALB, y = ARCPRED)) +
        geom_point() +
        scale_y_log10() +
        geom_smooth(se = F)
      cor.test(COV$BM, COV$ALB)
      cor.test(COV$BM, COV$CKDEPI)
      cor.test(COV$ALB, COV$CKDEPI)
      cor.test(COV$BM, COV$ARCPRED)
      cor.test(COV$ALB, COV$ARCPRED)
      # not correlated, so sample randomly and independent

test <- DS %>% ungroup() %>% select(ID, BM) %>% unique() %>% mutate(BM = log10(BM))
ggqqplot(test$BM)
shapiro.test(test$BM)
ggplot(data = test, mapping = aes(x = BM)) + geom_density()
# no normality of BM, but log10(BM) is normally distributed!!
test %>% summarise(mean = mean(BM, na.rm = T), sd = sd(BM, na.rm = T), min = min(BM), max = max(BM))

test <- DS %>% ungroup() %>% select(ID, ALB) %>% unique()
ggqqplot(test$ALB)
shapiro.test(test$ALB)
ggplot(data = test, mapping = aes(x = ALB)) + geom_density()
# normality!!
test %>% summarise(mean = mean(ALB, na.rm = T), sd = sd(ALB, na.rm = T), min = min(ALB), max = max(ALB))

test <- DS %>% ungroup() %>% select(ID, CKDEPI) %>% unique()
ggqqplot(test$CKDEPI)
shapiro.test(test$CKDEPI)
ggplot(data = test, mapping = aes(x = CKDEPI)) + geom_density()
# normality!!
test %>% summarise(mean = mean(CKDEPI, na.rm = T), sd = sd(CKDEPI, na.rm = T), min = min(CKDEPI, na.rm = T), max = max(CKDEPI, na.rm = T))

test <- DS %>% ungroup() %>% select(ID, ARCPRED) %>% unique() %>% mutate(ARCPRED = log(ARCPRED/(1-ARCPRED)))
ggqqplot(test$ARCPRED)
shapiro.test(test$ARCPRED)
ggplot(data = test, mapping = aes(x = ARCPRED)) + geom_density()
# no normality of BM, but logit(ARCPRED) is normally distributed!!
test %>% summarise(mean = mean(ARCPRED, na.rm = T), sd = sd(ARCPRED, na.rm = T), min = min(ARCPRED, na.rm = T), max = max(ARCPRED, na.rm = T))

# SIM <- SIM %>% group_by(ID) %>% mutate(BM     = runif(n = 1, min = 48, max = 129),
#                                        ALB    = runif(n = 1, min = 19.6, max = 45.4),
#                                        CKDEPI = runif(n = 1, min = 25, max = 140),
#                                        PARC   = runif(n = 1, min = 0.0010, max = 0.9808))

SIM <- SIM %>% group_by(ID) %>% mutate(BM     = rtruncnorm(n = 1, a = 1.681241, b = 2.11059, mean = 1.831872, sd = 0.1022259),
                                       BM     = 10^BM,
                                       ALB    = rtruncnorm(n = 1, a = 19.6  , b = 45.4  , mean = 30.3  , sd = 5.1),
                                       CKDEPI = rtruncnorm(n = 1, a = 25    , b = 140   , mean = 79    , sd = 28),
                                       PARC   = rtruncnorm(n = 1, a = -6.906755, b = 3.933458, mean = -2.396299, sd = 2.106325),
                                       PARC   = exp(PARC) / (1 + exp(PARC)))

SIM %>% mutate(ARC_CKDEPI = ifelse(CKDEPI >96.5, 1, 0)) %>% summarise(N_ARC_CKDEPI = sum(ARC_CKDEPI),
                                                                      N = n(),
                                                                      PERCENT_ARC_CKDEPI = sum(ARC_CKDEPI)/n())

# do not use normal distribution --> gives negative values!!! # SIM <- SIM %>% group_by(ID) %>% mutate(BM     = rnorm(n = 1, mean = 70.8, sd = 21.1), ALB    = rnorm(n = 1, mean = 30.6, sd = 5.69), CKDEPI = rnorm(n = 1, mean = 122.0, sd = 274))
# Check how the simulated covariate data are distributed
ggplot() +
  geom_density(data = SIM %>% ungroup() %>% select(ID, BM) %>% unique(), mapping = aes(x = BM), colour = "red") +
  geom_density(data = DS  %>% ungroup() %>% select(ID, BM) %>% unique(), mapping = aes (x = BM), colour = "green")
ggplot() +
  geom_density(data = SIM %>% ungroup() %>% select(ID, ALB) %>% unique(), mapping = aes(x = ALB), colour = "red") +
  geom_density(data = DS  %>% ungroup() %>% select(ID, ALB) %>% unique(), mapping = aes (x = ALB), colour = "green")
ggplot() +
  geom_density(data = SIM %>% ungroup() %>% select(ID, CKDEPI) %>% unique(), mapping = aes(x = CKDEPI), colour = "red") +
  geom_density(data = DS  %>% ungroup() %>% select(ID, CKDEPI) %>% unique(), mapping = aes (x = CKDEPI), colour = "green")
ggplot() +
  geom_density(data = SIM %>% ungroup() %>% select(ID, PARC) %>% unique(), mapping = aes(x = PARC), colour = "red") +
  geom_density(data = DS  %>% ungroup() %>% select(ID, ARCPRED) %>% unique(), mapping = aes (x = ARCPRED), colour = "green")

write.csv(SIM, file="Ceftriaxone_sim_input.csv", quote = F, na="-99", row.names = F)

# # 2021 02 02 Yuan predictions
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PKsim")
# SIM2 <- read.csv(file = "Ceftriaxone_sim_input.csv", na="", header = T, colClasses = "numeric") # 2g q12h vs q24h
# COV2 <- DS2 %>% ungroup() %>% select(YUANPREDs) %>% unique()
# DS2 %>% ungroup() %>% select(YUANPREDs) %>% unique() %>% quantile(probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = T)
# SIM2 <- SIM2 %>% group_by(ID) %>% mutate(YUANPRED = runif(n = 1, min = 14.87, max = 169.08))
# write.csv(SIM2, file="Ceftriaxone_sim_input2.csv", quote = F, na="-99", row.names = F)

##########

## Now run simulation in NONMEM

## Load results
SIM_res <- read.csv(file = "Ceftriaxone_PARC_sim_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
SIM_res <- read.csv(file = "Ceftriaxone_CKDEPI_sim_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
# SIM_res <- read.csv(file = "Ceftriaxone_YUAN_sim_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")

## Analyses
##########
analysis <- SIM_res %>% filter(DVID == 2) %>% select(ID, TIME, DV, IPRED, BWT, ALB, CKDEPI, PARC) %>% #, YUANPRED) %>% # Select unbound concentrations!! Use DV (not IPRED)!! # YUANPRED only present in that DS
  # mutate(TA = ifelse(DV >= 1, 1, 0))
  mutate(TA = ifelse(DV >= 4, 1, 0))
analysis %>% group_by(TIME) %>% summarise(PTA = 100*sum(TA)/n())
binom.confint(x = sum(analysis$TA[analysis$TIME == 24]), n = 2000, methods = "agresti-coull", conf.level = 0.95)
binom.confint(x = sum(analysis$TA[analysis$TIME == 12]), n = 2000, methods = "agresti-coull", conf.level = 0.95)

    OBS_res <- DS %>% filter(DVID == 2 & TAD >20) %>% select(ID, DV) %>% mutate(TA = ifelse(DV >= 4, 1, 0))
    OBS_res %>% ungroup() %>% summarise(PTA = 100*sum(TA)/n())
    # PTA = 81.81818%
    # PTA = 48.48485%
    binom.confint(x = sum(OBS_res$TA), n = 33, methods = "agresti-coull", conf.level = 0.95)
    binom.confint(x = 23, n = 29, methods = "agresti-coull", conf.level = 0.95)
    binom.confint(x = 14, n = 29, methods = "agresti-coull", conf.level = 0.95)
    
# choose:
ggplot(data = analysis %>% mutate(BM_cat = ifelse(BWT <64, 0, 1), CUTOFF = ifelse(TIME == 24, 0.057, 0.057)), mapping = aes(x = ALB, y = PARC)) +
# ggplot(data = analysis %>% mutate(BM_cat = ifelse(BWT <64, 0, 1), CUTOFF = ifelse(TIME == 24, 77.5, 77.5)), mapping = aes(x = ALB, y = CKDEPI)) +
# ggplot(data = analysis %>% mutate(BM_cat = ifelse(BWT <64, 0, 1), CUTOFF = ifelse(TIME == 24, 109.55, 109.55)), mapping = aes(x = ALB, y = YUANPRED)) +
      geom_point(mapping = aes(colour = as.factor(TA)), alpha = 0.3, size = 2) + # , shape = as.factor(BM_cat)
  scale_color_manual(values=c("red","green"))+
  scale_shape_manual(values=c(5,4))+
  facet_wrap(~ as.factor(TIME)) +
  labs(x = "Serum albumin concentration (g/L)",
# choose:
       y = expression(P["ARC,d+1"])
       # y = expression(eGFR["CKD-EPI"] * " (mL/min/1.73 " * m^2 * ")")
       # y = "Predicted creatinine clearance (mL/min)"
  ) +
# choose:
  geom_hline(aes(yintercept=CUTOFF), linetype = "dashed", colour = "grey36") +
  # scale_x_continuous(breaks = seq(0, 99, by = 5)) +
# choose:
  # scale_y_continuous(breaks = seq(0, 200, by = 0.1)) +
  # scale_y_continuous(breaks = seq(0, 200, by = 0.1)) +
  scale_y_log10(breaks = c(0, 0.057, 0.25, 0.5, 0.75, 1)) +
  # scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  theme(legend.position = "none") +
  theme(strip.text.x = element_blank()) +
  theme(text = element_text(size=22))

          analysis %>% mutate(CUTOFF = ifelse(CKDEPI > 77.5,  "ARC", "no ARC")) %>% group_by(TIME, CUTOFF, TA) %>% summarise(Numbers = n())
          analysis %>% mutate(CUTOFF = ifelse(PARC   > 0.057, "ARC", "no ARC")) %>% group_by(TIME, CUTOFF, TA) %>% summarise(Numbers = n())
          analysis %>% mutate(CUTOFF = ifelse(YUANPRED > 109.55, "ARC", "no ARC")) %>% group_by(TIME, CUTOFF, TA) %>% summarise(Numbers = n())
          
    # ROC analysis - PARC
    roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 24)
    # roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12)
    ggplot(data = roc, mapping = aes(x = PARC, group=as.factor(TA2), fill=as.factor(TA2)))+
      geom_density(alpha=0.3, linetype = "blank")+
      scale_color_manual(values=c("brown1", "cyan3"))+
      scale_fill_manual(values=c("brown1", "cyan3"))+
      scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
      # scale_x_log10(breaks = c(0, 0.057, 0.25, 0.50, 0.75, 1.0)) +
      # scale_y_continuous(breaks = seq(0, 100, by = 1))+
      geom_vline(xintercept = 0.057, size = 0.3, linetype = "dashed", colour = "grey36") +
      geom_jitter(mapping = aes(y = -0.4, colour = as.factor(TA2)), shape = 16, size = 1, height = 0.4, alpha = 0.5) +
      theme(legend.position = "none") +
      labs(y = "Density",
           x = expression(P["ARC"])
      ) +
      theme(text = element_text(size = 22))
    # rocobj <- auc(roc$TA2, roc$PARC, smooth=FALSE, plot=TRUE)
    summary(rocobj)
    ci.auc(rocobj)
    rocobj <- roc(roc$TA, roc$PARC, smooth=FALSE, plot=TRUE)
    ci.sp.obj <- ci.sp(roc$TA, roc$PARC, sensitivities=seq(0, 1, .01), boot.n=1000)
    plot(ci.sp.obj, type="shape", col="grey88")
    plot(rocobj, print.thres="best", print.thres.best.method="youden", add=TRUE)
    plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 2)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 3)
    round(coords(rocobj, transpose = F, 0.1, ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 3)
    round(coords(rocobj, transpose = F, 0.5, ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 3)
    round(coords(rocobj, transpose = F, 0.494, ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 3)
    # and plot them plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
    roc %>% mutate(PARCcutoff = ifelse(PARC <0.501, "no", "yes")) %>% group_by(TA2, PARCcutoff) %>% summarise(n = n())
    analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12) %>%
      mutate(PARCcutoff = ifelse(PARC <0.501, "no", "yes")) %>% group_by(TA2, PARCcutoff) %>% summarise(n = n())
        
        binom.confint(x = 878, n = 878+103, methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 997, n = 997+22 , methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 303, n = 303+771, methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 619, n = 619+307, methods = "agresti-coull", conf.level = 0.95)
        
        binom.confint(x = 878+619, n = 878+619+103+307, methods = "agresti-coull", conf.level = 0.95)
        
        binom.confint(x = 988 , n = 988+142, methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 852 , n = 852+18 , methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 341 , n = 341+819 , methods = "agresti-coull", conf.level = 0.95)
        binom.confint(x = 603, n = 603+237, methods = "agresti-coull", conf.level = 0.95)

        binom.confint(x = 988+603, n = 988+603+142+237, methods = "agresti-coull", conf.level = 0.95)
        
    # ROC analysis - CKDEPI
    roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) #%>% filter(TIME == 24)
    # roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12)
        ggplot(data = roc, mapping = aes(x = CKDEPI, group=as.factor(TA2), fill=as.factor(TA2)))+
        geom_density(alpha=0.3, linetype = "blank")+
        scale_color_manual(values=c("brown1", "cyan3"))+
        scale_fill_manual(values=c("brown1", "cyan3"))+
        scale_x_continuous(breaks = seq(0, 1000, by = 10)) +
        scale_y_continuous(breaks = seq(0, 1000, by = 0.005))+
        geom_vline(xintercept = 77.5, size = 0.3, linetype = "dashed", colour = "grey36") +
        geom_jitter(mapping = aes(y = -0.001, colour = as.factor(TA2)), shape = 16, size = 1, height = 0.0009, alpha = 0.5) +
        theme(legend.position = "none") +
        labs(y = "Density",
             x = expression(eGFR["CKD-EPI"] * " (mL/min/1.73 " * m^2 * ")")
        ) +
          theme(text = element_text(size = 22))
    rocobj <- auc(roc$TA2, roc$CKDEPI, smooth=FALSE, plot=TRUE)
    summary(rocobj)
    ci.auc(rocobj)
    rocobj <- roc(roc$TA, roc$CKDEPI, smooth=FALSE, plot=TRUE)
    ci.sp.obj <- ci.sp(roc$TA, roc$CKDEPI, sensitivities=seq(0, 1, .01), boot.n=1000)
    plot(ci.sp.obj, type="shape", col="grey88")
    plot(rocobj, print.thres="best", print.thres.best.method="youden", add=TRUE)
    plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
    plot(rocobj, print.thres="best", print.thres.best.method="topleft", add=TRUE)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 2)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 2)
        roc %>% mutate(ARCcutoff = ifelse(CKDEPI <77.46, "no", "yes")) %>% group_by(TA2, ARCcutoff) %>% summarise(n = n())
    # and plot them plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
        analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12) %>%
          mutate(ARCcutoff = ifelse(CKDEPI <92.245, "no", "yes")) %>% group_by(TA2, ARCcutoff) %>% summarise(n = n())
      binom.confint(x = 808 , n = 845 , methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 378 , n = 887 , methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 1147, n = 1155, methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 963 , n = 1113, methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 1771 , n = 1958, methods = "agresti-coull", conf.level = 0.95)
      
      library(binom)
      binom.confint(x = 11 , n = 43 , methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 5 , n = 15 , methods = "agresti-coull", conf.level = 0.95)
      x <- matrix(c(11, 32, 5, 10), ncol = 2)
      chisq.test(x)
      

      # ROC analysis - YUAN
      roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 24)
      # roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12)
      ggplot(data = roc, mapping = aes(x = YUANPRED, group=as.factor(TA2), fill=as.factor(TA2)))+
        geom_density(alpha=0.3, linetype = "blank")+
        scale_color_manual(values=c("brown1", "cyan3"))+
        scale_fill_manual(values=c("brown1", "cyan3"))+
        scale_x_continuous(breaks = seq(0, 1000, by = 10)) +
        scale_y_continuous(breaks = seq(0, 1000, by = 0.005))+
        geom_vline(xintercept = 109.55, size = 0.3, linetype = "dashed", colour = "grey36") +
        geom_jitter(mapping = aes(y = -0.001, colour = as.factor(TA2)), shape = 16, size = 1, height = 0.0009, alpha = 0.5) +
        theme(legend.position = "none") +
        labs(y = "Density",
             x = "Predicted creatinine clearance (mL/min)") +
        theme(text = element_text(size = 22))
      rocobj <- auc(roc$TA2, roc$YUANPRED, smooth=FALSE, plot=TRUE)
      summary(rocobj)
      ci.auc(rocobj)
      rocobj <- roc(roc$TA, roc$YUANPRED, smooth=FALSE, plot=TRUE)
      ci.sp.obj <- ci.sp(roc$TA, roc$YUANPRED, sensitivities=seq(0, 1, .01), boot.n=1000)
      plot(ci.sp.obj, type="shape", col="grey88")
      plot(rocobj, print.thres="best", print.thres.best.method="youden", add=TRUE)
      plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
      plot(rocobj, print.thres="best", print.thres.best.method="topleft", add=TRUE)
      round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 2)
      round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 2)
      roc %>% mutate(ARCcutoff = ifelse(YUANPRED <92.245, "no", "yes")) %>% group_by(TA2, ARCcutoff) %>% summarise(n = n())
      # and plot them plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
      analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 12) %>%
        mutate(ARCcutoff = ifelse(YUANPRED <92.245, "no", "yes")) %>% group_by(TA2, ARCcutoff) %>% summarise(n = n())
      binom.confint(x = 808 , n = 845 , methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 378 , n = 887 , methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 1147, n = 1155, methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 963 , n = 1113, methods = "agresti-coull", conf.level = 0.95)
      binom.confint(x = 1771 , n = 1958, methods = "agresti-coull", conf.level = 0.95)

    # ROC analysis - ALB
    roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 24)
    ggplot(data = roc, mapping = aes(x = ALB, group=as.factor(TA2), fill=as.factor(TA2)))+
      geom_density(alpha=0.3, linetype = "blank")+
      scale_color_manual(values=c("brown1", "cyan3"))+
      scale_fill_manual(values=c("brown1", "cyan3"))+
      scale_x_continuous(breaks = seq(0, 1000, by = 5)) +
      scale_y_continuous(breaks = seq(0, 1000, by = 0.01))+
      # geom_vline(xintercept = 96.7, size = 0.3, linetype = "dashed", colour = "red") +
      geom_jitter(mapping = aes(y = -0.004, colour = as.factor(TA2)), shape = 16, size = 1, height = 0.004, alpha = 0.5) +
      theme(legend.position = "none") +
      labs(y = "Density",
           x = "Serum albumin (g/L)") +
      theme(text = element_text(size = 22))
    rocobj <- auc(roc$TA2, roc$ALB, smooth=FALSE, plot=TRUE)
    summary(rocobj)
    ci.auc(rocobj)
    rocobj <- roc(roc$TA, roc$ALB, smooth=FALSE, plot=TRUE)
    ci.sp.obj <- ci.sp(roc$TA, roc$ALB, sensitivities=seq(0, 1, .01), boot.n=1000)
    plot(ci.sp.obj, type="shape", col="grey88")
    plot(rocobj, print.thres="best", print.thres.best.method="youden", add=TRUE)
    round(coords(rocobj, "b", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 1)
    # and plot them plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)

    # ROC analysis - BM
    roc <- analysis %>% mutate(TA2 = ifelse(TA == 1, "yes", "no")) %>% filter(TIME == 24)
    ggplot(data = roc, mapping = aes(x = BWT, group=as.factor(TA2), fill=as.factor(TA2)))+
      geom_density(alpha=0.3, linetype = "blank")+
      scale_color_manual(values=c("brown1", "cyan3"))+
      scale_fill_manual(values=c("brown1", "cyan3"))+
      scale_x_continuous(breaks = seq(0, 1000, by = 5)) +
      scale_y_continuous(breaks = seq(0, 1000, by = 0.01))+
      # geom_vline(xintercept = 96.7, size = 0.3, linetype = "dashed", colour = "red") +
      geom_jitter(mapping = aes(y = -0.0014, colour = as.factor(TA2)), shape = 16, size = 1, height = 0.0014, alpha = 0.5) +
      theme(legend.position = "none") +
      labs(y = "Density",
           x = "Body weight (kg)") +
      theme(text = element_text(size = 22))
    rocobj <- auc(roc$TA2, roc$BWT, smooth=FALSE, plot=TRUE)
    summary(rocobj)
    ci.auc(rocobj)
    rocobj <- roc(roc$TA, roc$BWT, smooth=FALSE, plot=TRUE)
    ci.sp.obj <- ci.sp(roc$TA, roc$BWT, sensitivities=seq(0, 1, .01), boot.n=1000)
    plot(ci.sp.obj, type="shape", col="grey88")
    plot(rocobj, print.thres="best", print.thres.best.method="youden", add=TRUE)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="youden"), 2)
    round(coords(rocobj, transpose = F, "best", ret=c("t", "sensitivity", "specificity", "ppv", "npv", "tn", "tp", "fn", "fp"), best.method="closest.topleft"), 2)
    # and plot them plot(rocobj, print.thres="best", print.thres.best.method="closest.topleft", add=TRUE)
##########

### Predictive performance assessment when considering simulated values as DV and IPRED as predictions
##########
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PKsim")
# CKDEPI:
SIM_res <- read.csv(file = "Ceftriaxone_CKDEPI_sim_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
SIM_res_PPA <- read.csv(file = "Ceftriaxone_CKDEPI_sim_PPA_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
# PARC:
SIM_res <- read.csv(file = "Ceftriaxone_PARC_sim_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
SIM_res_PPA <- read.csv(file = "Ceftriaxone_PARC_sim_PPA_output.csv", skip = 1, sep = "", na="", header = T, colClasses = "numeric")
# How often are obs (from $SIM) and pred (from $EST MAXEVAL=0) on the same/different sides of the MIC target?
SIM_res     <- SIM_res     %>% filter(DVID == 2) %>% select(ID, TIME, DV, BWT, CKDEPI, ALB) # CEFt + CEFu
SIM_res_PPA <- SIM_res_PPA %>% filter(DVID == 2) %>% select(ID, IPRED) # CEFt
# Check: The DV total CEF concentration should be the same for each patient in both datasets
SIM <- full_join(SIM_res, SIM_res_PPA, by = c("ID"))
SIM %>% mutate(ACCUR = ifelse(IPRED >= 4 & DV < 4, "overpred",
                                       ifelse(IPRED < 4 & DV >= 4, "underpred",
                                              ifelse(IPRED >= 4 & DV >= 4, "correct above",
                                                     ifelse(IPRED <4 & DV <4, "correct below",
                                                            "ERROR"))))) %>%
  group_by(TIME, ACCUR) %>%
  summarise(n = n())
##########

###################################################################################################################################################################################################

#### SOFA modelling

### Data exploration
##########
# Load dataset
# old
##########
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data/20201126_SOFA")
SOFA <- read_excel(path="Databank Ceftriaxone_SOFA_20201125.xlsx", sheet = "Hb, Alb, SOFA per dag", skip = 0, na=c("","NA"))
SOFA <- SOFA %>% mutate(Nummer = ifelse(Nummer == "ECMO 17", 17, ifelse(Nummer == "ECMO 27", 27, Nummer)), Nummer = as.numeric(Nummer))
SOFA1 <- SOFA %>% select(Nummer, Datum1, SOFA1) %>% rename(ID = Nummer, DATE = Datum1, SOFA = SOFA1) %>% mutate(DATE = as.character(DATE), DAY = 1)
SOFA2 <- SOFA %>% select(Nummer, Datum2, SOFA2) %>% rename(ID = Nummer, DATE = Datum2, SOFA = SOFA2) %>% mutate(DATE = as.character(DATE), DAY = 2)
SOFA3 <- SOFA %>% select(Nummer, Datum3, SOFA3) %>% rename(ID = Nummer, DATE = Datum3, SOFA = SOFA3) %>% mutate(DATE = as.character(DATE), DAY = 3)
SOFA4 <- SOFA %>% select(Nummer, Datum4, SOFA4) %>% rename(ID = Nummer, DATE = Datum4, SOFA = SOFA4) %>% mutate(DATE = as.character(DATE), DAY = 4)
SOFA5 <- SOFA %>% select(Nummer, Datum5, SOFA5) %>% rename(ID = Nummer, DATE = Datum5, SOFA = SOFA5) %>% mutate(DATE = as.character(DATE), DAY = 5)
SOFA6 <- SOFA %>% select(Nummer, Datum6, SOFA6...35) %>% rename(ID = Nummer, DATE = Datum6, SOFA = SOFA6...35) %>% mutate(DATE = as.character(DATE), DAY = 6)
SOFA7 <- SOFA %>% select(Nummer, Datum7, SOFA6...40) %>% rename(ID = Nummer, DATE = Datum7, SOFA = SOFA6...40) %>% mutate(DATE = as.character(DATE), DAY = 7)
SOFA_TAB <- rbind(SOFA1, SOFA2, SOFA3, SOFA4, SOFA5, SOFA6, SOFA7) %>% arrange(ID, DAY)
SOFA_TAB <- SOFA_TAB %>% group_by(ID) %>% mutate(PREV = lag(SOFA), PREV = ifelse(is.na(PREV), 99999, PREV),
                                                 DELTA = SOFA-PREV)
##########
# new
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Data/20201215_SOFA")
SOFA <- read_excel(path="Databank Ceftriaxone_SOFA_20201214.xlsx", sheet = "Hb, Alb, SOFA per dag", skip = 0, na=c("","NA"))
SOFA <- SOFA %>% mutate(Nummer = ifelse(Nummer == "ECMO 17", 17, ifelse(Nummer == "ECMO 27", 27, Nummer)), Nummer = as.numeric(Nummer))
SOFA1 <- SOFA %>% select(Nummer, Datum1, SOFA1, SOFA1_resp, SOFA1_ren, SOFA1_CV, SOFA1_hep, SOFA1_stol, SOFA1_neu) %>% rename(ID = Nummer, DATE = Datum1, SOFA = SOFA1, SOFA_res = SOFA1_resp, SOFA_ren = SOFA1_ren, SOFA_car = SOFA1_CV, SOFA_hep = SOFA1_hep, SOFA_coa = SOFA1_stol, SOFA_ner = SOFA1_neu) %>% mutate(DATE = as.character(DATE), DAY = 1)
SOFA2 <- SOFA %>% select(Nummer, Datum2, SOFA2, SOFA2_resp, SOFA2_ren, SOFA2_CV, SOFA2_hep, SOFA2_stol, SOFA2_neu) %>% rename(ID = Nummer, DATE = Datum2, SOFA = SOFA2, SOFA_res = SOFA2_resp, SOFA_ren = SOFA2_ren, SOFA_car = SOFA2_CV, SOFA_hep = SOFA2_hep, SOFA_coa = SOFA2_stol, SOFA_ner = SOFA2_neu) %>% mutate(DATE = as.character(DATE), DAY = 2)
SOFA3 <- SOFA %>% select(Nummer, Datum3, SOFA3, SOFA3_resp, SOFA3_ren, SOFA3_CV, SOFA3_hep, SOFA3_stol, SOFA3_neu) %>% rename(ID = Nummer, DATE = Datum3, SOFA = SOFA3, SOFA_res = SOFA3_resp, SOFA_ren = SOFA3_ren, SOFA_car = SOFA3_CV, SOFA_hep = SOFA3_hep, SOFA_coa = SOFA3_stol, SOFA_ner = SOFA3_neu) %>% mutate(DATE = as.character(DATE), DAY = 3)
SOFA4 <- SOFA %>% select(Nummer, Datum4, SOFA4, SOFA4_resp, SOFA4_ren, SOFA4_CV, SOFA4_hep, SOFA4_stol, SOFA4_neu) %>% rename(ID = Nummer, DATE = Datum4, SOFA = SOFA4, SOFA_res = SOFA4_resp, SOFA_ren = SOFA4_ren, SOFA_car = SOFA4_CV, SOFA_hep = SOFA4_hep, SOFA_coa = SOFA4_stol, SOFA_ner = SOFA4_neu) %>% mutate(DATE = as.character(DATE), DAY = 4)
SOFA5 <- SOFA %>% select(Nummer, Datum5, SOFA5, SOFA5_resp, SOFA5_ren, SOFA5_CV, SOFA5_hep, SOFA5_stol, SOFA5_neu) %>% rename(ID = Nummer, DATE = Datum5, SOFA = SOFA5, SOFA_res = SOFA5_resp, SOFA_ren = SOFA5_ren, SOFA_car = SOFA5_CV, SOFA_hep = SOFA5_hep, SOFA_coa = SOFA5_stol, SOFA_ner = SOFA5_neu) %>% mutate(DATE = as.character(DATE), DAY = 5)
SOFA6 <- SOFA %>% select(Nummer, Datum6, SOFA6...65, SOFA6_resp, SOFA6_ren, SOFA6_CV, SOFA6_hep, SOFA6_stol, SOFA6_neu) %>% rename(ID = Nummer, DATE = Datum6, SOFA = SOFA6...65, SOFA_res = SOFA6_resp, SOFA_ren = SOFA6_ren, SOFA_car = SOFA6_CV, SOFA_hep = SOFA6_hep, SOFA_coa = SOFA6_stol, SOFA_ner = SOFA6_neu) %>% mutate(DATE = as.character(DATE), DAY = 6)
SOFA7 <- SOFA %>% select(Nummer, Datum7, SOFA6...76, SOFA7_resp, SOFA7_ren, SOFA7_CV, SOFA7_hep, SOFA7_stol, SOFA7_neu) %>% rename(ID = Nummer, DATE = Datum7, SOFA = SOFA6...76, SOFA_res = SOFA7_resp, SOFA_ren = SOFA7_ren, SOFA_car = SOFA7_CV, SOFA_hep = SOFA7_hep, SOFA_coa = SOFA7_stol, SOFA_ner = SOFA7_neu) %>% mutate(DATE = as.character(DATE), DAY = 7)
SOFA_TAB <- rbind(SOFA1, SOFA2, SOFA3, SOFA4, SOFA5, SOFA6, SOFA7) %>% arrange(ID, DAY) %>% filter(!is.na(SOFA))
SOFA_TAB <- SOFA_TAB %>% group_by(ID) %>% mutate(PREV     = lag(SOFA)    , PREV     = ifelse(is.na(PREV)    , 99999, PREV),
                                                 PREV_res = lag(SOFA_res), PREV_res = ifelse(is.na(PREV_res), 99999, PREV_res),
                                                 PREV_ren = lag(SOFA_ren), PREV_ren = ifelse(is.na(PREV_ren), 99999, PREV_ren),
                                                 PREV_car = lag(SOFA_car), PREV_car = ifelse(is.na(PREV_car), 99999, PREV_car),
                                                 PREV_hep = lag(SOFA_hep), PREV_hep = ifelse(is.na(PREV_hep), 99999, PREV_hep),
                                                 PREV_coa = lag(SOFA_coa), PREV_coa = ifelse(is.na(PREV_coa), 99999, PREV_coa),
                                                 PREV_ner = lag(SOFA_ner), PREV_ner = ifelse(is.na(PREV_ner), 99999, PREV_ner),
                                                 DELTA     = SOFA    -PREV,
                                                 DELTA_res = SOFA_res-PREV_res,
                                                 DELTA_ren = SOFA_ren-PREV_ren,
                                                 DELTA_car = SOFA_car-PREV_car,
                                                 DELTA_hep = SOFA_hep-PREV_hep,
                                                 DELTA_coa = SOFA_coa-PREV_coa,
                                                 DELTA_ner = SOFA_ner-PREV_ner)

# Plotting
##########
plot <- SOFA_TAB %>% mutate(DAY = as.character(DAY)) %>% filter(!is.na(SOFA))
ggplot(data = plot, mapping = aes(x = SOFA, fill = DAY)) +
  geom_bar(position="dodge", width = 0.7) +
  labs(fill = "Day", x = "SOFA score", y = "Count") +
  scale_x_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 16)) +
  scale_y_continuous(breaks = seq(0, 999, by = 1))
plot <- SOFA_TAB %>% mutate(SOFA = as.character(SOFA), SOFA = ifelse(SOFA == "2", "02",
                                                                     ifelse(SOFA == "3", "03",
                                                                            ifelse(SOFA == "4", "04",
                                                                                   ifelse(SOFA == "5", "05",
                                                                                          ifelse(SOFA == "6", "06",
                                                                                                 ifelse(SOFA == "7", "07",
                                                                                                        ifelse(SOFA == "8", "08",
                                                                                                               ifelse(SOFA == "9", "09",
                                                                                                                      SOFA))))))))) %>% filter(!is.na(SOFA))
ggplot(data = plot, mapping = aes(x = DAY, fill = SOFA)) +
  geom_bar(position="dodge", width = 0.9) +
  labs(fill = "SOFA score", x = "Day", y = "Count") +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  scale_y_continuous(breaks = seq(0, 999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA score") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 16)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  facet_wrap(vars(ID), nrow = 4)
ggplot() +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ren, colour = "goldenrod2"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ren, colour = "goldenrod2"), alpha = 1) +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_car, colour = "brown1"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_car, colour = "brown1"), alpha = 1) +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_res, colour = "dodgerblue1"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_res, colour = "dodgerblue1"), alpha = 1) +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_hep, colour = "orangered3"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_hep, colour = "orangered3"), alpha = 1) +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_coa, colour = "coral4"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_coa, colour = "coral4"), alpha = 1) +
  geom_point(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ner, colour = "grey60"), alpha = 1) +
  geom_line( data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ner, colour = "grey60"), alpha = 1) +
  labs(x = "Day", y = "SOFA score", colour = "SOFA subscore") +
  scale_colour_manual(labels = c("Cardio"   , "Coag", "Respiratory", "Kidneys"     , "Nervous", "Liver"),
                      values = c("goldenrod2", "brown1"               , "dodgerblue1", "orangered3", "coral4"     , "grey60")) +
  theme(legend.position = c(0.8, 0.10), legend.background = element_rect(fill = "white", size = 0.5)) +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  facet_wrap(vars(ID), nrow = 4)

ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA score") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 16)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_hline(yintercept = 16, linetype = "dashed")

ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_res, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA respiratory subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ren, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA kidneys subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_car, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA cardio subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_hep, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA liver subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_coa, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA coag subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")
ggplot(data = SOFA_TAB, mapping = aes(x = DAY, y = SOFA_ner, colour = as.character(ID))) +
  geom_point() +
  geom_line() +
  labs(x = "Day", y = "SOFA nervous subscore") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 999, by = 1), limits = c(0, 4)) +
  scale_x_continuous(breaks = seq(0, 999, by = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 4, linetype = "dashed")

# Overall score evolution over time
data <- SOFA_TAB %>% group_by(DAY, SOFA) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 17)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "SOFA score", fill = "Frequency")

# Subscore evolution over time
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_res) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_res"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_res, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_res, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_res, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_res)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Respiratory SOFA sub-score", fill = "Frequency")
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_ren) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_ren"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_ren, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_ren, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_ren, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_ren)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Kidneys SOFA sub-score", fill = "Frequency")
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_car) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_car"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_car, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_car, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_car, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_car)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Cardio SOFA sub-score", fill = "Frequency")
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_hep) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_hep"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_hep, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_hep, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_hep, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_hep)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Liver SOFA sub-score", fill = "Frequency")
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_coa) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_coa"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_coa, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_coa, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_coa, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_coa)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Coag SOFA sub-score", fill = "Frequency")
data <- SOFA_TAB %>% ungroup() %>% group_by(DAY, SOFA_ner) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
PDplot <- left_join(SOFA_TAB, data, by = c("DAY", "SOFA_ner"))
data1 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(avg = quantile(SOFA_ner, probs = 0.50, na.rm = T))
data2 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(ub = quantile(SOFA_ner, probs = 0.95, na.rm = T))
data3 <- SOFA_TAB %>% ungroup() %>% group_by(DAY) %>% summarise(lb = quantile(SOFA_ner, probs = 0.05, na.rm = T))
PDplot <- left_join(PDplot, data1, by = c("DAY"))
PDplot <- left_join(PDplot, data2, by = c("DAY"))
PDplot <- left_join(PDplot, data3, by = c("DAY"))
ggplot(data = PDplot, mapping = aes(x = DAY, y = SOFA_ner)) +
  geom_raster(mapping = aes(fill = freq)) +
  geom_line(mapping = aes(y = avg), colour = "white", size = 1) +
  geom_line(mapping = aes(y = ub), linetype = "dashed", colour = "white", size = 1) +
  geom_line(mapping = aes(y = lb), linetype = "dashed", colour = "white", size = 1) +
  scale_y_continuous(breaks = seq(0, 999, 1), limits = c(-1, 5)) +
  scale_x_continuous(breaks = seq(0, 999, 1)) +
  labs(x = "Day", y = "Nervous SOFA sub-score", fill = "Frequency")



ggplot(data = SOFA_TAB, mapping = aes(x = DELTA)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA score between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-7, 7)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_ren)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA kidney subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_car)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA cardiovascular system subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_res)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA respiratory system subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_hep)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA liver subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_coa)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA coagulation subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
ggplot(data = SOFA_TAB, mapping = aes(x = DELTA_ner)) + # stat(density)
  geom_bar() +
  labs(x = "Change in SOFA nervous system subscore between two consecutive days", y = "Count")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-999, 999, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 9999, by = 1))
##########

### Collecting CEF exposure metrics (model run)
##########
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
dataset <- read.csv(file="Ceftriaxone.csv", na="-99", header = T, sep = ",", skip = 0)
dataset <- dataset %>% select(ID, REL_TIME, OCC, CMT, EVID, MDV, AMT, RATE, DV, DVID, BM, ALB, CKDEPI, L2) %>% mutate(SELECT = 0)
# lines to add:
D1 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 24 , OCC = 1, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D2 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 48 , OCC = 2, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D3 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 72 , OCC = 3, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D4 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 96 , OCC = 4, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D5 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 120, OCC = 5, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D6 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 144, OCC = 6, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D7 <- dataset %>% filter(REL_TIME == 0) %>% mutate(REL_TIME = 168, OCC = 7, CMT = 1, EVID = 0, MDV = 1, AMT = 0, RATE = 0, DV = 0, DVID = 2, BM = NA, ALB = NA, CKDEPI = NA, L2 = REL_TIME, SELECT = 1)
D1to7 <- rbind(D1, D2, D3, D4, D5, D6, D7)
dataset_expanded <- rbind(dataset, D1to7)
dataset_expanded <- dataset_expanded %>% arrange(ID, REL_TIME, EVID, DVID)
dataset_expanded <- dataset_expanded %>% group_by(ID) %>%
  fill(BM, .direction = "updown") %>%
  fill(ALB, .direction = "updown") %>%
  fill(CKDEPI, .direction = "updown")
write.csv(dataset_expanded, file="Ceftriaxone_2.csv", quote=F, na="-99", row.names = F)

# run model
##########

### NM input datasets
## PO and BI model
##########
# Base
PD_PO <- SOFA_TAB %>% filter(!is.na(SOFA))
PD_PO <- PD_PO %>% select(ID, DAY, SOFA) %>% arrange(ID, DAY)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
write.csv(PD_PO, file="PD_PO.csv", quote=F, na="-99", row.names = F)
# Cov
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1) %>% select(ID, TIME, CAUC, IPRED, CL, V1, BMAX, CKDEPI) %>% mutate(TIME = TIME/24) %>% rename(DAY = TIME) %>% arrange(ID, DAY) %>%
  group_by(ID) %>% mutate(DAUC = CAUC - lag(CAUC), DAUC = ifelse(is.na(DAUC), CAUC, DAUC))
PD_PO_PK <- full_join(PD_PO, PK, by = c("ID", "DAY"))
PD_PO_PK <- PD_PO_PK %>% filter(!is.na(SOFA))
 # 3-day CAUC
  PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
  PK <- PK %>% filter(SELECT == 1 & TIME == 72) %>% select(ID, CAUC) %>% rename(CAUC3D = CAUC)
  PD_PO_PK <- full_join(PD_PO_PK, PK, by = "ID")
  # 7-day CAUC
  PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
  PK <- PK %>% filter(SELECT == 1 & TIME == 168) %>% select(ID, CAUC) %>% rename(CAUC7D = CAUC)
  PD_PO_PK <- full_join(PD_PO_PK, PK, by = "ID")
PD_PO_PK %>% select(ID, CAUC3D) %>% unique() %>% ungroup() %>% summarise(medCAUC3D = median(CAUC3D))
PD_PO_PK %>% select(ID, CAUC7D) %>% unique() %>% ungroup() %>% summarise(medCAUC7D = median(CAUC7D))
PD_PO_PK %>% select(ID, CKDEPI) %>% unique() %>% ungroup() %>% summarise(medCKDEPI = median(CKDEPI))
  PD_PO_PK <- PD_PO_PK %>% mutate(CAUC3DCAT = ifelse(CAUC3D <685, 0, 1)) %>% select(ID, DAY, SOFA, CAUC, IPRED, CL, V1, BMAX, DAUC, CAUC3D, CAUC7D, CAUC3DCAT, CKDEPI)
PD_PO_PK <- PD_PO_PK %>% filter(!is.na(DAY))
# Don't use IPRED because sometimes 1h after infusion = peak
write.csv(PD_PO_PK, file="PD_PO_PK.csv", quote=F, na="-99", row.names = F)
##########
### mCTMM
##########
# Remove NAs
PD_mCTMM <- SOFA_TAB %>% filter(!is.na(SOFA))
PD_mCTMM <- PD_mCTMM %>% select(ID, DAY, SOFA) %>%
  mutate(EVID = 0, AMT = 0, CMT = 1234567890, ORDER =1) # set CMT later to "."

PD_mCTMM_cmp1a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -1, ORDER =2)
PD_mCTMM_cmp1b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  1, ORDER =3)
PD_mCTMM_cmp2a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -2, ORDER =4)
PD_mCTMM_cmp2b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  2, ORDER =5)
PD_mCTMM_cmp3a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -3, ORDER =6)
PD_mCTMM_cmp3b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  3, ORDER =7)
PD_mCTMM_cmp4a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -4, ORDER =8)
PD_mCTMM_cmp4b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  4, ORDER =9)
PD_mCTMM_cmp5a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -5, ORDER =10)
PD_mCTMM_cmp5b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  5, ORDER =11)
PD_mCTMM_cmp6a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -6, ORDER =12)
PD_mCTMM_cmp6b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  6, ORDER =13)
PD_mCTMM_cmp7a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -7, ORDER =14)
PD_mCTMM_cmp7b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  7, ORDER =15)
PD_mCTMM_cmp8a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -8, ORDER =16)
PD_mCTMM_cmp8b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  8, ORDER =17)
PD_mCTMM_cmp9a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -9, ORDER =18)
PD_mCTMM_cmp9b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  9, ORDER =19)
PD_mCTMM_cmp10a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -10, ORDER =20)
PD_mCTMM_cmp10b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  10, ORDER =21)
PD_mCTMM_cmp11a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -11, ORDER =22)
PD_mCTMM_cmp11b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  11, ORDER =23)
PD_mCTMM_cmp12a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -12, ORDER =24)
PD_mCTMM_cmp12b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  12, ORDER =25)
PD_mCTMM_cmp13a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -13, ORDER =26)
PD_mCTMM_cmp13b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  13, ORDER =27)
PD_mCTMM_cmp14a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -14, ORDER =28)
PD_mCTMM_cmp14b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  14, ORDER =29)
# PD_mCTMM_cmp15a <- PD_mCTMM %>% mutate(EVID = 2, CMT = -15, ORDER =30)
# PD_mCTMM_cmp15b <- PD_mCTMM %>% mutate(EVID = 2, CMT =  15, ORDER =31)

PD_mCTMM_reset <- PD_mCTMM %>% mutate(EVID = 1, AMT = 1, CMT = SOFA-1, ORDER =30)

PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp1a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp1b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp2a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp2b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp3a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp3b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp4a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp4b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp5a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp5b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp6a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp6b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp7a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp7b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp8a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp8b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp9a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp9b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp10a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp10b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp11a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp11b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp12a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp12b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp13a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp13b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp14a)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp14b)
# PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp15a)
# PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_cmp15b)
PD_mCTMM <- rbind(PD_mCTMM, PD_mCTMM_reset)

PD_mCTMM <- PD_mCTMM %>% mutate(CMT = ifelse(CMT == 1234567890, ".", CMT))

PD_mCTMM <- PD_mCTMM %>% arrange(ID, DAY, ORDER) %>% select(-ORDER)

setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
write.csv(PD_mCTMM, file="SOFA_mCTMM.csv", quote=F, na="-99", row.names = F)

## mCTMM is in time unit days. PK model was in time unit hours. Cannot just combine these!!
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
# COVS <- read.csv(file="sdtab005_BMallo_CKDEPI", na="-99", header = T, sep = "", skip = 1)
# COVS <- COVS %>% filter(EVID == 1) %>% select(ID, OCC, AMT, CL, V1, V2, Q) %>% unique() %>%
#   rename(DAY = OCC) %>% mutate(SOFA = 0, EVID = 1, CMT = "15") %>%
#   select(ID, DAY, SOFA, EVID, AMT, CMT, CL, V1, V2, Q)
# PD_mCTMM_PK <- PD_mCTMM # %>% mutate(CL = 0, V1 = 0, V2 = 0, Q = 0)
# PD_mCTMM_PK <- rbind(PD_mCTMM_PK, COVS)
# PD_mCTMM_PK <- PD_mCTMM_PK %>% arrange(ID, DAY, AMT, EVID) %>% group_by(ID) %>%
#   fill(CL, .direction = "downup") %>%
#   fill(V1, .direction = "downup")%>%
#   fill(V2, .direction = "downup")%>%
#   fill(Q, .direction = "downup") %>%
#   mutate(MDV = ifelse(CMT == 15, 1, 0))
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
# write.csv(PD_mCTMM_PK, file="SOFA_mCTMM_PK.csv", quote=F, na="-99", row.names = F)

# output:
out <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
out <- out %>% filter(SELECT == 1) %>% select(ID, TIME, CAUC, IPRED, CL, V1, BMAX) %>% mutate(TIME = (TIME/24)) %>% rename(DAY = TIME) %>% arrange(ID, DAY) %>%
  group_by(ID) %>% mutate(DAUC = CAUC - lag(CAUC), DAUC = ifelse(is.na(DAUC), CAUC, DAUC))
PD_mCTMM_PK <- full_join(PD_mCTMM, out, by = c("ID", "DAY"))
PD_mCTMM_PK <- PD_mCTMM_PK %>% filter(!is.na(SOFA)) # %>%
  # mutate(CAUC = ifelse(DAY == 1 & is.na(CAUC), 0, CAUC),
  #                                     dAUC = ifelse(DAY == 1 & is.na(dAUC), 0, dAUC),
  #                                     IPRED = ifelse(DAY == 1 & is.na(IPRED), 0, IPRED),
  #                                     CL = ifelse(DAY == 1 & is.na(CL), 0, CL),
  #                                     V1 = ifelse(DAY == 1 & is.na(V1), 0, V1),
  #                                     BMAX = ifelse(DAY == 1 & is.na(BMAX), 0, BMAX)) %>%
# <<< only relevant when setting TIME = (TIME/24)+1 

# 3-day CAUC
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1 & TIME == 72) %>% select(ID, CAUC) %>% rename(CAUC3D = CAUC)
PD_mCTMM_PK <- full_join(PD_mCTMM_PK, PK, by = "ID")
# 7-day CAUC
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1 & TIME == 168) %>% select(ID, CAUC) %>% rename(CAUC7D = CAUC)
PD_mCTMM_PK <- full_join(PD_mCTMM_PK, PK, by = "ID")
PD_mCTMM_PK %>% select(ID, CAUC3D) %>% unique() %>% ungroup() %>% summarise(medCAUC3D = median(CAUC3D))
PD_mCTMM_PK <- PD_mCTMM_PK %>% mutate(CAUC3DCAT = ifelse(CAUC3D <685, 0, 1))

write.csv(PD_mCTMM_PK, file="PD_mCTMM_PK.csv", quote=F, na="-99", row.names = F)

# Exploratory
PD_mCTMM_PK %>% select(dAUC) %>% unique() %>% ungroup() %>% summarise(mean_dAUC = mean(dAUC))
##########
# As continuous
##########
# PD_PK <- DS %>% filter(EVID == 1) %>% select(ID, REL_TIME, EVID, MDV, AMT, RATE) %>% unique() %>% rename(TIME = REL_TIME) %>% mutate(TIME = round(TIME, digits = 0))
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/PopPK")
# COVS <- read.csv(file="sdtab005_BMallo_CKDEPI", na="-99", header = T, sep = "", skip = 1)
# COVS <- COVS %>% filter(EVID == 1) %>% select(ID, TIME, CL, V1, V2, Q) %>% unique() %>% mutate(TIME = round(TIME, digits = 0))
# PD_PK_COVS <- full_join(PD_PK, COVS, by = c("ID", "TIME"))
# PD_PK_COVS <- PD_PK_COVS %>%  mutate(DV = 0)
# colnames(PD_PK_COVS)
# SOFAS <- SOFA_TAB %>% select(ID, DAY, SOFA) %>% mutate(TIME = (DAY-1)*24, EVID = 0, MDV = 0, AMT = 0, RATE = 0, CL = NA, V1 = NA, V2 = NA, Q = NA) %>% rename(DV = SOFA) %>%
#   select(ID, TIME, EVID, MDV, AMT, RATE, CL, V1, V2, Q, DV) %>% filter(!is.na(DV))
#                                                              #(DAY-1)*24 --> Day 1 = 0, Day 2 = 24
# PD_PK_COVS_SOFA <- rbind(PD_PK_COVS, SOFAS)
# PD_PK_COVS_SOFA <- PD_PK_COVS_SOFA %>% arrange(ID, TIME, EVID) %>% group_by(ID) %>%
#   fill(CL, .direction = "downup") %>%
#   fill(V1, .direction = "downup") %>%
#   fill(V2, .direction = "downup") %>%
#   fill(Q,  .direction = "downup") %>%
#   mutate(CMT = ifelse(EVID == 1, 1, 3))
# setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
# write.csv(PD_PK_COVS_SOFA, file="SOFA_PK.csv", quote=F, na="-99", row.names = F)
##########
# Data exploration
##########
# Relationship between covariates and SOFA scores
DS_b <- SOFA_TAB %>% rename(DATETIME = DATE) %>%
  mutate(DATE = as.character(DATETIME),
         DATE = substr(as.character(DATE), 1, 10)) %>%
  select(ID, DATE, DAY, SOFA) %>%
  filter(!is.na(SOFA))
DS_ab <- full_join(Tab1234567, DS_b, by = c("ID", "DATE"))
DS_ab <- DS_ab %>% filter(!is.na(DATETIME)) %>% rename(SOFA_old = SOFA.x, SOFA_timevarying = SOFA.y,
                                                        DAY_old =  DAY.x,  DAY_timevarying = DAY.y)
# Make plots
CKDEPI_SOFA <- DS_ab %>% ungroup() %>% select(ID, DAY_timevarying, SOFA_timevarying, CKDEPI) %>% filter(!is.na(CKDEPI)) %>% filter(!is.na(SOFA_timevarying)) %>% unique()
ggplot() +
  geom_point(data = CKDEPI_SOFA, mapping = aes(x = SOFA_timevarying, y = CKDEPI, colour = as.character(ID)), alpha = 0.8) +
  geom_line(data = CKDEPI_SOFA, mapping = aes(x = SOFA_timevarying, y = CKDEPI, colour = as.character(ID)), alpha = 0.2) +
  geom_smooth(data = CKDEPI_SOFA, mapping = aes(x = SOFA_timevarying, y = CKDEPI), se = F, method = "loess", colour = "grey", linetype = "dashed", size = 0.5) +
  geom_smooth(data = CKDEPI_SOFA, mapping = aes(x = SOFA_timevarying, y = CKDEPI), se = F, method = "lm") +
  scale_x_continuous(breaks = seq(0,24,1)) +
  scale_y_continuous(breaks = seq(0,1000,10)) +
  stat_cor(method="spearman", type = 'robust') +
  theme(legend.position = "none") +
  labs(x = "SOFA", y = "CKDEPI")
###  LOWER CKDEPI --> HIGHER SOFA SCORES (intrinsic!!! renal function is part of SOFA score) --> HIGHER CEFTRIAXONE LEVELS
##########
### IRT model (FAIL)
##########
PD_IRT <- SOFA_TAB %>% select(ID, DATE, SOFA, SOFA_res, SOFA_ren, SOFA_car, SOFA_hep, SOFA_coa, SOFA_ner) %>%
  gather("SOFA", "SOFA_res", "SOFA_ren", "SOFA_car", "SOFA_hep", "SOFA_coa", "SOFA_ner", key = ITEM, value = DV) %>%
  mutate(DATE = as.POSIXct(DATE)) %>%
  arrange(ID, DATE, ITEM) %>%
  group_by(ID) %>%
  mutate(INTERVAL = DATE - lag(DATE),
         INTERVAL = ifelse(is.na(INTERVAL), 0, INTERVAL),
         INTERVAL = INTERVAL/(60*60),
         TIME = round(cumsum(INTERVAL), digits = 2),
         ITEM = ifelse(ITEM == "SOFA_res", 1,
                       ifelse(ITEM == "SOFA_ren", 2,
                              ifelse(ITEM == "SOFA_car", 3,
                                     ifelse(ITEM == "SOFA_hep", 4,
                                            ifelse(ITEM == "SOFA_coa", 5,
                                                   ifelse(ITEM == "SOFA_ner", 6,
                                                          ifelse(ITEM == "SOFA", 100, "!!"))))))),
         ITEM = as.numeric(ITEM)) %>%
  select(ID, ITEM, TIME, DV) %>%
  mutate(MDV = ifelse(ITEM == 100, 1, 0)) %>%
  arrange(ID, TIME, ITEM)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
write.csv(PD_IRT, file="PD_IRT.csv", quote=F, na="-99", row.names = F)
##########
### mCTMM
##########
PD_mCTMM_res <- SOFA_TAB %>% filter(!is.na(SOFA_res))
PD_mCTMM_res <- PD_mCTMM_res %>% select(ID, DAY, SOFA_res) %>%
  mutate(EVID = 0, AMT = 0, CMT = 1234567890, ORDER =1) # set CMT later to "."

PD_mCTMM_cmp1a <- PD_mCTMM_res %>% mutate(EVID = 2, CMT = -1, ORDER =2)
PD_mCTMM_cmp1b <- PD_mCTMM_res %>% mutate(EVID = 2, CMT =  1, ORDER =3)
PD_mCTMM_cmp2a <- PD_mCTMM_res %>% mutate(EVID = 2, CMT = -2, ORDER =4)
PD_mCTMM_cmp2b <- PD_mCTMM_res %>% mutate(EVID = 2, CMT =  2, ORDER =5)
PD_mCTMM_cmp3a <- PD_mCTMM_res %>% mutate(EVID = 2, CMT = -3, ORDER =6)
PD_mCTMM_cmp3b <- PD_mCTMM_res %>% mutate(EVID = 2, CMT =  3, ORDER =7)
PD_mCTMM_cmp4a <- PD_mCTMM_res %>% mutate(EVID = 2, CMT = -4, ORDER =8)
PD_mCTMM_cmp4b <- PD_mCTMM_res %>% mutate(EVID = 2, CMT =  4, ORDER =9)

PD_mCTMM_res_reset <- PD_mCTMM_res %>% mutate(EVID = 1, AMT = 1, CMT = SOFA_res-1, ORDER =30)

PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp1a)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp1b)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp2a)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp2b)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp3a)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp3b)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp4a)
PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_cmp4b)

PD_mCTMM_res <- rbind(PD_mCTMM_res, PD_mCTMM_res_reset)

PD_mCTMM_res <- PD_mCTMM_res %>% mutate(CMT = ifelse(CMT == 1234567890, ".", CMT))

PD_mCTMM_res <- PD_mCTMM_res %>% arrange(ID, DAY, ORDER) %>% select(-ORDER)

setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
write.csv(PD_mCTMM_res, file="SOFA_mCTMM_res.csv", quote=F, na="-99", row.names = F)
##########



### BI SUBSCORE MODELS
##########
# Base
PD_BI_subs <- SOFA_TAB %>% select(ID, DAY, SOFA, SOFA_res, SOFA_ren, SOFA_car, SOFA_hep, SOFA_coa, SOFA_ner) %>% arrange(ID, DAY)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
write.csv(PD_BI_subs, file="PD_PO_subs.csv", quote=F, na="-99", row.names = F)
# Cov
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1) %>% select(ID, TIME, CAUC, IPRED, CL, V1, BMAX, CKDEPI) %>% mutate(TIME = TIME/24) %>% rename(DAY = TIME) %>% arrange(ID, DAY) %>%
  group_by(ID) %>% mutate(DAUC = CAUC - lag(CAUC), DAUC = ifelse(is.na(DAUC), CAUC, DAUC))
PD_BI_subs_PK <- full_join(PD_BI_subs, PK, by = c("ID", "DAY"))
PD_BI_subs_PK <- PD_BI_subs_PK %>% filter(!is.na(SOFA))
# 3-day CAUC
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1 & TIME == 72) %>% select(ID, CAUC) %>% rename(CAUC3D = CAUC)
PD_BI_subs_PK <- full_join(PD_BI_subs_PK, PK, by = "ID")
# 7-day CAUC
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
PK <- PK %>% filter(SELECT == 1 & TIME == 168) %>% select(ID, CAUC) %>% rename(CAUC7D = CAUC)
PD_BI_subs_PK <- full_join(PD_BI_subs_PK, PK, by = "ID")
PD_BI_subs_PK %>% select(ID, CAUC3D) %>% unique() %>% ungroup() %>% summarise(medCAUC3D = median(CAUC3D))
PD_BI_subs_PK %>% select(ID, CAUC7D) %>% unique() %>% ungroup() %>% summarise(medCAUC7D = median(CAUC7D))
PD_BI_subs_PK %>% select(ID, CKDEPI) %>% unique() %>% ungroup() %>% summarise(medCKDEPI = median(CKDEPI))
PD_BI_subs_PK <- PD_BI_subs_PK %>% mutate(CAUC3DCAT = ifelse(CAUC3D <685, 0, 1)) %>% select(ID, DAY, SOFA, SOFA_res, SOFA_ren, SOFA_car, SOFA_hep, SOFA_coa, SOFA_ner, CAUC, IPRED, CL, V1, BMAX, DAUC, CAUC3D, CAUC7D, CAUC3DCAT, CKDEPI)
PD_BI_subs_PK <- PD_BI_subs_PK %>% filter(!is.na(DAY))
# Don't use IPRED because sometimes 1h after infusion = peak
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
write.csv(PD_BI_subs_PK, file="PD_BI_subs_PK.csv", quote=F, na="-99", row.names = F)


# Plot exposure over time per ID (entire seven days! for all patients separately)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA")
PK <- read.csv(file="sdtab005_BMallo_CKDEPI_expanded.out", na="-99", header = T, sep = "", skip = 1)
ggplot() +
  geom_point(data = PK[PK$EVID == 0 & PK$DVID == 2 & PK$CMT == 1, ], mapping = aes(x = TIME, y = CAUC), color = "green") +
  geom_line( data = PK[PK$EVID == 0 & PK$DVID == 2 & PK$CMT == 1, ], mapping = aes(x = TIME, y = CAUC), color = "green", alpha = I(1/2)) +
  geom_vline(data = PK[PK$EVID == 1, ], mapping = aes(xintercept = TIME), alpha = I(1/2), color = "red") +
  xlab("Time since first dose (hours)") +
  ylab("Cumulative area under the ceftriaxone concentration-time curve (mg*h/L)") +
  facet_wrap(~ID, nrow = 4, scales = "fixed", labeller = label_bquote(cols = .(ID)))+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = seq(0,(24*6)+1, by = 24)) +
  scale_y_continuous(breaks = seq(0,9999, by = 500))



# Model evaluation
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
plot <- read.csv(file="out_BI_res_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_res_003.csv", na="-99", header = T, sep = "", skip = 1)
# plot <- read.csv(file="out_BI_res_008.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ren_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ren_002.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ren_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ren_004.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_car_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_car_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_hep_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_hep_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_coa_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_coa_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ner_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_ner_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_001.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_002.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_003.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_004.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_005.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_006.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_007.csv", na="-99", header = T, sep = "", skip = 1)
plot <- read.csv(file="out_BI_TOTAL_008.csv", na="-99", header = T, sep = "", skip = 1)
ggplot() +
  geom_boxplot(data = plot, mapping = aes(x = TIME, y = PWRES, group = as.factor(TIME)), colour = "grey") +
  geom_point(data = plot, mapping = aes(x = TIME, y = PWRES), shape = 1, colour = "blue", size = 2) +
  geom_line(data = plot, mapping = aes(x = TIME, y = PWRES, group = as.factor(ID), colour = as.factor(ID)), alpha = 0.2) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-99,99,1)) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(-99,99,1)) +
  labs(x = "Time (days)", y = "PWRES") +
  theme(legend.position = "none")


p <- c(1/25, 2/25, 3/25, 4/25, 5/25, 6/25, 7/25, 8/25, 9/25, 10/25, 11/25, 12/25, 13/25, 14/25, 15/25, 16/25, 17/25, 18/25, 19/25, 20/25, 21/25, 22/25, 23/25, 24/25)
p <- qnorm(p, mean=0, sd=1)
DS1 <- rnorm(mean = -0.263+(1-1)*(-0.138), sd = 0.166, n = 100000) # no ceftriaxone, day 0, CAUC7D=924
DS1 <- as.data.frame(DS1)
DS2 <- rnorm(mean = -0.263+(6-1)*(-0.138), sd = 0.166, n = 100000) # no ceftriaxone, day 6, CAUC7D=924
DS2 <- as.data.frame(DS2)
DS3 <- rnorm(mean = -0.263+(1-1)*(-0.138) +0.275*((200-924)/1000), sd = 0.166, n = 100000) # no ceftriaxone, day 1, CAUC7D=200
DS3 <- as.data.frame(DS3)
DS4 <- rnorm(mean = -0.263+(1-1)*(-0.138) +0.275*((2600-924)/1000), sd = 0.166, n = 100000) # no ceftriaxone, day 1, CAUC7D=2600
DS4 <- as.data.frame(DS4)
ggplot() +
  geom_density(kernel = "gaussian", data = DS1, mapping =aes(x = DS1), size = 1.5, colour = "orange", alpha = 0.4) +
  geom_density(kernel = "gaussian", data = DS2, mapping =aes(x = DS2), size = 1.5, colour = "green" , alpha = 0.4) +
  geom_density(kernel = "gaussian", data = DS3, mapping =aes(x = DS3), size = 1.5, colour = "blue"  , alpha = 0.4) +
  geom_density(kernel = "gaussian", data = DS4, mapping =aes(x = DS4), size = 1.5, colour = "violet", alpha = 0.4) +
  geom_vline(xintercept = -0.125, linetype = "dashed", colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_vline(xintercept = p, linetype = "solid", colour = "red", alpha = 0.2) +
  scale_x_continuous(breaks = seq(-999, 999, 0.2), limits = c(-1.8, 1.8)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())





### COMBINED SUBSCORE MODEL

## BI model
##########
colnames(PD_BI_subs_PK)
PD_BI_subs_PK_long <- melt(PD_BI_subs_PK, id.vars=c("ID","DAY","CAUC","IPRED","CL","V1","BMAX","DAUC","CAUC3D","CAUC7D","CAUC3DCAT","CKDEPI"))
PD_BI_subs_PK_long <- PD_BI_subs_PK_long %>% select(ID,DAY,value,variable,CAUC,IPRED,CL,V1,BMAX,DAUC,CAUC3D,CAUC7D,CAUC3DCAT,CKDEPI) %>%
  rename(DV = value, DVID = variable) %>%
  mutate(DVID = ifelse(DVID == "SOFA", 0,
                       ifelse(DVID == "SOFA_res", 1,
                              ifelse(DVID == "SOFA_ren", 2,
                                     ifelse(DVID == "SOFA_car", 3,
                                            ifelse(DVID == "SOFA_hep", 4,
                                                   ifelse(DVID == "SOFA_coa", 5,
                                                          ifelse(DVID == "SOFA_ner", 6, "!!")))))))) %>%
  arrange(ID, DAY, DVID)
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
write.csv(PD_BI_subs_PK_long, file="PD_BI_subs_PK_long.csv", quote=F, na="-99", row.names = F)

# PWRES
setwd("/Users/erwindreesen/Dropbox/Ceftriaxone/Analyses/SOFA/Subscore models")
OUT <- read.csv(file="BI_001_TOTAL_SIM.csv", na="-99", header = T, sep = "", skip = 1)



### Evaluation of exposure metrics
colnames(PD_BI_subs_PK_long)
Eval <- PD_BI_subs_PK_long %>% filter(DVID == 0)
Eval %>% summarise(median_DAUC = round(quantile(DAUC, probs = 0.5)), 0) # 216
Eval %>% select(ID, CAUC3D) %>% unique() %>% summarise(median_CAUC3D = round(quantile(CAUC3D, probs = 0.5)), 0) # 701
Eval %>% select(ID, CAUC7D) %>% unique() %>% summarise(median_CAUC7D = round(quantile(CAUC7D, probs = 0.5)), 0) # 955
Eval %>% select(ID, CAUC) %>% unique() %>% summarise(median_CAUC = round(quantile(CAUC, probs = 0.5)), 0) # 955
Eval %>% select(ID, CL) %>% unique() %>% summarise(median_CL = round(quantile(CL, probs = 0.5)), 0) # 8
# DAUC
ggplot() +
  geom_boxplot(data = Eval, mapping = aes(x = DAY, y = DAUC, group = as.factor(DAY)), colour = "grey") +
  geom_point(data = Eval, mapping = aes(x = DAY, y = DAUC), shape = 1, colour = "blue", size = 2) +
  geom_line(data = Eval, mapping = aes(x = DAY, y = DAUC, group = as.factor(ID), colour = as.factor(ID)), alpha = 0.2) +
  scale_y_continuous(limits = c(-1, 650), breaks = seq(-9999,9999,50)) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(-99,99,1)) +
  labs(x = "Time (days)", y = "24h AUC") +
  theme(legend.position = "none")
# CAUC
ggplot() +
  geom_boxplot(data = Eval, mapping = aes(x = DAY, y = CAUC, group = as.factor(DAY)), colour = "grey") +
  geom_point(data = Eval, mapping = aes(x = DAY, y = CAUC), shape = 1, colour = "blue", size = 2) +
  geom_line(data = Eval, mapping = aes(x = DAY, y = CAUC, group = as.factor(ID), colour = as.factor(ID)), alpha = 0.2) +
  scale_y_continuous(limits = c(-1, 3000), breaks = seq(-9999,9999,500)) +
  scale_x_continuous(limits = c(0, 7), breaks = seq(-99,99,1)) +
  labs(x = "Time (days)", y = "Cumulative AUC") +
  theme(legend.position = "none")

# Correlation 24h-AUC <> SOFA
ggplot(data = Eval, mapping = aes(x = DAUC, y = DV)) +
  geom_point(shape = 1, colour = "blue", size = 2) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(-9999,9999,1)) +
  scale_x_continuous(limits = c(0, 620), breaks = seq(-999,999,50)) +
  geom_smooth(se = F, colour = "orange") +
  labs(x = "24h AUC", y = "SOFA") +
  theme(legend.position = "none")
# Correlation CAUC7D <> SOFA per day
ggplot(data = Eval, mapping = aes(x = CAUC7D, y = DV, group = as.factor(DAY), colour = DAY)) +
  geom_point(shape = 1, size = 2) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(-9999,9999,1)) +
  scale_x_continuous(limits = c(0, 2800), breaks = seq(-9999,9999,250)) +
  geom_smooth(se = F) +
  labs(x = "7-day AUC", y = "SOFA") +
  theme(legend.position = "none")
