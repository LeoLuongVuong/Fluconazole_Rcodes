### My personal access tokens ### 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

#Read dataset
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes")
FlucTotIV <- read.csv("DatabankFluc_Tot_IV_finaal.csv", na.strings = "NA") #This is the new dataset
FlucTotIV_clean <- read.csv("FlucTotIV_clean.csv", na.strings = "NA")

### The work of 260423
### Then I create a new variable: fat free mass (FFM) - calculated from weight, height, sex, whether a patient is an adult or not - using The Boer Formula ###

for (i in 1:length(FlucTotIV_clean$SEX)) {
        if (FlucTotIV_clean$SEX[i]==1) {
                FlucTotIV_clean$FFM[i] <- 0.407*FlucTotIV_clean$BW2[i]+0.267*FlucTotIV_clean$LENGTH[i]*100-19.2
        } else {
                FlucTotIV_clean$FFM[i] <- 0.252*FlucTotIV_clean$BW2[i]+0.473*FlucTotIV_clean$LENGTH[i]*100-48.3
        } 
}

### Then I check the range of FFM
summary(FlucTotIV_clean$FFM) #minimum of 31.97 - maximum of 87.44. So I will simulate the range from 30 to 90 - aka 13 patients


FlucTotIV_clean_imputed <- read.csv("FlucTotIV_clean_imputed.csv", na.strings = "-99")
Fluc_NONMEM_mul_impute <-read.csv("Fluc_NONMEM_mul_impute.csv", na.strings = "NA")
#FlucTotIV_clean[FlucTotIV_clean == "NA"] <- NA
#Fill CRRT of hospital 8 to 0 first
#FlucTotIV$CRRT[FlucTotIV$HOSPITAL == 8] <- 0
write.csv(FlucTotIV,"DatabankFluc_Tot_IV_finaal.csv",quote=F,row.names=FALSE)

#FlucTotIV <- read.csv("FlucTotIV.csv") #this is the old dataset
Fluc_NONMEM_MAX<-read.csv("Fluc_NONMEM_MAX.csv")
Fluc_NONMEM_COM<-read.csv("Fluc_NONMEM_COM.csv",na.strings = "-99")
Fluc_NONMEM_MIN<-read.csv("Fluc_NONMEM_MIN.csv")
Fluc_NONMEM_MED<-read.csv("Fluc_NONMEM_MED.csv")
#FlucTotIVimputed <- read.csv("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazole/Databanken/Totaal/Ruth's Imputed dataset/DatabankFluc_Tot_IV_nonmem_finaal.csv", na = "-99") #we don't need this now

#Summarise dataset
head(FlucTotIV)
str(FlucTotIV)
summary(FlucTotIV)

#Summarise covariates including: body weight (BW), body mass index (BMI), 
#creatinine clearance (CG), estimated glomerular filtration rate (CKDEPI), 
#aspartate transaminase (AST), total billirubin (BILI), gamma-glutamyl transferase (GGT)
#and the predicted probability of having augmented renal clearance on the next day (ARCAlg)

library(table1)
library(dplyr)
library(zoo) #na.locf function
library(rstatix)
library(ggplot2)
#library(qqplot)
library(ggpubr)
library(mice)

#factor the basic variable that we're interested in                                     
#FlucTotIV$HOSPITAL<-             # I may not need this code anymore # #Only need it for data summarizing perhaps #      
        #factor(FlucTotIV$HOSPITAL,levels=c(1,2,3,4,5,6,7,8),
               #labels=c("Study 1","Study 2","Study 3","Study 4","Study 5",
                        #"Study 6","Study 7","Study 8"))
#rename LENGHT column into LENGTH
colnames(FlucTotIV)[colnames(FlucTotIV) == "LENGHT"] ="LENGTH"



#Convert data into the correct type
#FlucTotIV$DV <- as.numeric(as.character(FlucTotIV$DV))#
FlucTotIV$BW <- as.numeric(as.character(FlucTotIV$BW))
FlucTotIV$APACHE <- as.numeric(as.character(FlucTotIV$APACHE))
FlucTotIV$CL24 <- as.numeric(as.character(FlucTotIV$CL24))
FlucTotIV$AFT <- as.numeric(as.character(FlucTotIV$AFT))
FlucTotIV$ALT <- as.numeric(as.character(FlucTotIV$ALT))
FlucTotIV$SOFA <- as.numeric(as.character(FlucTotIV$SOFA))
FlucTotIV$HOSPITAL <- as.numeric(as.character(FlucTotIV$HOSPITAL))
FlucTotIV$BMI <- as.numeric(as.character(FlucTotIV$BMI))
FlucTotIV$BSA <- as.numeric(as.character(FlucTotIV$BSA))
FlucTotIV$LENGTH <- as.numeric(as.character(FlucTotIV$LENGTH))
FlucTotIV$CREAT <- as.numeric(as.character(FlucTotIV$CREAT))
FlucTotIV$CKDEPI <- as.numeric(as.character(FlucTotIV$CKDEPI))
#FlucTotIV$CG <- as.numeric(as.character(FlucTotIV$CG))
FlucTotIV$BILI <- as.numeric(as.character(FlucTotIV$BILI))
FlucTotIV$AST <- as.numeric(as.character(FlucTotIV$AST))
FlucTotIV$GGT <- as.numeric(as.character(FlucTotIV$GGT))
FlucTotIV$FLUID <- as.numeric(as.character(FlucTotIV$FLUID))
FlucTotIV$ADMIN <- as.factor(FlucTotIV$ADMIN) #I added from here ## NONMEM seems not to work with factor variable ##
FlucTotIV$CRRT <- as.factor(FlucTotIV$CRRT)
FlucTotIV$IHD <- as.factor(FlucTotIV$IHD)
FlucTotIV$ARCAlg <- as.factor(FlucTotIV$ARCAlg)



### create a assay column and then export into a new csv data source ###

# Check if my BW is unique for each patient #
library(dplyr)

bwcount<-FlucTotIV %>% 
        group_by(ID) %>% 
        summarise(n_distinct(BW))

# First of all, create a new column called ASSAY
for (i in 1:nrow(FlucTotIV)){
        if (FlucTotIV$HOSPITAL[i]==1){
                FlucTotIV$ASSAY[i]<-"UHPLC-DAD" #ultra-high pressure liquid chromatography and diode array detection
        } else if (FlucTotIV$HOSPITAL[i]==2) {  #after asking Dong Bui Quang: UHPLC-DAD = HPLC, LC-MC=UHPLC-MS/MS=LC-MS/MS
                FlucTotIV$ASSAY[i]<-"LC-MS"
        } else if (FlucTotIV$HOSPITAL[i]==3) {
                FlucTotIV$ASSAY[i]<-"HPLC"
        } else if (FlucTotIV$HOSPITAL[i]==4) {
                FlucTotIV$ASSAY[i]<-"HPLC"
        } else if (FlucTotIV$HOSPITAL[i]==5) {
                FlucTotIV$ASSAY[i]<-"GC" #gas chromatography 
        } else if (FlucTotIV$HOSPITAL[i]==6) {
                FlucTotIV$ASSAY[i]<-"UHPLC-MS/MS"
        } else if (FlucTotIV$HOSPITAL[i]==7) {
                FlucTotIV$ASSAY[i]<-"UHPLC-MS/MS"
        } else {
                FlucTotIV$ASSAY[i]<-"LC-MS/MS"
        }
}
#Somehow it doesn't work, try this way instead
FlucTotIV$ASSAY <- ifelse(FlucTotIV$HOSPITAL == 1, "UHPLC-DAD",
                          ifelse(FlucTotIV$HOSPITAL == 2, "LC-MS",
                                 ifelse(FlucTotIV$HOSPITAL == 3, "HPLC",
                                        ifelse(FlucTotIV$HOSPITAL == 4, "HPLC",
                                               ifelse(FlucTotIV$HOSPITAL == 5, "GC",
                                                      ifelse(FlucTotIV$HOSPITAL == 6, "UHPLC-MS/MS",
                                                             ifelse(FlucTotIV$HOSPITAL == 7, "UHPLC-MS/MS",
                                                                    "LC-MS/MS")))))))

# Then code assay into numbers
for (i in 1:nrow(FlucTotIV)){
        if (FlucTotIV$ASSAY[i]=="UHPLC-DAD"){
                FlucTotIV$ASSAY_CODE[i]<-1 
        } else if (FlucTotIV$ASSAY[i]=="LC-MS") {
                FlucTotIV$ASSAY_CODE[i]<-2
        } else if (FlucTotIV$ASSAY[i]=="HPLC") {
                FlucTotIV$ASSAY_CODE[i]<-1
        } else if (FlucTotIV$ASSAY[i]=="GC") {
                FlucTotIV$ASSAY_CODE[i]<-3
        } else if (FlucTotIV$ASSAY[i]=="UHPLC-MS/MS") {
                FlucTotIV$ASSAY_CODE[i]<-2  
        } else {
                FlucTotIV$ASSAY_CODE[i]<-2
        } 
}

## DV vs TIME plot ##

library(ggplot2)

# Create a new dataset with non-NA values in DV column
FlucTotIV_non_na <- FlucTotIV[!is.na(FlucTotIV$DV), ]

# Plot DV vs TIME
dvtime<-ggplot(FlucTotIV_non_na, aes(x = TIME, y = DV)) + 
        geom_point() +
        labs(x = "Time", y = "DV")

# Save DV vs TIME Plot to PNG file
ggsave("dvtime.png", plot = dvtime, dpi = 300, width = 10, height = 8)

## DV vs TAD plot ##

library(ggplot2)

# Create a new dataset with non-NA values in DV column
FlucTotIV_non_na <- FlucTotIV[!is.na(FlucTotIV$DV), ]

# Plot DV vs TIME
dvtad<-ggplot(FlucTotIV_non_na, aes(x = TAD, y = DV)) + 
        geom_point() +
        labs(x = "TAD", y = "DV")

# Save DV vs TIME Plot to PNG file
ggsave("dvtad.png", plot = dvtad, dpi = 300, width = 10, height = 8)

### We subset data for multiple imputations #our variables of interest are height, body weight, CG so we should subset Age, Gender, Body weight, Height, CG ### 

## Correlation coefficient between BW and LENGTH ## ## It might not be relevant now ##
#cor(FlucTotIV$BW,FlucTotIV$LENGTH) 
#account for the missing value
#cor(FlucTotIV$BW[complete.cases(FlucTotIV$BW) & complete.cases(FlucTotIV$LENGTH)], 
#FlucTotIV$LENGTH[complete.cases(FlucTotIV$BW) & complete.cases(FlucTotIV$LENGTH)])
#another simpler way to do so
#cor(na.omit(FlucTotIV$BW),na.omit(FlucTotIV$LENGTH)) 
#scatter plot using ggplot2
#ggplot(FlucTotIV, aes(LENGTH, BW)) + 
#geom_point() +  scale_x_continuous(limits = c(1.3, 2)) +
#scale_y_continuous(limits = c(50, 150))

## Check if all the variables that need to be imputed are normally distributed ##
#shapiro.test(FlucTotIV$BW)
#shapiro.test(log(FlucTotIV$BW))
#shapiro.test(FlucTotIV$LENGTH)
#shapiro.test(log(FlucTotIV$LENGTH))
#shapiro.test(FlucTotIV$CG)
#shapiro.test(log(FlucTotIV$CG))
#qqpnorm(log(FlucTotIV$CG))

# Another way for doing so #
#test_log <- FlucTotIV %>% ungroup() %>% select(ID, BW) %>% unique() %>% mutate(BW = log10(BW))
#ggqqplot(test_log$BW)
#shapiro.test(test_log$BW)
#ggplot(data = test_log, mapping = aes(x = BW)) + geom_density()
#test_log %>% summarise(mean = mean(BW, na.rm = T), sd = sd(BW, na.rm = T), min = min(BW), max = max(BW))

## They are all not normal so maybe we should utilize pnm method in mice package for imputation ##

### Add a new column called EVENT to Test for IOV ###
for (i in 1:length(FlucTotIV$TIME)){
        if (FlucTotIV$TIME[i]<=100){
                FlucTotIV$EVENT[i]<-1
        } else if (FlucTotIV$TIME[i]<=200 & FlucTotIV$TIME[i]>100){
                FlucTotIV$EVENT[i]<-2
        } else if (FlucTotIV$TIME[i]<=300 & FlucTotIV$TIME[i]>200){
                FlucTotIV$EVENT[i]<-3
        } else {FlucTotIV$EVENT[i]<-4}
}

### Dealing with CKDEPI data, I consider the values of those who have CRRT at least once over their course of treatment to be NA ###

## First of all, I count the number of patients who either have CREAT missing entirely and have CRRT at least once ##

library(dplyr)

# Identify unique patient IDs with all CREAT values missing
missing_creat_patients <- unique(FlucTotIV[is.na(FlucTotIV$CREAT), "ID"])

# Identify unique patient IDs with at least one CRRT value of 1
crrt_patients <- unique(FlucTotIV[FlucTotIV$CRRT == 1, "ID"])

# Combine the two sets of unique patient IDs and count the total number
total_patients <- length(unique(c(missing_creat_patients, crrt_patients)))

# Then I create a new column called CKDEPI_NoD (CKDEPI of those who don't have dialysis) that has the value of CKDEPI column if they don't meet the criteria above and NA otherwise #

FlucTotIV$CKDEPI_NoD <- ifelse(FlucTotIV$ID %in% c(missing_creat_patients, crrt_patients), NA, FlucTotIV$CKDEPI)

### Similarly, I am doing so with CG as well ###

FlucTotIV$CG_NoD <- ifelse(FlucTotIV$ID %in% c(missing_creat_patients, crrt_patients), NA, FlucTotIV$CG)

# Export to a new source data file #
write.csv(FlucTotIV,"FlucTotIV.csv",quote=F,row.names=FALSE,na='')

## We prepare dataset for NONMEM testing, in doing so I create a new dataset called Fluc_NONMEM ##
# First, I drop "CMT" column as it is not a necessary one #
Fluc_NONMEM <- FlucTotIV
Fluc_NONMEM$CMT <-NULL
# Then, I need to change "NA" in DV column into "."
#for (i in 1:length(Fluc_NONMEM$DV)) {
        #if (is.na(Fluc_NONMEM$DV[i]==TRUE)) {
                #Fluc_NONMEM$DV[i] <- "."
        #}
#}

# Then I fill all the NAs in my dataset with "." to be compatible with NONMEM #
#Fluc_NONMEM[is.na(Fluc_NONMEM)] <- "."

# Perhaps I only need to fill DV column with "." #
#Fluc_NONMEM$DV[is.na(Fluc_NONMEM$DV)] <- "."

### CREAT a dataset for complete case analysis ###

Fluc_NONMEM[is.na(Fluc_NONMEM)] <- -99

for (i in 1:length(Fluc_NONMEM$SEX)) {
        if (Fluc_NONMEM$SEX[i]==1) {
                Fluc_NONMEM$FFM[i] <- 0.407*Fluc_NONMEM$BW[i]+0.267*Fluc_NONMEM$LENGTH[i]*100-19.2
        } else {
                Fluc_NONMEM$FFM[i] <- 0.252*Fluc_NONMEM$BW[i]+0.473*Fluc_NONMEM$LENGTH[i]*100-48.3
        } 
}

# Export to a new source data file for complete case analysis #
Fluc_NONMEM$ASSAY<-NULL
#Fluc_NONMEM_MAX$BMI<-Fluc_NONMEM_MAX$BW/(Fluc_NONMEM_MAX$LENGTH)^2 - since already calculated
write.csv(Fluc_NONMEM,"Fluc_NONMEM_COM.csv",quote=F,row.names=FALSE,na='')

######################################################################################## 
#### I should test random SLOPE of Study ID, taking into account each study is different   
########################################################################################

### First I try to impute the missing values of BW, LENGTH, CG, CKDEPI with their maximum values and export it into csv file ### ### Then I need to calculate FFM & BMI accordingly ###

Fluc_NONMEM_MAX<-FlucTotIV
Fluc_NONMEM_MAX$CMT <-NULL
Fluc_NONMEM_MAX[is.na(Fluc_NONMEM_MAX$BW), "BW"] <- max(Fluc_NONMEM_MAX$BW, na.rm = TRUE)
Fluc_NONMEM_MAX[is.na(Fluc_NONMEM_MAX$LENGTH), "LENGTH"] <- max(Fluc_NONMEM_MAX$LENGTH, na.rm = TRUE)
Fluc_NONMEM_MAX[is.na(Fluc_NONMEM_MAX$CG_NoD), "CG_NoD"] <- max(Fluc_NONMEM_MAX$CG_NoD, na.rm = TRUE)
Fluc_NONMEM_MAX[is.na(Fluc_NONMEM_MAX$CKDEPI_NoD), "CKDEPI_NoD"] <- max(Fluc_NONMEM_MAX$CKDEPI_NoD, na.rm = TRUE)   
Fluc_NONMEM_MAX[is.na(Fluc_NONMEM_MAX)] <- -99

### Then I create a new variable: fat free mass (FFM) (one of my covariates) - calculated from weight, height, sex, whether a patient is an adult or not - using The Boer Formula ###

for (i in 1:length(Fluc_NONMEM_MAX$SEX)) {
        if (Fluc_NONMEM_MAX$SEX[i]==1) {
                Fluc_NONMEM_MAX$FFM[i] <- 0.407*Fluc_NONMEM_MAX$BW[i]+0.267*Fluc_NONMEM_MAX$LENGTH[i]*100-19.2
        } else {
                Fluc_NONMEM_MAX$FFM[i] <- 0.252*Fluc_NONMEM_MAX$BW[i]+0.473*Fluc_NONMEM_MAX$LENGTH[i]*100-48.3
        } 
}

### I also calculate BMI for those for are missing ###

Fluc_NONMEM_MAX$BMI<-Fluc_NONMEM_MAX$BW/(Fluc_NONMEM_MAX$LENGTH)^2


### Finally, I export it into a dataset for running NONMEM ###
Fluc_NONMEM_MAX$ASSAY<-NULL
write.csv(Fluc_NONMEM_MAX,"Fluc_NONMEM_MAX.csv",quote=F,row.names=FALSE)

### Similarly, I try to impute the missing values of BW, LENGTH, CG, CKDEPI with their minimum values and export it into csv file ### ### Then I need to calculate FFM & BMI accordingly ###

Fluc_NONMEM_MIN<-FlucTotIV
Fluc_NONMEM_MIN$CMT <-NULL
Fluc_NONMEM_MIN[is.na(Fluc_NONMEM_MIN$BW), "BW"] <- min(Fluc_NONMEM_MIN$BW, na.rm = TRUE)
Fluc_NONMEM_MIN[is.na(Fluc_NONMEM_MIN$LENGTH), "LENGTH"] <- min(Fluc_NONMEM_MIN$LENGTH, na.rm = TRUE)
Fluc_NONMEM_MIN[is.na(Fluc_NONMEM_MIN$CG_NoD), "CG_NoD"] <- min(Fluc_NONMEM_MIN$CG_NoD, na.rm = TRUE)
Fluc_NONMEM_MIN[is.na(Fluc_NONMEM_MIN$CKDEPI_NoD), "CKDEPI_NoD"] <- min(Fluc_NONMEM_MIN$CKDEPI_NoD, na.rm = TRUE)   
Fluc_NONMEM_MIN[is.na(Fluc_NONMEM_MIN)] <- -99

### Then I create a new variable: fat free mass (FFM) (one of my covariates) - calculated from weight, height, sex, whether a patient is an adult or not - using The Boer Formula ###

for (i in 1:length(Fluc_NONMEM_MIN$SEX)) {
        if (Fluc_NONMEM_MIN$SEX[i]==1) {
                Fluc_NONMEM_MIN$FFM[i] <- 0.407*Fluc_NONMEM_MIN$BW[i]+0.267*Fluc_NONMEM_MIN$LENGTH[i]*100-19.2
        } else {
                Fluc_NONMEM_MIN$FFM[i] <- 0.252*Fluc_NONMEM_MIN$BW[i]+0.473*Fluc_NONMEM_MIN$LENGTH[i]*100-48.3
        } 
}

### I also calculate BMI for those for are missing ###

Fluc_NONMEM_MIN$BMI<-Fluc_NONMEM_MIN$BW/(Fluc_NONMEM_MIN$LENGTH)^2


### Finally, I export it into a dataset for running NONMEM ###
Fluc_NONMEM_MIN$ASSAY<-NULL
write.csv(Fluc_NONMEM_MIN,"Fluc_NONMEM_MIN.csv",quote=F,row.names=FALSE)

### Also I try median imputation ###

Fluc_NONMEM_MED<-FlucTotIV
Fluc_NONMEM_MED$CMT <-NULL
Fluc_NONMEM_MED[is.na(Fluc_NONMEM_MED$BW), "BW"] <- median(Fluc_NONMEM_MED$BW, na.rm = TRUE)
Fluc_NONMEM_MED[is.na(Fluc_NONMEM_MED$LENGTH), "LENGTH"] <- median(Fluc_NONMEM_MED$LENGTH, na.rm = TRUE)
Fluc_NONMEM_MED[is.na(Fluc_NONMEM_MED$CG_NoD), "CG_NoD"] <- median(Fluc_NONMEM_MED$CG_NoD, na.rm = TRUE)
Fluc_NONMEM_MED[is.na(Fluc_NONMEM_MED$CKDEPI_NoD), "CKDEPI_NoD"] <- median(Fluc_NONMEM_MED$CKDEPI_NoD, na.rm = TRUE)   
Fluc_NONMEM_MED[is.na(Fluc_NONMEM_MED)] <- -99

### Then I create a new variable: fat free mass (FFM) (one of my covariates) - calculated from weight, height, sex, whether a patient is an adult or not - using The Boer Formula ###

for (i in 1:length(Fluc_NONMEM_MED$SEX)) {
        if (Fluc_NONMEM_MED$SEX[i]==1) {
                Fluc_NONMEM_MED$FFM[i] <- 0.407*Fluc_NONMEM_MED$BW[i]+0.267*Fluc_NONMEM_MED$LENGTH[i]*100-19.2
        } else {
                Fluc_NONMEM_MED$FFM[i] <- 0.252*Fluc_NONMEM_MED$BW[i]+0.473*Fluc_NONMEM_MED$LENGTH[i]*100-48.3
        } 
}

### I also calculate BMI for those for are missing ###

Fluc_NONMEM_MED$BMI<-Fluc_NONMEM_MED$BW/(Fluc_NONMEM_MED$LENGTH)^2

### Finally, I export it into a dataset for running NONMEM ###
Fluc_NONMEM_MED$ASSAY<-NULL

## In the next step, I test BW as a covariate for CL (r/d) & V1. As BW is not normally distributed, I need to obtain median value for modeling ##

# First, maximum datastet
median(Fluc_NONMEM_MAX$BW) #equals 80
# Similarly, I do so for BMI #
median(Fluc_NONMEM_MAX$BMI) #equals 27.3
# Likewise, for CKDEPI_NoD #
median(Fluc_NONMEM_MAX$CKDEPI_NoD) #equals 101
# And for FFM #
median(Fluc_NONMEM_MAX$FFM) #equals 58.763
# Likewise, for CG_NoD #
median(Fluc_NONMEM_MAX$CG_NoD) #equals 126.87

# Second, Median dataset

median(Fluc_NONMEM_MED$BW) #equals 80
# Similarly, I do so for BMI #
median(Fluc_NONMEM_MED$BMI) #equals 26.42
# Likewise, for CKDEPI_NoD #
median(Fluc_NONMEM_MED$CKDEPI_NoD) #equals 91.55
# And for FFM #
median(Fluc_NONMEM_MED$FFM) #equals 58.05
# Likewise, for CG_NoD #
median(Fluc_NONMEM_MED$CG_NoD) #equals 107.59

## Similarly, for the Fluc_NONNMEM_MIN dataset ##

median(Fluc_NONMEM_MIN$BW) #equals 80
# Similarly, I do so for BMI #
median(Fluc_NONMEM_MIN$BMI) #equals 26.23
# Likewise, for CKDEPI_NoD #
median(Fluc_NONMEM_MIN$CKDEPI_NoD) #equals 75.78
# And for FFM #
median(Fluc_NONMEM_MIN$FFM) #equals 57.68
# Likewise, for CG_NoD #
median(Fluc_NONMEM_MIN$CG_NoD) #equals 84.55

## Finally, for the Fluc_NONNMEM_COM dataset ##

median(Fluc_NONMEM_COM$BW[Fluc_NONMEM_COM$BW != -99]) #equals 80

#I update information on PAGE's abstract, based on Isabel's comments!
summary(Fluc_NONMEM_COM$FFM[Fluc_NONMEM_COM$FFM > 0 & Fluc_NONMEM_COM$DV !="."]) #updating according to Isabel - 58.5[50.6-65.1] - calculating on the sampling days only!
summary(Fluc_NONMEM_COM$CKDEPI_NoD[Fluc_NONMEM_COM$CKDEPI_NoD > 0 & !is.na(Fluc_NONMEM_COM$CKDEPI_NoD) & Fluc_NONMEM_COM$DV !="."]) #updating according to Isabel - 91.5[62.6-106.8] - calculating on the sampling days only!

# Similarly, I do so for BMI #
median(Fluc_NONMEM_COM$BMI[Fluc_NONMEM_COM$BMI != -99]) #equals 26.4
# Likewise, for CKDEPI_NoD #
median(Fluc_NONMEM_COM$CKDEPI_NoD[Fluc_NONMEM_COM$CKDEPI_NoD != -99]) #equals 91.55
# And for FFM #
median(Fluc_NONMEM_COM$FFM[Fluc_NONMEM_COM$FFM > 0]) #equals 58.15
# Likewise, for CG_NoD #
median(Fluc_NONMEM_COM$CG_NoD[Fluc_NONMEM_COM$CG_NoD > 0]) #equals 107.59


#library(writexl)
#write_xlsx(Fluc_NONMEM_MAX,"Fluc_NONMEM_MAX.xlsx")



#################################################################### 
####Here I make plot of CREAT over TIME for each individual patient 
####################################################################

# Create a vector of ID's for the patients who have CRRT all the time when DV != "." #I will hide this part for a moment
# Get unique IDs with CRRT == 1 only
#FlucTotIV_subset <- FlucTotIV[FlucTotIV$DV != ".",]
#crrt_1_ids <- unique(FlucTotIV_subset$ID[FlucTotIV_subset$CRRT == 1 & !FlucTotIV_subset$ID %in% FlucTotIV_subset$ID[FlucTotIV_subset$CRRT == 0]])

#library(dplyr)
#library(ggplot2)

# Select 10 random patients from crrt_IDs
#set.seed(123) # set seed for reproducibility
#crrt_IDs_1 <- sample(crrt_1_ids, size = 10, replace = FALSE)

# Create a subset of the original data frame with the selected patients
#CRRT_subset_1 <- FlucTotIV %>% 
        #filter(ID %in% crrt_IDs_1)
#CRRT_subset_2 <- FlucTotIV %>% 
        #filter(ID %in% crrt_1_ids) %>% 
        #filter(!(ID %in% crrt_IDs_1))

# Remove "." values in DV column
#CRRT_subset_1 <- CRRT_subset_1[CRRT_subset_1$DV != ".", ]
#CRRT_subset_1$DV <- as.numeric(CRRT_subset_1$DV)

# Plot CREAT over time for the first 10 patients with CRRT on all the time when DV != "."
#creat_time_crrt_1 <- ggplot(data = CRRT_subset_1, aes(x = TIME, y = CREAT, group = ID)) +
        #geom_line(aes(color = "CRRT"))  +
        #geom_point(size = 2) + 
        #scale_shape_manual(values = c(16)) +
        #labs(title = "CREAT over Time for 10 Random Patients with CRRT on all the time on sampling days", x = "Time", y = "CREAT") +
        #theme_bw() +
        #geom_rect(data = CRRT_subset_1, aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))

#ggsave("creat_time_crrt_1.png", plot = creat_time_crrt_1, dpi = 300, width = 10, height = 5)

# Remove "." values in DV column
#CRRT_subset_2 <- CRRT_subset_2[CRRT_subset_2$DV != ".", ]
#CRRT_subset_2$DV <- as.numeric(CRRT_subset_2$DV)

# Plot CREAT over time for the first 10 patients with CRRT on all the time when DV != "."
#creat_time_crrt_2 <- ggplot(data = CRRT_subset_2, aes(x = TIME, y = CREAT, group = ID)) +
        #geom_line(aes(color = factor(CRRT)))  +
        #geom_point(size = 2) + 
        #scale_shape_manual(values = c(16)) +
        #labs(title = "CREAT over Time for 11 remaing Patients with CRRT on all the time on sampling days", x = "Time", y = "CREAT") +
        #theme_bw() +
        #geom_rect(data = CRRT_subset_2, aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))

#ggsave("creat_time_crrt_2.png", plot = creat_time_crrt_2, dpi = 300, width = 10, height = 5)


# Here I plot all the the time points (dosing & sampling) of CREAT over time where CREAT equal -0.1 indicating that it's missing

# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT non impute")
library(dplyr)
FlucTotIV_clean_imputed <- FlucTotIV_clean_imputed %>%filter(!HOSPITAL%in%c(3,4))
FlucTotIV_clean_imputed$CREAT2<-ifelse(FlucTotIV_clean_imputed$CREAT2==-99,-0.1,FlucTotIV_clean_imputed$CREAT2) #This is the dataset that I will use thereafter
crrt_1_ids <- unique(FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 1 & !FlucTotIV_clean_imputed$ID %in% FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 0]]) #equals 15  
# that means we have 15 patients who were on CRRT for their whole treatment period while 21 patients who were on CRRT on all sampling events (these 15 contain 5 patients with CREAT missing completely)
# after I filter out hospital 3 & 4 with CREAT completely missing, I got 10 patients that were on CRRT all the time (including dosing events) 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# Remove "." values in DV column
#CRRT_subset_1 <- CRRT_subset_1[CRRT_subset_1$DV != ".", ]
#CRRT_subset_1$DV <- as.numeric(CRRT_subset_1$DV)

# Plot CREAT over time for the first 10 patients with CRRT on all the time when DV != "."
#creat_time_crrt_01 <- ggplot(data = CRRT_subset_1, aes(x = TIME, y = CREAT, group = ID)) +
        #geom_line(aes(color = factor(CRRT)))  +
        #geom_point(size = 2) + 
        #scale_shape_manual(values = c(16)) +
        #labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        #theme_bw() +
        #geom_rect(data = CRRT_subset_1, aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))

#ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
# Create data frame for patients with CREAT > 0
CRRT_subset_1_pos <- CRRT_subset_1 %>% 
        filter(CREAT2 > 0)

# Create data frame for patients with CREAT < 0
CRRT_subset_1_neg <- CRRT_subset_1 %>% 
        filter(CREAT2 < 0)

# Plot the two lines together
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
        geom_line(data = CRRT_subset_1_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Remove "." values in DV column
#CRRT_subset_2 <- CRRT_subset_2[CRRT_subset_2$DV != ".", ]
#CRRT_subset_2$DV <- as.numeric(CRRT_subset_2$DV)

# Plot CREAT over time for the first 10 patients with CRRT on all the time when DV != "."
#creat_time_crrt_02 <- ggplot(data = CRRT_subset_2, aes(x = TIME, y = CREAT, group = ID)) +
        #geom_line(aes(color = factor(CRRT)))  +
        #geom_point(size = 2) + 
        #scale_shape_manual(values = c(16)) +
        #labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        #theme_bw() +
        #geom_rect(data = CRRT_subset_2, aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))

#ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
# Create data frame for patients with CREAT > 0
CRRT_subset_2_pos <- CRRT_subset_2 %>% 
        filter(CREAT2 > 0)

# Create data frame for patients with CREAT < 0
CRRT_subset_2_neg <- CRRT_subset_2 %>% 
        filter(CREAT2 < 0)

# Plot the two lines together
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
        geom_line(data = CRRT_subset_2_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)

### Here I counted the number of patients who had CRRT at least once ###

#library(dplyr)
#crrt_counts_once <- FlucTotIV %>% filter(!is.na(DV)) %>%
        #filter(CRRT == 1) %>% 
        #summarize(num_crrt_patients = n_distinct(ID))
#crrt_counts_once #equals 31 #So there's 1 patient had CRRT on non-sampling day(s)

### Then I counted the number of patients who had both 0 and 1 in there CRRT history, which means there were times when they had CRRT and there were times when they didn't ###

#library(dplyr)
#crrt_counts_miscel <- FlucTotIV %>% filter (DV!=".") %>% #here I extract the observations where DVs are not missing only since CREATs were only measured where DVs are present
        #group_by(ID) %>% 
        #summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        #filter(has_both_crrt) %>% 
        #summarize(num_patients_with_both_crrt = n())
#crrt_counts_miscel #equals 10



#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 17 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_1)

# Plot these patients to see (hopefully) patterns
# Skip this
#library(ggplot2)

#ggplot(CRRT_miscel_1, aes(x = TIME, y = CREAT)) +
        #geom_point() +
        #geom_rect(data = CRRT_miscel_1 %>% filter(CRRT == 1), 
                  #aes(xmin = TIME, ymin = -Inf, xmax = lead(TIME), ymax = Inf), 
                  #fill = "gray50", alpha = 0.5) +
        #xlab("Time (hours)") +
        #ylab("Creatinine (mg/dL)") +
        #ggtitle("Creatinine vs. Time in CRRT patients with on/off history") 
# Not so good plot #

# That is what chatGPT suggested me to do
#library(ggplot2)

# Count number of patients who had CRRT in the first week
#CRRT_first_week <- length(unique(FlucTotIV$ID[FlucTotIV$CRRT == 1 & FlucTotIV$TIME <= 168])) #29
#CRRT_second_week<- length(unique(FlucTotIV$ID[FlucTotIV$CRRT == 1 & FlucTotIV$TIME > 168])) #16

# Remove "." values in DV column
#CRRT_miscel <- CRRT_miscel[CRRT_miscel$DV != ".", ]
#CRRT_miscel$DV<-as.numeric(CRRT_miscel$DV)

# Plot CREAT over time for the first 6 patients with CRRT on and off
#creat_time_miscel_01<-ggplot(data = CRRT_miscel_1, aes(x = TIME, y = CREAT, group = ID,shape = factor(CRRT))) +
        #geom_line(aes(color = factor(CRRT))) + 
        #scale_color_manual(values = c("black", "red")) +  geom_point(size = 2) + scale_shape_manual(values = c(16, 17)) +
        #labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        #theme_bw() +
        #geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))
#ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
# Create data frame for patients with CREAT > 0
CRRT_miscel_1_pos <- CRRT_miscel_1 %>% 
        filter(CREAT2 > 0)

# Create data frame for patients with CREAT < 0
CRRT_miscel_1_neg <- CRRT_miscel_1 %>% 
        filter(CREAT2 < 0)

# Plot the two lines together
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
        geom_line(data = CRRT_miscel_1_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)



# Plot DV over time for patients with CRRT on and off #Not needed for now so I'll hide it
#dv_time_miscel_01<-ggplot(data = CRRT_miscel_1, aes(x = TIME, y = DV, group = ID,shape = factor(CRRT))) +
        #geom_line(aes(color = factor(CRRT))) +
        #scale_color_manual(values = c("black", "red")) + geom_point(size = 2) + scale_shape_manual(values = c(16, 17)) +
        #labs(title = "DV over Time for Patients with CRRT on and off 01", x = "Time", y = "DV") +
        #theme_bw() +
        #geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))
#ggsave("dv_time_miscel_01.png", plot = dv_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_2)

# Plot CREAT over time for the 2nd 6 patients with CRRT on and off
#creat_time_miscel_02<-ggplot(data = CRRT_miscel_2, aes(x = TIME, y = CREAT, group = ID,shape = factor(CRRT))) +
        #geom_line(aes(color = factor(CRRT))) + 
        #scale_color_manual(values = c("black", "red")) +  geom_point(size = 2) + scale_shape_manual(values = c(16, 17)) +
        #labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time", y = "CREAT") +
        #theme_bw() +
        #geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))
#ggsave("creat_time_miscel_02_01.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
# Create data frame for patients with CREAT > 0
CRRT_miscel_2_pos <- CRRT_miscel_2 %>% 
        filter(CREAT2 > 0)

# Create data frame for patients with CREAT < 0
CRRT_miscel_2_neg <- CRRT_miscel_2 %>% 
        filter(CREAT2 < 0)

# Plot the two lines together
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
        geom_line(data = CRRT_miscel_2_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 5 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_3)

# Plot CREAT over time for the 2nd 6 patients with CRRT on and off
#creat_time_miscel_03<-ggplot(data = CRRT_miscel_3, aes(x = TIME, y = CREAT, group = ID,shape = factor(CRRT))) +
        #geom_line(aes(color = factor(CRRT))) + 
        #scale_color_manual(values = c("black", "red")) +  geom_point(size = 2) + scale_shape_manual(values = c(16, 17)) +
        #labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time", y = "CREAT") +
        #theme_bw() +
        #geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  #fill = "grey", alpha = 0.2) +
        #facet_wrap(~ ID, scales = "free") +
        #guides(color = guide_legend(title = "CRRT"))
#ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
# Create data frame for patients with CREAT > 0
CRRT_miscel_3_pos <- CRRT_miscel_3 %>% 
        filter(CREAT2 > 0)

# Create data frame for patients with CREAT < 0
CRRT_miscel_3_neg <- CRRT_miscel_3 %>% 
        filter(CREAT2 < 0)

# Plot the two lines together
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
        geom_line(data = CRRT_miscel_3_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, I'll check how many non-CRRT patients are there per each HOSPITAL
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed$ID),c(crrt_1_ids,miscel_IDs))
FlucTotIV_clean_imputed %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        group_by(HOSPITAL) %>% 
        summarize(n_distinct(ID))
#HOSPITAL `n_distinct(ID)`

#1               31
#2               13
#5               20
#6               22
#7               21
#8               33

## From here, I decide on the number of patients per plot in each Hospital
## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Can skip the following
#for (i in seq_along(hospital_1_groups)) {
        #nonCRRT_IDs <- hospital_1_groups[[i]]
        #CRRT_non <- FlucTotIV_clean_imputed %>% 
                #filter(ID %in% nonCRRT_IDs)
        #creat_time_nonCRRT <- ggplot(data = CRRT_non, aes(x = TIME, y = CREAT, group = ID, shape = factor(CRRT))) +
                #geom_line(aes(color = factor(CRRT))) + 
                #scale_color_manual(values = c("black")) + 
                #geom_point(size = 2) + 
                #scale_shape_manual(values = c(16)) +
                #labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 1 ", sprintf("%02d", i)), 
                     #x = "Time", y = "CREAT") +
                #theme_bw() +
                #geom_rect(data = CRRT_non[CRRT_non$CRRT == 0, ], 
                          #aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          #fill = "grey", alpha = 0.2) +
                #facet_wrap(~ ID, scales = "free") +
                #guides(color = guide_legend(title = "CRRT"))
        #ggsave(paste0("creat_time_nonCRRT_", sprintf("%02d", i), ".png"), 
               #plot = creat_time_nonCRRT, dpi = 300, width = 10, height = 5)
#}

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_1)
        CRRT_non_1_pos <- CRRT_non_1 %>% 
                filter(CREAT2 > 0)
        CRRT_non_1_neg <- CRRT_non_1 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_1_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 1 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_2)
        CRRT_non_2_pos <- CRRT_non_2 %>% 
                filter(CREAT2 > 0)
        CRRT_non_2_neg <- CRRT_non_2 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_2_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 2 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_5)
        CRRT_non_5_pos <- CRRT_non_5 %>% 
                filter(CREAT2 > 0)
        CRRT_non_5_neg <- CRRT_non_5 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_5_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_6)
        CRRT_non_6_pos <- CRRT_non_6 %>% 
                filter(CREAT2 > 0)
        CRRT_non_6_neg <- CRRT_non_6 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_6_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_7)
        CRRT_non_7_pos <- CRRT_non_7 %>% 
                filter(CREAT2 > 0)
        CRRT_non_7_neg <- CRRT_non_7 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_7_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_8)
        CRRT_non_8_pos <- CRRT_non_8 %>% 
                filter(CREAT2 > 0)
        CRRT_non_8_neg <- CRRT_non_8 %>% 
                filter(CREAT2 <0)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8_pos, aes(x = TIME, y = CREAT2, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_8_neg, aes(x = TIME, y = CREAT2, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT2, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}


library(dplyr)
library(ggplot2)

# Extract IDs with more than 1 unique BW
FlucTotIV_filtered <- FlucTotIV %>% filter(!is.na(BW))  %>% 
        group_by(ID) %>%
        filter(n_distinct(BW) > 1)

# Plot BW over TIME of the filtered IDs
BW<-ggplot(FlucTotIV_filtered, aes(x = TIME, y = BW, group = ID)) +
        geom_line() +
        geom_point() +
        labs(title = "BW over Time for IDs with more than 1 unique BW", x = "Time", y = "BW") +
        theme_bw() +
        facet_wrap(~ ID, scales = "free")
ggsave("Multiple BWs.png", plot = BW, dpi = 300, width = 10, height = 5)


# check in what studies do those CRRT patients come from
CRRT_hospital <- FlucTotIV %>% filter(CRRT == 1)
View(CRRT_hospital)
table(CRRT_hospital$HOSPITAL)


### Treatment duration for Omar
duration <- FlucTotIV %>% 
        group_by(ID) %>% 
        summarize(TIME = max(TIME)/24)
summary(duration$TIME)

### Eliminate consecutive NAs of CREAT in patient ID

# I create a new dataset to test the code to see if it works

library(dplyr)
library(tidyr)
FlucTotIV_clean <- FlucTotIV %>%
        group_by(ID) %>%
        mutate(dup = CREAT == lag(CREAT)) %>%
        mutate(dup_group = cumsum(!dup | is.na(dup))) %>%
        group_by(ID, dup_group) %>%
        mutate(CREAT = ifelse(row_number() > 1 & !is.na(CREAT), NA, CREAT)) %>%
        ungroup() %>%
        select(-dup_group)

### Next thing, fill in the CREAT of dosing events that are within 3 hours of the previous sampling events 
# (this applies mostly to Ruth's study because she collected trough concentrations only so the dosing and sampling events would be very close)
# 3 hours is what I derived from Ruth's dataset

FlucTotIV_clean <- FlucTotIV_clean %>%
        mutate(CREAT2 = CREAT) %>%
        group_by(ID) %>%
        mutate(CREAT2 = ifelse(is.na(CREAT2) & !is.na(lag(CREAT2)) & lag(DV) != "." & (TIME - lag(TIME)) <= 3,
                               lag(CREAT2), CREAT2)) %>%
        ungroup()

### Then I fill the NAs of BW with its baseline values. I create a new column for that called BW2.

FlucTotIV_clean <- FlucTotIV_clean %>%
        group_by(ID) %>%
        mutate(BW2 = ifelse(is.na(BW), first(BW, order_by = TIME), BW)) %>%
        ungroup()

# Then, I eliminate all the observations that come after the last concentration per each ID
FlucTotIV_clean <- FlucTotIV_clean %>%
        arrange(ID, TIME) %>%
        group_by(ID) %>%
        filter(row_number() <= max(which(DV!="."))) %>%
        ungroup()

# Then export my FlucTotIV_clean dataset
FlucTotIV_clean$dup <- NULL
write.csv(FlucTotIV_clean, "FlucTotIV_clean.csv",quote=F,row.names = FALSE)

# Then, descriptive statistic of this new CREAT2 compared to former CREAT
DataDVAMT1 <- FlucTotIV_clean
datasum <- unique(DataDVAMT1 %>% select(ID,OCC,TIME,TAD,DV,BW,LENGTH,AGE,SEX,CREAT,
                                        CREAT2,CKDEPI,CRRT,HOSPITAL))
# Render function
rndr <- function(x, name, ...) {
        if (!is.numeric(x)) return(render.categorical.default(x))
        what <- switch(name,
                       BW = "Median [Q1 - Q3]",
                       CREAT  = "Mean (SD)")
        parse.abbrev.render.code(c("", what))(x)
}
table1(~ CREAT + CREAT2 | HOSPITAL, data=datasum,
       render.continuous=c(.="Median [Q1-Q3]"))

# Manually check the data of patients who had CRRT in on-off fashion among 10 miscel patients #
#library(dplyr)
#CRRT_miscel_onoff <- FlucTotIV %>%  
        #filter(DV!=".") %>% 
        #filter(ID %in% c(113,134,144, 213, 511, 517))

#### I create a new column called CKDEPI_NEW, which takes into account each individual patient's CRRT profile in relation to their CREAT sampling TIME

# First of all, I have 5 patients who have CRRT on then off. 
# I consider their CKDEPI values at their CRRT days and 24h after CRRT invalid, so their CKDEPI_NEW will be NAs.
# Otherwise, CKDEPI_NEW will be equal to CKDEPI

library(dplyr)
# create new column CKDEPI_NEW and set default as NA
FlucTotIV$CKDEPI_NEW <- FlucTotIV$CKDEPI

# subset data frame for specified patients
patient_subset <- FlucTotIV[FlucTotIV$ID %in% c(113, 134, 144, 511, 517),]

# loop through each patient in the subset
for (patient_row in 1:nrow(patient_subset)) {
        
        # check if CRRT is 1 or if closest previous CRRT was within 24 hours
        if (patient_subset$CRRT[patient_row] == 1 | 
            (patient_subset$CRRT[patient_row] == 0 & 
             min(patient_subset$TIME[patient_subset$CRRT == 1 & 
                                     patient_subset$TIME < patient_subset$TIME[patient_row]]) >= (patient_subset$TIME[patient_row]-24))) {
                
                # set CKDEPI_NEW to NA
                patient_subset$CKDEPI_NEW[patient_row] <- NA
                
        } else {
                
                # set CKDEPI_NEW to original CKDEPI value
                patient_subset$CKDEPI_NEW[patient_row] <- patient_subset$CKDEPI[patient_row]
                
        }
}

# update the original data frame with the subset values
FlucTotIV[FlucTotIV$ID %in% c(113, 134, 144, 511, 517), "CKDEPI_NEW"] <- patient_subset$CKDEPI_NEW

# Then I move on with the patient 213, I accept only the last CREAT measurement since it was way after CRRT
FlucTotIV$CKDEPI_NEW[FlucTotIV$ID == 213] <- ifelse(FlucTotIV$CKDEPI[FlucTotIV$ID == 213] == 40.8, FlucTotIV$CKDEPI[FlucTotIV$ID == 213], NA)

# Next is patient 218, this patient has only 3 measurements. 
# However I still accept the CREAT to me repeated many times (limitation of the study), I only exclude the value of the last 2 concentrations because of CRRT
FlucTotIV$CKDEPI_NEW[FlucTotIV$ID == 218] <- ifelse(FlucTotIV$TIME[FlucTotIV$ID == 218] >= 72.58, NA, FlucTotIV$CKDEPI[FlucTotIV$ID == 218])

# After that, for patient 122, only exclude the last concentration's CREAT because of CRRT
FlucTotIV$CKDEPI_NEW[FlucTotIV$ID == 122] <- ifelse(FlucTotIV$TIME[FlucTotIV$ID == 122] >= 192, NA, FlucTotIV$CKDEPI[FlucTotIV$ID == 122])

# For 119 & 526, will exclude all of their CREAT since they have CRRT most of the time
FlucTotIV[FlucTotIV$ID %in% c(119, 526), "CKDEPI_NEW"] <- NA

## Next, I set the CKDEPI_NEW of patients who have CRRT all the time to NA
# Subset FlucTotIV to remove rows with DV == "."
FlucTotIV_subset <- FlucTotIV[FlucTotIV$DV != ".",]

# Get unique IDs with CRRT == 1 only
crrt_1_ids <- unique(FlucTotIV_subset$ID[FlucTotIV_subset$CRRT == 1 & !FlucTotIV_subset$ID %in% FlucTotIV_subset$ID[FlucTotIV_subset$CRRT == 0]])

# Set CKDEPI_NEW to NA for those IDs in the original FlucTotIV
FlucTotIV$CKDEPI_NEW[FlucTotIV$ID %in% crrt_1_ids] <- NA
# Then export a new FlucTotIV dataframe
write.csv(FlucTotIV,"FlucTotIV.csv",quote=F,row.names=FALSE)

# Then apply median imputation and export to a CSV file

Fluc_NONMEM_MED2<-FlucTotIV
Fluc_NONMEM_MED2$CMT <-NULL
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2$BW), "BW"] <- median(Fluc_NONMEM_MED2$BW, na.rm = TRUE)
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2$LENGTH), "LENGTH"] <- median(Fluc_NONMEM_MED2$LENGTH, na.rm = TRUE)
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2$CG_NoD), "CG_NoD"] <- median(Fluc_NONMEM_MED2$CG_NoD, na.rm = TRUE)
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2$CKDEPI_NoD), "CKDEPI_NoD"] <- median(Fluc_NONMEM_MED2$CKDEPI_NoD, na.rm = TRUE)   
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2$CKDEPI_NEW), "CKDEPI_NEW"] <- median(Fluc_NONMEM_MED2$CKDEPI_NEW, na.rm = TRUE)
Fluc_NONMEM_MED2[is.na(Fluc_NONMEM_MED2)] <- -99

### Then I create a new variable: fat free mass (FFM) (one of my covariates) - calculated from weight, height, sex, whether a patient is an adult or not - using The Boer Formula ###

for (i in 1:length(Fluc_NONMEM_MED2$SEX)) {
        if (Fluc_NONMEM_MED2$SEX[i]==1) {
                Fluc_NONMEM_MED2$FFM[i] <- 0.407*Fluc_NONMEM_MED2$BW[i]+0.267*Fluc_NONMEM_MED2$LENGTH[i]*100-19.2
        } else {
                Fluc_NONMEM_MED2$FFM[i] <- 0.252*Fluc_NONMEM_MED2$BW[i]+0.473*Fluc_NONMEM_MED2$LENGTH[i]*100-48.3
        } 
}

### I also calculate BMI for those for are missing ###

Fluc_NONMEM_MED2$BMI<-Fluc_NONMEM_MED2$BW/(Fluc_NONMEM_MED2$LENGTH)^2

### Finally, I export it into a dataset for running NONMEM ###
Fluc_NONMEM_MED2$ASSAY<-NULL
write.csv(Fluc_NONMEM_MED2,"Fluc_NONMEM_MED2.csv",quote=F,row.names=FALSE)
median(Fluc_NONMEM_MED2$CKDEPI_NEW) #equal 91

# Before doing imputation, fill DV in every row
DataDV <- FlucTotIV
DataDV$DV<- as.numeric(DataDV$DV) #DV to numeric
library(zoo)
# Fill in missing values in FlucTotIV
DataDV$DV <- ave(DataDV$DV, DataDV$ID, FUN = function(x) na.locf(x, na.rm = FALSE))


#### Here I try to create 10 imputation datasets, using pmm method. I need a subset containing AGE, SEX, BW2, DV, TIME, HOSPITAL, CRRT ####

### I will impute CREAT2, then use it to calculate CKDEPI ###

library(mice)

# Convert columns to appropriate data types
FlucTotIV_clean$AGE <- as.numeric(FlucTotIV_clean$AGE)
FlucTotIV_clean$BW2 <- as.numeric(FlucTotIV_clean$BW2)
FlucTotIV_clean$DV <- as.numeric(ifelse(FlucTotIV_clean$DV == ".", NA, FlucTotIV_clean$DV))
FlucTotIV_clean$TIME <- as.numeric(FlucTotIV_clean$TIME)
FlucTotIV_clean$CREAT2 <- as.numeric(FlucTotIV_clean$CREAT2)
FlucTotIV_clean$SEX <- as.factor(FlucTotIV_clean$SEX)
FlucTotIV_clean$HOSPITAL <- as.factor(FlucTotIV_clean$HOSPITAL)
FlucTotIV_clean$CRRT <- as.factor(FlucTotIV_clean$CRRT)

# First of all, impute BW & LENGTH at baseline
impute_data <- FlucTotIV_clean[, c("AGE","ID", "SEX", "BW2", "HOSPITAL","LENGTH")]
# Set the seed for reproducibility
set.seed(123)
# Perform multiple imputations using pmm method
imputed_data <- mice(impute_data, m = 10,maxit=50)
for (i in 1:10) {
        FlucTotIV_clean[[paste0("BW", sprintf("%02d", i))]] <- (complete(imputed_data, action = i))$BW2
}

for (i in 1:10) {
        FlucTotIV_clean[, paste0("FFM", sprintf("%02d", i))] <- ifelse(FlucTotIV_clean$SEX == 1, 
                                                                       0.407 * FlucTotIV_clean[, paste0("BW", sprintf("%02d", i))] + 
                                                                               0.267 * FlucTotIV_clean$LENGTH * 100 - 19.2, 
                                                                       0.252 * FlucTotIV_clean[, paste0("BW", sprintf("%02d", i))] + 
                                                                               0.473 * FlucTotIV_clean$LENGTH * 100 - 48.3)
}


# The one above doesn't work, here is the correct code for baseline BW & LENGTH imputation
library(dplyr)
library(mice)
impute_BW <- FlucTotIV_clean %>% 
        group_by(ID) %>% 
        slice(1) %>% 
        select(ID,AGE, SEX, BW, HOSPITAL, CRRT, CREAT2, LENGTH)
# Set the seed for reproducibility
set.seed(123)
# Perform multiple imputations using pmm method
imputed_BW <- mice(impute_BW, m = 10,maxit=50)
# Create new columns in the original data set for each imputed BW & LENGTH value
for (i in 1:10) {
        impute_BW[[paste0("BW", sprintf("%02d", i))]] <- (complete(imputed_BW, action = i))$BW
}
for (i in 1:10) {
        impute_BW[[paste0("LENGTH", sprintf("%02d", i))]] <- (complete(imputed_BW, action = i))$LENGTH
}
# eliminate redundant column
impute_BW <- impute_BW[, !names(impute_BW) %in% c("AGE", "SEX", "BW", "CRRT", "CREAT2", "LENGTH")]

# Then merge into the original dataset

# Subset FlucTotIV_clean to keep only the first row of each ID
FlucTotIV_clean_first <- FlucTotIV_clean[!duplicated(FlucTotIV_clean$ID), ]
# Merge impute_data with FlucTotIV_clean_first based on the ID column
imputed_first <- merge(FlucTotIV_clean_first[, "ID", drop = FALSE], impute_BW, by = "ID", all.x = TRUE)
# Merge imputed_first back into FlucTotIV_clean based on the ID and HOSPITAL columns
FlucTotIV_clean_imputed <- merge(FlucTotIV_clean, imputed_first, by = c("ID", "HOSPITAL"), all.x = TRUE)


# Not important code

# Subset the data for hospital 3
hospital3 <- FlucTotIV_clean[FlucTotIV_clean$HOSPITAL == 3,]
# Select columns for BW01 to BW10 and ID
BW_columns <- c("ID", paste0("BW", sprintf("%02d", 1:10)))
hospital3_BW <- hospital3[,BW_columns]



# Create a new data frame with only the specified predictors and CREAT2
impute_CREAT2 <- FlucTotIV_clean[, c("AGE", "SEX", "BW2", "DV", "TIME", "HOSPITAL", "CRRT", "CREAT2","OCC","LENGTH")]

# Set the seed for reproducibility
set.seed(123)

# Perform multiple imputations using pmm method
imputed_CREAT2 <- mice(impute_CREAT2, m = 10,maxit=50)

# Extract completed data sets
#completed_data <- complete(imputed_data, action = "long")
#imputed_data_1 <- complete(imputed_data, action = 1)

# Analyze the completed data sets
#summary(completed_data)

# Create new columns in the original data set for each imputed CREAT2 value
for (i in 1:10) {
        FlucTotIV_clean_imputed[[paste0("CREAT", sprintf("%02d", i))]] <- (complete(imputed_CREAT2, action = i))$CREAT2
}

# And final imputed dataset
Fluc_NONMEM_mul_impute <- FlucTotIV_clean_imputed

# Create CKDEPI01
Fluc_NONMEM_mul_impute$CKDEPI01 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT01 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT01/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT01 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT01/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT01 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT01/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT01/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPI02
Fluc_NONMEM_mul_impute$CKDEPI02 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT02 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT02/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT02 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT02/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT02 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT02/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT02/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPI03
Fluc_NONMEM_mul_impute$CKDEPI03 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT03 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT03/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT03 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT03/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT03 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT03/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT03/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPI04
Fluc_NONMEM_mul_impute$CKDEPI04 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT04 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT04/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT04 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT04/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT04 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT04/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT04/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPI05
Fluc_NONMEM_mul_impute$CKDEPI05 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT05 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT05/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT05 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT05/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT05 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT05/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT05/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))                                                                   

# Create CKDEPI06
Fluc_NONMEM_mul_impute$CKDEPI06 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT06 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT06/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT06 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT06/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT06 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT06/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT06/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))                                                                            

# Create CKDEPI07
Fluc_NONMEM_mul_impute$CKDEPI07 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT07 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT07/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT07 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT07/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT07 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT07/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT07/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))
# Create CKDEPI08
Fluc_NONMEM_mul_impute$CKDEPI08 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT08 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT08/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT08 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT08/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT08 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT08/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT08/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPI09
Fluc_NONMEM_mul_impute$CKDEPI09 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT09 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT09/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT09 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT09/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT09 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT09/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT09/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))
# Create CKDEPI10
Fluc_NONMEM_mul_impute$CKDEPI10 <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT10 < 0.9, 
                                          (141) * ((Fluc_NONMEM_mul_impute$CREAT10/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                          ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT10 >= 0.9, 
                                                 (141) * ((Fluc_NONMEM_mul_impute$CREAT10/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                 ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT10 < 0.7, 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT10/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                        (144) * ((Fluc_NONMEM_mul_impute$CREAT10/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Add FFM (already above)
#for (i in 1:length(FlucTotIV_clean$SEX)) {
        #if (FlucTotIV_clean$SEX[i]==1) {
                #FlucTotIV_clean$FFM[i] <- 0.407*FlucTotIV_clean$BW2[i]+0.267*FlucTotIV_clean$LENGTH[i]*100-19.2
        #} else {
                #FlucTotIV_clean$FFM[i] <- 0.252*FlucTotIV_clean$BW2[i]+0.473*FlucTotIV_clean$LENGTH[i]*100-48.3
        #} 
#}

# Add 10 new FFM columns
for (i in 1:10) {
        BW_col <- paste0("BW", sprintf("%02d", i))
        LENGTH_col <- paste0("LENGTH", sprintf("%02d", i))
        FFM_col <- paste0("FFM", sprintf("%02d", i))
        Fluc_NONMEM_mul_impute[, FFM_col] <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1,
                                                    0.407 * Fluc_NONMEM_mul_impute[[BW_col]] + 0.267 * Fluc_NONMEM_mul_impute[[LENGTH_col]] * 100 - 19.2,
                                                    0.252 * Fluc_NONMEM_mul_impute[[BW_col]] + 0.473 * Fluc_NONMEM_mul_impute[[LENGTH_col]] * 100 - 48.3)
}

# Then convert all NAs to -99
Fluc_NONMEM_mul_impute[is.na(Fluc_NONMEM_mul_impute)] <- -99

# Median of imputed CKDEPI
median(Fluc_NONMEM_mul_impute$CKDEPI01) #equals 80.15
median(Fluc_NONMEM_mul_impute$CKDEPI02) #equals 85.83
median(Fluc_NONMEM_mul_impute$CKDEPI03) #equals 85.34
median(Fluc_NONMEM_mul_impute$CKDEPI04) #equals 85.69
median(Fluc_NONMEM_mul_impute$CKDEPI05) #equals 87.00
median(Fluc_NONMEM_mul_impute$CKDEPI06) #equals 84.51
median(Fluc_NONMEM_mul_impute$CKDEPI07) #equals 91.85
median(Fluc_NONMEM_mul_impute$CKDEPI08) #equals 88.46
median(Fluc_NONMEM_mul_impute$CKDEPI09) #equals 82.96
median(Fluc_NONMEM_mul_impute$CKDEPI10) #equals 79.65
median(Fluc_NONMEM_mul_impute$CKDEPIMAX) #242.93
median(Fluc_NONMEM_mul_impute$CKDEPIMIN) #4.88
median(Fluc_NONMEM_mul_impute$CKDEPIMED) #78.65

# Median of imputed FFM
median(Fluc_NONMEM_mul_impute$FFM01) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM02) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM03) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM04) #equals 57.94
median(Fluc_NONMEM_mul_impute$FFM05) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM06) #equals 57.94
median(Fluc_NONMEM_mul_impute$FFM07) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM08) #equals 57.94
median(Fluc_NONMEM_mul_impute$FFM09) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFM10) #equals 58.15
median(Fluc_NONMEM_mul_impute$FFMMAX) #equals 58.76
median(Fluc_NONMEM_mul_impute$FFMMIN) #equals 57.68
median(Fluc_NONMEM_mul_impute$FFMMED) #equals 58.15

# Do the same for my second imputed dataset (dataset that I adopted a different imputation strategy)
# Median of imputed CKDEPI
median(Fluc_NONMEM_mul_impute_2$CKDEPI01) #equals 79.65
median(Fluc_NONMEM_mul_impute_2$CKDEPI02) #equals 70.07
median(Fluc_NONMEM_mul_impute_2$CKDEPI03) #equals 71.78
median(Fluc_NONMEM_mul_impute_2$CKDEPI04) #equals 73.19
median(Fluc_NONMEM_mul_impute_2$CKDEPI05) #equals 73.27
median(Fluc_NONMEM_mul_impute_2$CKDEPI06) #equals 78.84
median(Fluc_NONMEM_mul_impute_2$CKDEPI07) #equals 78.46
median(Fluc_NONMEM_mul_impute_2$CKDEPI08) #equals 82.50
median(Fluc_NONMEM_mul_impute_2$CKDEPI09) #equals 75.19
median(Fluc_NONMEM_mul_impute_2$CKDEPI10) #equals 82.15


# Maximum & Minimum imputation
# Create a new column CREATMAX
# Fluc_NONMEM_mul_impute <- FlucTotIV_clean_imputed2 #This code I only use once
# Fluc_NONMEM_mul_impute[is.na(Fluc_NONMEM_mul_impute)] <- -99
# Fluc_NONMEM_mul_impute$CREATMAX<-NULL
# Fluc_NONMEM_mul_impute$CREATMIN<-NULL
# Fluc_NONMEM_mul_impute$CREATMED<-NULL
Fluc_NONMEM_mul_impute$CREATMAX <- ifelse(Fluc_NONMEM_mul_impute$CREAT2 != -99, 
                                          Fluc_NONMEM_mul_impute$CREAT2, 
                                          max(Fluc_NONMEM_mul_impute$CREAT2[Fluc_NONMEM_mul_impute$CREAT2 != -99], na.rm = TRUE))

# Create a new column CREATMIN
Fluc_NONMEM_mul_impute$CREATMIN <- ifelse(Fluc_NONMEM_mul_impute$CREAT2 != -99, 
                                          Fluc_NONMEM_mul_impute$CREAT2, 
                                          min(Fluc_NONMEM_mul_impute$CREAT2[Fluc_NONMEM_mul_impute$CREAT2 != -99], na.rm = TRUE))

# Create a new column CREATMED
Fluc_NONMEM_mul_impute$CREATMED <- ifelse(Fluc_NONMEM_mul_impute$CREAT2 != -99, 
                                          Fluc_NONMEM_mul_impute$CREAT2, 
                                          median(Fluc_NONMEM_mul_impute$CREAT2[Fluc_NONMEM_mul_impute$CREAT2 != -99], na.rm = TRUE))

# Create a new column BWMAX
Fluc_NONMEM_mul_impute$BWMAX <- ifelse(Fluc_NONMEM_mul_impute$BW2 != -99, 
                                          Fluc_NONMEM_mul_impute$BW2, 
                                          max(Fluc_NONMEM_mul_impute$BW2[Fluc_NONMEM_mul_impute$BW2 != -99], na.rm = TRUE))

# Create a new column BWMIN
Fluc_NONMEM_mul_impute$BWMIN <- ifelse(Fluc_NONMEM_mul_impute$BW2 != -99, 
                                          Fluc_NONMEM_mul_impute$BW2, 
                                          min(Fluc_NONMEM_mul_impute$BW2[Fluc_NONMEM_mul_impute$BW2 != -99], na.rm = TRUE))

# Create a new column BWMED
Fluc_NONMEM_mul_impute$BWMED <- ifelse(Fluc_NONMEM_mul_impute$BW2 != -99, 
                                          Fluc_NONMEM_mul_impute$BW2, 
                                          median(Fluc_NONMEM_mul_impute$BW2[Fluc_NONMEM_mul_impute$BW2 != -99], na.rm = TRUE))

# Create a new column LENGTHMAX
Fluc_NONMEM_mul_impute$LENGTHMAX <- ifelse(Fluc_NONMEM_mul_impute$LENGTH != -99, 
                                       Fluc_NONMEM_mul_impute$LENGTH, 
                                       max(Fluc_NONMEM_mul_impute$LENGTH[Fluc_NONMEM_mul_impute$LENGTH != -99], na.rm = TRUE))

# Create a new column LENGTHMIN
Fluc_NONMEM_mul_impute$LENGTHMIN <- ifelse(Fluc_NONMEM_mul_impute$LENGTH != -99, 
                                       Fluc_NONMEM_mul_impute$LENGTH, 
                                       min(Fluc_NONMEM_mul_impute$LENGTH[Fluc_NONMEM_mul_impute$LENGTH != -99], na.rm = TRUE))

# Create a new column LENGTHMED
Fluc_NONMEM_mul_impute$LENGTHMED <- ifelse(Fluc_NONMEM_mul_impute$LENGTH != -99, 
                                       Fluc_NONMEM_mul_impute$LENGTH, 
                                       median(Fluc_NONMEM_mul_impute$LENGTH[Fluc_NONMEM_mul_impute$LENGTH != -99], na.rm = TRUE))
# CKDEPIMAX,CKDEPIMIN,CKDEPIMED
# Create CKDEPIMAX column
Fluc_NONMEM_mul_impute$CKDEPIMAX <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMIN < 0.9, 
                                           (141) * ((Fluc_NONMEM_mul_impute$CREATMIN/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                           ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMIN >= 0.9, 
                                                  (141) * ((Fluc_NONMEM_mul_impute$CREATMIN/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                  ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREATMIN < 0.7, 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMIN/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMIN/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPIMIN column
Fluc_NONMEM_mul_impute$CKDEPIMIN <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMAX < 0.9, 
                                           (141) * ((Fluc_NONMEM_mul_impute$CREATMAX/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                           ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMAX >= 0.9, 
                                                  (141) * ((Fluc_NONMEM_mul_impute$CREATMAX/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                  ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREATMAX < 0.7, 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMAX/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMAX/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))

# Create CKDEPIMED column
Fluc_NONMEM_mul_impute$CKDEPIMED <- ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMED < 0.9, 
                                           (141) * ((Fluc_NONMEM_mul_impute$CREATMED/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                           ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREATMED >= 0.9, 
                                                  (141) * ((Fluc_NONMEM_mul_impute$CREATMED/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                  ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREATMED < 0.7, 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMED/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                         (144) * ((Fluc_NONMEM_mul_impute$CREATMED/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE))))
                                                          
# FFMMAX, FFMMIN & FFMMED
for (i in 1:length(Fluc_NONMEM_mul_impute$SEX)) {
        if (Fluc_NONMEM_mul_impute$SEX[i]==1) {
                Fluc_NONMEM_mul_impute$FFMMAX[i] <- 0.407*Fluc_NONMEM_mul_impute$BWMAX[i]+0.267*Fluc_NONMEM_mul_impute$LENGTHMAX[i]*100-19.2
        } else {
                Fluc_NONMEM_mul_impute$FFMMAX[i] <- 0.252*Fluc_NONMEM_mul_impute$BWMAX[i]+0.473*Fluc_NONMEM_mul_impute$LENGTHMAX[i]*100-48.3
        } 
}
for (i in 1:length(Fluc_NONMEM_mul_impute$SEX)) {
        if (Fluc_NONMEM_mul_impute$SEX[i]==1) {
                Fluc_NONMEM_mul_impute$FFMMIN[i] <- 0.407*Fluc_NONMEM_mul_impute$BWMIN[i]+0.267*Fluc_NONMEM_mul_impute$LENGTHMIN[i]*100-19.2
        } else {
                Fluc_NONMEM_mul_impute$FFMMIN[i] <- 0.252*Fluc_NONMEM_mul_impute$BWMIN[i]+0.473*Fluc_NONMEM_mul_impute$LENGTHMIN[i]*100-48.3
        } 
}
for (i in 1:length(Fluc_NONMEM_mul_impute$SEX)) {
        if (Fluc_NONMEM_mul_impute$SEX[i]==1) {
                Fluc_NONMEM_mul_impute$FFMMED[i] <- 0.407*Fluc_NONMEM_mul_impute$BWMED[i]+0.267*Fluc_NONMEM_mul_impute$LENGTHMED[i]*100-19.2
        } else {
                Fluc_NONMEM_mul_impute$FFMMED[i] <- 0.252*Fluc_NONMEM_mul_impute$BWMED[i]+0.473*Fluc_NONMEM_mul_impute$LENGTHMED[i]*100-48.3
        } 
}

# Calculate the observed CKDEPI2 column
Fluc_NONMEM_mul_impute$CKDEPI2<-ifelse(Fluc_NONMEM_mul_impute$CREAT2==-99,-99,ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT2 < 0.9, 
                                                                                     (141) * ((Fluc_NONMEM_mul_impute$CREAT2/0.9)^-0.411) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                                                     ifelse(Fluc_NONMEM_mul_impute$SEX == 1 & Fluc_NONMEM_mul_impute$CREAT2 >= 0.9, 
                                                                                            (141) * ((Fluc_NONMEM_mul_impute$CREAT2/0.9)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE),
                                                                                            ifelse(Fluc_NONMEM_mul_impute$SEX == 2 & Fluc_NONMEM_mul_impute$CREAT2 < 0.7, 
                                                                                                   (144) * ((Fluc_NONMEM_mul_impute$CREAT2/0.7)^-0.329) * (0.993^Fluc_NONMEM_mul_impute$AGE), 
                                                                                                   (144) * ((Fluc_NONMEM_mul_impute$CREAT2/0.7)^-1.209) * (0.993^Fluc_NONMEM_mul_impute$AGE)))))
# Relocate CKDEPI2 column to right after CREAT2 column
library(dplyr)
Fluc_NONMEM_mul_impute<-Fluc_NONMEM_mul_impute %>% relocate(CKDEPI2, .after=CREAT2)

#### NOTE ####

# Now I need to creat 2 datasets for future use - FlucTotIV_clean_imputed: a clean dataset that has all the needed imputed and observed variables
# And Fluc_NONMEM_mul_impute for data analysis in NONMEM

# FlucTotIV_clean_imputed
# FlucTotIV_clean_imputed <- Fluc_NONMEM_mul_impute[, !names(Fluc_NONMEM_mul_impute) %in% #This code may not be very helpful anymore
                                                         #c("CMT", "APACHE", "RACE", "CL24", "GGT", "AFT", "ALT", "AST", "BILI", "ALB", "SOFA", "IHD", "UF", "ECMO", "OCC3", "ARCCKD", "ARC24", "ARCAIg", "BMI", "BSI", "dup")]
write.csv(FlucTotIV_clean_imputed, "FlucTotIV_clean_imputed.csv",quote=F,row.names = FALSE)

# And Fluc_NONMEM_mul_impute
Fluc_NONMEM_mul_impute_2 <- FlucTotIV_clean_imputed[, !names(FlucTotIV_clean_imputed) %in% 
                                                         c("LENGTHMAX","LENGTHMIN","LENGTHMED","CREATMAX","CREATMIN","CREATMED",paste0("CREAT", sprintf("%02d", 1:10)),paste0("LENGTH", sprintf("%02d", 1:10)))]
write.csv(Fluc_NONMEM_mul_impute_2, "Fluc_NONMEM_mul_impute.csv",quote=F,row.names = FALSE)


## Here for the other dataset where I applied a different imputing strategy
# FlucTotIV_clean_imputed
FlucTotIV_clean_imputed <- Fluc_NONMEM_mul_impute
write.csv(FlucTotIV_clean_imputed, "FlucTotIV_clean_imputed2.csv",quote=F,row.names = FALSE)

# And Fluc_NONMEM_mul_impute
Fluc_NONMEM_mul_impute_2 <- FlucTotIV_clean_imputed[, !names(FlucTotIV_clean_imputed) %in% 
                                                            c("LENGTHMAX","LENGTHMIN","LENGTHMED","CREATMAX","CREATMIN","CREATMED",paste0("CREAT", sprintf("%02d", 1:10)),paste0("LENGTH", sprintf("%02d", 1:10)))]
write.csv(Fluc_NONMEM_mul_impute_2, "Fluc_NONMEM_mul_impute2.csv",quote=F,row.names = FALSE)


## Here I plot imputed CREATs over TIME 
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 01")
library(dplyr)
FlucTotIV_clean_imputed <- FlucTotIV_clean_imputed %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 1 & !FlucTotIV_clean_imputed$ID %in% FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 02
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 02")
library(dplyr)
FlucTotIV_clean_imputed <- FlucTotIV_clean_imputed %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 1 & !FlucTotIV_clean_imputed$ID %in% FlucTotIV_clean_imputed$ID[FlucTotIV_clean_imputed$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}


#http://127.0.0.1:33591/graphics/plot_zoom_png?width=1200&height=900

## Make plots of imputed CREAT
# First we make a density plot comparing the distribution of 10 imputed CREAT to the distribution of the observe one
library(ggplot2)
dense_creat_impute<-ggplot(FlucTotIV_clean_imputed, aes(x = CREAT01)) + 
        geom_density(aes(fill = "CREAT01"), alpha = 0.5) + 
        geom_density(aes(x = CREAT02, fill = "CREAT02"), alpha = 0.5) + 
        geom_density(aes(x = CREAT03, fill = "CREAT03"), alpha = 0.5) + 
        geom_density(aes(x = CREAT04, fill = "CREAT04"), alpha = 0.5) + 
        geom_density(aes(x = CREAT05, fill = "CREAT05"), alpha = 0.5) + 
        geom_density(aes(x = CREAT06, fill = "CREAT06"), alpha = 0.5) + 
        geom_density(aes(x = CREAT07, fill = "CREAT07"), alpha = 0.5) + 
        geom_density(aes(x = CREAT08, fill = "CREAT08"), alpha = 0.5) + 
        geom_density(aes(x = CREAT09, fill = "CREAT09"), alpha = 0.5) + 
        geom_density(aes(x = CREAT10, fill = "CREAT10"), alpha = 0.5) +
        geom_density(aes(x = CREAT2, fill = "CREAT2"), alpha = 0.5) +
        scale_fill_manual(name = "Variable", values = c("CREAT01" = "blue", "CREAT02" = "lightblue", "CREAT03" = "green",
                                                        "CREAT04" = "orange", "CREAT05" = "purple", "CREAT06" = "pink",
                                                        "CREAT07" = "brown", "CREAT08" = "gray", "CREAT09" = "black",
                                                        "CREAT10" = "yellow","CREAT2" = "red")) +
        xlab("Creatinine Levels") +
        ylab("Density") +
        ggtitle("Density Plot of 10 imputed creatinin vs the original creatinin")
ggsave("dense_creat_impute.png", plot = dense_creat_impute, dpi = 300, width = 10, height = 5)
# The result is that they have an identical distribution 

# Then I check my another imputed dataset that I use a different imputing strategy
# First we make a density plot comparing the distribution of 10 imputed CREAT to the distribution of the observe one
library(ggplot2)
dense_creat_impute<-ggplot(FlucTotIV_clean_imputed2, aes(x = CREAT01)) + 
        geom_density(aes(fill = "CREAT01"), alpha = 0.5) + 
        geom_density(aes(x = CREAT02, fill = "CREAT02"), alpha = 0.5) + 
        geom_density(aes(x = CREAT03, fill = "CREAT03"), alpha = 0.5) + 
        geom_density(aes(x = CREAT04, fill = "CREAT04"), alpha = 0.5) + 
        geom_density(aes(x = CREAT05, fill = "CREAT05"), alpha = 0.5) + 
        geom_density(aes(x = CREAT06, fill = "CREAT06"), alpha = 0.5) + 
        geom_density(aes(x = CREAT07, fill = "CREAT07"), alpha = 0.5) + 
        geom_density(aes(x = CREAT08, fill = "CREAT08"), alpha = 0.5) + 
        geom_density(aes(x = CREAT09, fill = "CREAT09"), alpha = 0.5) + 
        geom_density(aes(x = CREAT10, fill = "CREAT10"), alpha = 0.5) +
        geom_density(aes(x = CREAT2, fill = "CREAT2"), alpha = 0.5) +
        scale_fill_manual(name = "Variable", values = c("CREAT01" = "blue", "CREAT02" = "lightblue", "CREAT03" = "green",
                                                        "CREAT04" = "orange", "CREAT05" = "purple", "CREAT06" = "pink",
                                                        "CREAT07" = "brown", "CREAT08" = "gray", "CREAT09" = "black",
                                                        "CREAT10" = "yellow","CREAT2" = "red")) +
        xlab("Creatinine Levels") +
        ylab("Density") +
        ggtitle("Density Plot of 10 imputed creatinin vs the original creatinin")
ggsave("dense_creat_impute2.png", plot = dense_creat_impute, dpi = 300, width = 10, height = 5)
# The result is that they have an identical distribution 


# Eliminate redundant columns
Fluc_NONMEM_mul_impute <- Fluc_NONMEM_mul_impute[, !names(Fluc_NONMEM_mul_impute) %in% 
                                                         c("BWMAX", "BWMIN", "BWMED", "LENGTHMAX", "LENGTHMIN", "LENGTHMED", "CREATMAX", "CREATMIN", "CREATMED")]
write.csv(Fluc_NONMEM_mul_impute, "Fluc_NONMEM_mul_impute.csv",quote=F,row.names = FALSE)


# Not important code
# Load the mice package
library(mice)

# Create a subset of FlucTotIV where DV != "."
subset <- FlucTotIV[FlucTotIV$DV != ".", c("BW", "LENGTH", "CKDEPI_NEW", "AGE", "SEX", "DV")]

# Set up the mice imputation model
impute_model <- mice(subset, method = "pmm", m = 10, maxit = 10,seed=123)

# Create an empty list to store the imputed data frames
imputed_data_list <- list()

# Loop over the completed imputed data frames and combine them into a list
for (i in 1:10) {
        imputed_data <- complete(impute_model, i)
        names(imputed_data) <- paste0(names(imputed_data), i)
        imputed_data_list[[i]] <- imputed_data
}

# Combine the imputed data frames with the original FlucTotIV data frame
for (i in 1:10) {
        FlucTotIV[FlucTotIV$DV != ".", paste0(names(imputed_data_list[[i]]))] <- imputed_data_list[[i]]
}

# Delete the unecessary columns from my FlucTotIV dataframe
# Loop over the indices from 1 to 10
for (i in 1:10) {
        # Generate the column names
        age_col <- paste0("AGE", i)
        sex_col <- paste0("SEX", i)
        dv_col <- paste0("DV", i)
        
        # Remove the columns from the FlucTotIV data frame
        FlucTotIV <- subset(FlucTotIV, select = setdiff(names(FlucTotIV), c(age_col, sex_col, dv_col)))
}

# Fill the NAs with the values from their corresponding original columns
# loop through each i value
for (i in 1:10) {
        # create a logical vector indicating which rows have NA values in the BWi column
        na_idx <- is.na(FlucTotIV[,paste0("BW", i)])
        # check if there are any NA values in the BWi column
        if (sum(na_idx) > 0) {
                # subset the non-NA values in the BW column that correspond to the NA values in the BWi column
                bw_non_na <- FlucTotIV[!na_idx, "BW"]
                # check if bw_non_na is not empty
                if (length(bw_non_na) > 0) {
                        # replace the NA values in the BWi column with the corresponding non-NA values from the BW column
                        FlucTotIV[na_idx, paste0("BW", i)] <- bw_non_na[1:length(which(na_idx))]
                }
        }
}








# Then I count the NAs to make sure the dataset was succesfully imputed #
#sum(is.na(Fluc_NONMEM$BW))
#sum(is.na(fluco_total_imputed01$BW))

# After that I export 5 imputed datasets into 5 .csv files #
#fluco_total_list <- list(fluco_total_imputed01, fluco_total_imputed02, fluco_total_imputed03,fluco_total_imputed04,fluco_total_imputed05)
#for (i in 1:length(fluco_total_list)) {
        #write.csv(fluco_total_list[[i]], paste0("fluco_total_imputed0", i, ".csv"))
#}

#### Check the assumption of model Run019, that is CLr in patients who have CRRT lower than those who don't ####

################################################################################
#### These following codes are not important
################################################################################

setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/NONMEM Models/Luong_model/No_machine")
# First of all, read sdtab table (for parameters estimates and covariates)
sdtab022<-read.table("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/NONMEM Models/Luong_model/No_machine/sdtab022.csv",header=TRUE,sep=",")
#sdtab019 <- sdtab019 / 10000 #don't need this anymore
#sdtab[] <- lapply(sdtab, function(x) as.numeric(as.character(x))) - after converting columns in csv file into number, it should be fine
# Create density plot
library(ggplot2)
dens022<-ggplot(sdtab022, aes(x = CLr, fill = factor(CRRT))) + 
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        labs(title = "Density Plot of CLr by CRRT",
             x = "CLr",
             fill = "CRRT")
# Define custom color palette
color_palette <- c("#F8766D", "#00BFC4")
# Create boxplot with jitters
box022<-ggplot(sdtab022, aes(x = factor(CRRT), y = CLr)) +
        geom_boxplot(fill = "white", color = "gray40", outlier.shape = NA, size = 0.8, alpha = 0.8) +
        geom_jitter(aes(color = factor(CRRT)), width = 0.2, alpha = 0.7, size = 3.5) +
        scale_color_manual(values = color_palette) +
        labs(title = "Distribution of CLR by CRRT",
             x = "CRRT",
             y = "CLr",
             color = "CRRT") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.position = c(0.8, 0.85),
              legend.key.size = unit(1.2, "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.margin = unit(c(1, 1, 1, 1), "cm"))+theme(legend.position="none")
# Save density plot to PNG file
ggsave("density_CLr_022.png", plot = dens022, dpi = 300, width = 4, height = 5)
# Save box plot to PNG file
ggsave("boxplot_CLr_022.png", plot = box022, dpi = 300, width = 7, height = 5)

#### Here I plot the distribution of Scr (in Fluc_NONMEM_MAX) according CRRT status ####

# Density plot
library(ggplot2)
dens_SCr<-ggplot(subset(Fluc_NONMEM_MAX, DV != "."), aes(x = CREAT, fill = factor(CRRT))) + 
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        labs(title = "Density Plot of SCr by CRRT",
             x = "SCr (mg/L)",
             fill = "CRRT")

# Box plot
box_SCr<-ggplot(subset(Fluc_NONMEM_MAX, DV != "."), aes(x = factor(CRRT), y = CREAT)) +
geom_boxplot(fill = "white", color = "gray40", outlier.shape = NA, size = 0.8, alpha = 0.8) +
geom_jitter(aes(color = factor(CRRT)), width = 0.2, alpha = 0.7, size = 3.5) +
scale_color_manual(values = color_palette) +
labs(title = "Distribution of SCr by CRRT",
x = "CRRT",
y = "SCr (mg/dL)",
color = "CRRT") +
theme_minimal(base_size = 16) +
theme(plot.title = element_text(face = "bold", hjust = 0.5),
axis.title = element_text(face = "bold"),
legend.title = element_text(face = "bold"),
legend.position = c(0.8, 0.85),
legend.key.size = unit(1.2, "lines"),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank(),
plot.margin = unit(c(1, 1, 1, 1), "cm"))+theme(legend.position="none")

#Place the two plots right next to each other
library(gridExtra)
SCr_CRRT<-grid.arrange(dens_SCr, box_SCr, ncol = 2)

# Save the plot to PNG file
ggsave("SCr_CRRT.png", plot = SCr_CRRT, dpi = 300, width = 10, height = 5)

#### Check the assumption of model Run006, that is CL in patients who have CRRT lower than those who don't ####

# First of all, read sdtab table (for parameters estimates and covariates)
sdtab021<-read.table("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/NONMEM Models/Luong_model/No_machine/sdtab021.csv",header=TRUE,sep=",")
#sdtab006 <- sdtab006 / 10000
#sdtab[] <- lapply(sdtab, function(x) as.numeric(as.character(x))) - after converting columns in csv file into number, it should be fine
# Create density plot
library(ggplot2)
dens021<-ggplot(sdtab021, aes(x = CL, fill = factor(CRRT))) + 
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        labs(title = "Density Plot of CL by CRRT",
             x = "CL",
             fill = "CRRT")
# Define custom color palette
color_palette <- c("#F8766D", "#00BFC4")
# Create boxplot with jitters
box021<-ggplot(sdtab021, aes(x = factor(CRRT), y = CL)) +
        geom_boxplot(fill = "white", color = "gray40", outlier.shape = NA, size = 0.8, alpha = 0.8) +
        geom_jitter(aes(color = factor(CRRT)), width = 0.2, alpha = 0.7, size = 3.5) +
        scale_color_manual(values = color_palette) +
        labs(title = "Distribution of CL by CRRT",
             x = "CRRT",
             y = "CL",
             color = "CRRT") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.position = c(0.8, 0.85),
              legend.key.size = unit(1.2, "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.margin = unit(c(1, 1, 1, 1), "cm"))+theme(legend.position="none")
# Save density plot to PNG file
ggsave("density_CLr_021.png", plot = dens021, dpi = 300, width = 4, height = 5)
# Save box plot to PNG file
ggsave("boxplot_CLr_021.png", plot = box021, dpi = 300, width = 7, height = 5)


#### Check the assumption of model Run020, that is CL in patients who have CRRT lower than those who don't ####

# First of all, read sdtab table (for parameters estimates and covariates)
sdtab020<-read.table("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/NONMEM Models/Luong_model/No_machine/sdtab020.csv",header=TRUE,sep=",")
#sdtab020 <- sdtab020 / 10000
#sdtab[] <- lapply(sdtab, function(x) as.numeric(as.character(x))) - after converting columns in csv file into number, it should be fine
# Create density plot
library(ggplot2)
dens020<-ggplot(sdtab020, aes(x = CL, fill = factor(CRRT))) + 
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        labs(title = "Density Plot of CL by CRRT",
             x = "CL",
             fill = "CRRT")
# Define custom color palette
color_palette <- c("#F8766D", "#00BFC4")
# Create boxplot with jitters
box020<-ggplot(sdtab020, aes(x = factor(CRRT), y = CL)) +
        geom_boxplot(fill = "white", color = "gray40", outlier.shape = NA, size = 0.8, alpha = 0.8) +
        geom_jitter(aes(color = factor(CRRT)), width = 0.2, alpha = 0.7, size = 3.5) +
        scale_color_manual(values = color_palette) +
        labs(title = "Distribution of CL by CRRT",
             x = "CRRT",
             y = "CL",
             color = "CRRT") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.position = c(0.8, 0.85),
              legend.key.size = unit(1.2, "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              plot.margin = unit(c(1, 1, 1, 1), "cm"))+theme(legend.position="none")
# Save density plot to PNG file
ggsave("density_CLr_020.png", plot = dens020, dpi = 300, width = 4, height = 5)
# Save box plot to PNG file
ggsave("boxplot_CLr_020.png", plot = box020, dpi = 300, width = 7, height = 5)
################################################################################
#### End of the non-important codes
################################################################################



#### Now I am intereseted in plotting Concentration over time ####
##################################################################

#### First of all, DV over time ####

library(dplyr)
library(ggplot2)

sdtab020_CRRT <- sdtab020 %>%
        group_by(ID) %>%
        filter(any(CRRT == 1)) %>%
        mutate(DV = ifelse(TAD == 0 & row_number() != 1, NA, DV),
               TAD = ifelse(TAD == 0 & row_number() != 1, NA, TAD)) %>%
        select(ID, DV, TAD, TIME, IPRED, CRRT)
# Recode ID as a factor
sdtab020_CRRT$ID <- factor(as.integer(as.character(sdtab020_CRRT$ID)))
# Recode CRRT as a factor
sdtab020_CRRT$CRRT <- factor(sdtab020_CRRT$CRRT)

# Split random_ids into two groups of 5 IDs each
set.seed(123)
random_ids <- sample(levels(sdtab020_CRRT$ID), 10, replace = FALSE)
group1 <- random_ids[1:5]
group2 <- random_ids[6:10]

# Create two plots with 5 IDs each
library(gridExtra)
plot1DV <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group1)), aes(x = TIME, y = DV, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Concentration (mg/L)") +
        ggtitle("DV over Time by ID and CRRT status (Group 1)") +
        theme_bw()

plot2DV <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group2)), aes(x = TIME, y = DV, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Concentration (mg/L)") +
        ggtitle("DV over Time by ID and CRRT status (Group 2)") +
        theme_bw()

# Show the plots separately
#plot1DV
#plot2DV

#### This space is for drafting ####
library(ggplot2)

# Define a function to create the combined DV and SCr plot for each patient
create_plot <- function(id) {
        subset_data <- subset(Fluc_NONMEM_MAX, ID == id & DV != "." & !is.na(DV) & !is.na(CREAT))
        
        p <- ggplot(subset_data, aes(x = TIME, group = ID)) +
                geom_line(aes(y = DV, color = ID, shape = factor(CRRT))) +
                geom_line(aes(y = CREAT * 10, color = ID, linetype = factor(CRRT))) +
                scale_color_discrete(name = "ID") +
                scale_shape_manual(values = c(1, 2), name = "CRRT") +
                scale_linetype_manual(values = c(1, 2), name = "CRRT", guide = "none") +
                xlab("Time") +
                ylab("Concentration (mg/L)") +
                scale_y_continuous(name = "DV", sec.axis = sec_axis(~./10, name = "SCr (mg/dL)")) +
                ggtitle(paste("DV and SCr over Time by ID (", id, ")")) +
                theme_bw()
        
        return(p)
}

# Create a list of plots for each patient
plot_list <- lapply(random_ids, create_plot) #we already had random_ids above
                     

# Set seed for reproducibility
#set.seed(123)

# Randomly select 10 IDs
#random_ids <- sample(levels(FlucTotIV$ID), 10, replace = FALSE)

# Create a list to store the plots
plots <- list()

# Loop through each ID and create a plot
for (i in random_ids) {
        
        # Subset the data for the current ID
        data_subset <- subset(Fluc_NONMEM_MAX, ID == i & DV != ".")
        
        # Create the plot
        plot <- ggplot(data_subset, aes(x = TIME)) +
                geom_line(aes(y = DV, group = CRRT, color = ID, linetype = CRRT)) +
                geom_line(aes(y = CREAT * 10, group = CRRT, color = ID, linetype = CRRT), yaxis = "right") +
                scale_color_discrete(name = "ID") +
                scale_linetype_manual(name = "CRRT", values = c(1, 2)) +
                scale_y_continuous(name = "DV", sec.axis = sec_axis(~./10, name = "SCr")) +
                xlab("Time") +
                ggtitle(paste0("Combined DV and SCr over Time by ID (", i, ") and CRRT status")) +
                theme_bw()
        
        # Add the plot to the list
        plots[[i]] <- plot
}

ggplot(na.omit(subset(Fluc_NONMEM_MAX, ID %in% random_ids)), aes(x = TIME)) +
        geom_line(aes(y = DV, group = ID, color = ID, linetype = factor(CRRT))) +
        geom_line(aes(y = CREAT * 10, group = ID, color = ID, linetype =factor(CRRT)), mapping = aes(y = ..y.. / 10)) +
        scale_y_continuous(
                name = "DV", sec.axis = sec_axis(~./10, name = "SCr")
        ) +
        xlab("Time") +
        ggtitle("DV and SCr over Time by ID and CRRT status") +
        theme_bw()


# Arrange the plots into a grid
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)



#### Second of all, IPRED over TIME in exact same IDs ####

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED020 <- ggplot((subset(sdtab020_CRRT, ID %in% group1)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED020 <- ggplot((subset(sdtab020_CRRT, ID %in% group2)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 2)") +
        theme_bw()

# Arrange the plots side by side
DV_IPRED_020_01<-grid.arrange(plot1DV, plot1IPRED020, ncol = 2)
DV_IPRED_020_02<-grid.arrange(plot2DV, plot2IPRED020, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TIME 020_01.png", plot = DV_IPRED_020_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TIME 020_02.png", plot = DV_IPRED_020_02, dpi = 300, width = 10, height = 5)

#### Third of all, DV over TAD in exact same IDs ####

# Create two plots with 5 IDs each
plot1DV_TAD <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group1)), aes(x = TAD, y = DV, group = ID, color = ID, shape = factor(CRRT))) +
geom_line() +
geom_point(size = 2) +
xlab("TAD") +
ylab("Concentration (mg/L)") +
ggtitle("DV over TAD by ID and CRRT status (Group 1)") +
theme_bw()

plot2DV_TAD <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group2)), aes(x = TAD, y = DV, group = ID, color = ID, shape = factor(CRRT))) +
geom_line() +
geom_point(size = 2) +
xlab("TAD") +
ylab("Concentration (mg/L)") +
ggtitle("DV over TAD by ID and CRRT status (Group 2)") +
theme_bw()

#### Fourth of all, IPRED over TAD in exact same IDs ####

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED020_TAD <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group1)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED020_TAD <- ggplot(na.omit(subset(sdtab020_CRRT, ID %in% group2)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 2)") +
        theme_bw()
# Arrange the plots side by side
DV_IPRED_020_TAD_01<-grid.arrange(plot1DV_TAD, plot1IPRED020_TAD, ncol = 2)
DV_IPRED_020_TAD_02<-grid.arrange(plot2DV_TAD, plot2IPRED020_TAD, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TAD 020_01.png", plot = DV_IPRED_020_TAD_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TAD 020_02.png", plot = DV_IPRED_020_TAD_02, dpi = 300, width = 10, height = 5)


#### Like wise, we do that with model 006 and 019, but we only need IPRED plot this time ####
#############################################################################################

#### DV & IPRED over TIME in exact same IDs, MOD 006 ####

library(dplyr)
library(ggplot2)

sdtab006_CRRT <- sdtab006 %>%
        group_by(ID) %>%
        filter(any(CRRT == 1)) %>%
        mutate(DV = ifelse(TAD == 0 & row_number() != 1, NA, DV),
               TAD = ifelse(TAD == 0 & row_number() != 1, NA, TAD)) %>%
        select(ID, DV, TAD, TIME, IPRED, CRRT)
# Recode ID as a factor
sdtab006_CRRT$ID <- factor(as.integer(as.character(sdtab006_CRRT$ID)))
# Recode CRRT as a factor
sdtab006_CRRT$CRRT <- factor(sdtab006_CRRT$CRRT)

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED006 <- ggplot((subset(sdtab006_CRRT, ID %in% group1)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED006 <- ggplot((subset(sdtab006_CRRT, ID %in% group2)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 2)") +
        theme_bw()

# Arrange the plots side by side
DV_IPRED_006_01<-grid.arrange(plot1DV, plot1IPRED006, ncol = 2)
DV_IPRED_006_02<-grid.arrange(plot2DV, plot2IPRED006, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TIME 006_01.png", plot = DV_IPRED_006_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TIME 006_02.png", plot = DV_IPRED_006_02, dpi = 300, width = 10, height = 5)

#### DV & IPRED over TAD in exact same IDs, MOD 006 ####

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED006_TAD <- ggplot(na.omit(subset(sdtab006_CRRT, ID %in% group1)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED006_TAD <- ggplot(na.omit(subset(sdtab006_CRRT, ID %in% group2)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 2)") +
        theme_bw()
# Arrange the plots side by side
DV_IPRED_006_TAD_01<-grid.arrange(plot1DV_TAD, plot1IPRED006_TAD, ncol = 2)
DV_IPRED_006_TAD_02<-grid.arrange(plot2DV_TAD, plot2IPRED006_TAD, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TAD 006_01.png", plot = DV_IPRED_006_TAD_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TAD 006_02.png", plot = DV_IPRED_006_TAD_02, dpi = 300, width = 10, height = 5)


#### DV & IPRED over TIME in exact same IDs, MOD 019 ####

library(dplyr)
library(ggplot2)

sdtab019_CRRT <- sdtab019 %>%
        group_by(ID) %>%
        filter(any(CRRT == 1)) %>%
        mutate(DV = ifelse(TAD == 0 & row_number() != 1, NA, DV),
               TAD = ifelse(TAD == 0 & row_number() != 1, NA, TAD)) %>%
        select(ID, DV, TAD, TIME, IPRED, CRRT)
# Recode ID as a factor
sdtab019_CRRT$ID <- factor(as.integer(as.character(sdtab019_CRRT$ID)))
# Recode CRRT as a factor
sdtab019_CRRT$CRRT <- factor(sdtab019_CRRT$CRRT)

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED019 <- ggplot((subset(sdtab019_CRRT, ID %in% group1)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED019 <- ggplot((subset(sdtab019_CRRT, ID %in% group2)), aes(x = TIME, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over Time by ID and CRRT status (Group 2)") +
        theme_bw()

# Arrange the plots side by side
DV_IPRED_019_01<-grid.arrange(plot1DV, plot1IPRED019, ncol = 2)
DV_IPRED_019_02<-grid.arrange(plot2DV, plot2IPRED019, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TIME 019_01.png", plot = DV_IPRED_019_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TIME 019_02.png", plot = DV_IPRED_019_02, dpi = 300, width = 10, height = 5)

#### DV & IPRED over TAD in exact same IDs, MOD 019 ####

# Create two plots with 5 IDs each
library(gridExtra)
plot1IPRED019_TAD <- ggplot(na.omit(subset(sdtab019_CRRT, ID %in% group1)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 1)") +
        theme_bw()

plot2IPRED019_TAD <- ggplot(na.omit(subset(sdtab019_CRRT, ID %in% group2)), aes(x = TAD, y = IPRED, group = ID, color = ID, shape = factor(CRRT))) +
        geom_line() +
        geom_point(size = 2) +
        xlab("Time") +
        ylab("Predicted Concentration (mg/L)") +
        ggtitle("IPRED over TAD by ID and CRRT status (Group 2)") +
        theme_bw()
# Arrange the plots side by side
DV_IPRED_019_TAD_01<-grid.arrange(plot1DV_TAD, plot1IPRED019_TAD, ncol = 2)
DV_IPRED_019_TAD_02<-grid.arrange(plot2DV_TAD, plot2IPRED019_TAD, ncol = 2)
# Save these two plots to png
ggsave("DV vs IPRED over TAD 019_01.png", plot = DV_IPRED_019_TAD_01, dpi = 300, width = 10, height = 5)
ggsave("DV vs IPRED over TAD 019_02.png", plot = DV_IPRED_019_TAD_02, dpi = 300, width = 10, height = 5)


# Create a new data frame with selected columns and remove rows where DV is NA
#sdtab020_plot <- subset(sdtab020_CRRT, !is.na(DV), select = c("ID", "TIME", "DV", "CRRT"))
# Recode CRRT as a factor
#sdtab020_plot$CRRT <- factor(sdtab020_plot$CRRT)
# Recode the first row of each ID in DV to 0 instead of NA
#library(dplyr)
#sdtab020_plot <- sdtab020_plot %>%
        #group_by(ID) %>%
        #mutate(DV = replace(DV, row_number() == 1, 0))
# Create the plot
#ggplot(sdtab020_plot, aes(x = TIME, y = DV, group = ID, color = CRRT)) +
        #geom_line() +
        #scale_color_manual(values = c("#F8766D", "#00BA38")) +
        #labs(x = "Time", y = "Concentration", color = "CRRT") +
        #theme_bw()

## Correlation coefficient between BW and CG ##
#BW_complete <- FlucTotIV$BW[!is.na(FlucTotIV$BW) & !is.na(FlucTotIV$CG)]
#CG_complete_1 <- FlucTotIV$CG[!is.na(FlucTotIV$BW) & !is.na(FlucTotIV$CG)]
#cor(BW_complete, CG_complete_1)

## Correlation coefficient between LENGTH and CG ##
#LENGTH_complete <- FlucTotIV$LENGTH[!is.na(FlucTotIV$LENGTH) & !is.na(FlucTotIV$CG)]
#CG_complete_2 <- FlucTotIV$CG[!is.na(FlucTotIV$LENGTH) & !is.na(FlucTotIV$CG)]
#cor(LENGTH_complete, CG_complete_2)

#We replace NAs in BW and LENGTH columns by -99

FlucTotIV$LENGTH[is.na(FlucTotIV$LENGTH)] <- -99
FlucTotIV$BW[is.na(FlucTotIV$BW)] <- -99

### calculate how many percents of patients have C trough below 15 ###

# Subset data to include only rows where TAD is between 23 and 25
sub_data <- subset(FlucTotIV, TAD >= 23 & TAD <= 25)

# Remove any NA values from DV
dv_no_na <- na.omit(sub_data$DV)

# Calculate percentage of DV values less than 15
percent_less_than_15 <- mean(dv_no_na < 15) * 100 #46.8%

# Calculate percentage of DV values less than 7.5
percent_less_than_7.5 <- mean(dv_no_na < 7.5) * 100 #18.99%

# Print the result
cat(paste0("Percentage of DV column values less than 15 among those with TAD between 23 and 25: ", percent_less_than_15, "%"))

#### Result for PAGE abstract ####

# Extract subset of data with valid DV values after 23-25 hours
subset_data <- FlucTotIV[!is.na(FlucTotIV$DV) & FlucTotIV$TIME >= 23 & FlucTotIV$TIME <= 25,]

# Calculate percentage of patients with DV between 7.5 and 15 mg/L
percentage7.5 <- 100 * sum(subset_data$DV < 7.5) / nrow(subset_data)
percentage15 <- 100 * sum(subset_data$DV < 15) / nrow(subset_data) #81.8%

# Print result
cat("Percentage of patients with DV below 7.5 after 23-25 hours:", round(percentage7.5, 2), "%\n") #%36.4% patients did not achieve target within 24h

# Calculate baseline FFM & eGFR
library(dplyr)
baseline_data <- Fluc_NONMEM_COM%>%filter(OCC==1&FFM>0)%>%distinct(ID, .keep_all = TRUE)
summary(baseline_data$FFM,na.rm=TRUE)
summary(baseline_data$CKDEPI_NoD,na.rm=TRUE)
shapiro.test(baseline_data$FFM)
mean(baseline_data$FFM) #57.3
sd(baseline_data$FFM) #10.2
shapiro.test(na.omit(baseline_data$CKDEPI_NoD))
summary(baseline_data$CKDEPI_NoD) #median 88.10, q1-q3: 53.22-107.89

### Answer Isabel's question ###
#CL24 is missing in 5/8 studies (only available in 3 studies - 1,4,7)
39+5+21 #65/177 patients

#### These following codes are rendered unimportant for now ####

setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/NONMEM Models/Luong_model")
FLUCO<-read.csv("sdtab085.csv",sep=",")

library(ggplot2)

# Convert EVENT1 to factor
FLUCO$EVENT1 <- factor(FLUCO$EVENT1, labels = c("1", "2"))

# Plot CL over TIME, stratifying on EVENT1
ggplot(FLUCO, aes(x = TIME, y = CL, color = EVENT1)) + 
        geom_point() + 
        geom_smooth(se = FALSE) +
        labs(x = "Time (hours)", y = "Clearance (continuous)") +
        scale_color_discrete(name = "Event1")

# Convert EVENT1 and CRRT to factors
FLUCO$EVENT1 <- factor(FLUCO$EVENT1, levels = c(1, 2), labels = c("Event 1", "Event 2"))
FLUCO$CRRT <- factor(FLUCO$CRRT, levels = c(0, 1), labels = c("No CRRT", "CRRT"))

# Plot clearance (CL) over time, stratified by EVENT1 and CRRT status
library(ggplot2)
ggplot(FLUCO, aes(x = TIME, y = CL, color = EVENT1, linetype = CRRT)) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Time (hours)", y = "Clearance (continuous)", color = "Event 1", linetype = "CRRT")

#### Here I want to test if the after-last-concentration observations are used in NONMEM ####

# First, I eliminate all the after-the-last concentration observations from my dataset, and run it in NONMEM
library(dplyr)

Fluc_NONMEM_MED_test <- Fluc_NONMEM_MED %>%
        arrange(ID, TIME) %>%
        group_by(ID) %>%
        filter(row_number() <= max(which(DV!="."))) %>%
        ungroup()
write.csv(Fluc_NONMEM_MED_test, "Fluc_NONMEM_MED_test.csv",quote=F,row.names = FALSE)
# The result is that OFV is the same with slightly different parameter estimates' precision

# Next, I added 1000 dosing events after the ID 101 to see if they are used in NONMEM
library(dplyr)
# Find the index of the last observation of patient ID 101
last_obs_idx <- tail(which(Fluc_NONMEM_MED_test$ID == 101), 1)

# Select the row to add
row_to_add <- Fluc_NONMEM_MED_test %>% filter(ID == 101, DV == ".") %>% slice(1)

# Convert ID column to character
row_to_add$ID <- as.character(row_to_add$ID)

# Get the time value of the last observation of ID 101
last_obs_time <- Fluc_NONMEM_MED_test$TIME[last_obs_idx]

# Repeat the row 1000 times and modify TIME values
rows_to_add <- lapply(1:1000, function(i) {
        new_row <- row_to_add
        new_row$TIME <- last_obs_time + i
        new_row
}) %>% 
        bind_rows()

# Convert ID column to integer
rows_to_add$ID <- as.integer(rows_to_add$ID)

# Add the rows after the last observation of patient ID 101
Fluc_NONMEM_MED_test2 <- Fluc_NONMEM_MED_test %>% 
        add_row(rows_to_add, .before = last_obs_idx + 1) %>% 
        arrange(ID, TIME)
write.csv(Fluc_NONMEM_MED_test2, "Fluc_NONMEM_MED_test2.csv",quote=F,row.names = FALSE) #The OFV and parameter estimates are still unchanged
# Now I confirm that the after-the-last-concentration observations are not used!


## I test the effect of covariate CKDEPI_NoD on dosing events on my popPK model
# replace the values of CKDEPI_NoD where AMT is not "." with 500

Fluc_NONMEM_MED$CKDEPI_NoD[Fluc_NONMEM_MED$AMT != "."] <- 500
# save the updated dataset
write.csv(Fluc_NONMEM_MED, "Fluc_NONMEM_MED_test.csv")
median(Fluc_NONMEM_MED$CKDEPI_NoD[Fluc_NONMEM_MED$DV!="."])
median(Fluc_NONMEM_MED$CKDEPI_NoD) #equals 257


###############################################
#### We create table 1 using the following code
###############################################

#Before report dose, fill dose in every row
DataDVAMT1 <- FlucTotIV
#DataDVAMT1$AMT<- as.numeric(as.character(DataDVAMT1$AMT)) #AMT to numeric
#for (i in unique(DataDVAMT1$ID)){
        #DataDVAMT1$AMT[DataDVAMT1$ID==i] = na.locf(DataDVAMT1$AMT[DataDVAMT1$ID==i],na.rm=F, fromLast = F) #na.locf could be used to replace user-written function
#}

#create a new dataset for summary statistics
datasum <- unique(DataDVAMT1 %>% select(ID,OCC,TIME,TAD,DV,APACHE,BW,LENGTH,AGE,SEX,CREAT,
                                       CKDEPI,CL24,GGT,AFT,AST,ALT,BILI,ALB,SOFA,
                                       FLUID,BMI,ARCAlg,HOSPITAL,CRRT)) #Now I don't have CG anymore

##Probably need to separate variables measured 1 time and those measured multiple times

#1 time: ID, APACHE, LENGTH, AGE, SEX, HOSPITAL

#Multiple times: ID,DV,BW,CREAT, CKDEPI,CL24,GGT,AFT,AST,
#ALT,BILI,ALB,SOFA,FLUID,BMI,CG,ARCAlg,HOSPITAL

datasum<-datasum %>% filter (DV!=".") #for sampling events
datasum<-datasum  #for dosing event
#1 measurement for all data
datasum1<-unique(datasum %>% select (ID, APACHE, LENGTH, AGE, SEX, HOSPITAL))
#assign label to sex
datasum1$SEX <- 
        factor(datasum1$SEX, levels=c(1,2),
               labels=c("Male", 
                        "Female"))
label(datasum1$LENGTH) <- "HEIGHT"
#different measurements per each occasion data
datasum2<-unique(datasum %>% select (ID,OCC,TIME,DV,BW,CREAT, CKDEPI,CL24,GGT,AFT,AST,
                                     ALT,BILI,ALB,SOFA,FLUID,BMI,HOSPITAL,CRRT)) #and no CG here either

## Displaying different statistics for different variables

# Render function
rndr <- function(x, name, ...) {
if (!is.numeric(x)) return(render.categorical.default(x))
what <- switch(name,
BW = "Median [Q1 - Q3]",
CREAT  = "Mean (SD)")
parse.abbrev.render.code(c("", what))(x)
}

table1(~ APACHE + LENGTH + AGE + SEX|HOSPITAL, data=datasum1,render.continuous=
               c(.="Median [Q1-Q3]"))
table1(~ BW + CREAT + CKDEPI + CL24 + GGT + AFT + AST +
               ALT + BILI + ALB + SOFA + FLUID + BMI+CRRT|HOSPITAL, data=datasum2,
       render.continuous=c(.="Median [Q1-Q3]"))
table1(~ BW + CREAT + CKDEPI +CRRT|HOSPITAL, data=datasum2,
       render.continuous=c(.="Median [Q1-Q3]"))

## CREAT 2 divided by CRRT state
## Assign a new column to discriminate between 32 patients who have at least one CRRT vs other
crrt_patients <- unique(FlucTotIV_clean_imputed[FlucTotIV_clean_imputed$CRRT == 1, "ID"])
library(table1)
library(dplyr)
FlucTotIV_clean_imputed_crrt<-FlucTotIV_clean_imputed%>%filter(ID%in%crrt_patients)
FlucTotIV_clean_imputed_nocrrt<-FlucTotIV_clean_imputed%>%filter(!ID%in%(crrt_patients))
table1(~ CREAT2|HOSPITAL, data=FlucTotIV_clean_imputed_nocrrt,
       render.continuous=c(.="Median [Q1-Q3]")) #74.3%
table1(~ CREAT2|HOSPITAL, data=FlucTotIV_clean_imputed_crrt,
       render.continuous=c(.="Median [Q1-Q3]")) #69.8%


# Then I will impute CREAT2 independently between these 2 datasets
# Create a new data frame with only the specified predictors and CREAT2
# For CRRT
library(mice)
impute_CREAT2 <- FlucTotIV_clean_imputed_crrt[, c("AGE", "SEX", "BW2", "DV", "TIME", "HOSPITAL", "CRRT", "CREAT2","OCC","LENGTH")]

# Set the seed for reproducibility
set.seed(123)

# Perform multiple imputations using pmm method
imputed_CREAT2 <- mice(impute_CREAT2, m = 10,maxit=50)

# Create new columns in the original data set for each imputed CREAT2 value
for (i in 1:10) {
        FlucTotIV_clean_imputed_crrt[[paste0("CREAT", sprintf("%02d", i))]] <- (complete(imputed_CREAT2, action = i))$CREAT2
}
# For non CRRT
impute_CREAT2 <- FlucTotIV_clean_imputed_nocrrt[, c("AGE", "SEX", "BW2", "DV", "TIME", "HOSPITAL", "CREAT2","OCC","LENGTH")]

# Set the seed for reproducibility
set.seed(123)

# Perform multiple imputations using pmm method
imputed_CREAT2 <- mice(impute_CREAT2, m = 10,maxit=50)

# Create new columns in the original data set for each imputed CREAT2 value
for (i in 1:10) {
        FlucTotIV_clean_imputed_nocrrt[[paste0("CREAT", sprintf("%02d", i))]] <- (complete(imputed_CREAT2, action = i))$CREAT2
}

# Then merge 2 imputed datasets
# Merge imputed_first back into FlucTotIV_clean based on the ID and HOSPITAL columns
FlucTotIV_clean_imputed2 <- rbind(FlucTotIV_clean_imputed_crrt, FlucTotIV_clean_imputed_nocrrt)

### I make CREAT plots to compare the performance of this imputing strategy with the previous one

## Here I plot imputed CREATs over TIME 
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 01 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT01, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT01, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 02
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 02 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT02, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT02, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 03
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 03 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT03, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT03, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}


# Impute 04
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 04 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT04, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT04, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 05
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 05 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT05, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT05, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 06
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 06 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT06, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT06, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 07
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 07 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT07, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT07, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 08
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 08 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT08, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT08, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 09
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 09 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT09, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT09, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}

# Impute 10
# Get unique IDs with CRRT == 1 only
setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/My datasets and R codes/Plots/CREAT impute 10 dataset2")
library(dplyr)
FlucTotIV_clean_imputed2 <- FlucTotIV_clean_imputed2 %>%filter(!HOSPITAL%in%c(3,4))
crrt_1_ids <- unique(FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 1 & !FlucTotIV_clean_imputed2$ID %in% FlucTotIV_clean_imputed2$ID[FlucTotIV_clean_imputed2$CRRT == 0]]) #ids of those who have CRRT all the time 

library(ggplot2)

# Select 5 random patients from crrt_IDs
set.seed(123) # set seed for reproducibility
crrt_IDs_1 <- sample(crrt_1_ids, size = 5, replace = FALSE)

# Create a subset of the original data frame with the selected patients
CRRT_subset_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_IDs_1)
CRRT_subset_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% crrt_1_ids) %>% 
        filter(!(ID %in% crrt_IDs_1))

# First plot
creat_time_crrt_01<-ggplot() +
        geom_line(data = CRRT_subset_1, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_1, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 Random Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_1[CRRT_subset_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_01.png", plot = creat_time_crrt_01, dpi = 300, width = 10, height = 5)

# Second plot
creat_time_crrt_02<-ggplot() +
        geom_line(data = CRRT_subset_2, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_subset_2, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(17)) +
        scale_color_manual(values = c("red")) +
        labs(title = "CREAT over Time for 5 remaing Patients with CRRT on all the time on both sampling & dosing days", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_subset_2[CRRT_subset_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_crrt_02.png", plot = creat_time_crrt_02, dpi = 300, width = 10, height = 5)


#### I make the same plot for those who are on CRRT on & off on the whole treatment period

# First selecting the ID of those who have CRRT non-missing and CRRT on and off
library(dplyr)
miscel_IDs <- FlucTotIV_clean_imputed2 %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 16 

#library(ggplot2)
# Select 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_1 <- sample(miscel_IDs, size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_1)

# First plot
creat_time_miscel_01<-ggplot() +
        geom_line(data = CRRT_miscel_1, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_1, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 01", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_1[CRRT_miscel_1$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_01.png", plot = creat_time_miscel_01, dpi = 300, width = 10, height = 5)

# Select another 6 random patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_2 <- sample(setdiff(miscel_IDs, miscel_IDs_1), size = 6, replace = FALSE)

# create a subset of the original data frame with the 6 patients
CRRT_miscel_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_2)

# Second plot
creat_time_miscel_02<-ggplot() +
        geom_line(data = CRRT_miscel_2, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_2, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 02", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_2[CRRT_miscel_2$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_02.png", plot = creat_time_miscel_02, dpi = 300, width = 10, height = 5)

## And finally, select the remaining 4 patients from miscel_IDs
set.seed(123) # set seed for reproducibility
miscel_IDs_3 <- setdiff(miscel_IDs, c(miscel_IDs_1,miscel_IDs_2))

# create a subset of the original data frame with the 6 patients
CRRT_miscel_3 <- FlucTotIV_clean_imputed2 %>% 
        filter(ID %in% miscel_IDs_3)

# Third plot
creat_time_miscel_03<-ggplot() +
        geom_line(data = CRRT_miscel_3, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
        geom_point(data = CRRT_miscel_3, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
        scale_shape_manual(values = c(16, 17)) +
        scale_color_manual(values = c("black", "red")) +
        labs(title = "CREAT over Time for Patients with CRRT on and off 03", x = "Time (hours)", y = "CREAT (mg/dL)") +
        theme_bw() +
        geom_rect(data = CRRT_miscel_3[CRRT_miscel_3$CRRT == 1, ], aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                  fill = "grey", alpha = 0.2) +
        facet_wrap(~ ID, scales = "free") +
        guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT")) 
ggsave("creat_time_miscel_03.png", plot = creat_time_miscel_03, dpi = 300, width = 10, height = 5)

## After that, select IDs of those who don't have CRRT
nonCRRT_IDs<-setdiff(unique(FlucTotIV_clean_imputed2$ID),c(crrt_1_ids,miscel_IDs))

## I will plot per hospital because there might be similar patterns in the way they deal with missing CREAT
## First of all, hospital 1
library(dplyr)

hospital_1 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_1)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 01 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_1[CRRT_non_1$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos1_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_01, dpi = 300, width = 10, height = 5)
}

## Second of all, hospital 2
library(dplyr)

hospital_2 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_2)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 02 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_2[CRRT_non_2$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos2_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_02, dpi = 300, width = 10, height = 5)
}

## Third of all, hospital 5
library(dplyr)

hospital_5 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_5)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_5[CRRT_non_5$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos5_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_05, dpi = 300, width = 10, height = 5)
}

## Fourth of all, hospital 6
hospital_6 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_6)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_6[CRRT_non_6$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos6_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_06, dpi = 300, width = 10, height = 5)
}

## Fifth of all, hospital 7
hospital_7 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_7)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_7[CRRT_non_7$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos7_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_07, dpi = 300, width = 10, height = 5)
}

## Sixth of all, hospital 8
hospital_8 <- FlucTotIV_clean_imputed2 %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_clean_imputed2 %>% 
                filter(ID %in% nonCRRT_IDs_8)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8, aes(x = TIME, y = CREAT10, group = ID, color = factor(CRRT)), size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT10, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL)") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}
                                 
## Table for concentration
table(datasum$DV)
table1(~ DV|HOSPITAL, data=datasum,render.continuous=
               c(.="Median [Q1-Q3]"))
# Trough concentration only - Select trough levels = TAD between 23 and 25
trough<- datasum %>% filter (!is.na(DV)) %>% filter(TAD>23&TAD<25)
table1(~ DV|HOSPITAL, data=trough,render.continuous=
               c(.="Median [Q1-Q3]"))

###################################
#### Imputed dataset
###################################

FlucTotIVimputed <- read.csv("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazole/Databanken/Totaal/Imputed dataset/DatabankFluc_Tot_IV_nonmem_finaal.csv", na = "-99")

#Summarise dataset
head(FlucTotIVimputed)
str(FlucTotIVimputed)
summary(FlucTotIVimputed)

#Summarise covariates including: body weight (BW), body mass index (BMI), 
#creatinine clearance (CG), estimated glomerular filtration rate (CKDEPI), 
#aspartate transaminase (AST), total billirubin (BILI), gamma-glutamyl transferase (GGT)
#and the predicted probability of having augmented renal clearance on the next day (ARCAlg)

library(table1)
library(dplyr)
library(zoo) #na.locf function
library(rstatix)

#factor the basic variable that we're interested in
FlucTotIVimputed$HOSPITAL<-
        factor(FlucTotIVimputed$HOSPITAL,levels=c(1,2,3,4,5,6,7,8),
               labels=c("Study 1","Study 2","Study 3","Study 4","Study 5",
                        "Study 6","Study 7","Study 8"))
#rename LENGHT column into LENGTH
colnames(FlucTotIVimputed)[colnames(FlucTotIVimputed) == "LENGHT"] ="LENGTH"

#Convert data into the correct type
FlucTotIVimputed$DV <- as.numeric(as.character(FlucTotIVimputed$DV))
FlucTotIVimputed$BW <- as.numeric(as.character(FlucTotIVimputed$BW))
FlucTotIVimputed$BMI <- as.numeric(as.character(FlucTotIVimputed$BMI))
FlucTotIVimputed$BSA <- as.numeric(as.character(FlucTotIVimputed$BSA))
FlucTotIVimputed$LENGTH <- as.numeric(as.character(FlucTotIVimputed$LENGTH))
FlucTotIVimputed$CREAT <- as.numeric(as.character(FlucTotIVimputed$CREAT))
FlucTotIVimputed$CKDEPI <- as.numeric(as.character(FlucTotIVimputed$CKDEPI))
FlucTotIVimputed$CG <- as.numeric(as.character(FlucTotIVimputed$CG))
FlucTotIVimputed$BILI <- as.numeric(as.character(FlucTotIVimputed$BILI))
FlucTotIVimputed$AST <- as.numeric(as.character(FlucTotIVimputed$AST))
FlucTotIVimputed$GGT <- as.numeric(as.character(FlucTotIVimputed$GGT))
FlucTotIVimputed$FLUID <- as.numeric(as.character(FlucTotIVimputed$FLUID))
FlucTotIVimputed$ADMIN <- as.factor(FlucTotIVimputed$ADMIN) #I added from here
FlucTotIVimputed$CRRT <- as.factor(FlucTotIVimputed$CRRT)
FlucTotIVimputed$IHD <- as.factor(FlucTotIVimputed$IHD)
FlucTotIVimputed$ARCAlg <- as.factor(FlucTotIVimputed$ARCAlg)

#Before report dose, fill dose in every row
DataDVAMT1 <- FlucTotIVimputed
DataDVAMT1$AMT<- as.numeric(as.character(DataDVAMT1$AMT)) #AMT to numeric
for (i in unique(DataDVAMT1$ID)){
        DataDVAMT1$AMT[DataDVAMT1$ID==i] = na.locf(DataDVAMT1$AMT[DataDVAMT1$ID==i],na.rm=F, fromLast = F) #na.locf could be used to replace user-written function
}

#create a new dataset for summary statistics
datasum <- unique(DataDVAMT1 %>% select(ID,OCC,TIME,TAD,DV,APACHE,BW,LENGTH,AGE,SEX,CREAT,
                                        CKDEPI,CL24,GGT,AFT,AST,ALT,BILI,ALB,SOFA,
                                        FLUID,BMI,CG,ARCAlg,HOSPITAL))

##Probably need to separate variables measured 1 time and those measured multiple times

#1 time: ID, APACHE, LENGTH, AGE, SEX, HOSPITAL

#Multiple times: ID,DV,BW,CREAT, CKDEPI,CL24,GGT,AFT,AST,
#ALT,BILI,ALB,SOFA,FLUID,BMI,CG,ARCAlg,HOSPITAL

datasum<-datasum %>% filter (!is.na(DV))
#1 measurement for all data
datasum1<-unique(datasum %>% select (ID, APACHE, LENGTH, AGE, SEX, HOSPITAL))
#assign label to sex
datasum1$SEX <- 
        factor(datasum1$SEX, levels=c(1,2),
               labels=c("Male", 
                        "Female"))
label(datasum1$LENGTH) <- "HEIGHT"
#different measurements per each occasion data
datasum2<-unique(datasum %>% select (ID,OCC,TIME,DV,BW,CREAT, CKDEPI,CL24,GGT,AFT,AST,
                                     ALT,BILI,ALB,SOFA,FLUID,BMI,CG,HOSPITAL))

## Displaying different statistics for different variables

# Render function
rndr <- function(x, name, ...) {
        if (!is.numeric(x)) return(render.categorical.default(x))
        what <- switch(name,
                       BW = "Median [Q1 - Q3]",
                       CREAT  = "Mean (SD)")
        parse.abbrev.render.code(c("", what))(x)
}

# Apply render function 
table1(~ BW + CREAT | HOSPITAL, data=datasum2,
       render=rndr)

## Test normality

# The whole dataset
test <- datasum %>% shapiro_test(LENGTH, AGE,BW,CREAT, CKDEPI,
                                 GGT,AST,BILI,SOFA,FLUID,BMI,CG)
test

# Each individual dataset
study1 <-datasum %>% filter(HOSPITAL=="Study 1")
test1 <- study1 %>% shapiro_test(LENGTH,AGE,BW,CREAT,CKDEPI,
                                 GGT,AST,BILI,SOFA,FLUID,BMI,CG)
test1

# As all continuous variables are not normally distributed, we report medidan and IQR

table1(~  LENGTH + AGE + SEX|HOSPITAL, data=datasum1,render.continuous=
               c(.="Median [Q1-Q3]"))
table1(~ BW + CREAT + CKDEPI + GGT + AST +
                BILI  + SOFA + FLUID + BMI + CG|HOSPITAL, data=datasum2,
       render.continuous=c(.="Median [Q1-Q3]"))

## Table for concentration
table(datasum$DV)
table1(~ DV|HOSPITAL, data=datasum,render.continuous=
               c(.="Median [Q1-Q3]"))
# Trough concentration only - Select trough levels = TAD between 23 and 25
trough<- datasum %>% filter (!is.na(DV)) %>% filter(TAD>23&TAD<25)
table1(~ DV|HOSPITAL, data=trough,render.continuous=
               c(.="Median [Q1-Q3]"))







