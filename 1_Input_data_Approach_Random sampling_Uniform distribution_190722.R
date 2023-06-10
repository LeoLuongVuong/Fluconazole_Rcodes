# Clear all objects
rm(list = ls())
gc(verbose=T)

# remove.packages("ggplot2")

# Packages installation
### install.packages("readxl")
### install.packages("writexl")
### install.packages("dplyr")
### install.packages("zoo")
### install.packages("tidyr")
### install.packages("ggplot2",dep=T)
### install.packages("reshape2")
### install.packages("writexl")
### install.packages("ggpubr")
### install.packages("gridExtra")
### install.packages("fitdistrplus")
### install.packages("tidyverse")
### install.packages("patchwork")
### install.packages("moments")
### install.packages("bestNormalize")
### install.packages("JWileymisc")
### install.packages("MASS")
### install.packages("GGally")
### install.packages("survival")
### install.packages("truncnorm")
### install.packages("tmvtnorm")
### install.packages("psych")
### install.packages("ragg")

.libPaths()

# Required library
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(zoo)
library(writexl)
library(ggpubr)
library(readxl)
library(tidyr)
library(fitdistrplus)
library(gridExtra)
library(tidyverse)
library(patchwork)
library(moments)
library(truncnorm)
library(JWileymisc)
library(MASS)
library(GGally)
library(mvtnorm)
library(Matrix)
library(stats4)
library(gmm)
library(sandwich)
library(tmvtnorm)
library(psych)
library(moments)
library(ragg)

# Setting
setwd("C:/Users/u0147124/OneDrive - KU Leuven/PhD_KU Leuven/3_Amikacin_ER_PhD_KU Leuven/Data analysis")


# Read the excel document
dat1.0 <- readxl::read_excel("Dr Sabrina_NONMEM database -12082019-excldialyse.xlsx")

# Rename the columns
colnames(dat1.0)
colnames(dat1.0)[which(names(dat1.0) == "TBW (kg)")] <- "TBW"
colnames(dat1.0)[which(names(dat1.0) == "BMI")] <- "BMI"
colnames(dat1.0)[which(names(dat1.0) == "TIME PROT")] <- "TIMEP"
colnames(dat1.0)[which(names(dat1.0) == "CKD-EPI +24h (opvolg) (mL/min/1,73m^2)")] <- "CKEP2"
colnames(dat1.0)[which(names(dat1.0) == "total protein at 0h (g/L)")] <- "TOPR"
colnames(dat1.0)[which(names(dat1.0) == "Serum sodium at 0h (mmol/L)")] <- "SODI"
colnames(dat1.0)[which(names(dat1.0) == "Balance: 0h till 24h (mL)")] <- "BALA2"
colnames(dat1.0)[which(names(dat1.0) == "DV (mg/L)")] <- "DV"
colnames(dat1.0)

dat2.0 <- dat1.0[, c("ID","TIMEP","DV","TBW","BMI","CKEP2","TOPR","SODI","BALA2")]

dat2.0$ID <- as.numeric(dat2.0$ID)
length(unique(dat2.0$ID))
#dat2.0$TBW <- as.numeric(dplyr::na_if(dat2.0$TBW , "MISS"))
#dat2.0$BMI <- as.numeric(dplyr::na_if(dat2.0$BMI , "MISS"))
dat2.0$CKEP2 <- as.numeric(dplyr::na_if(dat2.0$CKEP2 , "MISS"))
dat2.0$TOPR <- as.numeric(dplyr::na_if(dat2.0$TOPR , "MISS"))
#dat2.0$SODI <- as.numeric(dplyr::na_if(dat2.0$SODI , "MISS"))
dat2.0$BALA2 <- as.numeric(dplyr::na_if(dat2.0$BALA2 , "MISS"))
dat2.0$DV <- as.numeric(dplyr::na_if(dat2.0$DV , "MISS"))

# Concentrations below the limit of quantification are never removed
# They are censored
dat2.0$CEN <-ifelse(is.na(dat2.0$DV),3,
                    ifelse(dat2.0$DV<=0.8 & dat2.0$DV!=0,2,
                           ifelse(dat2.0$DV==0,1,0)))
dat2.0$CEN

# Make the standard data
dat_sd <- dat2.0
str(dat_sd)
nrow(dat_sd)

# Print the standard data in csv
OriginalData<-data.frame(lapply(dat_sd, as.character), stringsAsFactors=FALSE)
# write.table(OriginalData, file="OriginalData.csv", row.names=FALSE, na="", col.names = TRUE, sep=",")

# Calculation of patients characteristics
dat2.0b <- dat_sd[with(dat_sd,order(ID,TIMEP)),]
nrow(dat2.0b)/length(unique(dat2.0b$TIMEP))
# View(dat2.0b)
# summary(dat2.0b)
dat3.0a <- dat2.0b[!duplicated(dat2.0b[ , c("ID","TBW","BMI","CKEP2","TOPR","SODI","BALA2")]), ] 
# View(dat3.0a)
# nrow(dat3.0a)
dat3.0 <- subset(dat3.0a,DV==0) # No need to group by ID # Observations at 24 hours are same as obssrvations at 0 h
# View(dat3.0)
nrow(dat3.0) # This is the number reported in the article

Covariates <- c("TBW","BMI","SODI") # These variables have no missing values

Median <- c(round(quantile(dat3.0$TBW,0.5,na.rm = T),1),round(quantile(dat3.0$BMI,0.5,na.rm = T),1),round(quantile(dat3.0$SODI, 0.5,na.rm = T),1))
Median

Minimum <- c(round(min(dat3.0$TBW,na.rm = T),1),round(min(dat3.0$BMI,na.rm = T),1),round(min(dat3.0$SODI,na.rm = T),1))
Minimum

First_quartile <- c(round(quantile(dat3.0$TBW,0.25,na.rm = T),1),round(quantile(dat3.0$BMI,0.25,na.rm = T),1),round(quantile(dat3.0$SODI, 0.25,na.rm = T),1))
First_quartile

Maximum <- c(round(max(dat3.0$TBW,na.rm = T),1),round(max(dat3.0$BMI,na.rm = T),1),round(max(dat3.0$SODI,na.rm = T),1))
Maximum

Third_quartile <- c(round(quantile(dat3.0$TBW,0.75,na.rm = T),1),round(quantile(dat3.0$BMI,0.75,na.rm = T),1),round(quantile(dat3.0$SODI, 0.75,na.rm = T),1))
Third_quartile

dat4.0 <- data.frame(Covariates,Minimum,First_quartile,Median,Third_quartile,Maximum)
# View(dat4.0)
dat4.0a <- dat4.0 %>% slice(1:2)
dat4.0b <- dat4.0 %>% slice(3)

dat3.01 <- dat3.0[, c("ID","CKEP2","TOPR","BALA2")]
# View(dat3.01)

A <- nrow(dat3.01)
A
B <- 10*A/100 # Median used if less than 10% of values are missing
B
C <- 50*A/100 # DV-time was ignored if 10-50% of values are missing # covariate was excluded for > 50%
C

summary(dat3.01)

D <- sum(is.na(dat3.01$CKEP2))
D
E <- sum(is.na(dat3.01$TOPR))
E
F <- sum(is.na(dat3.01$BALA2))
F


dat3.01a <- subset(dat3.0,(!is.na(dat3.0$BALA2)))
# View(dat3.01a)
V1 <- sort(as.numeric(dat3.01a$BALA2))
Covariates <- c("BALA2")
Minimum <-round((min(dat3.01a$BALA2,na.rm = T)/1000),1)
First_quartile<-round((0.5*(V1[round(1*length(dat3.01a$BALA2)/4,1)]+V1[round(1*length(dat3.01a$BALA2)/4,1)+1]))/1000,1)
Median<-round((V1[round(2*length(dat3.01a$BALA2)/4,1)+1])/1000,1)
Third_quartile<-round((0.5*(V1[round(3*length(dat3.01a$BALA2)/4,1)]+V1[round(3*length(dat3.01a$BALA2)/4,1)+1]))/1000,1)
Maximum <-round((max(dat3.01a$BALA2,na.rm = T)/1000),1)
dat5.0 <- data.frame(Covariates,Minimum,First_quartile,Median,Third_quartile, Maximum)
# View(dat5.0)

dat3.01a <- subset(dat3.0,(!is.na(dat3.0$CKEP2)))
V1 <- sort(c(dat3.01a$CKEP2))
Covariates <- c("CKEP2")
Minimum <-round(min(dat3.01a$CKEP2,na.rm = T),1)
First_quartile<-0.5*(V1[1*length(dat3.01a$CKEP2)/4]+V1[round(1*length(dat3.01a$CKEP2)/4,1)])
Median<-V1[2*length(dat3.01a$CKEP2)/4+1]
Third_quartile<-0.5*(V1[round(3*length(dat3.01a$CKEP2)/4,1)]+V1[round(3*length(dat3.01a$CKEP2)/4,1)])
Maximum <-round(max(dat3.01a$CKEP2,na.rm = T),1)
dat6.0 <- data.frame(Covariates,Minimum,First_quartile,Median,Third_quartile, Maximum)
# View(dat6.0)

dat3.01a <- subset(dat3.0,(!is.na(dat3.0$TOPR)))
V1 <- sort(c(dat3.01a$TOPR))
Covariates <- c("TOPR")
Minimum <-round(min(dat3.01a$TOPR,na.rm = T),1)
First_quartile<-0.5*(V1[1*length(dat3.01a$TOPR)/4]+V1[round(1*length(dat3.01a$TOPR)/4,1)])
Median<-V1[2*length(dat3.01a$TOPR)/4+1]
Third_quartile<-0.5*(V1[round(3*length(dat3.01a$TOPR)/4,1)]+V1[round(3*length(dat3.01a$TOPR)/4,1)])
Maximum <-round(max(dat3.01a$TOPR,na.rm = T),1)
dat7.0 <- data.frame(Covariates,Minimum,First_quartile,Median,Third_quartile, Maximum)
# View(dat7.0)

Data_basicstatistics <- rbind(dat4.0a,dat5.0,dat6.0,dat4.0b,dat7.0)

head(Data_basicstatistics)

# Print the csv
Basicstatistics_OriginalData<-data.frame(lapply(Data_basicstatistics , as.character), stringsAsFactors=FALSE)
write.table(Basicstatistics_OriginalData, file="Basicstatistics_OriginalData.csv", row.names=FALSE, na="", col.names = TRUE, sep=",")
###############################################################################################################################################

# Check the correlation in scatter plot between covariates
# dat_sd
colnames(dat_sd)
dat_testingcharacteristics <- subset(dat_sd,DV==0)
# Covariates are : BMI,CKEP2,TOPR,SODI,BALA2
# Approach used by Erwin
# The name of the script is "Distribution and correlation_ED"
library("dplyr")
test <- dat_testingcharacteristics %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()

shapiro.test (test$TBW[!is.na(test$TBW)])         ## p-value = 0.2228 > 0.05
shapiro.test(test$BMI[!is.na(test$BMI)])         ## p-value = 0.538 > 0.05
shapiro.test(test$CKEP2[!is.na(test$CKEP2)])          ## p-value = 0.0008355 < 0.05  ### No normal distribution
shapiro.test(test$TOPR[!is.na(test$TOPR)])          ## p-value = 0.1026 > 0.05
shapiro.test(test$SODI[!is.na(test$SODI)])          ## p-value = 0.1256 > 0.05
shapiro.test(test$BALA2[!is.na(test$BALA2)])          ## p-value = 7.45e-07 < 0.05  ### No normal distribution

# Pearson test is used in case both x and y are normally distributed
# P-value < 0.05 indicates significant correlation
# Spearman test is used in case of non-normality
# Interpretation of scatter plot based on https://statisticsbyjim.com/graphs/scatterplots/
# Code of Erwin
# The name of the script is "Distribution and correlation_ED"
# SE displays the confidence around smooth (TRUE is the default)
# geom_smooth uses the "loess" method and "y=x" formula


dat_testingcharacteristics$transf_fluidbalance <- dat_testingcharacteristics$BALA2/1000
COV <- dat_testingcharacteristics %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, transf_fluidbalance) %>% unique()

library("ggplot2")

COVTBWBMI <- COV[!(is.na(COV$TBW)) & !(is.na(COV$BMI)), ]
Plt1 <- ggplot2::ggplot(data = COVTBWBMI, mapping = aes(x = TBW, y = BMI)) +
  xlab("Body weight")+
  ylab("Body mass index")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVTBWCKEP2 <- COV[!(is.na(COV$TBW)) & !(is.na(COV$CKEP2)), ]
Plt2 <- ggplot(data = COVTBWCKEP2, mapping = aes(x = TBW, y = CKEP2)) +
  xlab("Body weight")+
  # ylab(expression(eGFR[CKD-EPI24h]))+
  ylab("eGFR")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVTBWTOPR <- COV[!(is.na(COV$TBW)) & !(is.na(COV$TOPR)), ]
Plt3 <- ggplot(data = COVTBWTOPR, mapping = aes(x = TBW, y = TOPR)) +
  xlab("Body weight")+
  ylab("Serum total protein")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVTBWSODI <- COV[!(is.na(COV$TBW)) & !(is.na(COV$SODI)), ]
Plt4 <- ggplot(data = COVTBWSODI, mapping = aes(x = TBW, y = SODI)) +
  xlab("Body weight")+
  ylab("Serum sodium")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVTBWtransf_fluidbalance <- COV[!(is.na(COV$TBW)) & !(is.na(COV$transf_fluidbalance )), ]
Plt5 <- ggplot(data = COVTBWtransf_fluidbalance , mapping = aes(x = TBW, y = transf_fluidbalance )) +
  xlab("Body weight")+
  # ylab(expression(FB["24h"]))+
  ylab("24h fluid balance")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVBMICKEP2 <- COV[!(is.na(COV$BMI)) & !(is.na(COV$CKEP2)), ]
Plt6 <- ggplot(data = COVBMICKEP2, mapping = aes(x = BMI, y = CKEP2)) +
  xlab("Body mass index")+
  # ylab(expression(eGFR[CKD-EPI24h]))+
  ylab("eGFR")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVBMITOPR <- COV[!(is.na(COV$BMI)) & !(is.na(COV$TOPR)), ]
Plt7 <- ggplot(data = COVBMITOPR, mapping = aes(x = BMI, y = TOPR)) +
  xlab("Body mass index")+
  ylab("Serum total protein")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVBMISODI <- COV[!(is.na(COV$BMI)) & !(is.na(COV$SODI)), ]
Plt8 <- ggplot(data = COVBMISODI, mapping = aes(x = BMI, y = SODI)) +
  xlab("Body mass index")+
  ylab("Serum sodium")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVBMItransf_fluidbalance <- COV[!(is.na(COV$BMI)) & !(is.na(COV$transf_fluidbalance)), ]
Plt9 <- ggplot(data = COVBMItransf_fluidbalance, mapping = aes(x = BMI, y = transf_fluidbalance )) +
  xlab("Body mass index")+
  # ylab(expression(FB["24h"]))+
  ylab("24h fluid balance")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVCKEP2TOPR <- COV[!(is.na(COV$CKEP2)) & !(is.na(COV$TOPR)), ]
Plt10 <- ggplot(data = COVCKEP2TOPR, mapping = aes(x = CKEP2, y = TOPR)) +
  # xlab(expression(eGFR[CKD-EPI24h]))+
  xlab("eGFR")+
  ylab("Serum total protein")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVCKEP2SODI <- COV[!(is.na(COV$CKEP2)) & !(is.na(COV$SODI)), ]
Plt11 <- ggplot(data = COVCKEP2SODI, mapping = aes(x = CKEP2, y = SODI)) +
  # xlab(expression(eGFR[CKD-EPI24h]))+
  xlab("eGFR")+
  ylab("Serum sodium")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVCKEP2transf_fluidbalance <- COV[!(is.na(COV$CKEP2)) & !(is.na(COV$transf_fluidbalance)), ]
Plt12 <- ggplot(data = COVCKEP2transf_fluidbalance, mapping = aes(x = CKEP2, y = transf_fluidbalance )) +
  # xlab(expression(eGFR[CKD-EPI24h]))+
  xlab("eGFR")+
  # ylab(expression(FB["24h"]))+
  ylab("24h fluid balance")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

COVTOPRSODI <- COV[!(is.na(COV$TOPR)) & !(is.na(COV$SODI)), ]
Plt13 <-ggplot(data = COVTOPRSODI, mapping = aes(x = TOPR, y = SODI)) +
  xlab("Serum total protein")+
  ylab("Serum sodium")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15, hjust = +0.7))

COVTOPRtransf_fluidbalance <- COV[!(is.na(COV$TOPR)) & !(is.na(COV$transf_fluidbalance)), ]
Plt14 <-ggplot(data = COVTOPRtransf_fluidbalance, mapping = aes(x = TOPR, y = transf_fluidbalance )) +
  xlab("Serum total protein")+
  # ylab(expression(FB["24h"]))+
  ylab("24h fluid balance")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15, hjust = +0.7))

COVSODItransf_fluidbalance <- COV[!(is.na(COV$SODI)) & !(is.na(COV$transf_fluidbalance)), ]
Plt15 <-ggplot(data = COVSODItransf_fluidbalance, mapping = aes(x = SODI, y = transf_fluidbalance )) +
  xlab("Serum sodium")+
  # ylab(expression(FB["24h"]))+
  ylab("24h fluid balance")+
  geom_point() +
  geom_smooth(se = FALSE)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size =15),
        axis.line = element_line(colour="black"), axis.text = element_text(size =15))

library("gridExtra")

Cor_Plot <- ggpubr::ggarrange(
  Plt1,Plt2,Plt3,Plt4,Plt5,Plt6,Plt7,Plt8,Plt9,Plt10,Plt11,Plt12,Plt13,Plt14,Plt15,
  ncol=3,
  nrow=5,
  align="hv")


setwd("C:/Users/u0147124/OneDrive - KU Leuven/PhD_KU Leuven/3_Amikacin_ER_PhD_KU Leuven/Data analysis/Article_Rcode_Fifth draft/Article_Data_Plots_third")
ggplot2::ggsave(filename="Correlation_Plots.jpg",plot = Cor_Plot,device='jpg', bg="white", width = 10, height = 15 , units = "in")
setwd("C:/Users/u0147124/OneDrive - KU Leuven/PhD_KU Leuven/3_Amikacin_ER_PhD_KU Leuven/Data analysis")

cor.test(COV$TBW, COV$BMI, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = 0.8818063 > 0.20 --> significant correlation 
# p-value  = 2.2e-16   < 0.05 --> significant p-value
cor.test(COV$TBW, COV$CKEP2, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = -0.2824636 < 0.20 
# p-value  =  0.005548   < 0.05 --> significant p-value 
cor.test(COV$TBW, COV$TOPR, method = c("pearson"), use="complete.obs")    
# cor_coef = 0.1272549  < 0.20
# p-value  = 0.2347
cor.test(COV$TBW, COV$SODI, method = c("pearson"), use="complete.obs")    
# cor_coef = -0.1005213 < 0.20
# p-value  = 0.3272
cor.test(COV$TBW, 1000*COV$transf_fluidbalance, method = c("spearman"), use="complete.obs", exact = FALSE)   
# cor_coef = 0.06093526  < 0.20
# p-value  = 0.5796

cor.test(COV$BMI, COV$CKEP2, method = c("spearman"), use="complete.obs", exact = FALSE)   
# cor_coef = -0.3593476 < 0.20
# p-value  = 0.0003484  < 0.05 --> significant p-value
cor.test(COV$BMI, COV$TOPR, method = c("pearson"), use="complete.obs")    
# cor_coef = 0.1743633 < 0.20
# p-value  = 0.1022
cor.test(COV$BMI, COV$SODI, method = c("pearson"), use="complete.obs")    
# cor_coef = -0.0305495  < 0.20
# p-value  = 0.7664
cor.test(COV$BMI, 1000*COV$transf_fluidbalance, method = c("spearman"), use="complete.obs", exact = FALSE)   
# cor_coef = 0.02007094 < 0.20
# p-value  = 0.8553

cor.test(COV$CKEP2, COV$TOPR, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = -0.1473841  < 0.20
# p-value  = 0.1706
cor.test(COV$CKEP2, COV$SODI, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = -0.2003199 < 0.20
# p-value  = 0.05161
cor.test(COV$CKEP2, 1000*COV$transf_fluidbalance, method = c("spearman"), use="complete.obs", exact = FALSE) 
# cor_coef = -0.2357241 < 0.20
# p-value  = 0.02987    < 0.05 --> significant p-value 

cor.test(COV$TOPR, COV$SODI, method = c("pearson"), use="complete.obs", exact = FALSE)   
# cor_coef = -0.01633785 < 0.20
# p-value  = 0.8792
cor.test(COV$TOPR, 1000*COV$transf_fluidbalance, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = -0.09728141 < 0.20
# p-value  = 0.3937

cor.test(COV$SODI, 1000*COV$transf_fluidbalance, method = c("spearman"), use="complete.obs", exact = FALSE)  
# cor_coef = 0.1321776  < 0.20
# p-value  = 0.2279

# Another approach to check the correlation using the code of Erwin
# The name of the script is "Distribution and correlation_ED"
library(dplyr)
test <- dat_testingcharacteristics %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()

library("ggpubr")
Plt16a <- ggplot(test,aes(x=TBW)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Total body weight", x="Total body weight (kg))", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt16a

Plt16b <- ggplot(test,aes(x=BMI)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Body mass index", x="Body mass index (kg/m2", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt16b

testCKEP2 <- test[!(is.na(test$CKEP2)), ]
Plt17 <- ggplot(testCKEP2,aes(x=CKEP2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Glomerular filtration rate", x="Glomerular filtration rate", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt17

testTOPR <- test[!(is.na(test$TOPR)), ]
Plt18 <- ggplot(testTOPR,aes(x=TOPR)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Total protein", x="Total protein", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt18

Plt19 <- ggplot(test,aes(x=SODI)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Sodium", x="Sodium", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt19

testBALA2 <- test[!(is.na(test$BALA2)), ]
Plt20 <- ggplot(testBALA2,aes(x=BALA2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Fluid balance curve", x="Fluid balance (mL)", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt20

gridExtra::grid.arrange(Plt16a,Plt16b,Plt17,Plt18,Plt19,Plt20)

# Variance-Covariance matrix
dat_cov <- dat_testingcharacteristics[c("TBW", "BMI", "transf_fluidbalance","CKEP2","SODI","TOPR")]
colnames(dat_cov) <- c("Body weight", "Body mass index", "24h fluid balance","eGFR","Serum sodium", "Serum total protein")
Matrix_cov <- round(cov(dat_cov, use = "complete.obs"),1)
write.table(Matrix_cov, 'Covariance.csv',row.names=TRUE,col.names=TRUE, sep=";")

# For the normally distributed variables: BMI, TOPR and SODI
# Assume for practicality
DS <- subset(dat_sd,DV==0)

library(fitdistrplus)
library(dplyr)
# For the normally distributed variables: TBW
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
library(psych)
pairs.panels(test)
descdist(test$TBW)
library(ggpubr)
ggqqplot(test$TBW)
shapiro.test(test$TBW) # p-value = 0.2228 > 0.05  ### Normal distribution
ggplot(data = test, mapping = aes(x = TBW)) + geom_density()+ stat_overlay_normal_density(color="red",linetype="dashed")
# normality!!
V_TBW <- test %>% summarise(mean = mean(TBW, na.rm = T), sd = sd(TBW, na.rm = T), min = min(TBW, na.rm = T), max = max(TBW, na.rm = T))

# For the normally distributed variables: BMI
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
shapiro.test(test$BMI) # p-value = 0.538 > 0.05  ### Normal distribution
ggplot(data = test, mapping = aes(x = BMI)) + geom_density()+ stat_overlay_normal_density(color="red",linetype="dashed")
# normality!!
V_BMI <- test %>% summarise(mean = mean(BMI, na.rm = T), sd = sd(BMI, na.rm = T), min = min(BMI, na.rm = T), max = max(BMI, na.rm = T))

# For the normally distributed variables: TOPR
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
test <- subset(test, TOPR!=is.na(TOPR))
shapiro.test(test$TOPR) # p-value = 0.1026 > 0.05  ### Normal distribution
### Suggestion by Erwin
## Apply 
## ks.test(test$TOPR,"pnorm")
ggplot(data = test, mapping = aes(x = TOPR)) + geom_density()+ stat_overlay_normal_density(color="red",linetype="dashed")
# normality!!
V_TOPR <- test %>% summarise(mean = mean(TOPR, na.rm = T), sd = sd(TOPR, na.rm = T), min = min(TOPR, na.rm = T), max = max(TOPR, na.rm = T))

# For the normally distributed variables: SODI
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
test <- subset(test, SODI!=is.na(SODI))
shapiro.test(test$SODI) # p-value = 0.1256 > 0.05  ### Normal distribution
ggplot(data = test, mapping = aes(x = SODI)) + geom_density()+ stat_overlay_normal_density(color="red",linetype="dashed")
# normality!!
V_SODI <- test %>% summarise(mean = mean(SODI, na.rm = T), sd = sd(SODI, na.rm = T), min = min(SODI, na.rm = T), max = max(SODI, na.rm = T))

# Transform the not normally distributed covariates into normal distribution: CKEP2 and BALA2
# Based on www.datanovia.com/en/lessons
# Based on https://www.statology.org/transform-data-in-r/
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
test <- subset(test, CKEP2!=is.na(CKEP2))
descdist(test$CKEP2)
ggplot(data = test, mapping = aes(x = CKEP2)) + geom_density() + stat_overlay_normal_density(color="red",linetype="dashed")
shapiro.test(test$CKEP2)            ## p-value = 0.0008355 < 0.05  ### No normal distribution
library(moments)
skewness(test$CKEP2,na.rm=TRUE)     ## 0.7411504 > 0
# If the skewness is positive, add the following
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(CKEP21 = log10(CKEP2))
shapiro.test(test$CKEP21) # p-value = 0.0006134 < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(CKEP22 = 1/(CKEP2))
shapiro.test(test$CKEP22) # p-value = 1.188e-12 < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(CKEP22 = CKEP2^(1/3))
shapiro.test(test$CKEP22) # p-value = 0.09675 > 0.05 ==> Normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(CKEP23 = sqrt(CKEP2))
shapiro.test(test$CKEP23) # p-value = 0.1792 > 0.05 ==> Normally distributed
# Decision
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(SQRT_CKEP2 = sqrt(CKEP2))
shapiro.test(test$SQRT_CKEP2) # p-value = 0.1792 > 0.05 ==> Normally distributed
testSQRT_CKEP2 <- test[!(is.na(test$SQRT_CKEP2)), ]
ggplot(data = testSQRT_CKEP2, mapping = aes(x = SQRT_CKEP2)) + geom_density() + stat_overlay_normal_density(color="red",linetype="dashed")
# no normality of CKEP2, but sqrt(CKEP2) is normally distributed!!
V_SQRT_CKEP2 <- test %>% summarise(mean = mean(SQRT_CKEP2, na.rm = T), sd = sd(SQRT_CKEP2, na.rm = T), 
                                   min = min(SQRT_CKEP2, na.rm = T), max = max(SQRT_CKEP2, na.rm = T))
V_SQRT_CKEP2

test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()
test <- subset(test, BALA2!=is.na(BALA2))
descdist(test$BALA2)
ggplot(data = test, mapping = aes(x = BALA2)) + geom_density()+ stat_overlay_normal_density(color="red",linetype="dashed")
shapiro.test(test$BALA2)            ## p-value = 7.45e-07 < 0.05  ### No normal distribution
skewness(test$BALA2,na.rm=TRUE)     ## 1.731979  > 0
# The covariate has positive and negative values
# A constant was added to make all the values positive
# Based on https://www.researchgate.net/post/How-can-I-log-transform-a-series-with-both-positive-and-negative-values
summary(test$BALA2)
dat_NoNA_BALA2 <- subset(test,!is.na(test$BALA2))
MaxNegValue <- -min(dat_NoNA_BALA2$BALA2)
MaxNegValue
# test_Pos_BALA2 <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(BALA2 = (BALA2+MaxNegValue+1))
# summary(test_Pos_BALA2$BALA2)
# shapiro.test(test_Pos_BALA2$BALA2) ## p-value = 1.165e-06 < 0.05 ==> Not normally distributed
# If the skewness is positive, add the following from low to severe skewness
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(BALA21 = log10(BALA2+MaxNegValue+1))
shapiro.test(test$BALA21) # p-value = 7.859e-16 < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(BALA22 = 1/(BALA2+MaxNegValue+1))
shapiro.test(test$BALA22) # p-value = 2.2e-16   < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(BALA23 = sqrt(BALA2+MaxNegValue+1))
shapiro.test(test$BALA23) # p-value = 0.002763 < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()
dat_NoNA_BALA2 <- subset(test,!is.na(test$BALA2))
transform(dat_NoNA_BALA2$BALA2, method="Box-Cox")
shapiro.test(dat_NoNA_BALA2$BALA2) # p-value = 7.45e-07 < 0.05 ==> Not normally distributed
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()
dat_NoNA_BALA2 <- subset(test,!is.na(test$BALA2))
transform(dat_NoNA_BALA2$BALA2, method="Yeo-Johnson")
shapiro.test(dat_NoNA_BALA2$BALA2) # p-value = 7.45e-07 < 0.05 ==> Not normally distributed
# None of the strategy worked
# Per discussion with Erwin, it was decided to implement the method with the closest p-value to 0.05
# Erwin expects that it is difficult to have a smooth plot of normal distribution because the sample size is small
test <- DS %>% ungroup() %>% dplyr::select(TBW, BMI,CKEP2, TOPR, SODI, BALA2) %>% unique()%>% mutate(SQRT_BALA2 = sqrt(BALA2+MaxNegValue+1))
shapiro.test(test$SQRT_BALA2) # p-value = 0.002763 < 0.05 ==> Not normally distributed
testSQRT_BALA2 <- test[!(is.na(test$SQRT_BALA2)), ]
ggplot(data = testSQRT_BALA2, mapping = aes(x = SQRT_BALA2)) + geom_density() + stat_overlay_normal_density(color="red",linetype="dashed")
# no normality of BALA2, but sqrt(BALA2+MaxNegValue+1) is almost normally distributed!!
V_SQRT_BALA2 <- test %>% summarise(mean = mean(SQRT_BALA2, na.rm = T), sd = sd(SQRT_BALA2, na.rm = T),
                                   min = min(SQRT_BALA2, na.rm = T),max = max(SQRT_BALA2, na.rm = T))
V_SQRT_BALA2

# Incorporate the modifications
# DS1 and DS2 have transformed values
DS <- subset(dat_sd,DV==0)
unique(is.na(DS$CKEP2))
DS1 <- DS %>% mutate(SQRT_CKEP2 = ifelse(CKEP2==is.na(CKEP2),is.na(CKEP2),sqrt(CKEP2)))
V_CKEP2_DS1 <- DS1 %>% summarise(mean = mean(CKEP2, na.rm = T), sd = sd(CKEP2, na.rm = T), 
                                 min = min(CKEP2, na.rm = T), max = max(CKEP2, na.rm = T))
V_CKEP2_DS1

V_SQRT_CKEP2_DS1 <- DS1 %>% summarise(mean = mean(SQRT_CKEP2, na.rm = T), sd = sd(SQRT_CKEP2, na.rm = T), 
                                      min = min(SQRT_CKEP2, na.rm = T), max = max(SQRT_CKEP2, na.rm = T))
V_SQRT_CKEP2_DS1 

V_SQRT_CKEP2  

unique(is.na(DS1$BALA2))
DS2 <- DS1 %>% mutate(SQRT_BALA2 = ifelse(BALA2==is.na(BALA2),is.na(BALA2),sqrt(BALA2+MaxNegValue+1)))

V_BALA2_DS2 <- DS2 %>% summarise(mean = mean(BALA2, na.rm = T), sd = sd(BALA2, na.rm = T), 
                                 min = min(BALA2, na.rm = T), max = max(BALA2, na.rm = T))
V_BALA2_DS2

V_SQRT_BALA2_DS2 <- DS2 %>% summarise(mean = mean(SQRT_BALA2, na.rm = T), sd = sd(SQRT_BALA2, na.rm = T), 
                                      min = min(SQRT_BALA2, na.rm = T), max = max(SQRT_BALA2, na.rm = T))
V_SQRT_BALA2_DS2

V_SQRT_BALA2

test <- DS2 %>% ungroup() %>% dplyr::select(TBW, BMI, SQRT_CKEP2, TOPR, SODI,  SQRT_BALA2) %>% unique()

shapiro.test(test$TBW[!is.na(test$TBW)])       # p-value = 0.2228    > 0.05 ==> Normally distributed
shapiro.test(test$BMI[!is.na(test$BMI)])        # p-value = 0.538    > 0.05 ==> Normally distributed
shapiro.test(test$SQRT_CKEP2[!is.na(test$SQRT_CKEP2)])       # p-value = 0.1792    > 0.05 ==> Normally distributed
shapiro.test(test$TOPR[!is.na(test$TOPR)])       # p-value = 0.1026    > 0.05 ==> Normally distributed
shapiro.test(test$SODI[!is.na(test$SODI)]) # p-value = 0.1256    > 0.05 ==> Normally distributed
shapiro.test(test$SQRT_BALA2[!is.na(test$SQRT_BALA2)]) # p-value = 0.002763    < 0.05 ==> Not normally distributed

DS2CKEP2 <- DS2[!(is.na(DS2$CKEP2)), ]
Plt21 <- ggplot(DS2CKEP2,aes(x=CKEP2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Estimated glomerular filtration curve", x="Estimated glomerular filtration (mL/min/1.73 m2)", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt21

DS2SQRT_CKEP2 <- DS2[!(is.na(DS2$SQRT_CKEP2)), ]
Plt22 <- ggplot(DS2SQRT_CKEP2,aes(x=SQRT_CKEP2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Estimated glomerular filtration curve", x="Modified estimated glomerular filtration (mL/min/1.73 m2)", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt22

DS2BALA2 <- DS2[!(is.na(DS2$BALA2)), ]
Plt23 <- ggplot(DS2BALA2,aes(x=BALA2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Fluid balance curve", x="Fluid balance (mL)", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt23

DS2SQRT_BALA2 <- DS2[!(is.na(DS2$SQRT_BALA2)), ]
Plt24 <- ggplot(DS2SQRT_BALA2,aes(x=SQRT_BALA2)) +
  # geom_histogram(aes(y = ..density..), binwidth = 2, color="black",fill="lightgrey")+
  # geom_vline(aes(xintercept=mean(TBW)),color="black",linetype="dashed", size=1.5)+
  stat_overlay_normal_density(color="red",linetype="dashed")+
  geom_density(linetype="solid",color="black",fill="#0072B2",alpha=0.2)+
  labs(title="Fluid balance curve", x="Modified fluid balance (mL)", y= "Density")+
  theme(plot.title = element_text(size=18),
        text = element_text(size = 18),
        panel.background = element_rect(colour = "white",fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position="top",
        #legend.position=c(0.75,0.55),
        #legend.justification =c(0,0), 
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=9))
Plt24

gridExtra::grid.arrange(Plt21,Plt22,Plt23,Plt24)

##################################################################################################################################################

# Generation of random numbers
n=1000000
# In the original data, 
# Correlation exists between TBW and BMI, TBW and CKEP2, BMI and CKEP2, SODI and CKEP2, BALA2 and CKEP2
# Make 1 dataframe for six correlated variables
# consider truncation on both sides for the 6 variables

# Apply the multivariate truncated to the 6 variables
# Set an initial number or seed that guarantees reproducibility
set.seed(51121)

# Calculate the elements needed including the correlation matrix
# ?rtmvnorm
colnames(DS2)
dat_cov <- subset(DS2, select=c("TBW","BMI","SQRT_CKEP2","TOPR","SODI","SQRT_BALA2"))
summary(dat_cov)
V_colName <- colnames(dat_cov)
V_colName

Sigma <- round(cor2cov(round(cor(dat_cov,use = "complete.obs"),4), round(sapply(dat_cov,sd,na.rm = TRUE),4)),2)
Sigma
# var(dat_cov$TBW)
# cov(dat_cov$TBW,dat_cov$BMI)


V_min <-c(round(min(DS2$TBW,na.rm = TRUE),2),round(min(DS2$BMI,na.rm = TRUE),2),round(min(DS2$SQRT_CKEP2,na.rm = TRUE),2),
          round(min(DS2$TOPR,na.rm = TRUE),2),round(min(DS2$SODI,na.rm = TRUE),2),round(min(DS2$SQRT_BALA2,na.rm = TRUE),2))
V_min
DS3 <- rbind(V_TBW, V_BMI,V_SQRT_CKEP2,V_TOPR,V_SODI,V_SQRT_BALA2)
str(DS3)
V_min_DS3 <- round(DS3$min,2)
V_min_DS3
all.equal(V_min, V_min_DS3)


V_mean <-c(round(mean(DS2$TBW,na.rm = TRUE),2),round(mean(DS2$BMI,na.rm = TRUE),2),round(mean(DS2$SQRT_CKEP2,na.rm = TRUE),2),
           round(mean(DS2$TOPR,na.rm = TRUE),2), round(mean(DS2$SODI,na.rm = TRUE),2),round(mean(DS2$SQRT_BALA2,na.rm = TRUE),2))
V_mean
V_mean_DS3 <- round(DS3$mean,2)
V_mean_DS3
all.equal(V_mean, V_mean_DS3)


V_max <-c(round(max(DS2$TBW,na.rm = TRUE),2),round(max(DS2$BMI,na.rm = TRUE),2),round(max(DS2$SQRT_CKEP2,na.rm = TRUE),2), 
          round(max(DS2$TOPR,na.rm = TRUE),2), round(max(DS2$SODI,na.rm = TRUE),2), round(max(DS2$SQRT_BALA2,na.rm = TRUE),2))
V_max
V_max_DS3 <- round(DS3$max,2)
V_max_DS3
all.equal(V_max, V_max_DS3)

# Generate the truncated multivariate normal distribution
# Rejection sampling
library(tmvtnorm)
SIM_Cor_trunc_rej_tot <- round(rtmvnorm(n=n, mean=V_mean, sigma = Sigma, lower=V_min, upper=V_max, algorithm="rejection"),2)
# Convert matrix to dataframe
class(SIM_Cor_trunc_rej_tot)
SIM_Cor_trunc_rej_tot_DF_transf <- as.data.frame(SIM_Cor_trunc_rej_tot)
class(SIM_Cor_trunc_rej_tot_DF_transf)
# Add names to columns
colnames(SIM_Cor_trunc_rej_tot_DF_transf) <- V_colName
SIM_Cor_trunc_rej_tot_DF_transf <- SIM_Cor_trunc_rej_tot_DF_transf[order(SIM_Cor_trunc_rej_tot_DF_transf$TBW),]
str(SIM_Cor_trunc_rej_tot_DF_transf)

# Prob, also known as alpha, generates the estimated absolute error ("error") and a status message ("msg")
# ?pmvnorm
prob <- pmvnorm(lower=V_min, upper=V_max, mean=V_mean, sigma=Sigma)
print (prob)
# If Corr is used instead of cov, upper limits should be standardized
# The message ("msg") indicates "normal completion" versus "completion with error"
# The estimated absolute error determines the probability of having values between lower and upper limits
# The probability is 0.9324112
# This also known as acceptance rate alpha
# It should be high. Otherwise, the Gibbs sampler is preferable
# The estimated error is 0.0005716116

# Convert to numeric columns
sapply(SIM_Cor_trunc_rej_tot_DF_transf,class)
SIM_Cor_trunc_rej_tot_DF_transf[] <-lapply(SIM_Cor_trunc_rej_tot_DF_transf,function(x) as.numeric(as.character(x)))
sapply(SIM_Cor_trunc_rej_tot_DF_transf,class)
Data_MVND <- SIM_Cor_trunc_rej_tot_DF_transf

# Convert to the state before transformation
# This was done previously sqrt(CKEP2) and sqrt(BALA2+MaxNegValue+1)
# SIM_Cor_trunc_rej_tot_DF <- subset(SIM_Cor_trunc_rej_tot_DF_transf, select= c("TBW","BMI","SQRT_CKEP2","TOPR","SODI","SQRT_BALA2"))
# colnames(SIM_Cor_trunc_rej_tot_DF)[3] <- "CKEP2"
# colnames(SIM_Cor_trunc_rej_tot_DF)[6] <- "BALA2"
# str(SIM_Cor_trunc_rej_tot_DF)
SIM_Cor_trunc_rej_tot_DF_transf$CKEP2 <- (SIM_Cor_trunc_rej_tot_DF_transf$SQRT_CKEP2)^2
SIM_Cor_trunc_rej_tot_DF_transf$BALA2 <- (SIM_Cor_trunc_rej_tot_DF_transf$SQRT_BALA2^2)-MaxNegValue-1
str(SIM_Cor_trunc_rej_tot_DF_transf)
SIM_Cor_trunc_rej_tot_DF <- subset(SIM_Cor_trunc_rej_tot_DF_transf, select= c("TBW","BMI","CKEP2","TOPR","SODI","BALA2"))
summary(SIM_Cor_trunc_rej_tot_DF)
str(SIM_Cor_trunc_rej_tot_DF)


# I removed body weight below 35 kg as suggested by Dr. Isabel, though this value is not present
min(SIM_Cor_trunc_rej_tot_DF$TBW)
SIM_Cor_trunc_rej_tot_DF <- subset(SIM_Cor_trunc_rej_tot_DF, TBW >= 35)
min(SIM_Cor_trunc_rej_tot_DF$TBW)
# Remove body height below 1.5 m
# Not needed in this approach
# min(SIM_Cor_trunc_rej_tot_DF$HT)
# SIM_Cor_trunc_rej_tot_DF <- subset(SIM_Cor_trunc_rej_tot_DF,HT>=1.5)
# min(SIM_Cor_trunc_rej_tot_DF$HT)
str(SIM_Cor_trunc_rej_tot_DF)
Big_data <- SIM_Cor_trunc_rej_tot_DF

#######################################################################################################################################################
#sampling <- Big_data %>% slice_sample(prop = 0., replace = T)
#sampling <- sample_frac(Big_data,0.2)

# Columns are rounded to the number of decimals listed in Table 1 of De Winter et al. (Quantification and Explanation...)
V_dec_TBW     <- 0 
V_dec_BMI     <- 1
V_dec_CKEP2   <- 0.5
V_dec_TOPR    <- 1
V_dec_SODI    <- 1 
V_dec_BALA2   <- 500

# Check up of the distribution of the sampled data
ggplot() +
  #geom_density(data = sampling %>% ungroup()%>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = TBW), colour = "red") +
  geom_density(data = Big_data %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = TBW), colour = "purple") +
  geom_density(data = Big_data %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(TBW=round(TBW,V_dec_TBW)), mapping = aes(x = TBW), colour = "red") +
  geom_density(data = DS       %>% ungroup() %>% dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = TBW), colour = "blue")

ggplot() +
  #geom_density(data = sampling%>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = BMI), colour = "red")+ 
  geom_density(data = Big_data %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = BMI), colour = "purple")+
  geom_density(data = Big_data %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(BMI=round(BMI,V_dec_BMI)), mapping = aes(x = BMI), colour = "red") +
  geom_density(data = DS       %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = BMI), colour = "blue")

Big_dataCKEP2 <- Big_data[!(is.na(Big_data$CKEP2)), ]
DSCKEP2 <- DS[!(is.na(DS$CKEP2)), ]
ggplot() +
  #geom_density(data = sampling %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = CKEP2), colour = "red")+ 
  geom_density(data = Big_dataCKEP2  %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = CKEP2), colour = "purple") +
  geom_density(data = Big_dataCKEP2  %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(CKEP2=plyr::round_any(CKEP2,V_dec_CKEP2)), mapping = aes(x = CKEP2), colour = "red") +
  geom_density(data = DSCKEP2        %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = CKEP2), colour = "blue") 

Big_dataTOPR <- Big_data[!(is.na(Big_data$TOPR)), ]
DSTOPR <- DS[!(is.na(DS$TOPR)), ]
ggplot() +
  #geom_density(data = sampling %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = TOPR), colour = "red")+ 
  geom_density(data = Big_dataTOPR  %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = TOPR), colour = "purple") +
  geom_density(data = Big_dataTOPR  %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(TOPR=round(TOPR,V_dec_TOPR)), mapping = aes(x = TOPR), colour = "red") +
  geom_density(data = DSTOPR        %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = TOPR), colour = "blue")

ggplot() +
  #geom_density(data = sampling %>% ungroup()%>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = SODI), colour = "red")+ 
  geom_density(data = Big_data %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = SODI), colour = "purple") +
  geom_density(data = Big_data %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(SODI=round(SODI,V_dec_SODI)), mapping = aes(x = SODI), colour = "red") +
  geom_density(data = DS       %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = SODI), colour = "blue")

Big_dataBALA2 <- Big_data[!(is.na(Big_data$BALA2)), ]
DSBALA2 <- DS[!(is.na(DS$BALA2)), ]
ggplot() +
  #geom_density(data= sampling      %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = BALA2), colour = "red")+ 
  geom_density(data = Big_dataBALA2 %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes(x = BALA2), colour = "purple") +
  geom_density(data = Big_dataBALA2 %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique()  %>% mutate(TBW=plyr::round_any(BALA2,V_dec_BALA2)), mapping = aes(x = BALA2), colour = "red") +
  geom_density(data = DSBALA2       %>% ungroup() %>%  dplyr::select(TBW, BMI, CKEP2, TOPR, SODI, BALA2) %>% unique(), mapping = aes (x = BALA2), colour = "blue") 
 
#######################################################################################################################################################
# Round the columns of interest
# Convert BALA2 from mL into L
Big_data$TBW   <- round(Big_data$TBW,V_dec_TBW)
Big_data$BMI   <- round(Big_data$BMI,V_dec_BMI)
Big_data$CKEP2 <- plyr::round_any(Big_data$CKEP2,V_dec_CKEP2)
Big_data$TOPR  <- round(Big_data$TOPR,V_dec_TOPR)
Big_data$SODI  <- round(Big_data$SODI,V_dec_SODI)
Big_data$BALA2 <- plyr::round_any(Big_data$BALA2,V_dec_BALA2)

#######################################################################################################################################################
# Creation a data for NONMEM, at the time of drug administration

V_AMT         <- c("15 mg/kg","25 mg/kg","30 mg/kg","35 mg/kg",
                   "15 mg/kg limited to 1,200 mg","25 mg/kg limited to 2,000 mg",
                   "1,500 mg","2,000 mg","3,000 mg","3,500 mg", "3,500 mg followed by 3,500 mg", 
                   "3,500 mg followed by 4,000 mg", "1,000 mg")
#,seq(from=1000, to=2500, by=100))
dat_T0         <- as.data.frame(V_AMT)
dat_T0$GROUP   <- seq(from=1, to=length(unique(V_AMT)), by=1)
dat_T0$V_TIME  <- rep(0, times=nrow(dat_T0))
dat_T0$DV      <- rep(0, times=nrow(dat_T0))
dat_T0$MDV     <- rep(1, times=nrow(dat_T0))
dat_T0$EVID    <- rep(1, times=nrow(dat_T0))
dat_T0$AUC     <- rep(0, times=nrow(dat_T0))
# View(dat_T0)

# Creation a data after the administration
# V_TIME         <- c(24,seq(from=96, to=192, by=12))
V_TIME           <- c(1,12,18,23,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108,114,120)
dat_AMT0         <- as.data.frame(V_TIME)
dat_AMT0$DV      <- rep(0, times=nrow(dat_AMT0))
dat_AMT0$MDV     <- rep(0, times=nrow(dat_AMT0))
dat_AMT0$EVID    <- rep(0, times=nrow(dat_AMT0))
dat_AMT0         <- dat_AMT0[rep(seq_len(nrow(dat_AMT0)),each=length(unique(V_AMT))),]
dat_AMT0$V_AMT   <- rep(V_AMT,nrow=length(unique(V_TIME)))
dat_AMT0$GROUP   <- rep(seq(from=1, to=length(unique(V_AMT)), by=1),nrow=length(unique(V_TIME)))
dat_AMT0         <- dat_AMT0[order(dat_AMT0$GROUP,dat_AMT0$V_TIME),]
dat_AMT0$AUC     <- rep(0, times=nrow(dat_AMT0))
# View(dat_AMT0)

# Combine the dataframes
dat_ID1 <- rbind(dat_T0,dat_AMT0)
names(dat_ID1)[names(dat_ID1)=="V_TIME"] <- "TIME"
dat_ID1 <- dat_ID1[order(dat_ID1$GROUP,dat_ID1$TIME),]
# View(dat_ID1)


# Create a data with a uniform distrbution covariate 
#########################################################################################################################
# Stratified random sampling
library(dplyr)
summary(Big_data)
Size_COV   <- 50
inc_BMI    <- 5.0
inc_TBW    <- 10
inc_CKEP2  <- 10
inc_TOPR   <- 10
inc_SODI   <- 7.5
inc_BALA2  <- 1000
n          <- 1

#BMI
unique(Big_data$BMI)
Small_data_BMI <- Big_data %>% group_by(BMI) %>% sample_n(size=Size_COV, replace=T)
# Verifications
all.equal(length(unique(Big_data$BMI)), length(unique(Small_data_BMI$BMI)))
all.equal(unique(Big_data$BMI), unique(Small_data_BMI$BMI))
test <- Small_data_BMI %>% group_by(BMI) %>% dplyr::summarise(count=n())
test
all.equal(Size_COV, min(test$count))
nrow(Small_data_BMI)
# Select values based on specified increments
V_inc_BMI <- seq(min(Small_data_BMI$BMI),max(Small_data_BMI$BMI),by=inc_BMI)
Small_data_BMI$Flg_inc_BMI <- as.numeric(Small_data_BMI$BMI %in% V_inc_BMI)
Small_data_BMI <- subset(Small_data_BMI,Flg_inc_BMI ==1)
Small_data_BMI <- Small_data_BMI[order(Small_data_BMI$BMI),]
# Remove the created columns
Small_data_BMI <- dplyr::select(Small_data_BMI,-contains("Flg"))
head(Small_data_BMI)
unique(Small_data_BMI$BMI)
# View(print(table(test)))
subset(Small_data_BMI,BMI==min(Small_data_BMI$BMI))
# Merge the dataframes with the simulated dataset for BMI
Big_data_BMI    <- Small_data_BMI[rep(seq_len(nrow(Small_data_BMI)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_BMI$ID <- rep(seq(from=1, to=nrow(Small_data_BMI)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_BMI)
dat_BMI_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_BMI)),]
dat_BMI_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_BMI)*length(unique(dat_BMI_IDs$V_AMT)), by=1), each=length(unique(dat_BMI_IDs$TIME))) 
# View(dat_BMI_IDs)
dat_BMI  <- merge(dat_BMI_IDs,Big_data_BMI)
dat_BMI  <- dat_BMI  %>% relocate (ID)
dat_BMI <- dat_BMI[order(dat_BMI$ID,dat_BMI$TIME),]
# View(dat_BMI)
# Add a flag
unique(dat_BMI$GROUP)
dat_BMI$Flg <- ifelse(dat_BMI$GROUP==1,11,ifelse(dat_BMI$GROUP==2,12,ifelse(dat_BMI$GROUP==3,13,
               ifelse(dat_BMI$GROUP==4,14,ifelse(dat_BMI$GROUP==5,15,ifelse(dat_BMI$GROUP==6,16,
               ifelse(dat_BMI$GROUP==7,17,ifelse(dat_BMI$GROUP==8,18,
               ifelse(dat_BMI$GROUP==9,19,ifelse(dat_BMI$GROUP==10,10,
               ifelse(dat_BMI$GROUP==11,111,ifelse(dat_BMI$GROUP==12,112,ifelse(dat_BMI$GROUP==13,113,
               0)))))))))))))
unique(dat_BMI$Flg)
unique(dat_BMI$BMI)
Uni_BMI <-length(unique(dat_BMI$BMI))
ID_BMI <-max(dat_BMI$ID)/length(unique(dat_BMI$GROUP))
head(dat_BMI)

#TBW
unique(Big_data$TBW)
head(Big_data)
Small_data_TBW <- Big_data %>% group_by(TBW) %>% sample_n(size=Size_COV, replace=T)
# Verifications
all.equal(length(unique(Big_data$TBW)), length(unique(Small_data_TBW$TBW)))
all.equal(unique(Big_data$TBW), unique(Small_data_TBW$TBW))
test <- Small_data_TBW %>% group_by(TBW) %>% dplyr::summarise(count=n())
test
# View(print(table(test)))
subset(Small_data_TBW,TBW==min(Small_data_TBW$TBW))
all.equal(Size_COV, min(test$count))
nrow(Small_data_TBW)
# Select values based on specified increments
V_inc_TBW <- seq(min(Small_data_TBW$TBW),max(Small_data_TBW$TBW),by=inc_TBW)
Small_data_TBW$Flg_inc_TBW <- as.numeric(Small_data_TBW$TBW %in% V_inc_TBW)
Small_data_TBW <- subset(Small_data_TBW,Flg_inc_TBW ==1)
Small_data_TBW <- Small_data_TBW[order(Small_data_TBW$TBW),]
# Remove the created columns
Small_data_TBW <- dplyr::select(Small_data_TBW,-contains("Flg"))
head(Small_data_TBW)
unique(Small_data_TBW$TBW)
# Merge the dataframes with the simulated dataset for TBW
Big_data_TBW    <- Small_data_TBW[rep(seq_len(nrow(Small_data_TBW)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_TBW$ID <- rep(seq(from=1, to=nrow(Small_data_TBW)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_TBW)
dat_TBW_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_TBW)),]
dat_TBW_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_TBW)*length(unique(dat_TBW_IDs$V_AMT)), by=1), each=length(unique(dat_TBW_IDs$TIME))) 
# View(dat_TBW_IDs)
dat_TBW  <- merge(dat_TBW_IDs,Big_data_TBW)
dat_TBW  <- dat_TBW  %>% relocate (ID)
dat_TBW <- dat_TBW[order(dat_TBW$ID,dat_TBW$TIME),]
unique(dat_TBW$TBW)
unique(dat_TBW$GROUP)
# View(dat_TBW)
# Add a flag
dat_TBW$Flg <- ifelse(dat_TBW$GROUP==1,21,ifelse(dat_TBW$GROUP==2,22,ifelse(dat_TBW$GROUP==3,23,
               ifelse(dat_TBW$GROUP==4,24,ifelse(dat_TBW$GROUP==5,25,ifelse(dat_TBW$GROUP==6,26,
               ifelse(dat_TBW$GROUP==7,27,ifelse(dat_TBW$GROUP==8,28,
               ifelse(dat_TBW$GROUP==9,29,ifelse(dat_TBW$GROUP==10,20,
               ifelse(dat_TBW$GROUP==11,121,ifelse(dat_TBW$GROUP==12,122,ifelse(dat_TBW$GROUP==13,123,
               0))))))))))))) 
unique(dat_TBW$Flg)
unique(dat_TBW$TBW)
Uni_TBW <-length(unique(dat_TBW$TBW))
ID_TBW <-max(dat_TBW$ID)/length(unique(dat_TBW$GROUP))
head(dat_TBW)

#CKEP2
unique(Big_data$CKEP2)
head(Big_data)
Small_data_CKEP2 <- Big_data %>% group_by(CKEP2) %>% sample_n(size=Size_COV, replace=T)
unique(Small_data_CKEP2$CKEP2)
# Verifications
all.equal(length(unique(Big_data$CKEP2)), length(unique(Small_data_CKEP2$CKEP2)))
all.equal(unique(Big_data$CKEP2), unique(Small_data_CKEP2$CKEP2))
test <- Small_data_CKEP2 %>% group_by(CKEP2) %>% dplyr::summarise(count=n())
test
# View(print(table(test)))
subset(Small_data_CKEP2,CKEP2==min(Small_data_CKEP2$CKEP2))
unique(test$CKEP2)
all.equal(Size_COV, min(test$count))
nrow(Small_data_CKEP2)
# Select values based on specified increments
V_inc_CKEP2 <- seq(min(Small_data_CKEP2$CKEP2),max(Small_data_CKEP2$CKEP2),by=inc_CKEP2)
Small_data_CKEP2$Flg_inc_CKEP2 <- as.numeric(Small_data_CKEP2$CKEP2 %in% V_inc_CKEP2)
Small_data_CKEP2 <- subset(Small_data_CKEP2,Flg_inc_CKEP2 ==1)
Small_data_CKEP2 <- Small_data_CKEP2[order(Small_data_CKEP2$CKEP2),]
# Remove the created columns
Small_data_CKEP2 <- dplyr::select(Small_data_CKEP2,-contains("Flg"))
head(Small_data_CKEP2)
unique(Small_data_CKEP2$CKEP2)
# Merge the dataframes with the simulated dataset for CKEP2
Big_data_CKEP2    <- Small_data_CKEP2[rep(seq_len(nrow(Small_data_CKEP2)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_CKEP2$ID <- rep(seq(from=1, to=nrow(Small_data_CKEP2)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_CKEP2)
dat_CKEP2_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_CKEP2)),]
dat_CKEP2_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_CKEP2)*length(unique(dat_CKEP2_IDs$V_AMT)), by=1), each=length(unique(dat_CKEP2_IDs$TIME))) 
# View(dat_CKEP2_IDs)
dat_CKEP2  <- merge(dat_CKEP2_IDs,Big_data_CKEP2)
dat_CKEP2  <- dat_CKEP2  %>% relocate (ID)
dat_CKEP2 <- dat_CKEP2[order(dat_CKEP2$ID,dat_CKEP2$TIME),]
unique(dat_CKEP2$CKEP2)
unique(dat_CKEP2$GROUP)
# View(dat_CKEP2)
# Add a flag
dat_CKEP2$Flg <- ifelse(dat_CKEP2$GROUP==1,31,ifelse(dat_CKEP2$GROUP==2,32,ifelse(dat_CKEP2$GROUP==3,33,
                 ifelse(dat_CKEP2$GROUP==4,34,ifelse(dat_CKEP2$GROUP==5,35,ifelse(dat_CKEP2$GROUP==6,36,
                 ifelse(dat_CKEP2$GROUP==7,37,ifelse(dat_CKEP2$GROUP==8,38,
                 ifelse(dat_CKEP2$GROUP==9,39,ifelse(dat_CKEP2$GROUP==10,30,
                 ifelse(dat_CKEP2$GROUP==11,131,ifelse(dat_CKEP2$GROUP==12,132,ifelse(dat_CKEP2$GROUP==13,133,0))))))))))))) 
unique(dat_CKEP2$Flg)
unique(dat_CKEP2$CKEP2)
Uni_CKEP2 <-length(unique(dat_CKEP2$CKEP2))
ID_CKEP2 <-max(dat_CKEP2$ID)/length(unique(dat_CKEP2$GROUP))
head(dat_CKEP2)

#TOPR
unique(Big_data$TOPR)
Small_data_TOPR <- Big_data %>% group_by(TOPR) %>% sample_n(size=Size_COV, replace=T)
# Verifications
all.equal(length(unique(Big_data$TOPR)), length(unique(Small_data_TOPR$TOPR)))
all.equal(unique(Big_data$TOPR), unique(Small_data_TOPR$TOPR))
test <- Small_data_TOPR %>% group_by(TOPR) %>% dplyr::summarise(count=n())
test
# View(print(table(test)))
subset(Small_data_TOPR,TOPR==min(Small_data_TOPR$TOPR))
unique(test$TOPR)
all.equal(Size_COV, min(test$count))
nrow(Small_data_TOPR)
# Select values based on specified increments
V_inc_TOPR <- seq(min(Small_data_TOPR$TOPR),max(Small_data_TOPR$TOPR),by=inc_TOPR)
Small_data_TOPR$Flg_inc_TOPR <- as.numeric(Small_data_TOPR$TOPR %in% V_inc_TOPR)
Small_data_TOPR <- subset(Small_data_TOPR,Flg_inc_TOPR ==1)
Small_data_TOPR <- Small_data_TOPR[order(Small_data_TOPR$TOPR),]
# Remove the created columns
Small_data_TOPR <- dplyr::select(Small_data_TOPR,-contains("Flg"))
head(Small_data_TOPR)
unique(Small_data_TOPR$TOPR)
# Merge the dataframes with the simulated dataset for TOPR
Big_data_TOPR    <- Small_data_TOPR[rep(seq_len(nrow(Small_data_TOPR)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_TOPR$ID <- rep(seq(from=1, to=nrow(Small_data_TOPR)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_TOPR)
dat_TOPR_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_TOPR)),]
dat_TOPR_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_TOPR)*length(unique(dat_TOPR_IDs$V_AMT)), by=1), each=length(unique(dat_TOPR_IDs$TIME))) 
# View(dat_TOPR_IDs)
dat_TOPR  <- merge(dat_TOPR_IDs,Big_data_TOPR)
dat_TOPR  <- dat_TOPR  %>% relocate (ID)
dat_TOPR <- dat_TOPR[order(dat_TOPR$ID,dat_TOPR$TIME),]
# View(dat_TOPR)
# Add a flag
unique(dat_TOPR$GROUP)
dat_TOPR$Flg <- ifelse(dat_TOPR$GROUP==1,41,ifelse(dat_TOPR$GROUP==2,42,ifelse(dat_TOPR$GROUP==3,43,
                ifelse(dat_TOPR$GROUP==4,44,ifelse(dat_TOPR$GROUP==5,45,ifelse(dat_TOPR$GROUP==6,46,
                ifelse(dat_TOPR$GROUP==7,47,ifelse(dat_TOPR$GROUP==8,48,
                ifelse(dat_TOPR$GROUP==9,49,ifelse(dat_TOPR$GROUP==10,40,
                ifelse(dat_TOPR$GROUP==11,141,ifelse(dat_TOPR$GROUP==12,142,ifelse(dat_TOPR$GROUP==13,143,
                0))))))))))))) 
unique(dat_TOPR$Flg)
unique(dat_TOPR$TOPR)
Uni_TOPR <-length(unique(dat_TOPR$TOPR))
ID_TOPR <-max(dat_TOPR$ID)/length(unique(dat_TOPR$GROUP))
head(dat_TOPR)

#SODI
unique(Big_data$SODI)
Small_data_SODI <- Big_data %>% group_by(SODI) %>% sample_n(size=Size_COV, replace=T)
# Verifications
all.equal(length(unique(Big_data$SODI)), length(unique(Small_data_SODI$SODI)))
all.equal(unique(Big_data$SODI), unique(Small_data_SODI$SODI))
test <- Small_data_SODI %>% group_by(SODI) %>% dplyr::summarise(count=n())
test
# View(print(table(test)))
subset(Small_data_SODI,SODI==min(Small_data_SODI$SODI))
unique(test$SODI)
all.equal(Size_COV, min(test$count))
nrow(Small_data_SODI)
# Select values based on specified increments
V_inc_SODI <- seq(min(Small_data_SODI$SODI),max(Small_data_SODI$SODI),by=inc_SODI)
Small_data_SODI$Flg_inc_SODI <- as.numeric(Small_data_SODI$SODI %in% V_inc_SODI)
Small_data_SODI <- subset(Small_data_SODI,Flg_inc_SODI ==1)
Small_data_SODI <- Small_data_SODI[order(Small_data_SODI$SODI),]
# Remove the created columns
Small_data_SODI <- dplyr::select(Small_data_SODI,-contains("Flg"))
head(Small_data_SODI)
unique(Small_data_SODI$SODI)
# Merge the dataframes with the simulated dataset for SODI
Big_data_SODI    <- Small_data_SODI[rep(seq_len(nrow(Small_data_SODI)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_SODI$ID <- rep(seq(from=1, to=nrow(Small_data_SODI)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_SODI)
dat_SODI_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_SODI)),]
dat_SODI_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_SODI)*length(unique(dat_SODI_IDs$V_AMT)), by=1), each=length(unique(dat_SODI_IDs$TIME))) 
# View(dat_SODI_IDs)
dat_SODI  <- merge(dat_SODI_IDs,Big_data_SODI)
dat_SODI  <- dat_SODI  %>% relocate (ID)
dat_SODI <- dat_SODI[order(dat_SODI$ID,dat_SODI$TIME),]
# View(dat_SODI)
# Add a flag
unique(dat_SODI$GROUP)
dat_SODI$Flg <- ifelse(dat_SODI$GROUP==1,51,ifelse(dat_SODI$GROUP==2,52,ifelse(dat_SODI$GROUP==3,53,
                ifelse(dat_SODI$GROUP==4,54,ifelse(dat_SODI$GROUP==5,55,ifelse(dat_SODI$GROUP==6,56,
                ifelse(dat_SODI$GROUP==7,57,ifelse(dat_SODI$GROUP==8,58,
                ifelse(dat_SODI$GROUP==9,59,ifelse(dat_SODI$GROUP==10,50, 
                ifelse(dat_SODI$GROUP==11,151,ifelse(dat_SODI$GROUP==12,152,ifelse(dat_SODI$GROUP==13,153,
                0))))))))))))) 
unique(dat_SODI$Flg)
unique(dat_SODI$SODI)
Uni_SODI <-length(unique(dat_SODI$SODI))
ID_SODI <-max(dat_SODI$ID)/length(unique(dat_SODI$GROUP))
head(dat_SODI)

#BALA2
unique(Big_data$BALA2)
Small_data_BALA2 <- Big_data %>% group_by(BALA2) %>% sample_n(size=Size_COV, replace=T)
# Verifications
all.equal(length(unique(Big_data$BALA2)), length(unique(Small_data_BALA2$BALA2)))
all.equal(unique(Big_data$BALA2), unique(Small_data_BALA2$BALA2))
test <- Small_data_BALA2 %>% group_by(BALA2) %>% dplyr::summarise(count=n())
test
# View(print(table(test)))
subset(Small_data_BALA2,BALA2==min(Small_data_BALA2$BALA2))
unique(test$BALA2)
all.equal(Size_COV, min(test$count))
nrow(Small_data_BALA2)
# Select values based on specified increments
V_inc_BALA2 <- seq(min(Small_data_BALA2$BALA2),max(Small_data_BALA2$BALA2),by=inc_BALA2)
Small_data_BALA2$Flg_inc_BALA2 <- as.numeric(Small_data_BALA2$BALA2 %in% V_inc_BALA2)
Small_data_BALA2 <- subset(Small_data_BALA2,Flg_inc_BALA2 ==1)
Small_data_BALA2 <- Small_data_BALA2[order(Small_data_BALA2$BALA2),]
# Remove the created columns
Small_data_BALA2 <- dplyr::select(Small_data_BALA2,-contains("Flg"))
head(Small_data_BALA2)
unique(Small_data_BALA2$BALA2)
# Merge the dataframes with the simulated dataset for BALA2
Big_data_BALA2    <- Small_data_BALA2[rep(seq_len(nrow(Small_data_BALA2)),each=length(unique(dat_ID1$V_AMT))),]
Big_data_BALA2$ID <- rep(seq(from=1, to=nrow(Small_data_BALA2)*length(unique(dat_ID1$V_AMT)), by=1))
# View(Big_data_BALA2)
dat_BALA2_IDs     <- dat_ID1[rep(seq_len(nrow(dat_ID1)),times=nrow(Small_data_BALA2)),]
dat_BALA2_IDs$ID  <- rep(seq(from=1, to=nrow(Small_data_BALA2)*length(unique(dat_BALA2_IDs$V_AMT)), by=1), each=length(unique(dat_BALA2_IDs$TIME))) 
# View(dat_BALA2_IDs)
dat_BALA2  <- merge(dat_BALA2_IDs,Big_data_BALA2)
dat_BALA2  <- dat_BALA2  %>% relocate (ID)
dat_BALA2 <- dat_BALA2[order(dat_BALA2$ID,dat_BALA2$TIME),]
# View(dat_BALA2)
# Add a flag
unique(dat_BALA2$GROUP)
dat_BALA2$Flg <- ifelse(dat_BALA2$GROUP==1,61,ifelse(dat_BALA2$GROUP==2,62,ifelse(dat_BALA2$GROUP==3,63,
                 ifelse(dat_BALA2$GROUP==4,64,ifelse(dat_BALA2$GROUP==5,65,ifelse(dat_BALA2$GROUP==6,66,
                 ifelse(dat_BALA2$GROUP==7,67,ifelse(dat_BALA2$GROUP==8,68,
                 ifelse(dat_BALA2$GROUP==9,69,ifelse(dat_BALA2$GROUP==10,60,
                 ifelse(dat_BALA2$GROUP==11,161,ifelse(dat_BALA2$GROUP==12,162,ifelse(dat_BALA2$GROUP==13,163,
                 0))))))))))))) 
unique(dat_BALA2$Flg)
unique(dat_BALA2$BALA2)
Uni_BALA2 <-length(unique(dat_BALA2$BALA2))
ID_BALA2 <-max(dat_BALA2$ID)/length(unique(dat_BALA2$GROUP))
head(dat_BALA2)

# Verification if the difference between the unique values of covariates is different than the interval
Interval_BMI          <- diff(unique(dat_BMI$BMI))
Interval_BMI
CheckInterval_BMI     <- ifelse(sprintf("%.10f",(diff(unique(dat_BMI$BMI))))==sprintf("%.10f",inc_BMI),0,1)
CheckInterval_BMI
Interval_TBW          <- diff(unique(dat_TBW$TBW))
Interval_TBW
CheckInterval_TBW     <- ifelse(sprintf("%.10f",(diff(unique(dat_TBW$TBW))))==sprintf("%.10f",inc_TBW),0,1)
CheckInterval_TBW
Interval_CKEP2        <- diff(unique(dat_CKEP2$CKEP2))
Interval_CKEP2
CheckInterval_CKEP2   <- ifelse(sprintf("%.10f",(diff(unique(dat_CKEP2$CKEP2))))==sprintf("%.10f",inc_CKEP2),0,1)
CheckInterval_CKEP2
Interval_BALA2        <- diff(unique(dat_BALA2$BALA2))
Interval_BALA2
CheckInterval_BALA2   <- ifelse(sprintf("%.10f",(diff(unique(dat_BALA2$BALA2))))==sprintf("%.10f",inc_BALA2),0,1)
CheckInterval_BALA2
Interval_TOPR        <- diff(unique(dat_TOPR$TOPR))
Interval_TOPR
CheckInterval_TOPR   <- ifelse(sprintf("%.10f",(diff(unique(dat_TOPR$TOPR))))==sprintf("%.10f",inc_TOPR),0,1)
CheckInterval_TOPR
Interval_SODI        <- diff(unique(dat_SODI$SODI))
Interval_SODI
CheckInterval_SODI   <- ifelse(sprintf("%.10f",(diff(unique(dat_SODI$SODI))))==sprintf("%.10f",inc_SODI),0,1)
CheckInterval_SODI

# Join the data
dat_extra <- rbind(dat_BMI,dat_TBW,dat_CKEP2,dat_BALA2,dat_TOPR,dat_SODI)
max(unique(dat_extra$BALA2))

# Calculate the sample size
Cov <- c("BMI","TBW","CKEP2","TOPR","SODI","BALA2")
Unique_Cov <- c(Uni_BMI,Uni_TBW,Uni_CKEP2,Uni_TOPR,Uni_SODI,Uni_BALA2)
Individuals_Cov <- c(ID_BMI,ID_TBW,ID_CKEP2,ID_TOPR,ID_SODI,ID_BALA2)
Data_Sample <- data.frame(Cov,Individuals_Cov,Unique_Cov)
Data_Sample$UniqueIndividuals_Cov <- Data_Sample$Individuals_Cov/Data_Sample$Unique_Cov
write.csv(Data_Sample,file="Sample Size per category.csv")

Sample_Size <- nrow(dat_extra)/(length(unique(dat_extra$GROUP))*length(unique(dat_extra$TIME)))
SumIndividuals_Cov <- sum(ID_BMI,ID_TBW,ID_CKEP2,ID_TOPR,ID_SODI,ID_BALA2)
Title <- c("Sample size")
Answer <- print(Sample_Size)
Data_Sample <- data.frame(Title,Answer,SumIndividuals_Cov)
write.csv(Data_Sample,file="Sample Size.csv")


# Calculate the amount of administered medication
Timetolabresult <- 23
CutoffGFR <- 126

unique(dat_extra$V_AMT)
dat_extra$AMT   <- as.numeric(ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="15 mg/kg",15*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="25 mg/kg",25*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="30 mg/kg",30*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="35 mg/kg",35*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="15 mg/kg limited to 1,200 mg" & dat_extra$TBW <=80,15*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="15 mg/kg limited to 1,200 mg" & dat_extra$TBW >80,1200,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="25 mg/kg limited to 2,000 mg" & dat_extra$TBW <=80,25*dat_extra$TBW,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="25 mg/kg limited to 2,000 mg" & dat_extra$TBW >80,2000,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="1,500 mg",1500,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="2,000 mg",2000,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="3,000 mg",3000,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="3,500 mg",3500,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="3,500 mg followed by 3,500 mg",3500,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="3,500 mg followed by 4,000 mg",3500,ifelse(
  dat_extra$TIME==0 & dat_extra$V_AMT=="1,000 mg",1000,ifelse(
  dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 3,500 mg" & dat_extra$CKEP2>=CutoffGFR,3500,ifelse(
  dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 4,000 mg" & dat_extra$CKEP2>=CutoffGFR,4000,
    0))))))))))))))))))
unique(dat_extra$V_AMT)
dat_extra$RATE    <- 2*dat_extra$AMT

# dat_extra_test <- subset(dat_extra, select=c("ID","TIME","GROUP","V_AMT","AMT","TBW","RATE"))
# View(dat_extra_test)

# Conversion of MDV and EVID at the chosen Timetolabresult
dat_extra$MDV    <- ifelse(dat_extra$TIME==0
                      |dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 3,500 mg" & dat_extra$CKEP2>=CutoffGFR
                      |dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 4,000 mg" & dat_extra$CKEP2>=CutoffGFR,1,0)

dat_extra$EVID    <- ifelse(dat_extra$TIME==0 
                       |dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 3,500 mg" & dat_extra$CKEP2>=CutoffGFR
                       |dat_extra$TIME==Timetolabresult & dat_extra$V_AMT=="3,500 mg followed by 4,000 mg" & dat_extra$CKEP2>=CutoffGFR,1,0)

#dat_extra_test <- subset(dat_extra, V_AMT=="3,500 mg followed by 3,500 mg")
#dat_extra_test <- subset(dat_extra_test, CKEP2>=CutoffGFR)
#View(dat_extra_test)


# Reorders columns by name
colnames(dat_extra)
dat_extra <- subset(dat_extra, select=c("ID","TIME","V_AMT","AMT","RATE","DV","MDV","EVID","GROUP","BMI","TBW","CKEP2",
                          "TOPR","SODI","BALA2","Flg"))
head(dat_extra)
unique(dat_extra$Flg)
unique(dat_extra$V_AMT)

# Rename the columns similarly to the ".mod" file
names(dat_extra)[names(dat_extra)=="TBW"]    <- "WT"
names(dat_extra)[names(dat_extra)=="V_AMT"]    <- "Dose"
colnames(dat_extra)

# Check if all the columns are numeric
str(dat_extra)

# Check data characteristics
length(unique(dat_extra$ID))
unique(dat_extra$GROUP)
unique(dat_extra$Dose)
unique(dat_extra$Flg)
unique(dat_extra$TIME)
# dat_extra_test <- subset(dat_extra, Flg==0)
# View(dat_extra_test)

dat_extra$FTIM  <- ifelse(dat_extra$TIME>0 & dat_extra$TIME<=48,1,ifelse(dat_extra$TIME>48,2,0))

dat_extra$FAUC     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 24                     ,1,0)
dat_extra$FPEAK    <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 1                      ,1,0)
dat_extra$FBOT     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 12| dat_extra$TIME== 48,1,0)
dat_extra$FFIN     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 18| dat_extra$TIME== 30| dat_extra$TIME== 36| dat_extra$TIME== 42,1,0)
dat_extra$FHIG     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 72| dat_extra$TIME== 96,1,0)
dat_extra$FSIN     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 54| dat_extra$TIME== 60| dat_extra$TIME== 66| dat_extra$TIME== 78| dat_extra$TIME== 84,1,0)
dat_extra$FTIN     <- ifelse(dat_extra$TIME==0  | dat_extra$TIME== Timetolabresult   | dat_extra$TIME== 90| dat_extra$TIME== 102| dat_extra$TIME==108| dat_extra$TIME==114| dat_extra$TIME==120,1,0)

dat_extra$FFAUC <- ifelse(dat_extra$FAUC  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FAUC  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FFPEAK<- ifelse(dat_extra$FPEAK ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FPEAK ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FFBOT <- ifelse(dat_extra$FBOT  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FBOT  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FFIN  <- ifelse(dat_extra$FFIN  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FFIN  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FFHIG <- ifelse(dat_extra$FHIG  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FHIG  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FSIN  <- ifelse(dat_extra$FSIN  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FSIN  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)
dat_extra$FTIN  <- ifelse(dat_extra$FTIN  ==1 & dat_extra$Flg>=min(dat_extra$Flg)|dat_extra$FTIN  ==1 & dat_extra$Flg<=min(dat_extra$Flg),dat_extra$Flg,1000)

# dat_extra_test <- subset(dat_extra, FFAUC==13)
# dat_extra_test <- subset(dat_extra_test, GROUP==12 & CKEP2>=CutoffGFR)
# View(dat_extra_test)

# Remove the column of dose
dat_extra <- subset(dat_extra, select=c("ID","TIME","AMT","RATE","DV","MDV","EVID","GROUP",
                   "BMI","WT","CKEP2","TOPR","SODI","BALA2","Flg","FFPEAK","FFAUC","FFBOT","FFIN","FSIN","FTIN","FFHIG"))
str(dat_extra)
unique(dat_extra$ID)
# Print the csv
setwd("C:/Users/u0147124/OneDrive - KU Leuven/PhD_KU Leuven/3_Amikacin_ER_PhD_KU Leuven/Data analysis")
# setwd("~/Amikacin")
write.table(dat_extra, file="Input_data_D030522.csv",quote = F,row.names=FALSE,col.names=TRUE, sep=" ")

