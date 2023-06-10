library(table1)
library(dplyr)
library(zoo) #na.locf function
library(rstatix)
library(ggplot2)
#library(qqplot)
library(ggpubr)
library(mice)

#Before report dose, fill dose in every row
DataDVAMT1 <- clean_fluctotiv
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

#datasum<-datasum %>% filter (DV!=".") #for sampling events
#datasum<-datasum  #for dosing event
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

# Apply render function 
#table1(~ BW + CREAT | HOSPITAL, data=datasum2,
#render=rndr)

## Test normality

# The whole dataset
#test <- datasum %>% shapiro_test(APACHE, LENGTH, AGE,BW,CREAT, CKDEPI,CL24,
#GGT,AFT,AST,ALT,BILI,ALB,SOFA,FLUID,BMI,CG)
#test

# Each individual dataset
#study1 <-datasum %>% filter(HOSPITAL=="Study 1")
#test1 <- study1 %>% shapiro_test(APACHE, LENGTH, AGE,BW,CREAT, CKDEPI,CL24,
#GGT,AFT,AST,ALT,BILI,ALB,SOFA,FLUID,BMI,CG)
#test1

# As all continuous variables are not normally distributed, we report medidan and IQR

#class(datasum1$APACHE)

table1(~ APACHE + LENGTH + AGE + SEX|HOSPITAL, data=datasum1,render.continuous=
               c(.="Median [Q1-Q3]"))
table1(~ BW + CREAT + CKDEPI + CL24 + GGT + AFT + AST +
               ALT + BILI + ALB + SOFA + FLUID + BMI+CRRT|HOSPITAL, data=datasum2,
       render.continuous=c(.="Median [Q1-Q3]"))
#table1(~ BW + CREAT + CKDEPI +CRRT|HOSPITAL, data=datasum2,
       #render.continuous=c(.="Median [Q1-Q3]"))

setwd("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazol_project/working documents")
library(dplyr)
library(ggplot2)
FlucTotIV_imputecreat <- clean_fluctotiv %>%filter(!HOSPITAL%in%c(3,4))
FlucTotIV_imputecreat$CREAT<-ifelse(is.na(FlucTotIV_imputecreat$CREAT),-0.1,FlucTotIV_imputecreat$CREAT) #This is the dataset that I will use thereafter

crrt_1_ids <- unique(FlucTotIV_imputecreat$ID[FlucTotIV_imputecreat$CRRT == 1 & !FlucTotIV_imputecreat$ID %in% FlucTotIV_imputecreat$ID[FlucTotIV_imputecreat$CRRT == 0]]) 
miscel_IDs <- FlucTotIV_imputecreat %>%
        group_by(ID) %>% 
        summarize(has_both_crrt = n_distinct(CRRT) == 2) %>% 
        filter(has_both_crrt) %>% 
        pull(ID) #equal 17 
## After that, I'll check how many non-CRRT patients are there per each HOSPITAL
nonCRRT_IDs<-setdiff(unique(FlucTotIV_imputecreat$ID),c(crrt_1_ids,miscel_IDs))
FlucTotIV_imputecreat %>% 
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

hospital_1 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 1) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_1_groups <- split(hospital_1, ceiling(seq_along(hospital_1)/4))

# Here I try to plot it in a different fashion for better illustration (discriminate between NAs and non_NAs also between CRRT and non-CRRT)
for (i in seq_along(hospital_1_groups)) {
        nonCRRT_IDs_1 <- hospital_1_groups[[i]]
        CRRT_non_1 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_1)
        CRRT_non_1_pos <- CRRT_non_1 %>% 
                filter(CREAT > 0)
        CRRT_non_1_neg <- CRRT_non_1 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_01 <- ggplot() +
                geom_line(data = CRRT_non_1_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_1_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_1, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 1 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
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

hospital_2 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 2) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_2_groups <- split(hospital_2, ceiling(seq_along(hospital_2)/5))
for (i in seq_along(hospital_2_groups)) {
        nonCRRT_IDs_2 <- hospital_2_groups[[i]]
        CRRT_non_2 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_2)
        CRRT_non_2_pos <- CRRT_non_2 %>% 
                filter(CREAT > 0)
        CRRT_non_2_neg <- CRRT_non_2 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_02 <- ggplot() +
                geom_line(data = CRRT_non_2_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_2_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_2, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 2 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
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

hospital_5 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 5) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_5_groups <- split(hospital_5, ceiling(seq_along(hospital_5)/5))
for (i in seq_along(hospital_5_groups)) {
        nonCRRT_IDs_5 <- hospital_5_groups[[i]]
        CRRT_non_5 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_5)
        CRRT_non_5_pos <- CRRT_non_5 %>% 
                filter(CREAT > 0)
        CRRT_non_5_neg <- CRRT_non_5 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_05 <- ggplot() +
                geom_line(data = CRRT_non_5_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_5_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_5, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 5 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
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
hospital_6 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 6) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_6_groups <- split(hospital_6, ceiling(seq_along(hospital_6)/6))
for (i in seq_along(hospital_6_groups)) {
        nonCRRT_IDs_6 <- hospital_6_groups[[i]]
        CRRT_non_6 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_6)
        CRRT_non_6_pos <- CRRT_non_6 %>% 
                filter(CREAT > 0)
        CRRT_non_6_neg <- CRRT_non_6 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_06 <- ggplot() +
                geom_line(data = CRRT_non_6_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_6_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_6, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 6 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
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
hospital_7 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 7) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_7_groups <- split(hospital_7, ceiling(seq_along(hospital_7)/6))
for (i in seq_along(hospital_7_groups)) {
        nonCRRT_IDs_7 <- hospital_7_groups[[i]]
        CRRT_non_7 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_7)
        CRRT_non_7_pos <- CRRT_non_7 %>% 
                filter(CREAT > 0)
        CRRT_non_7_neg <- CRRT_non_7 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_07 <- ggplot() +
                geom_line(data = CRRT_non_7_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_7_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_7, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 7 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
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
hospital_8 <- FlucTotIV_imputecreat %>% 
        filter(HOSPITAL == 8) %>% 
        select(ID) %>% 
        distinct() %>% 
        filter(ID %in% nonCRRT_IDs) %>% 
        pull(ID)

hospital_8_groups <- split(hospital_8, ceiling(seq_along(hospital_8)/6))
for (i in seq_along(hospital_8_groups)) {
        nonCRRT_IDs_8 <- hospital_8_groups[[i]]
        CRRT_non_8 <- FlucTotIV_imputecreat %>% 
                filter(ID %in% nonCRRT_IDs_8)
        CRRT_non_8_pos <- CRRT_non_8 %>% 
                filter(CREAT > 0)
        CRRT_non_8_neg <- CRRT_non_8 %>% 
                filter(CREAT <0)
        creat_time_nonCRRT_08 <- ggplot() +
                geom_line(data = CRRT_non_8_pos, aes(x = TIME, y = CREAT, group = ID, color = factor(CRRT)), size = 1) +
                geom_line(data = CRRT_non_8_neg, aes(x = TIME, y = CREAT, group = ID), color = "grey", size = 1) +
                geom_point(data = CRRT_non_8, aes(x = TIME, y = CREAT, shape = factor(CRRT)), size = 2) +
                scale_shape_manual(values = c(16)) +
                scale_color_manual(values = c("black")) +
                labs(title = paste0("CREAT over Time for Patients with no CRRT of hospital 8 ", sprintf("%02d", i)), 
                     x = "Time (hours)", y = "CREAT (mg/dL") +
                theme_bw() +
                geom_rect(data = CRRT_non_8[CRRT_non_8$CRRT == 0, ], 
                          aes(xmin = TIME, xmax = TIME+0.1, ymin = -Inf, ymax = Inf),
                          fill = "grey", alpha = 0.2) +
                facet_wrap(~ ID, scales = "free") +
                guides(shape = guide_legend(title = "CRRT"), color = guide_legend(title = "CRRT"))
        ggsave(paste0("creat_time_nonCRRT_hos8_", sprintf("%02d", i), ".png"), 
               plot = creat_time_nonCRRT_08, dpi = 300, width = 10, height = 5)
}