#### We first set some initial objects that we will use later on. 
#### We also specify the confidence interval and prediction intervals and the bin times. 
#### The bin times are needed to group observations together if not all the samples were taken at the same time points.

#### Load libraries ####
library(ggplot2)
library(xpose)
library(dplyr)
library(PsNR)
library(magrittr)
library(methods)
library(xpose4)
library(vpc)
library(cowplot)
#########################################
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run01") #update wd according to new computer 131123

Number_of_simulations_performed <- 1000 ## Used as 'samples' argument in PsN vpc function. Needs to be a round number.


#########################################
# Set the prediction intervals

PI <- 95 #specify prediction interval in percentage. Common is 95% interval (2.5-97.5%)
CI <- 95 #specify confidence interval in percentage. Common is 95%

# Make a vector for upper and lower percentile
perc_PI <- c(0+(1-PI/100)/2, 1-(1-PI/100)/2)
perc_CI <- c(0+(1-CI/100)/2, 1-(1-CI/100)/2)

#########################################
# Specify the bin times manually


#bin_times <- c(0.304387974683544,3.73,9.74,14.865,21.225,39.035,72.24216)
bin_times <- c(0.01549,1.33,1.77,3.665,5.81,7.275,9.6,10.77,12.3835,13.135,14.425,15.485,16.905,18.035,19.54,21.225,22.225,23.1585,24.63,25.52549)


Number_of_bins <- length(bin_times)-1


#### We then search for the created simulation by PsN and read it using the xpose function read.nm.tables() and only select the observations (MDV ==0). 
#### We also need to create a new column which specifies from which of the 500 replicates the simulations originate and link the bin number with the observations.

#########################################
######### Load in simulated data 

## Search for the generated file by PsN in the sub-folders
files <- list.files(pattern = "1.npctab.dta", recursive = TRUE, include.dirs = TRUE)

# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run01")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod01 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod01 <- dataframe_simulations_mod01[dataframe_simulations_mod01$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod01$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod01)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod01 <- unique(dataframe_simulations_mod01$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod01$BIN[dataframe_simulations_mod01$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod01 <- dataframe_simulations_mod01[dataframe_simulations_mod01$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod01 <- merge(dataframe_simulations_mod01,PRED_BIN_mod01,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod01$PCDV <- dataframe_simulations_mod01$DV *(dataframe_simulations_mod01$PREDBIN/dataframe_simulations_mod01$PRED)


dataframe_simulations_mod01 <- dataframe_simulations_mod01[order(dataframe_simulations_mod01$replicate,dataframe_simulations_mod01$ID,dataframe_simulations_mod01$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod01 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod01){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod01 <- dataframe_simulations_mod01 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod01 <- rbind(sim_PI_mod01, sim_vpc_ci_mod01)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod01 <- sim_PI_mod01 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod01$x1 <- NA
sim_CI_mod01$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod01$x1[sim_CI_mod01$BIN == i] <-bin_times[i]
        sim_CI_mod01$x2[sim_CI_mod01$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod01<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run01'
observations_tablefile_mod01 <- paste0(working.directory_mod01, '/vpc_original.npctab.dta')
Obs_mod01 <- read_nonmem_table(observations_tablefile_mod01)
Obs_mod01<- Obs_mod01[Obs_mod01$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod01 <-  dataframe_simulations_mod01[ #leave this out for a second 131123
        #dataframe_simulations_mod01$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod01 <- merge(Obs_mod01,Rep1_mod01,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod01$BIN[Obs_mod01$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod01 <- merge(Obs_mod01,PRED_BIN_mod01,by='BIN')


Obs_mod01$PCDV <- Obs_mod01$DV *(Obs_mod01$PREDBIN/Obs_mod01$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod01 <- Obs_mod01 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod01 <- Obs_mod01 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod01 <- bin_middle_mod01$bin_middle


obs_vpc_mod01$TAD <- bin_middle_mod01

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod01 <- ggplot() +
        
        
        ## Set bins
        geom_rect(data=sim_CI_mod01, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
        geom_rect(data=sim_CI_mod01, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
        geom_rect(data=sim_CI_mod01, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
        
        
        # Lines of the observations
        geom_line(data = obs_vpc_mod01, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        geom_line(data = obs_vpc_mod01, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        geom_line(data = obs_vpc_mod01, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        
        
        ###### Add observations
        geom_point(data = Obs_mod01,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
      
    ###### Set x & y axis limit, and their labels
        scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
        scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
    ## Add title and subtitle
        ggtitle("Model 01") +
        
        
    ####### Set ggplot2 theme and settings
        theme_bw()+
        theme(axis.title=element_text(size=12),
              axis.text = element_text(size = 12),
              plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),legend.position="none") +
        
        
        # Set axis labels
        labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
        
        
        # Add vertical lines to indicate dosing
        geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154') #+
        
        
        # Set axis
        #scale_y_log10(expand=c(0.01,0))+
        #scale_x_continuous(expand=c(0.01,0))#+
        
        
        

#########################################
# Mod 02
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run02")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod02 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod02 <- dataframe_simulations_mod02[dataframe_simulations_mod02$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod02$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod02)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod02 <- unique(dataframe_simulations_mod02$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod02$BIN[dataframe_simulations_mod02$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod02 <- dataframe_simulations_mod02[dataframe_simulations_mod02$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod02 <- merge(dataframe_simulations_mod02,PRED_BIN_mod02,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod02$PCDV <- dataframe_simulations_mod02$DV *(dataframe_simulations_mod02$PREDBIN/dataframe_simulations_mod02$PRED)


dataframe_simulations_mod02 <- dataframe_simulations_mod02[order(dataframe_simulations_mod02$replicate,dataframe_simulations_mod02$ID,dataframe_simulations_mod02$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod02 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod02){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod02 <- dataframe_simulations_mod02 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod02 <- rbind(sim_PI_mod02, sim_vpc_ci_mod02)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod02 <- sim_PI_mod02 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod02$x1 <- NA
sim_CI_mod02$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod02$x1[sim_CI_mod02$BIN == i] <-bin_times[i]
        sim_CI_mod02$x2[sim_CI_mod02$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod02<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run02'
observations_tablefile_mod02 <- paste0(working.directory_mod02, '/vpc_original.npctab.dta')
Obs_mod02 <- read_nonmem_table(observations_tablefile_mod02)
Obs_mod02<- Obs_mod02[Obs_mod02$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod02 <-  dataframe_simulations_mod02[ #commented this out for a second - 131123
        #dataframe_simulations_mod02$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod02 <- merge(Obs_mod02,Rep1_mod02,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod02$BIN[Obs_mod02$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod02 <- merge(Obs_mod02,PRED_BIN_mod02,by='BIN')


Obs_mod02$PCDV <- Obs_mod02$DV *(Obs_mod02$PREDBIN/Obs_mod02$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod02 <- Obs_mod02 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod02 <- Obs_mod02 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod02 <- bin_middle_mod02$bin_middle


obs_vpc_mod02$TAD <- bin_middle_mod02

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod02 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod02, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod02, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod02, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod02, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod02, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod02, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod02,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 02") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')
        
        
        # Set axis
        #scale_y_log10(expand=c(0.01,0))+
        #scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod02
        

#########################################
# Mod 03
# Set working directory of model file
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run03")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod03 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod03 <- dataframe_simulations_mod03[dataframe_simulations_mod03$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod03$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod03)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod03 <- unique(dataframe_simulations_mod03$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod03$BIN[dataframe_simulations_mod03$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod03 <- dataframe_simulations_mod03[dataframe_simulations_mod03$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod03 <- merge(dataframe_simulations_mod03,PRED_BIN_mod03,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod03$PCDV <- dataframe_simulations_mod03$DV *(dataframe_simulations_mod03$PREDBIN/dataframe_simulations_mod03$PRED)


dataframe_simulations_mod03 <- dataframe_simulations_mod03[order(dataframe_simulations_mod03$replicate,dataframe_simulations_mod03$ID,dataframe_simulations_mod03$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod03 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod03){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod03 <- dataframe_simulations_mod03 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod03 <- rbind(sim_PI_mod03, sim_vpc_ci_mod03)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod03 <- sim_PI_mod03 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod03$x1 <- NA
sim_CI_mod03$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod03$x1[sim_CI_mod03$BIN == i] <-bin_times[i]
        sim_CI_mod03$x2[sim_CI_mod03$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod03<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run03'
observations_tablefile_mod03 <- paste0(working.directory_mod03, '/vpc_original.npctab.dta')
Obs_mod03 <- read_nonmem_table(observations_tablefile_mod03)
Obs_mod03<- Obs_mod03[Obs_mod03$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


# Rep1_mod03 <-  dataframe_simulations_mod03[ 
        # dataframe_simulations_mod03$replicate ==1,c("ID","TAD","PRED")] 


# Obs_mod03 <- merge(Obs_mod03,Rep1_mod03,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod03$BIN[Obs_mod03$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod03 <- merge(Obs_mod03,PRED_BIN_mod03,by='BIN')


Obs_mod03$PCDV <- Obs_mod03$DV *(Obs_mod03$PREDBIN/Obs_mod03$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod03 <- Obs_mod03 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod03 <- Obs_mod03 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod03 <- bin_middle_mod03$bin_middle


obs_vpc_mod03$TAD <- bin_middle_mod03

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod03 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod03, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod03, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod03, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod03, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod03, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod03, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod03,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 03") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod03    

#########################################
# Mod 04
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run04")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod04 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod04 <- dataframe_simulations_mod04[dataframe_simulations_mod04$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod04$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod04)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod04 <- unique(dataframe_simulations_mod04$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod04$BIN[dataframe_simulations_mod04$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod04 <- dataframe_simulations_mod04[dataframe_simulations_mod04$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod04 <- merge(dataframe_simulations_mod04,PRED_BIN_mod04,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod04$PCDV <- dataframe_simulations_mod04$DV *(dataframe_simulations_mod04$PREDBIN/dataframe_simulations_mod04$PRED)


dataframe_simulations_mod04 <- dataframe_simulations_mod04[order(dataframe_simulations_mod04$replicate,dataframe_simulations_mod04$ID,dataframe_simulations_mod04$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod04 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod04){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod04 <- dataframe_simulations_mod04 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod04 <- rbind(sim_PI_mod04, sim_vpc_ci_mod04)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod04 <- sim_PI_mod04 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod04$x1 <- NA
sim_CI_mod04$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod04$x1[sim_CI_mod04$BIN == i] <-bin_times[i]
        sim_CI_mod04$x2[sim_CI_mod04$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod04<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run04'
observations_tablefile_mod04 <- paste0(working.directory_mod04, '/vpc_original.npctab.dta')
Obs_mod04 <- read_nonmem_table(observations_tablefile_mod04)
Obs_mod04<- Obs_mod04[Obs_mod04$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod04 <-  dataframe_simulations_mod04[    #temporarily commented out 131123
        #dataframe_simulations_mod04$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod04 <- merge(Obs_mod04,Rep1_mod04,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod04$BIN[Obs_mod04$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod04 <- merge(Obs_mod04,PRED_BIN_mod04,by='BIN')


Obs_mod04$PCDV <- Obs_mod04$DV *(Obs_mod04$PREDBIN/Obs_mod04$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod04 <- Obs_mod04 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod04 <- Obs_mod04 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod04 <- bin_middle_mod04$bin_middle


obs_vpc_mod04$TAD <- bin_middle_mod04

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod04 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod04, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod04, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod04, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod04, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod04, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod04, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod04,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 04") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod04    

#########################################
# Mod 05
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run05")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod05 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod05 <- dataframe_simulations_mod05[dataframe_simulations_mod05$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod05$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod05)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod05 <- unique(dataframe_simulations_mod05$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod05$BIN[dataframe_simulations_mod05$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod05 <- dataframe_simulations_mod05[dataframe_simulations_mod05$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod05 <- merge(dataframe_simulations_mod05,PRED_BIN_mod05,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod05$PCDV <- dataframe_simulations_mod05$DV *(dataframe_simulations_mod05$PREDBIN/dataframe_simulations_mod05$PRED)


dataframe_simulations_mod05 <- dataframe_simulations_mod05[order(dataframe_simulations_mod05$replicate,dataframe_simulations_mod05$ID,dataframe_simulations_mod05$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod05 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod05){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod05 <- dataframe_simulations_mod05 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod05 <- rbind(sim_PI_mod05, sim_vpc_ci_mod05)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod05 <- sim_PI_mod05 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod05$x1 <- NA
sim_CI_mod05$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod05$x1[sim_CI_mod05$BIN == i] <-bin_times[i]
        sim_CI_mod05$x2[sim_CI_mod05$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod05<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run05'
observations_tablefile_mod05 <- paste0(working.directory_mod05, '/vpc_original.npctab.dta')
Obs_mod05 <- read_nonmem_table(observations_tablefile_mod05)
Obs_mod05<- Obs_mod05[Obs_mod05$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod05 <-  dataframe_simulations_mod05[ 
        #dataframe_simulations_mod05$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod05 <- merge(Obs_mod05,Rep1_mod05,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod05$BIN[Obs_mod05$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod05 <- merge(Obs_mod05,PRED_BIN_mod05,by='BIN')


Obs_mod05$PCDV <- Obs_mod05$DV *(Obs_mod05$PREDBIN/Obs_mod05$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod05 <- Obs_mod05 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod05 <- Obs_mod05 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod05 <- bin_middle_mod05$bin_middle


obs_vpc_mod05$TAD <- bin_middle_mod05

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod05 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod05, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod05, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod05, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod05, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod05, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod05, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod05,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 05") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod05 
                        
#########################################
# Mod 06
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run06")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod06 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod06 <- dataframe_simulations_mod06[dataframe_simulations_mod06$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod06$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod06)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod06 <- unique(dataframe_simulations_mod06$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod06$BIN[dataframe_simulations_mod06$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod06 <- dataframe_simulations_mod06[dataframe_simulations_mod06$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod06 <- merge(dataframe_simulations_mod06,PRED_BIN_mod06,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod06$PCDV <- dataframe_simulations_mod06$DV *(dataframe_simulations_mod06$PREDBIN/dataframe_simulations_mod06$PRED)


dataframe_simulations_mod06 <- dataframe_simulations_mod06[order(dataframe_simulations_mod06$replicate,dataframe_simulations_mod06$ID,dataframe_simulations_mod06$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod06 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod06){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod06 <- dataframe_simulations_mod06 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod06 <- rbind(sim_PI_mod06, sim_vpc_ci_mod06)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod06 <- sim_PI_mod06 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod06$x1 <- NA
sim_CI_mod06$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod06$x1[sim_CI_mod06$BIN == i] <-bin_times[i]
        sim_CI_mod06$x2[sim_CI_mod06$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod06<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run06'
observations_tablefile_mod06 <- paste0(working.directory_mod06, '/vpc_original.npctab.dta')
Obs_mod06 <- read_nonmem_table(observations_tablefile_mod06)
Obs_mod06<- Obs_mod06[Obs_mod06$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod06 <-  dataframe_simulations_mod06[ 
        #dataframe_simulations_mod06$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod06 <- merge(Obs_mod06,Rep1_mod06,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod06$BIN[Obs_mod06$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod06 <- merge(Obs_mod06,PRED_BIN_mod06,by='BIN')


Obs_mod06$PCDV <- Obs_mod06$DV *(Obs_mod06$PREDBIN/Obs_mod06$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod06 <- Obs_mod06 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod06 <- Obs_mod06 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod06 <- bin_middle_mod06$bin_middle


obs_vpc_mod06$TAD <- bin_middle_mod06

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod06 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod06, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod06, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod06, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod06, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod06, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod06, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod06,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 06") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod06 
        
#########################################
# Mod 07
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run07")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod07 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod07 <- dataframe_simulations_mod07[dataframe_simulations_mod07$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod07$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod07)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod07 <- unique(dataframe_simulations_mod07$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod07$BIN[dataframe_simulations_mod07$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod07 <- dataframe_simulations_mod07[dataframe_simulations_mod07$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod07 <- merge(dataframe_simulations_mod07,PRED_BIN_mod07,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod07$PCDV <- dataframe_simulations_mod07$DV *(dataframe_simulations_mod07$PREDBIN/dataframe_simulations_mod07$PRED)


dataframe_simulations_mod07 <- dataframe_simulations_mod07[order(dataframe_simulations_mod07$replicate,dataframe_simulations_mod07$ID,dataframe_simulations_mod07$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod07 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod07){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod07 <- dataframe_simulations_mod07 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod07 <- rbind(sim_PI_mod07, sim_vpc_ci_mod07)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod07 <- sim_PI_mod07 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod07$x1 <- NA
sim_CI_mod07$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod07$x1[sim_CI_mod07$BIN == i] <-bin_times[i]
        sim_CI_mod07$x2[sim_CI_mod07$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod07<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run07'
observations_tablefile_mod07 <- paste0(working.directory_mod07, '/vpc_original.npctab.dta')
Obs_mod07 <- read_nonmem_table(observations_tablefile_mod07)
Obs_mod07<- Obs_mod07[Obs_mod07$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod07 <-  dataframe_simulations_mod07[ 
        #dataframe_simulations_mod07$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod07 <- merge(Obs_mod07,Rep1_mod07,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod07$BIN[Obs_mod07$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod07 <- merge(Obs_mod07,PRED_BIN_mod07,by='BIN')


Obs_mod07$PCDV <- Obs_mod07$DV *(Obs_mod07$PREDBIN/Obs_mod07$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod07 <- Obs_mod07 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod07 <- Obs_mod07 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod07 <- bin_middle_mod07$bin_middle


obs_vpc_mod07$TAD <- bin_middle_mod07

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod07 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod07, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod07, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod07, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod07, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod07, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod07, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod07,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 07") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod07

#########################################
# Mod 08
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run08")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod08 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod08 <- dataframe_simulations_mod08[dataframe_simulations_mod08$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod08$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod08)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod08 <- unique(dataframe_simulations_mod08$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod08$BIN[dataframe_simulations_mod08$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod08 <- dataframe_simulations_mod08[dataframe_simulations_mod08$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod08 <- merge(dataframe_simulations_mod08,PRED_BIN_mod08,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod08$PCDV <- dataframe_simulations_mod08$DV *(dataframe_simulations_mod08$PREDBIN/dataframe_simulations_mod08$PRED)


dataframe_simulations_mod08 <- dataframe_simulations_mod08[order(dataframe_simulations_mod08$replicate,dataframe_simulations_mod08$ID,dataframe_simulations_mod08$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod08 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod08){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod08 <- dataframe_simulations_mod08 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod08 <- rbind(sim_PI_mod08, sim_vpc_ci_mod08)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod08 <- sim_PI_mod08 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod08$x1 <- NA
sim_CI_mod08$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod08$x1[sim_CI_mod08$BIN == i] <-bin_times[i]
        sim_CI_mod08$x2[sim_CI_mod08$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod08<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run08'
observations_tablefile_mod08 <- paste0(working.directory_mod08, '/vpc_original.npctab.dta')
Obs_mod08 <- read_nonmem_table(observations_tablefile_mod08)
Obs_mod08<- Obs_mod08[Obs_mod08$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod08 <-  dataframe_simulations_mod08[ 
        #dataframe_simulations_mod08$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod08 <- merge(Obs_mod08,Rep1_mod08,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod08$BIN[Obs_mod08$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod08 <- merge(Obs_mod08,PRED_BIN_mod08,by='BIN')


Obs_mod08$PCDV <- Obs_mod08$DV *(Obs_mod08$PREDBIN/Obs_mod08$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod08 <- Obs_mod08 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod08 <- Obs_mod08 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod08 <- bin_middle_mod08$bin_middle


obs_vpc_mod08$TAD <- bin_middle_mod08

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod08 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod08, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod08, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod08, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod08, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod08, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod08, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod08,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 08") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod08              

#########################################
# Mod 09
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run09")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod09 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod09 <- dataframe_simulations_mod09[dataframe_simulations_mod09$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod09$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod09)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod09 <- unique(dataframe_simulations_mod09$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod09$BIN[dataframe_simulations_mod09$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod09 <- dataframe_simulations_mod09[dataframe_simulations_mod09$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod09 <- merge(dataframe_simulations_mod09,PRED_BIN_mod09,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod09$PCDV <- dataframe_simulations_mod09$DV *(dataframe_simulations_mod09$PREDBIN/dataframe_simulations_mod09$PRED)


dataframe_simulations_mod09 <- dataframe_simulations_mod09[order(dataframe_simulations_mod09$replicate,dataframe_simulations_mod09$ID,dataframe_simulations_mod09$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod09 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod09){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod09 <- dataframe_simulations_mod09 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod09 <- rbind(sim_PI_mod09, sim_vpc_ci_mod09)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod09 <- sim_PI_mod09 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod09$x1 <- NA
sim_CI_mod09$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod09$x1[sim_CI_mod09$BIN == i] <-bin_times[i]
        sim_CI_mod09$x2[sim_CI_mod09$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod09<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run09'
observations_tablefile_mod09 <- paste0(working.directory_mod09, '/vpc_original.npctab.dta')
Obs_mod09 <- read_nonmem_table(observations_tablefile_mod09)
Obs_mod09<- Obs_mod09[Obs_mod09$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod09 <-  dataframe_simulations_mod09[ 
        #dataframe_simulations_mod09$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod09 <- merge(Obs_mod09,Rep1_mod09,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod09$BIN[Obs_mod09$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod09 <- merge(Obs_mod09,PRED_BIN_mod09,by='BIN')


Obs_mod09$PCDV <- Obs_mod09$DV *(Obs_mod09$PREDBIN/Obs_mod09$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod09 <- Obs_mod09 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod09 <- Obs_mod09 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod09 <- bin_middle_mod09$bin_middle


obs_vpc_mod09$TAD <- bin_middle_mod09

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod09 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod09, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod09, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod09, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod09, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod09, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod09, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod09,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 09") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod09    

#########################################
# Mod 10
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run10")

# This reads the VPC npctab.dta file and save it in dataframe_simulations
dataframe_simulations_mod10 <- read_nm_tables(paste('.//',files,sep=""))
dataframe_simulations_mod10 <- dataframe_simulations_mod10[dataframe_simulations_mod10$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod10$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod10)/Number_of_simulations_performed)


# Set vector with unique replicates
Replicate_vector_mod10 <- unique(dataframe_simulations_mod10$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod10$BIN[dataframe_simulations_mod10$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## Calculate median PRED per bin
PRED_BIN_mod10 <- dataframe_simulations_mod10[dataframe_simulations_mod10$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

#### We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

dataframe_simulations_mod10 <- merge(dataframe_simulations_mod10,PRED_BIN_mod10,by='BIN')


# Calculate prediction corrected simulated observations (PCDV)
dataframe_simulations_mod10$PCDV <- dataframe_simulations_mod10$DV *(dataframe_simulations_mod10$PREDBIN/dataframe_simulations_mod10$PRED)


dataframe_simulations_mod10 <- dataframe_simulations_mod10[order(dataframe_simulations_mod10$replicate,dataframe_simulations_mod10$ID,dataframe_simulations_mod10$TAD),]


#### We use this PCDV to calculate the prediction intervals and the confidence intervals 
#### as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod10 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod10){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod10 <- dataframe_simulations_mod10 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod10 <- rbind(sim_PI_mod10, sim_vpc_ci_mod10)
}


###########################
# Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod10 <- sim_PI_mod10 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


### Set bin boundaries in dataset
sim_CI_mod10$x1 <- NA
sim_CI_mod10$x2 <- NA


for(i in 1:Number_of_bins){
        sim_CI_mod10$x1[sim_CI_mod10$BIN == i] <-bin_times[i]
        sim_CI_mod10$x2[sim_CI_mod10$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod10<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_2l.pan/vpc_run10'
observations_tablefile_mod10 <- paste0(working.directory_mod10, '/vpc_original.npctab.dta')
Obs_mod10 <- read_nonmem_table(observations_tablefile_mod10)
Obs_mod10<- Obs_mod10[Obs_mod10$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod10 <-  dataframe_simulations_mod10[ 
        #dataframe_simulations_mod10$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod10 <- merge(Obs_mod10,Rep1_mod10,by=c("ID","TAD","PRED"))

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod10$BIN[Obs_mod10$TAD <= bin_times[i+1]] <-i
}


## ADD PRED BIN TO OBSERVATIONS
Obs_mod10 <- merge(Obs_mod10,PRED_BIN_mod10,by='BIN')


Obs_mod10$PCDV <- Obs_mod10$DV *(Obs_mod10$PREDBIN/Obs_mod10$PRED)

#### We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

############################################
## Calculate confidence intervals of the observations
obs_vpc_mod10 <- Obs_mod10 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))


## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod10 <- Obs_mod10 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))


# As a vector
bin_middle_mod10 <- bin_middle_mod10$bin_middle


obs_vpc_mod10$TAD <- bin_middle_mod10

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale
CI_VPC_lin_mod10 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod10, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod10, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod10, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod10, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod10, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod10, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod10,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3)+
  
  
  ###### Set x & y axis limit, and their labels
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)",
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  ggtitle("Model 10") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14,face="bold"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Set axis labels
  labs(x="Time since last dose (hours)",y="Prediction-corrected fluconazole concentration (mg/L)")+
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154')


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+
CI_VPC_lin_mod10        
                        
###############################################################################################
########################## Combining 10 VPCs-VPC_Pooled #######################################

setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/Plots/vpc.2lpan/vpc_pooled")

# Combine the plots using plot_grid()
grid <- plot_grid(CI_VPC_lin_mod01, CI_VPC_lin_mod02, CI_VPC_lin_mod03, 
                  CI_VPC_lin_mod04, CI_VPC_lin_mod05, CI_VPC_lin_mod06, CI_VPC_lin_mod07, 
                  CI_VPC_lin_mod08, CI_VPC_lin_mod09, CI_VPC_lin_mod10, nrow = 2, align = "h")

# Add a title to the grid
title <- ggdraw() + 
        draw_label("pcVPC of 10 multiple imputed models", fontface = "bold", size = 16)

# Arrange the title and grid using plot_grid()
combined_plot <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))

# Save the plot as a high-resolution png file
ggsave("vpc_pooled.png", combined_plot, dpi = 300, width = 30, height = 16)

###############################################################################################
########################## My overall vpc #####################################################

#####################################
# Set working directory of model file
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall")

Number_of_simulations_performed <- 1000 ## Used as 'samples' argument in PsN vpc function. Needs to be a round number.


##############################
# Set the prediction intervals

PI <- 95 #specify prediction interval in percentage. Common is 80% interval (10-90%)
CI <- 95 #specify confidence interval in percentage. Common is 95

# Make a vector for upper and lower percentile
perc_PI <- c(0+(1-PI/100)/2, 1-(1-PI/100)/2)
perc_CI <- c(0+(1-CI/100)/2, 1-(1-CI/100)/2)

################################
# Specify the bin times manually


#bin_times <- c(0.304387974683544,3.73,9.74,14.865,21.225,39.035,72.24216)
bin_times <- c(0.01549,1.33,1.77,3.665,5.81,7.275,9.6,10.77,12.3835,13.135,14.425,15.485,16.905,18.035,19.54,21.225,22.225,23.1585,24.63,25.52549)

Number_of_bins <- length(bin_times)-1


#### We then search for the created simulation by PsN and read it using the xpose function read.nm.tables() and only select the observations (MDV ==0). 
#### We also need to create a new column which specifies from which of the 500 replicates the simulations originate and link the bin number with the observations.

#########################################
######### Load in simulated data 

## Search for the generated file by PsN in the sub-folders
files <- list.files(pattern = "1.npctab.dta", recursive = TRUE, include.dirs = TRUE)

# Set working directory of model file
working.directory_mod_pooled<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall'

# Test my alternative strategy
#working.directory_mod_pooled<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_overall/2l.pan/test'

# This reads the VPC npctab.dta file and save it in dataframe_simulations
#simulations_tablefile_mod_pooled <- paste0(working.directory_mod_pooled, '/vpc_simulation_overall.1.npctab.dta')
simulations_tablefile_mod_pooled <- paste0(working.directory_mod_pooled, '/vpc_sim_overall_refined.1.npctab.dta') #alternative approach #refined dataset
dataframe_simulations_mod_pooled <- read_nonmem_table(simulations_tablefile_mod_pooled)
dataframe_simulations_mod_pooled <- dataframe_simulations_mod_pooled[dataframe_simulations_mod_pooled$MDV == 0,]


## Set the replicate number to the simulated dataset
dataframe_simulations_mod_pooled$replicate <- rep(1:Number_of_simulations_performed,each=nrow(dataframe_simulations_mod_pooled)/Number_of_simulations_performed)

# Set vector with unique replicates
Replicate_vector_mod_pooled <- unique(dataframe_simulations_mod_pooled$replicate)


#########################################
######### Create bins in simulated dataset


### Set bins by backward for-loop
for(i in Number_of_bins:1){
        dataframe_simulations_mod_pooled$BIN[dataframe_simulations_mod_pooled$TAD <= bin_times[i+1]] <-i
} 


#### An important value to calculate is the population prediction (PREDbin) per bin. 
#### This will be done using a piping function in which we select only the data from the first replicate to save some time. 
#### Hence, the population prediction will be equal for each replicate since we did not incorporate any parameter uncertainty in the simulation.

## ADD dosing_interval_flag to dataframe_simulations_mod_pooled
dataframe_simulations_mod_pooled <- dataframe_simulations_mod_pooled %>%
  mutate(
    dosing_interval_flag = ifelse(ID %in% distinctID_dosing_interval$ID, 1, 0)
  )

########## From here onward analysing flag == 1 ###########

sim_mod_pooled_flag1<-dataframe_simulations_mod_pooled%>%filter(dosing_interval_flag==1)

## Calculate median PRED per bin 
PRED_BIN_mod_pooled_flag1 <- sim_mod_pooled_flag1[sim_mod_pooled_flag1$replicate ==1,] %>%
        group_by(BIN) %>% # Calculate the PRED per bin
        summarize(PREDBIN = median(PRED))

## We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

sim_mod_pooled_flag1 <- merge(sim_mod_pooled_flag1,PRED_BIN_mod_pooled_flag1,by='BIN')

## Calculate prediction corrected simulated observations (PCDV)

sim_mod_pooled_flag1$PCDV <- sim_mod_pooled_flag1$DV *(sim_mod_pooled_flag1$PREDBIN/sim_mod_pooled_flag1$PRED)

## Rearrange the dataset

sim_mod_pooled_flag1 <- sim_mod_pooled_flag1[order(sim_mod_pooled_flag1$replicate,sim_mod_pooled_flag1$ID,sim_mod_pooled_flag1$TAD),]


## We use this PCDV to calculate the prediction intervals and the confidence intervals 
## as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod_pooled_flag1 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod_pooled){
        
        
        # Run this for each replicate
        sim_vpc_ci_mod_pooled_flag1 <- sim_mod_pooled_flag1 %>%
                filter(replicate %in% i) %>% # Select an individual replicate
                group_by(BIN) %>% # Calculate everything per bin
                summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
                mutate(replicate = i) # Include replicate number
        
        
        sim_PI_mod_pooled_flag1 <- rbind(sim_PI_mod_pooled_flag1, sim_vpc_ci_mod_pooled_flag1)
}

## Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod_pooled_flag1 <- sim_PI_mod_pooled_flag1 %>%
        group_by(BIN) %>%
        summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
                  C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
                  C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
        )


## Set bin boundaries in dataset
sim_CI_mod_pooled_flag1$x1 <- NA
sim_CI_mod_pooled_flag1$x2 <- NA


for(i in 1:Number_of_bins){
  sim_CI_mod_pooled_flag1$x1[sim_CI_mod_pooled_flag1$BIN == i] <-bin_times[i]
  sim_CI_mod_pooled_flag1$x2[sim_CI_mod_pooled_flag1$BIN == i] <-bin_times[i+1]
}


########## From here onward analysing flag == 0 ###########

sim_mod_pooled_flag0<-dataframe_simulations_mod_pooled%>%filter(dosing_interval_flag==0)

## Calculate median PRED per bin 
PRED_BIN_mod_pooled_flag0 <- sim_mod_pooled_flag0[sim_mod_pooled_flag0$replicate ==1,] %>%
  group_by(BIN) %>% # Calculate the PRED per bin
  summarize(PREDBIN = median(PRED))

## We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

sim_mod_pooled_flag0 <- merge(sim_mod_pooled_flag0,PRED_BIN_mod_pooled_flag0,by='BIN')

## Calculate prediction corrected simulated observations (PCDV)

sim_mod_pooled_flag0$PCDV <- sim_mod_pooled_flag0$DV *(sim_mod_pooled_flag0$PREDBIN/sim_mod_pooled_flag0$PRED)

## Rearrange the dataset

sim_mod_pooled_flag0 <- sim_mod_pooled_flag0[order(sim_mod_pooled_flag0$replicate,sim_mod_pooled_flag0$ID,sim_mod_pooled_flag0$TAD),]


## We use this PCDV to calculate the prediction intervals and the confidence intervals 
## as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod_pooled_flag0 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod_pooled){
  
  
  # Run this for each replicate
  sim_vpc_ci_mod_pooled_flag0 <- sim_mod_pooled_flag0 %>%
    filter(replicate %in% i) %>% # Select an individual replicate
    group_by(BIN) %>% # Calculate everything per bin
    summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
    mutate(replicate = i) # Include replicate number
  
  
  sim_PI_mod_pooled_flag0 <- rbind(sim_PI_mod_pooled_flag0, sim_vpc_ci_mod_pooled_flag0)
}

## Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod_pooled_flag0 <- sim_PI_mod_pooled_flag0 %>%
  group_by(BIN) %>%
  summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
            C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
            C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
  )


## Set bin boundaries in dataset
sim_CI_mod_pooled_flag0$x1 <- NA
sim_CI_mod_pooled_flag0$x2 <- NA


for(i in 1:Number_of_bins){
  sim_CI_mod_pooled_flag0$x1[sim_CI_mod_pooled_flag0$BIN == i] <-bin_times[i]
  sim_CI_mod_pooled_flag0$x2[sim_CI_mod_pooled_flag0$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod_pooled<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall'
observations_tablefile_mod_pooled <- paste0(working.directory_mod_pooled, '/vpc_original_overall_refined.npctab.dta') #refined dataset
Obs_mod_pooled <- read_nonmem_table(observations_tablefile_mod_pooled)
Obs_mod_pooled<- Obs_mod_pooled[Obs_mod_pooled$MDV == 0,]


### Add the population prediction to each observation (only use the data from 1 replicate)


#Rep1_mod_pooled <-  dataframe_simulations_mod_pooled[ 
        #dataframe_simulations_mod_pooled$replicate ==1,c("ID","TAD","PRED")] 


#Obs_mod_pooled <- merge(Obs_mod_pooled,Rep1_mod_pooled,by=c("ID","TAD","PRED")) #Don't understand this step, why do we need to merge here??? 200623

#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
        Obs_mod_pooled$BIN[Obs_mod_pooled$TAD <= bin_times[i+1]] <-i
}

### ADD dosing_interval_flag to Obs_mod_pooled
Obs_mod_pooled <- Obs_mod_pooled %>%
  mutate(
    dosing_interval_flag = ifelse(ID %in% distinctID_dosing_interval$ID, 1, 0)
  )

########## From here onward analysing flag == 1 ###########

Obs_mod_pooled_flag1 <- Obs_mod_pooled %>% filter(dosing_interval_flag==1)

## ADD PRED BIN TO OBSERVATIONS
Obs_mod_pooled_flag1 <- merge(Obs_mod_pooled_flag1,PRED_BIN_mod_pooled_flag1,by='BIN')

## Calculate PCDV
Obs_mod_pooled_flag1$PCDV <- Obs_mod_pooled_flag1$DV *(Obs_mod_pooled_flag1$PREDBIN/Obs_mod_pooled_flag1$PRED)

## We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

## Calculate confidence intervals of the observations
obs_vpc_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
        group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
        summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))

## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
        group_by(BIN) %>%
        summarize(bin_middle = median(TAD, na.rm = T))

## As a vector
bin_middle_mod_pooled_flag1 <- bin_middle_mod_pooled_flag1$bin_middle

obs_vpc_mod_pooled_flag1$TAD <- bin_middle_mod_pooled_flag1

########## From here onward analysing flag == 0 ###########

Obs_mod_pooled_flag0 <- Obs_mod_pooled %>% filter(dosing_interval_flag==0)

## ADD PRED BIN TO OBSERVATIONS
Obs_mod_pooled_flag0 <- merge(Obs_mod_pooled_flag0,PRED_BIN_mod_pooled_flag0,by='BIN')

## Calculate PCDV
Obs_mod_pooled_flag0$PCDV <- Obs_mod_pooled_flag0$DV *(Obs_mod_pooled_flag0$PREDBIN/Obs_mod_pooled_flag0$PRED)

## We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

## Calculate confidence intervals of the observations
obs_vpc_mod_pooled_flag0 <- Obs_mod_pooled_flag0 %>%
  group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
  summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))

## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod_pooled_flag0 <- Obs_mod_pooled_flag0 %>%
  group_by(BIN) %>%
  summarize(bin_middle = median(TAD, na.rm = T))

## As a vector
bin_middle_mod_pooled_flag0 <- bin_middle_mod_pooled_flag0$bin_middle

obs_vpc_mod_pooled_flag0$TAD <- bin_middle_mod_pooled_flag0


#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale

### We create seperately for flag 1 and flag 0

## First of all, CI_VPC_lin_mod_pooled_flag1

CI_VPC_lin_mod_pooled_flag1 <- ggplot() +
        
        
        ## Set bins
        geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
        geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
        geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
        
        
        # Lines of the observations
        geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
        
        
        ###### Add observations
        geom_point(data = Obs_mod_pooled_flag1,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3) +
        
        
        ###### Set x & y axis limit, and their labels
        #scale_x_continuous(limits = c(0, 72.24216), breaks = seq(0, 72.24216, by = 10), name = "Time since last dose (hours)", 
                           #expand = c(0.01,0)) +
        #scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                           #expand = c(0.01,0)) +
        scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)", #changes I made in 200623
                           expand = c(0.01,0)) +
        scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                           expand = c(0.01,0)) +
  
        ## Add title and subtitle
        #ggtitle("Dosing interval 01") +
        
        
        ####### Set ggplot2 theme and settings
        theme_bw()+
        theme(axis.title=element_text(size=12),
              #plot.title = element_text(hjust = 0.5, size = 14,face="bold"),
              axis.text = element_text(size = 10))+
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),legend.position="none") +
        
        
        # Add vertical lines to indicate dosing
        geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154') #+
        
        
        # Set axis
        #scale_y_log10(expand=c(0.01,0))+
        #scale_x_continuous(expand=c(0.01,0))#+


## Second of all, CI_VPC_lin_mod_pooled_flag0

CI_VPC_lin_mod_pooled_flag0 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod_pooled_flag0, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod_pooled_flag0, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod_pooled_flag0, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod_pooled_flag0, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod_pooled_flag0, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod_pooled_flag0, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod_pooled_flag0,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3) +
  
  
  ###### Set x & y axis limit, and their labels
  #scale_x_continuous(limits = c(0, 72.24216), breaks = seq(0, 72.24216, by = 10), name = "Time since last dose (hours)", 
  #expand = c(0.01,0)) +
  #scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
  #expand = c(0.01,0)) +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)", #changes I made in 200623
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ### Add title and subtitle
  #ggtitle("Dosing interval 02") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        #plot.title = element_text(hjust = 0.5, size = 14,face="bold"),
        axis.text = element_text(size = 10))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154') #+


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+


### Adding panel title and combining the two plots

# Create common title
title_flag1 <- ggdraw() + 
  draw_label("Dosing interval 01", fontface = "bold", size = 14) 

title_flag0 <- ggdraw() + 
  draw_label("Dosing interval 02", fontface = "bold", size = 14) 

# Arrange the title and grid using plot_grid()

CI_VPC_lin_mod_pooled_flag1 <- plot_grid(title_flag1, CI_VPC_lin_mod_pooled_flag1, ncol = 1, rel_heights = c(0.04, 1))

CI_VPC_lin_mod_pooled_flag0 <- plot_grid(title_flag0, CI_VPC_lin_mod_pooled_flag0, ncol = 1, rel_heights = c(0.04, 1))

CI_VPC_lin_mod_pooled_flag1_a <-grid.arrange(textGrob("(a)", x = 0.05, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14)), CI_VPC_lin_mod_pooled_flag1, ncol = 1, heights = c(0, 1))

CI_VPC_lin_mod_pooled_flag0_b<-grid.arrange(textGrob("(b)", x = 0.05, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14)), CI_VPC_lin_mod_pooled_flag0, ncol = 1, heights = c(0, 1))

CI_VPC_lin_mod_pooled_stratified_interval <- plot_grid(CI_VPC_lin_mod_pooled_flag1_a, CI_VPC_lin_mod_pooled_flag0_b, ncol = 2, align = "h")

### save vpc overall
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/Plots/vpc.2lpan/vpc_overall")
# When I test the alternative approach
#setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/vpc_plots/vpc_pooled/2l.method/test")
ggsave("dosing_interval_stratified.png", CI_VPC_lin_mod_pooled_stratified_interval, dpi = 300, width = 12, height = 7)


#################### Dose_and_interval analysis ################################

## ADD dose_and_interval to dataframe_simulations_mod_pooled
dataframe_simulations_mod_pooled <- dataframe_simulations_mod_pooled %>%
  mutate(
    dose_and_interval = ifelse(ID %in% dose_and_interval_1$ID, 1, ifelse(ID %in% dose_and_interval_2$ID, 1,2))
  )

########## Dose_and_interval == 1 ###########

sim_mod_pooled_flag1<-dataframe_simulations_mod_pooled%>%filter(dose_and_interval==1)

## Calculate median PRED per bin 
PRED_BIN_mod_pooled_flag1 <- sim_mod_pooled_flag1[sim_mod_pooled_flag1$replicate ==1,] %>%
  group_by(BIN) %>% # Calculate the PRED per bin
  summarize(PREDBIN = median(PRED))

## We can then merge this with our simulated dataset and calculate the prediction corrected simulated observations (PCDV).

sim_mod_pooled_flag1 <- merge(sim_mod_pooled_flag1,PRED_BIN_mod_pooled_flag1,by='BIN')

## Calculate prediction corrected simulated observations (PCDV)

sim_mod_pooled_flag1$PCDV <- sim_mod_pooled_flag1$DV *(sim_mod_pooled_flag1$PREDBIN/sim_mod_pooled_flag1$PRED)

## Rearrange the dataset

sim_mod_pooled_flag1 <- sim_mod_pooled_flag1[order(sim_mod_pooled_flag1$replicate,sim_mod_pooled_flag1$ID,sim_mod_pooled_flag1$TAD),]


## We use this PCDV to calculate the prediction intervals and the confidence intervals 
## as we have previously done which are the final calculations after which we have our simulated dataset ready.

sim_PI_mod_pooled_flag1 <- NULL


## Calculate predictions intervals per bin
for(i in Replicate_vector_mod_pooled){
  
  
  # Run this for each replicate
  sim_vpc_ci_mod_pooled_flag1 <- sim_mod_pooled_flag1 %>%
    filter(replicate %in% i) %>% # Select an individual replicate
    group_by(BIN) %>% # Calculate everything per bin
    summarize(C_median = median(PCDV), C_lower = quantile(PCDV, perc_PI[1]), C_upper = quantile(PCDV, perc_PI[2])) %>% # Calculate prediction intervals
    mutate(replicate = i) # Include replicate number
  
  
  sim_PI_mod_pooled_flag1 <- rbind(sim_PI_mod_pooled_flag1, sim_vpc_ci_mod_pooled_flag1)
}

## Calculate confidence intervals around these prediction intervals calculated with each replicate


sim_CI_mod_pooled_flag1 <- sim_PI_mod_pooled_flag1 %>%
  group_by(BIN) %>%
  summarize(C_median_CI_lwr = quantile(C_median, perc_CI[1]), C_median_CI_upr = quantile(C_median, perc_CI[2]), # Median
            C_low_lwr = quantile(C_lower, perc_CI[1]), C_low_upr = quantile(C_lower, perc_CI[2]), # Lower percentages
            C_up_lwr = quantile(C_upper, perc_CI[1]), C_up_upr = quantile(C_upper, perc_CI[2]) # High percentages
  )


## Set bin boundaries in dataset
sim_CI_mod_pooled_flag1$x1 <- NA
sim_CI_mod_pooled_flag1$x2 <- NA


for(i in 1:Number_of_bins){
  sim_CI_mod_pooled_flag1$x1[sim_CI_mod_pooled_flag1$BIN == i] <-bin_times[i]
  sim_CI_mod_pooled_flag1$x2[sim_CI_mod_pooled_flag1$BIN == i] <-bin_times[i+1]
}


#### Our observations dataset can be loaded as a .csv file after which we select only the observations (CMT == 2 in this case).

#### In order to calculate the PCDV, we also need to apply the correction to our observations. 
#### For this, we need to have the PRED at each observation. 
#### We can do this by selecting the rows of the dataframe_simulations (row 1 until the number of observations in the original dataset).

############################################################
######### Read dataset with original observations
working.directory_mod_pooled<-'C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall'
observations_tablefile_mod_pooled <- paste0(working.directory_mod_pooled, '/vpc_original_overall_refined.npctab.dta') #refined dataset
Obs_mod_pooled <- read_nonmem_table(observations_tablefile_mod_pooled)
Obs_mod_pooled<- Obs_mod_pooled[Obs_mod_pooled$MDV == 0,]


#### We then add the bin numbers to the dataset, merge the population prediction per bin and calculate the PCDV.

#########################################
######### Create bins in original dataset

### Set bins by backward for loop
for(i in Number_of_bins:1){
  Obs_mod_pooled$BIN[Obs_mod_pooled$TAD <= bin_times[i+1]] <-i
}

### ADD dose_and_interval to Obs_mod_pooled
Obs_mod_pooled <- Obs_mod_pooled %>%
  mutate(
    dose_and_interval = ifelse(ID %in% dose_and_interval_1$ID, 1, ifelse(ID %in% dose_and_interval_2$ID, 1,2))
  )

########## dose_and_interval == 1 - obs ###########

Obs_mod_pooled_flag1 <- Obs_mod_pooled %>% filter(dose_and_interval==1)

## ADD PRED BIN TO OBSERVATIONS
Obs_mod_pooled_flag1 <- merge(Obs_mod_pooled_flag1,PRED_BIN_mod_pooled_flag1,by='BIN')

## Calculate PCDV
Obs_mod_pooled_flag1$PCDV <- Obs_mod_pooled_flag1$DV *(Obs_mod_pooled_flag1$PREDBIN/Obs_mod_pooled_flag1$PRED)

## We can use the PCDV for the calculation of the percentiles which are going to compare with the simulated distributions.

## Calculate confidence intervals of the observations
obs_vpc_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
  group_by(BIN) %>% ## Set the stratification identifiers (e.g. TIME, DOSE, CMT, BIN)
  summarize(C_median = median(PCDV, na.rm = T), C_lower = quantile(PCDV, perc_PI[1], na.rm = T), C_upper = quantile(PCDV, perc_PI[2], na.rm = T))

## Get median TAD of each bin of observations for plotting purposes
bin_middle_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
  group_by(BIN) %>%
  summarize(bin_middle = median(TAD, na.rm = T))

## As a vector
bin_middle_mod_pooled_flag1 <- bin_middle_mod_pooled_flag1$bin_middle

obs_vpc_mod_pooled_flag1$TAD <- bin_middle_mod_pooled_flag1

#### We now have all the data we need to generate our pcVPC :).

#### The ggplot object that will be generated is actually the same as for the confidence interval VPC. 
#### We have rectangles (shaded areas) for the distribution of the simulations, lines for the distribution of the data, and dots for the observed prediction corrected concentrations.

### VPC on a linear scale

### We create seperately for flag 1 and flag 0

## First of all, CI_VPC_lin_mod_pooled_flag1

CI_VPC_lin_mod_pooled_flag1 <- ggplot() +
  
  
  ## Set bins
  geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_median_CI_lwr, ymax=C_median_CI_upr), fill='#fde725', alpha=0.25) +
  geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_low_lwr, ymax=C_low_upr), fill='#21918c', alpha=0.25) +
  geom_rect(data=sim_CI_mod_pooled_flag1, mapping=aes(xmin=x1, xmax=x2, ymin=C_up_lwr, ymax=C_up_upr), fill='#21918c', alpha=0.25) +
  
  
  # Lines of the observations
  geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_median), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_lower), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  geom_line(data = obs_vpc_mod_pooled_flag1, aes(TAD, C_upper), col = '#440154', linetype = 'dashed', linewidth = 1.25) +
  
  
  ###### Add observations
  geom_point(data = Obs_mod_pooled_flag1,aes(x=TAD,y=PCDV),color='#440154',alpha=0.3) +
  
  
  ###### Set x & y axis limit, and their labels
  #scale_x_continuous(limits = c(0, 72.24216), breaks = seq(0, 72.24216, by = 10), name = "Time since last dose (hours)", 
  #expand = c(0.01,0)) +
  #scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
  #expand = c(0.01,0)) +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 26, by = 5), name = "Time since last dose (hours)", #changes I made in 200623
                     expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), name = "Prediction-corrected fluconazole concentration (mg/L)", 
                     expand = c(0.01,0)) +
  
  ## Add title and subtitle
  #ggtitle("Dosing interval 01") +
  
  
  ####### Set ggplot2 theme and settings
  theme_bw()+
  theme(axis.title=element_text(size=12),
        #plot.title = element_text(hjust = 0.5, size = 14,face="bold"),
        axis.text = element_text(size = 10))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),legend.position="none") +
  
  
  # Add vertical lines to indicate dosing
  geom_vline(xintercept = 0, linetype="dashed", size=0.5,color='#440154') #+


# Set axis
#scale_y_log10(expand=c(0.01,0))+
#scale_x_continuous(expand=c(0.01,0))#+

#################### VPC stratified by HOSPITAL ################################

setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall")

Number_of_simulations_performed <- 1000
PI <- 95
CI <- 95
perc_PI <- c(0 + (1 - PI/100) / 2, 1 - (1 - PI/100) / 2)
perc_CI <- c(0 + (1 - CI/100) / 2, 1 - (1 - CI/100) / 2)
bin_times <- c(0.01549, 1.33, 1.77, 3.665, 5.81, 7.275, 9.6, 10.77, 12.3835, 13.135, 14.425, 15.485, 16.905, 18.035, 19.54, 21.225, 22.225, 23.1585, 24.63, 25.52549)
Number_of_bins <- length(bin_times) - 1

# Load simulation data
simulations_tablefile_mod_pooled <- "vpc_sim_overall_refined.1.npctab.dta" # better use this
#simulations_tablefile_mod_pooled <- "vpc_sim_overall.1.npctab.dta" #will use overall dataset 
dataframe_simulations_mod_pooled <- read_nonmem_table(simulations_tablefile_mod_pooled)
dataframe_simulations_mod_pooled <- dataframe_simulations_mod_pooled[dataframe_simulations_mod_pooled$MDV == 0,]
dataframe_simulations_mod_pooled$replicate <- rep(1:Number_of_simulations_performed, each = nrow(dataframe_simulations_mod_pooled) / Number_of_simulations_performed)

# Set vector with unique replicates
Replicate_vector_mod_pooled <- unique(dataframe_simulations_mod_pooled$replicate)

# Define BIN based on TAD
for (i in Number_of_bins:1) {
  dataframe_simulations_mod_pooled$BIN[dataframe_simulations_mod_pooled$TAD <= bin_times[i + 1]] <- i
}

# Create new column called HOSPITAL
dataframe_simulations_mod_pooled$HOSPITAL <- as.numeric(substr(dataframe_simulations_mod_pooled$ID, 1, 1))

# Load observations data
#observations_tablefile_mod_pooled <- "vpc_original_overall.npctab.dta" #I use the original vpc
observations_tablefile_mod_pooled <- "vpc_original_overall_refined.npctab.dta" #better use the refined
Obs_mod_pooled <- read_nonmem_table(observations_tablefile_mod_pooled)
Obs_mod_pooled <- Obs_mod_pooled[Obs_mod_pooled$MDV == 0,]

# Define BIN based on TAD
for (i in Number_of_bins:1) {
  Obs_mod_pooled$BIN[Obs_mod_pooled$TAD <= bin_times[i + 1]] <- i
}

# Create new column called HOSPITAL observed dataset
Obs_mod_pooled$HOSPITAL <- as.numeric(substr(Obs_mod_pooled$ID, 1, 1))

# List to store plots for HOSPITAL 2 to 8
hospital_plots <- list()

# Title of 7 plots
hospital_titles <- c(
  "Muilwijk et al.", "Bergner et al.", "Buijk et al.", "Sandaradura et al.",
  "Sinnollareddy et al.", "Alobaid et al.", "Boonstra et al."
)

# Loop through HOSPITAL values (2 to 8)
for (hosp in 2:8) { #Hospital 1 is an outlier, therefore I selected hospital 2 to 8 instead
  # Filter simulation data for the current hospital
  sim_mod_pooled_flag <- dataframe_simulations_mod_pooled %>% filter(HOSPITAL == hosp)
  
  # Calculate PRED_BIN and merge
  PRED_BIN_mod_pooled_flag <- sim_mod_pooled_flag[sim_mod_pooled_flag$replicate == 1, ] %>%
    group_by(BIN) %>%
    summarize(PREDBIN = median(PRED))
  
  sim_mod_pooled_flag <- merge(sim_mod_pooled_flag, PRED_BIN_mod_pooled_flag, by = 'BIN')
  
  # Calculate PCDV
  sim_mod_pooled_flag$PCDV <- sim_mod_pooled_flag$DV * (sim_mod_pooled_flag$PREDBIN / sim_mod_pooled_flag$PRED)
  
  # Order data
  sim_mod_pooled_flag <- sim_mod_pooled_flag[order(sim_mod_pooled_flag$replicate, sim_mod_pooled_flag$ID, sim_mod_pooled_flag$TAD),]
  
  # Initialize storage for PI data
  sim_PI_mod_pooled_flag <- NULL
  
  # Loop through replicates
  for (i in Replicate_vector_mod_pooled) {
    sim_vpc_ci_mod_pooled_flag <- sim_mod_pooled_flag %>%
      filter(replicate %in% i) %>%
      group_by(BIN) %>%
      summarize(
        C_median = median(PCDV),
        C_lower = quantile(PCDV, perc_PI[1]),
        C_upper = quantile(PCDV, perc_PI[2])
      ) %>%
      mutate(replicate = i)
    
    sim_PI_mod_pooled_flag <- rbind(sim_PI_mod_pooled_flag, sim_vpc_ci_mod_pooled_flag)
  }
  
  # Calculate CI
  sim_CI_mod_pooled_flag <- sim_PI_mod_pooled_flag %>%
    group_by(BIN) %>%
    summarize(
      C_median_CI_lwr = quantile(C_median, perc_CI[1]),
      C_median_CI_upr = quantile(C_median, perc_CI[2]),
      C_low_lwr = quantile(C_lower, perc_CI[1]),
      C_low_upr = quantile(C_lower, perc_CI[2]),
      C_up_lwr = quantile(C_upper, perc_CI[1]),
      C_up_upr = quantile(C_upper, perc_CI[2])
    )
  
  # Assign NA values
  sim_CI_mod_pooled_flag$x1 <- NA
  sim_CI_mod_pooled_flag$x2 <- NA
  
  # Assign BIN values based on bin_times
  for (i in 1:Number_of_bins) {
    sim_CI_mod_pooled_flag$x1[sim_CI_mod_pooled_flag$BIN == i] <- bin_times[i]
    sim_CI_mod_pooled_flag$x2[sim_CI_mod_pooled_flag$BIN == i] <- bin_times[i + 1]
  }
  
    # Filter Obs_mod_pooled for the current hospital
  Obs_mod_pooled_flag <- Obs_mod_pooled %>% filter(HOSPITAL == hosp)
  
  # Merge with PRED_BIN_mod_pooled_flag
  Obs_mod_pooled_flag <- merge(Obs_mod_pooled_flag, PRED_BIN_mod_pooled_flag, by = 'BIN')
  
  # Calculate PCDV
  Obs_mod_pooled_flag$PCDV <- Obs_mod_pooled_flag$DV * (Obs_mod_pooled_flag$PREDBIN / Obs_mod_pooled_flag$PRED)
  
  # Summarize data
  obs_vpc_mod_pooled_flag <- Obs_mod_pooled_flag %>%
    group_by(BIN) %>% 
    summarize(
      C_median = median(PCDV, na.rm = TRUE),
      C_lower = quantile(PCDV, perc_PI[1], na.rm = TRUE),
      C_upper = quantile(PCDV, perc_PI[2], na.rm = TRUE)
    )
  
  bin_middle_mod_pooled_flag <- Obs_mod_pooled_flag %>%
    group_by(BIN) %>%
    summarize(bin_middle = median(TAD, na.rm = TRUE))
  
  bin_middle_mod_pooled_flag <- bin_middle_mod_pooled_flag$bin_middle
  
  obs_vpc_mod_pooled_flag$TAD <- bin_middle_mod_pooled_flag
  
  # Create VPC plot
  CI_VPC_lin_mod_pooled_flag <- ggplot() +
    geom_rect(
      data = sim_CI_mod_pooled_flag,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_median_CI_lwr, ymax = C_median_CI_upr),
      fill = '#fde725',
      alpha = 0.25
    ) +
    geom_rect(
      data = sim_CI_mod_pooled_flag,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_low_lwr, ymax = C_low_upr),
      fill = '#21918c',
      alpha = 0.25
    ) +
    geom_rect(
      data = sim_CI_mod_pooled_flag,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_up_lwr, ymax = C_up_upr),
      fill = '#21918c',
      alpha = 0.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag,
      aes(TAD, C_median),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag,
      aes(TAD, C_lower),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag,
      aes(TAD, C_upper),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_point(
      data = Obs_mod_pooled_flag,
      aes(x = TAD, y = PCDV),
      color = '#440154',
      alpha = 0.3
    ) +
    scale_x_continuous(
      limits = c(0, 26),
      breaks = seq(0, 25, by = 5),
      name = "Time since last dose (hours)",
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 80),
      breaks = seq(0, 80, by = 10),
      name = "Prediction-corrected fluconazole concentration (mg/L)",
      expand = c(0.01, 0)
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 18), #increase the text size - 041223
      axis.text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, size = 21,face="bold"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "none"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = '#440154')
    
  # Save the plot in the list with the corresponding title
  hospital_plots[[as.character(hosp)]] <- CI_VPC_lin_mod_pooled_flag +
    ggtitle(hospital_titles[hosp - 1])  # Subtract 1 because the index starts from 2
}
  
# Add this after making plot for hospital 1
hospital_plots <- c(list("1" = CI_VPC_lin_mod_pooled_flag1), hospital_plots)

# Combine all the plots
combined_plot <- cowplot::plot_grid(
  plotlist = lapply(names(hospital_plots), function(hosp) {
    return(hospital_plots[[hosp]])
  }),
  nrow = 2
)

# Save the combine plot
setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/Plots/vpc.2lpan/vpc_pooled")
ggsave("vpc_per_study.png", combined_plot, dpi = 300, width = 30, height = 16)


#### Special case hos HOSPITAL 1 ####

  sim_mod_pooled_flag1 <- dataframe_simulations_mod_pooled %>% filter(HOSPITAL == 1)
  
  # Calculate PRED_BIN and merge
  PRED_BIN_mod_pooled_flag1 <- sim_mod_pooled_flag1[sim_mod_pooled_flag1$replicate == 1, ] %>%
    group_by(BIN) %>%
    summarize(PREDBIN = median(PRED))
  
  sim_mod_pooled_flag1 <- merge(sim_mod_pooled_flag1, PRED_BIN_mod_pooled_flag1, by = 'BIN')
  
  # Calculate PCDV
  sim_mod_pooled_flag1$PCDV <- sim_mod_pooled_flag1$DV * (sim_mod_pooled_flag1$PREDBIN / sim_mod_pooled_flag1$PRED)
  
  # Order data
  sim_mod_pooled_flag1 <- sim_mod_pooled_flag1[order(sim_mod_pooled_flag1$replicate, sim_mod_pooled_flag1$ID, sim_mod_pooled_flag1$TAD),]
  
  # Initialize storage for PI data
  sim_PI_mod_pooled_flag1 <- NULL
  
  # Loop through replicates
  for (i in Replicate_vector_mod_pooled) {
    sim_vpc_ci_mod_pooled_flag1 <- sim_mod_pooled_flag1 %>%
      filter(replicate %in% i) %>%
      group_by(BIN) %>%
      summarize(
        C_median = median(PCDV),
        C_lower = quantile(PCDV, perc_PI[1]),
        C_upper = quantile(PCDV, perc_PI[2])
      ) %>%
      mutate(replicate = i)
    
    sim_PI_mod_pooled_flag1 <- rbind(sim_PI_mod_pooled_flag1, sim_vpc_ci_mod_pooled_flag1)
  }
  
  # Calculate CI
  sim_CI_mod_pooled_flag1 <- sim_PI_mod_pooled_flag1 %>%
    group_by(BIN) %>%
    summarize(
      C_median_CI_lwr = quantile(C_median, perc_CI[1]),
      C_median_CI_upr = quantile(C_median, perc_CI[2]),
      C_low_lwr = quantile(C_lower, perc_CI[1]),
      C_low_upr = quantile(C_lower, perc_CI[2]),
      C_up_lwr = quantile(C_upper, perc_CI[1]),
      C_up_upr = quantile(C_upper, perc_CI[2])
    )
  
  # Assign NA values
  sim_CI_mod_pooled_flag1$x1 <- NA
  sim_CI_mod_pooled_flag1$x2 <- NA
  
  # Assign BIN values based on bin_times
  for (i in 1:Number_of_bins) {
    sim_CI_mod_pooled_flag1$x1[sim_CI_mod_pooled_flag1$BIN == i] <- bin_times[i]
    sim_CI_mod_pooled_flag1$x2[sim_CI_mod_pooled_flag1$BIN == i] <- bin_times[i + 1]
  }
  
  # Filter Obs_mod_pooled for the current hospital
  Obs_mod_pooled_flag1 <- Obs_mod_pooled %>% filter(HOSPITAL == hosp)
  
  # Merge with PRED_BIN_mod_pooled_flag1
  Obs_mod_pooled_flag1 <- merge(Obs_mod_pooled_flag1, PRED_BIN_mod_pooled_flag1, by = 'BIN')
  
  # Calculate PCDV
  Obs_mod_pooled_flag1$PCDV <- Obs_mod_pooled_flag1$DV * (Obs_mod_pooled_flag1$PREDBIN / Obs_mod_pooled_flag1$PRED)
  
  # Summarize data
  obs_vpc_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
    group_by(BIN) %>% 
    summarize(
      C_median = median(PCDV, na.rm = TRUE),
      C_lower = quantile(PCDV, perc_PI[1], na.rm = TRUE),
      C_upper = quantile(PCDV, perc_PI[2], na.rm = TRUE)
    )
  
  bin_middle_mod_pooled_flag1 <- Obs_mod_pooled_flag1 %>%
    group_by(BIN) %>%
    summarize(bin_middle = median(TAD, na.rm = TRUE))
  
  bin_middle_mod_pooled_flag1 <- bin_middle_mod_pooled_flag1$bin_middle
  
  obs_vpc_mod_pooled_flag1$TAD <- bin_middle_mod_pooled_flag1
  
  # Create VPC plot
  CI_VPC_lin_mod_pooled_flag1 <- ggplot() +
    geom_rect(
      data = sim_CI_mod_pooled_flag1,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_median_CI_lwr, ymax = C_median_CI_upr),
      fill = '#fde725',
      alpha = 0.25
    ) +
    geom_rect(
      data = sim_CI_mod_pooled_flag1,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_low_lwr, ymax = C_low_upr),
      fill = '#21918c',
      alpha = 0.25
    ) +
    geom_rect(
      data = sim_CI_mod_pooled_flag1,
      mapping = aes(xmin = x1, xmax = x2, ymin = C_up_lwr, ymax = C_up_upr),
      fill = '#21918c',
      alpha = 0.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag1,
      aes(TAD, C_median),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag1,
      aes(TAD, C_lower),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_line(
      data = obs_vpc_mod_pooled_flag1,
      aes(TAD, C_upper),
      col = '#440154',
      linetype = 'dashed',
      linewidth = 1.25
    ) +
    geom_point(
      data = Obs_mod_pooled_flag1,
      aes(x = TAD, y = PCDV),
      color = '#440154',
      alpha = 0.3
    ) +
    scale_x_continuous(
      limits = c(10, 26),
      breaks = seq(10, 25, by = 5),
      name = "Time since last dose (hours)",
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 80),
      breaks = seq(0, 80, by = 10),
      name = "Prediction-corrected fluconazole concentration (mg/L)",
      expand = c(0.01, 0)
    ) +
    ggtitle("Van Daele et al.") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 21,face="bold"),
      legend.position = "none"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = '#440154')
  
  
####### VPC stratified by CRRT ########
 
  setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/My datasets/Imputed_dos_int_datasets/2l method/2l.pan/vpc/2l.pan/vpc_overall")
  
  Number_of_simulations_performed <- 1000
  PI <- 95
  CI <- 95
  perc_PI <- c(0 + (1 - PI/100) / 2, 1 - (1 - PI/100) / 2)
  perc_CI <- c(0 + (1 - CI/100) / 2, 1 - (1 - CI/100) / 2)
  bin_times <- c(0.01549, 1.33, 1.77, 3.665, 5.81, 7.275, 9.6, 10.77, 12.3835, 13.135, 14.425, 15.485, 16.905, 18.035, 19.54, 21.225, 22.225, 23.1585, 24.63, 25.52549)
  Number_of_bins <- length(bin_times) - 1
  
  # Load simulation data
  simulations_tablefile_mod_pooled <- "vpc_sim_overall_refined.1.npctab.dta" # better use this
  #simulations_tablefile_mod_pooled <- "vpc_sim_overall.1.npctab.dta" #will use overall dataset 
  dataframe_simulations_mod_pooled <- read_nonmem_table(simulations_tablefile_mod_pooled)
  dataframe_simulations_mod_pooled <- dataframe_simulations_mod_pooled[dataframe_simulations_mod_pooled$MDV == 0,]
  dataframe_simulations_mod_pooled$replicate <- rep(1:Number_of_simulations_performed, each = nrow(dataframe_simulations_mod_pooled) / Number_of_simulations_performed)
  
  # Set vector with unique replicates
  Replicate_vector_mod_pooled <- unique(dataframe_simulations_mod_pooled$replicate)
  
  # Define BIN based on TAD
  for (i in Number_of_bins:1) {
    dataframe_simulations_mod_pooled$BIN[dataframe_simulations_mod_pooled$TAD <= bin_times[i + 1]] <- i
  }
  
  # Load observations data
  #observations_tablefile_mod_pooled <- "vpc_original_overall.npctab.dta" #I use the original vpc
  observations_tablefile_mod_pooled <- "vpc_original_overall_refined.npctab.dta" #better use the refined
  Obs_mod_pooled <- read_nonmem_table(observations_tablefile_mod_pooled)
  Obs_mod_pooled <- Obs_mod_pooled[Obs_mod_pooled$MDV == 0,]
  
  # Define BIN based on TAD
  for (i in Number_of_bins:1) {
    Obs_mod_pooled$BIN[Obs_mod_pooled$TAD <= bin_times[i + 1]] <- i
  }
  
  # List to store plots for HOSPITAL 2 to 8
  crrt_plots <- list()
  
  # Title of 7 plots
  crrt_titles <- c(
    "Non-CRRT","CRRT"
  )
  
  # Loop through CRRT values (0 to 1)
  for (crrt in 0:1) { 
    sim_mod_pooled_flag <- dataframe_simulations_mod_pooled %>% filter(CRRT == crrt)
    
    # Calculate PRED_BIN and merge
    PRED_BIN_mod_pooled_flag <- sim_mod_pooled_flag[sim_mod_pooled_flag$replicate == 1, ] %>%
      group_by(BIN) %>%
      summarize(PREDBIN = median(PRED))
    
    sim_mod_pooled_flag <- merge(sim_mod_pooled_flag, PRED_BIN_mod_pooled_flag, by = 'BIN')
    
    # Calculate PCDV
    sim_mod_pooled_flag$PCDV <- sim_mod_pooled_flag$DV * (sim_mod_pooled_flag$PREDBIN / sim_mod_pooled_flag$PRED)
    
    # Order data
    sim_mod_pooled_flag <- sim_mod_pooled_flag[order(sim_mod_pooled_flag$replicate, sim_mod_pooled_flag$ID, sim_mod_pooled_flag$TAD),]
    
    # Initialize storage for PI data
    sim_PI_mod_pooled_flag <- NULL
    
    # Loop through replicates
    for (i in Replicate_vector_mod_pooled) {
      sim_vpc_ci_mod_pooled_flag <- sim_mod_pooled_flag %>%
        filter(replicate %in% i) %>%
        group_by(BIN) %>%
        summarize(
          C_median = median(PCDV),
          C_lower = quantile(PCDV, perc_PI[1]),
          C_upper = quantile(PCDV, perc_PI[2])
        ) %>%
        mutate(replicate = i)
      
      sim_PI_mod_pooled_flag <- rbind(sim_PI_mod_pooled_flag, sim_vpc_ci_mod_pooled_flag)
    }
    
    # Calculate CI
    sim_CI_mod_pooled_flag <- sim_PI_mod_pooled_flag %>%
      group_by(BIN) %>%
      summarize(
        C_median_CI_lwr = quantile(C_median, perc_CI[1]),
        C_median_CI_upr = quantile(C_median, perc_CI[2]),
        C_low_lwr = quantile(C_lower, perc_CI[1]),
        C_low_upr = quantile(C_lower, perc_CI[2]),
        C_up_lwr = quantile(C_upper, perc_CI[1]),
        C_up_upr = quantile(C_upper, perc_CI[2])
      )
    
    # Assign NA values
    sim_CI_mod_pooled_flag$x1 <- NA
    sim_CI_mod_pooled_flag$x2 <- NA
    
    # Assign BIN values based on bin_times
    for (i in 1:Number_of_bins) {
      sim_CI_mod_pooled_flag$x1[sim_CI_mod_pooled_flag$BIN == i] <- bin_times[i]
      sim_CI_mod_pooled_flag$x2[sim_CI_mod_pooled_flag$BIN == i] <- bin_times[i + 1]
    }
    
    # Filter Obs_mod_pooled for the current hospital
    Obs_mod_pooled_flag <- Obs_mod_pooled %>% filter(CRRT == crrt)
    
    # Merge with PRED_BIN_mod_pooled_flag
    Obs_mod_pooled_flag <- merge(Obs_mod_pooled_flag, PRED_BIN_mod_pooled_flag, by = 'BIN')
    
    # Calculate PCDV
    Obs_mod_pooled_flag$PCDV <- Obs_mod_pooled_flag$DV * (Obs_mod_pooled_flag$PREDBIN / Obs_mod_pooled_flag$PRED)
    
    # Summarize data
    obs_vpc_mod_pooled_flag <- Obs_mod_pooled_flag %>%
      group_by(BIN) %>% 
      summarize(
        C_median = median(PCDV, na.rm = TRUE),
        C_lower = quantile(PCDV, perc_PI[1], na.rm = TRUE),
        C_upper = quantile(PCDV, perc_PI[2], na.rm = TRUE)
      )
    
    bin_middle_mod_pooled_flag <- Obs_mod_pooled_flag %>%
      group_by(BIN) %>%
      summarize(bin_middle = median(TAD, na.rm = TRUE))
    
    bin_middle_mod_pooled_flag <- bin_middle_mod_pooled_flag$bin_middle
    
    obs_vpc_mod_pooled_flag$TAD <- bin_middle_mod_pooled_flag
    
    # Create VPC plot
    CI_VPC_lin_mod_pooled_flag <- ggplot() +
      geom_rect(
        data = sim_CI_mod_pooled_flag,
        mapping = aes(xmin = x1, xmax = x2, ymin = C_median_CI_lwr, ymax = C_median_CI_upr),
        fill = '#fde725',
        alpha = 0.25
      ) +
      geom_rect(
        data = sim_CI_mod_pooled_flag,
        mapping = aes(xmin = x1, xmax = x2, ymin = C_low_lwr, ymax = C_low_upr),
        fill = '#21918c',
        alpha = 0.25
      ) +
      geom_rect(
        data = sim_CI_mod_pooled_flag,
        mapping = aes(xmin = x1, xmax = x2, ymin = C_up_lwr, ymax = C_up_upr),
        fill = '#21918c',
        alpha = 0.25
      ) +
      geom_line(
        data = obs_vpc_mod_pooled_flag,
        aes(TAD, C_median),
        col = '#440154',
        linetype = 'dashed',
        linewidth = 1.25
      ) +
      geom_line(
        data = obs_vpc_mod_pooled_flag,
        aes(TAD, C_lower),
        col = '#440154',
        linetype = 'dashed',
        linewidth = 1.25
      ) +
      geom_line(
        data = obs_vpc_mod_pooled_flag,
        aes(TAD, C_upper),
        col = '#440154',
        linetype = 'dashed',
        linewidth = 1.25
      ) +
      geom_point(
        data = Obs_mod_pooled_flag,
        aes(x = TAD, y = PCDV),
        color = '#440154',
        alpha = 0.3
      ) +
      scale_x_continuous(
        limits = c(0, 26),
        breaks = seq(0, 25, by = 5),
        name = "Time since last dose (hours)",
        expand = c(0.01, 0)
      ) +
      scale_y_continuous(
        limits = c(0, 80),
        breaks = seq(0, 80, by = 10),
        name = "Prediction-corrected fluconazole concentration (mg/L)",
        expand = c(0.01, 0)
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 24), #increase text size 041223
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 28,face="bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none"
      ) +
      geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = '#440154')
    
    # Save the plot in the list with the corresponding title
    crrt_plots[[as.character(crrt)]] <- CI_VPC_lin_mod_pooled_flag +
      ggtitle(crrt_titles[crrt+1])  
  }
  
  
  # Combine all the plots
  combined_plot <- cowplot::plot_grid(
    plotlist = lapply(names(crrt_plots), function(crrt) {
      return(crrt_plots[[crrt]])
    }),
    nrow = 1
  )
  
  # Save the combine plot
  setwd("C:/Users/u0164053/OneDrive - KU Leuven/Fluconazole PoPPK/Fluconazol_project/Plots/vpc.2lpan/vpc_pooled")
  ggsave("vpc_per_crrtstatus.png", combined_plot, dpi = 300, width = 30, height = 16)

###################### Adjusting overall VPC 200623 ############################

High_conc<-Obs_mod_pooled%>%filter(TAD>=23&TAD<=25,PCDV>50) %>% select(ID,TAD,DV,PCDV) #all individuals with high PCDV
View(High_conc)
High_conc$ID
High_conc$TAD
High_conc$DV
# Eliminating concentrations beyond 25.5 hours


###################### Some code to try out by RtutoAI ############################
obs_vpc_mod_pooled <- list()
# Loop through HOSPITAL values 2 to 8 
for (hospital in 2:8) { # Filter Obs_mod_pooled for the current HOSPITAL value 
  Obs_mod_pooled_flag <- Obs_mod_pooled %>% filter(HOSPITAL == hospital) 
  # Merge with PRED_BIN_mod_pooled_flag using 'BIN' as the common column 
  Obs_mod_pooled_flag <- merge(Obs_mod_pooled_flag, PRED_BIN_mod_pooled_flag, by = 'BIN') 
  # Calculate PCDV 
  Obs_mod_pooled_flag$PCDV <- Obs_mod_pooled_flag$DV * (Obs_mod_pooled_flag$PREDBIN / Obs_mod_pooled_flag$PRED) 
  # Calculate summary statistics for each BIN 
  obs_vpc_mod_pooled_flag <- Obs_mod_pooled_flag %>% group_by(BIN) %>% summarize( C_median = median(PCDV, na.rm = TRUE), C_lower = quantile(PCDV, perc_PI[1], na.rm = TRUE), C_upper = quantile(PCDV, perc_PI[2], na.rm = TRUE) ) 
  # Calculate bin_middle 
  bin_middle_mod_pooled_flag <- Obs_mod_pooled_flag %>% group_by(BIN) %>% summarize(bin_middle = median(TAD, na.rm = TRUE)) 
  # Add bin_middle to obs_vpc_mod_pooled_flag 
  obs_vpc_mod_pooled_flag$TAD <- bin_middle_mod_pooled_flag$bin_middle 
  # Store the results for the current HOSPITAL value in the list 
  obs_vpc_mod_pooled[[as.character(hospital)]] <- obs_vpc_mod_pooled_flag }
  # Access the results for a specific HOSPITAL value, e.g., HOSPITAL == 3 
obs_vpc_mod_pooled_flag3 <- obs_vpc_mod_pooled[["3"]] 


  