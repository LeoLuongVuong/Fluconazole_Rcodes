#PREPARATION---------------------------------------------------------------------------------

    #Load packages
    library(Hmisc)
    library(zoo)
    library(ggplot2) #deel tidyverse
    library(dplyr) #deel tidyverse
    library(tidyr) #deel tidyverse
    library(stats)
    library(xpose4) #import table from nonmem
    library(naniar) #replace a value in dataset with another value
    library(knitr)
    library(data.table)
    library(ggiraph)
    library(plotly)
    library(stats)
    library(data.table)
    library(tableone)
library(vtable) # I added this line to use st function
    
    #Set plot style
    PLOTSTYLE <- 
      theme(axis.text.x = element_text(colour = "black", size=15)) +
      theme(axis.text.y = element_text(colour = "black", size=15)) +
      theme(axis.title.x = element_text(face='bold', size= 13)) +
      theme(axis.title.y = element_text(face='bold', angle=90, vjust=0.25, size=13)) +
      theme(panel.background = element_rect(fill = "white")) +
      theme(panel.border     = element_rect(fill = NA)) +
      theme(panel.grid.major=element_line(colour=NA)) +
      theme(panel.grid.minor=element_line(colour=NA))+
      theme(legend.text = element_text(colour = "black", size=15))+
      theme(legend.title = element_text(colour = "black", size=15))+
      theme(strip.text = element_text(colour = "black", size=15))

  
      
#LOAD TOTAL DATASET------------------------------------------------------------------------------------------------------------------------
      FlucTotIV <- read.csv("D:/Projects/Fluconazole PopPK KU Leuven/Fluconazole/Databanken/Totaal/DatabankFluc_Tot_IV_finaal.csv", na = "-99")

#EXPLORE THE DATA
    summary(FlucTotIV)
    head(FlucTotIV)
    names(FlucTotIV)
    str(FlucTotIV)
     

    
#COVARIATES------------------------------------------------------------------------------------------------------------------------------------
    
   #All covariates
      FlucTotIV$DV <- as.numeric(as.character(FlucTotIV$DV))
      FlucTotIV$BW <- as.numeric(as.character(FlucTotIV$BW))
      FlucTotIV$BMI <- as.numeric(as.character(FlucTotIV$BMI))
      FlucTotIV$BSA <- as.numeric(as.character(FlucTotIV$BSA))
      FlucTotIV$CREAT <- as.numeric(as.character(FlucTotIV$CREAT))
      FlucTotIV$CKDEPI <- as.numeric(as.character(FlucTotIV$CKDEPI))
      FlucTotIV$CG <- as.numeric(as.character(FlucTotIV$CG))
      FlucTotIV$BILI <- as.numeric(as.character(FlucTotIV$BILI))
      FlucTotIV$AST <- as.numeric(as.character(FlucTotIV$AST))
      FlucTotIV$GGT <- as.numeric(as.character(FlucTotIV$GGT))
      FlucTotIV$FLUID <- as.numeric(as.character(FlucTotIV$FLUID))
      FlucTotIV$ADMIN <- as.factor(FlucTotIV$ADMIN) #I added from here
      FlucTotIV$CRRT <- as.factor(FlucTotIV$CRRT)
      FlucTotIV$IHD <- as.factor(FlucTotIV$IHD)
      FlucTotIV$ARCAlg <- as.factor(FlucTotIV$ARCAlg)  #What is ARClg and how it is defined? 
      
      Datacont <- subset(FlucTotIV, select=c('DV','BW','BMI', 'BSA', 'CREAT','CKDEPI','CG','BILI','AST','GGT','FLUID'))
      Datacat <- subset(FlucTotIV, select=c('ADMIN', 'CRRT', 'IHD', 'ARCAlg'))
      Datatotal <- data.frame(Datacont, Datacat) # I addedl this line of code to create a different summary table
      
      kable(summarytools::descr(Datacont, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                transpose = TRUE), digits=2, booktabs =T, caption ="Covariates with concentrations")
      #Create tables in LaTeX, HTML, Markdown and reStructuredText
      
      #A different to create a summary table
      st(Datatotal,summ = list(
                 c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                   'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
         summ.names = list(
                 c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                 c('Count','Percent')))
      
      table(Datacat$ADMIN) #descriptive summary statistics
      table(Datacat$CRRT)
      table(Datacat$IHD)
      table(Datacat$ARCAlg)
      
    
   #Covariaten enkel bij spiegels
      DataDV = subset(FlucTotIV, EVID == 0) #Enkel stalen selecteren #only observations are selected 
      DataDV$DV <- as.numeric(as.character(DataDV$DV))
      DataDV$BW <- as.numeric(as.character(DataDV$BW))
      DataDV$BMI <- as.numeric(as.character(DataDV$BMI))
      DataDV$BSA <- as.numeric(as.character(DataDV$BSA))
      DataDV$CREAT <- as.numeric(as.character(DataDV$CREAT))
      DataDV$CKDEPI <- as.numeric(as.character(DataDV$CKDEPI))
      DataDV$CG <- as.numeric(as.character(DataDV$CG))
      DataDV$BILI <- as.numeric(as.character(DataDV$BILI))
      DataDV$AST <- as.numeric(as.character(DataDV$AST))
      DataDV$GGT <- as.numeric(as.character(DataDV$GGT))
      DataDV$FLUID <- as.numeric(as.character(DataDV$FLUID))

      DataDVcont <- subset(DataDV, select=c('DV','BW','BMI', 'BSA', 'CREAT','CKDEPI', 'CG','BILI','AST', 'GGT', 'FLUID'))
      DataDVcat <- subset(DataDV, select=c('ADMIN', 'CRRT', 'IHD', 'ARCAlg')) 
      DataDVtotal <- data.frame(DataDVcont,DataDVcat)
      st(DataDVtotal,summ = list(
              c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
         summ.names = list(
                 c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                 c('Count','Percent')))
    
      kable(summarytools::descr(DataDVcont, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                transpose = TRUE), digits=2, booktabs =T, caption ="Covariates with concentrations")
      table(DataDVcat$ADMIN)
      table(DataDVcat$CRRT)
      table(DataDVcat$IHD)
      table(DataDVcat$ARCAlg)
      
      #Correlation covariates
      DataDV$CKDEPI <- as.numeric(as.character(DataDV$CKDEPI))
      plot(density(DataDV$CKDEPI, na.rm = TRUE))
      

   #Covariates by dose only
      DataDos = subset(FlucTotIV, EVID == 1) #Enkel stalen selecteren #only dosing events are selected 
      DataDos$RATE <- as.numeric(as.character(DataDos$RATE))
      DataDos$AMT <- as.numeric(as.character(DataDos$AMT))
      DataDoscont <- subset(DataDos, select=c('RATE','AMT'))
      
      kable(summarytools::descr(DataDoscont, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                transpose = TRUE), digits=1, booktabs =T, caption ="Covariates with dosing events")
      st(DataDoscont,summ = list(
              c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
         summ.names = list(
                 c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                 c('Count','Percent')))
  
      
   #Covariates in patients
      #Unique patients
          #Select unique patients
          Patients<-unique(FlucTotIV$ID) #look length --> 177
          PatientEig <- FlucTotIV[c('ID', 'AGE', 'SEX', 'LENGHT', 'RACE')] #select unique patients based on unique parameters
          #Ask about the coding of RACE - what do 1 and 2 represent?
          
          PatientEig_un<- unique(setDT(PatientEig), by=names(PatientEig$ID))
          
          #Covariates
          PatientEig_un$AGE <- as.numeric(as.character(PatientEig_un$AGE))
          PatientEig_un$LENGHT <- as.numeric(as.character(PatientEig_un$LENGHT))
          PatientEig_un$SEX <- as.factor(PatientEig_un$SEX)
          PatientEig_un$RACE <- as.factor(PatientEig_un$RACE)
          DataPatcont <- subset(PatientEig_un, select=c('AGE', 'LENGHT'))
          DataPatcat <- subset(PatientEig_un, select=c('SEX', 'RACE'))
          
          table(DataPatcat$SEX) #62 (35%) v and 115 (65%) m #SEX: 1 is male and 2 is female
          kable(summarytools::descr(DataPatcont, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                    transpose = TRUE), digits=2, booktabs =T, caption ="Covariates with patients")
          
          st(PatientEig_un,summ = list(
                  c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                    'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
             summ.names = list(
                     c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                     c('Count','Percent')))
          
      #Unique patients with more covariates (sometimes weight differs per pte and then you have 2 lines for that pte)
          PatientEig3 <- FlucTotIV[c('ID', 'AGE', 'SEX', 'BW', 'BSA', 'BMI', 'LENGHT', 'APACHE')] #select unique patients based on unique parameters
          PatientEig3_un<- unique(setDT(PatientEig3), by=names(PatientEig3$ID)) #197 observations --> 20 double because only 177 pte --> look in database which rows have to be removed because they are duplicates: 44, 47, 49, 51, 52, 54, 56, 57, 60, 62, 63, 65, 67, 68, 70, 72, 73, 76, 77, 108
          PatientEig3_un <- PatientEig3_un[-c(44, 47, 49, 51, 52, 54, 56, 57, 60, 62, 63, 65, 67, 68, 70, 72, 73, 76, 77, 108), ]
          PatientenEig3<-unique(PatientEig3_un$ID) #look length --> 177 = ok
          
          #Covariates
          PatientEig3_un$AGE <- as.numeric(as.character(PatientEig3_un$AGE))
          PatientEig3_un$LENGHT <- as.numeric(as.character(PatientEig3_un$LENGHT))
          PatientEig3_un$BMI <- as.numeric(as.character(PatientEig3_un$BMI))
          PatientEig3_un$BSA <- as.numeric(as.character(PatientEig3_un$BSA))
          PatientEig3_un$APACHE <- as.numeric(as.character(PatientEig3_un$APACHE))
          PatientEig3_un$SEX <- as.factor(PatientEig3_un$SEX)
          DataPatcont3 <- subset(PatientEig3_un, select=c('ID', 'AGE', 'LENGHT', 'BW', 'BMI','BSA','APACHE'))
          DataPatcat3 <- subset(PatientEig3_un, select=c('SEX'))
          
          kable(summarytools::descr(DataPatcont3, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                    transpose = TRUE), digits=2, booktabs =T, caption ="Covariates with patients")
          
          st(PatientEig3_un,summ = list(
                  c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                    'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
             summ.names = list(
                     c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                     c('Count','Percent')))
      
      
    #Relevant covariates per patient and their correlation (for comparison with virtual dataset)
                #Is about: AGE, SEX, CREAT, BW, CRRT en CKDEPI
          
                #Preparation
                  #Create a database for this
                    PatientEig2 <- FlucTotIV[c('ID', 'AGE', 'SEX', 'LENGHT', 'RACE', 'CREAT', 'BW', 'CKDEPI', 'CRRT')] #I added CRRT
                    PatientEig2_un<- unique(setDT(PatientEig2), by=names(PatientEig2$ID))
                  
                  #All numeric covariates , make numeric
                    PatientEig2_un$AGE <- as.numeric(as.character(PatientEig2_un$AGE))
                    PatientEig2_un$BW <- as.numeric(as.character(PatientEig2_un$BW))
                    PatientEig2_un$CREAT <- as.numeric(as.character(PatientEig2_un$CREAT))
                    PatientEig2_un$CKDEPI <- as.numeric(as.character(PatientEig2_un$CKDEPI))
                    PatientEig2_un$SEX <- as.factor(PatientEig2_un$SEX)
                    PatientEig2_un$RACE <- as.factor(PatientEig2_un$RACE)
                    PatientEig2_un$CRRT <- as.factor(PatientEig2_un$CRRT)
      
                #General
                    #General overview
                    kable(summarytools::descr(PatientEig2_un, stats = c("med","q1","q3","mean","sd","min","max","n.valid"),
                                              transpose = TRUE), digits=2, booktabs =T, caption ="Covariates with patients")
                    st(PatientEig2_un,summ = list(
                            c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                              'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
                       summ.names = list(
                               c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                               c('Count','Percent')))
                    
                    
                    ## Mean, SD values by SEX
                    
                    #Followings are preparatory steps for CreateTableOne function
                    
                    # Make categorical variables factor
                    #varsToFactor <- c("SEX","RACE","CRRT") #these 2 lines don't work
                    #PatientEig2_un[varsToFactor]<-lapply(PatientEig2_un[varsToFactor],factor)
                    
                    # Create a variable list
                    dput(names(PatientEig2_un))
                    myVars <- c("AGE","BW","CREAT","CKDEPI","RACE","CRRT")
                    
                    # Create Table 1 stratified by SEX
                   tab<-CreateTableOne(vars = myVars, strata =
                                           c("SEX"),factorVars=c("RACE","CRRT"),data = PatientEig2_un) 
                    print(tab,showAllLevels = TRUE) #print all levels of categorical variables
                   
                #Mutual correlation at CRRT=0
                    #Selection
                      DatasetPatientCRRT0 <- subset(PatientEig2_un, CRRT==0)
                    #AGE versus SEX
                      boxplot(AGE~SEX,data=DatasetPatientCRRT0, main="REAL CRRT=0, AGE vs SEX",
                              xlab="SEX", ylab="AGE", ylim=c(15,100))
                    #BW versus SEX
                      boxplot(BW~SEX,data=DatasetPatientCRRT0, main="REAL CRRT=0, BW vs SEX",
                              xlab="SEX", ylab="BW")
                    #CREAT versus SEX
                      boxplot(CREAT~SEX,data=DatasetPatientCRRT0, main="REAL CRRT=0, CREAT vs SEX",
                              xlab="SEX", ylab="CREAT")
                        #Mean, SD values per SEX
                        tab1<-CreateTableOne(vars = myVars,strata =
                                         "SEX",factorVars=c("RACE","CRRT"),data = DatasetPatientCRRT0)
                        print(tab1,showAllLevels = TRUE) 
                  
                          #AGE versus CREAT
                      plot(DatasetPatientCRRT0$AGE, DatasetPatientCRRT0$CREAT, pch = 19)
                      abline(lm(DatasetPatientCRRT0$CREAT ~ DatasetPatientCRRT0$AGE), col = "red", lwd = 3)
                      model7 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT0)
                      summary(model7)
                          #SEX=2
                          DatasetPatientCRRT0SEX2 <- subset(DatasetPatientCRRT0, SEX==2)
                          ggplot(DatasetPatientCRRT0SEX2, aes(AGE, CREAT)) +
                            geom_point() +
                            stat_smooth(method = lm) +
                            PLOTSTYLE
                          model1 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT0SEX2)
                          summary(model1)
                              #CREAT = 0.69 + 0.0069 *AGE
                          #SEX=1
                          DatasetPatientCRRT0SEX1 <- subset(DatasetPatientCRRT0, SEX==1)
                          ggplot(DatasetPatientCRRT0SEX1, aes(AGE, CREAT)) +
                            geom_point() +
                            stat_smooth(method = lm) +
                            PLOTSTYLE
                          model2 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT0SEX1)
                          summary(model2)
                              #CREAT = 0.47 + 0.0119 *AGE
                          #Mean, SD values per SEX
                          CreateTableOne(vars = c("CREAT","CKDEPI"),strata =
                                           "SEX",data = DatasetPatientCRRT0)
                    #CREAT versus CRRT
                      boxplot(CREAT~CRRT,data=PatientEig2_un, main="REAL CRRT=0, CREAT vs CRRT",
                              xlab="CRRT", ylab="CREAT") #I changed the dataset for it to make sense
                    #AGE versus BW
                      plot(DatasetPatientCRRT0$AGE, DatasetPatientCRRT0$BW, pch = 19)
                      abline(lm(DatasetPatientCRRT0$BW ~ DatasetPatientCRRT0$AGE), col = "red", lwd = 3)
                    
            
                #Mutual correlation at CRRT=1
                      #Selection
                      DatasetPatientCRRT1 <- subset(PatientEig2_un, CRRT==1)
                      #AGE versus SEX
                      boxplot(AGE~SEX,data=DatasetPatientCRRT1, main="REAL CRRT=1, AGE vs SEX",
                              xlab="SEX", ylab="AGE", ylim=c(15,100))
                      #BW versus SEX
                      boxplot(BW~SEX,data=DatasetPatientCRRT1, main="REAL CRRT=1, BW vs SEX",
                              xlab="SEX", ylab="BW")
                      #CREAT versus SEX
                      boxplot(CREAT~SEX,data=DatasetPatientCRRT1, main="REAL CRRT=1, Creat vs SEX",
                              xlab="SEX", ylab="CREAT", ylim=c(0,5))
                      
                          #Mean, SD values per SEX
                          CreateTableOne(vars = c("AGE","BW","CREAT","CKDEPI","CRRT"),strata =
                                           "SEX",data = DatasetPatientCRRT1)
                      #AGE versus CREAT
                      plot(DatasetPatientCRRT1$AGE, DatasetPatientCRRT1$CREAT, pch = 19)
                      abline(lm(DatasetPatientCRRT1$CREAT ~ DatasetPatientCRRT1$AGE), col = "red", lwd = 3)
                      model8 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT1)
                      summary(model8)
                          #SEX=2
                          DatasetPatientCRRT1SEX2 <- subset(DatasetPatientCRRT1, SEX==2)
                          ggplot(DatasetPatientCRRT1SEX2, aes(AGE, CREAT)) +
                            geom_point() +
                            stat_smooth(method = lm) +
                            PLOTSTYLE
                          model3 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT1SEX2)
                          summary(model3)
                              #CREAT = 2.76 - 0.0272 *AGE
                          #SEX=1
                          DatasetPatientCRRT1SEX1 <- subset(DatasetPatientCRRT1, SEX==1)
                          ggplot(DatasetPatientCRRT1SEX1, aes(AGE, CREAT)) +
                            geom_point() +
                            stat_smooth(method = lm) +
                            PLOTSTYLE
                          model4 <- lm(CREAT ~ AGE, data = DatasetPatientCRRT1SEX1)
                          summary(model4)
                              #CREAT = 0.42 + 0.0187 *AGE
                          #Mean, SD values per SEX
                          CreateTableOne(vars = c("CREAT","CKDEPI"),strata =
                                           "SEX",data = DatasetPatientCRRT1)
                      #CREAT versus CRRT
                      boxplot(CREAT~CRRT,data=PatientEig2_un, main="REAL CRRT=1, CREAT vs CRRT",
                              xlab="CRRT", ylab="CREAT", ylim=c(0,5))
                      #AGE versus BW
                      plot(DatasetPatientCRRT1$AGE, DatasetPatientCRRT1$BW, pch = 19)
                      abline(lm(DatasetPatientCRRT1$BW ~ DatasetPatientCRRT1$AGE), col = "red", lwd = 3)
      
                  
                  #Min and max values
                   kable(summarytools::descr(PatientEig2_un, stats = c("min","max","n.valid"),
                                            transpose = TRUE), digits=1, booktabs =T, caption ="Min and max covariates")
          
                  #Density distribution
                    plot(density(PatientEig2_un$AGE))
                    #plot(density(PatientEig2_un$SEX)) #can't produce density plot for a categorical variable 
                    plot(density(PatientEig2_un$CREAT, na.rm = TRUE))
                    plot(density(PatientEig2_un$BW, na.rm = TRUE))
                    #plot(density(PatientEig2_un$CRRT)) #the same with SEX
      
            
   #Liver parameters
      #Select liver parameters
      Liver <- subset(FlucTotIV, select=c('ID','DV', 'EVID', 'GGT','AFT', 'AST', 'ALT', 'BILI', 'ALB'))
      LiverDV <- subset(Liver,EVID==0) #select observations only #DV: dependent variable
      
      #Overview parameters
      kable(summarytools::descr(LiverDV, stats = c("q1","med","q3","min","max","n.valid"),
                                transpose = TRUE), digits=1, booktabs =T, caption ="Liver parameters")
      st(LiverDV,summ = list(
              c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
         summ.names = list(
                 c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                 c('Count','Percent')))
      
      
   #Median FLUID of database 1 and 7
      DatabaseFluid <- subset(FlucTotIV, HOSPITAL==1 | HOSPITAL == 7)
      median(DatabaseFluid$FLUID, na.rm = TRUE) #is same as median entire database because only these 2 databases have a FLUID
      
      DatabaseNOFluid <- subset(FlucTotIV, HOSPITAL==2 | HOSPITAL == 3 | HOSPITAL == 4 | HOSPITAL == 5 | HOSPITAL == 6 | HOSPITAL == 8 | HOSPITAL == 9 )
      summary(DatabaseNOFluid$FLUID) #all FLUID = NA
      
      #=> so you shouldn't make a separate Vd in mod.file for HOSP1&7 because all the rest is still FLUID=median and has no influence on model
      #Don't really get it
  
      
  #Min and max difference between "creat" value within 1 patient
      #Select database with multiple "creat" values per patient
      DataCreatvar <- subset(FlucTotIV,  HOSPITAL== 1 | HOSPITAL == 2 | HOSPITAL == 3 | HOSPITAL == 6)
      #Select database only ID and CREAT
      DataCreat <- DataCreatvar[c('ID', 'CREAT')]
      #Select unique rows
      DataCreat_un<- unique(setDT(DataCreat), by=names(DataCreat$ID))
      #Per patient the difference between max creat of that patient and min creat of that patient
      DataCreat_Diff <- DataCreat_un %>% group_by(ID) %>% summarise(Diff = max(CREAT)- min(CREAT))
      #Per patient mean creat
      DataCreat_Mean <- DataCreat_un %>% group_by(ID) %>% summarise(Mean = mean(CREAT))
      #Per patient the difference % change
      DataCreat_Perc <- DataCreat_un %>% group_by(ID) %>% summarise(Perc = (max(CREAT)- min(CREAT))/mean(CREAT)*100)
      #Bind together
      DataCreat_Resul <- cbind(DataCreat_Diff, DataCreat_Mean,DataCreat_Perc)
      #Range of Perc
      min(DataCreat_Resul$Perc,na.rm=TRUE)
      max(DataCreat_Resul$Perc,na.rm=TRUE)
      median(DataCreat_Resul$Perc,na.rm=TRUE)
        #Creat within a PTE can range from 0 to 158% of that PTE's average #PTE: patient
      
  #Amount of samples in each OCC of ADMIN
      DataDV = subset(FlucTotIV, EVID == 0) #Select samples only
      table(DataDV$OCC3) #number of samples per OCC #what is OCC?

      
      
#HOGE DALSPIEGELS OP BASIS VAN DE CONCENTRATIE OP ZICH-------------------------------------------------------------------------------------------------------------------------------------------------
      #HIGH TALL LEVELS BASED ON THE CONCENTRATION IN ITSELF?
      
   #Convert DV to numeric without changing the values
        DataDV = subset(FlucTotIV, EVID == 0)
        DataDV$DV <- as.numeric(as.character(DataDV$DV))
      
   #Select trough levels = TAD between 23 and 25 = DataDVDal
        DataDVDal <- DataDV %>% filter (TAD > 23 & TAD < 25) #why choose time after dose around 24 mins? #it should be hour instead of minute
        summary(DataDVDal$DV)
      
   #Above 30 mg\L (on VPC that lowest point is for the rise at trough)
        #Select rows with DV greater than 30
        HighConc30 = subset(DataDVDal, DV>30)
        
        #Patients
        Patients30<-unique(HighConc30$ID) #watch length --> 21
        PatientEig30 <- HighConc30[c('ID', 'AGE', 'SEX', 'LENGHT', 'RACE', 'HOSPITAL')]
        PatientEig_un30<- unique(setDT(PatientEig30), by=names(PatientEig30$ID))
        table(PatientEig_un30$HOSPITAL)
        
        #Covariates
        HighConc30cont <- subset(HighConc30, select=c('DV','BW','CREAT','CKDEPI'))
        HighConc30cat <- subset(HighConc30, select=c('ADMIN', 'CRRT'))
        
        kable(summarytools::descr(HighConc30cont, stats = c("q1","med","q3","min","max","n.valid"),
                                  transpose = TRUE), digits=1, booktabs =T, caption ="Cmin above 30 mg/L")
        st(HighConc30cont,summ = list(
                c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                  'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
           summ.names = list(
                   c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                   c('Count','Percent')))
        
        table(HighConc30cat$ADMIN)
        table(HighConc30cat$CRRT)
        
        
    #Above 50 mg\L (upper top)
        #Select rows with DV greater than 50
        HighConc50 = subset(DataDVDal, DV>50)
        
        #Patients
        Patients50<-unique(HighConc50$ID) #watchlength --> 5
        PatientEig50 <- HighConc50[c('ID', 'AGE', 'SEX', 'LENGHT', 'RACE', 'HOSPITAL')]
        PatientEig_un50<- unique(setDT(PatientEig50), by=names(PatientEig50$ID))
        table(PatientEig_un50$HOSPITAL)
        
        #Covariates
        HighConc50cont <- subset(HighConc50, select=c('DV','BW','CREAT','CKDEPI'))
        HighConc50cat <- subset(HighConc50, select=c('ADMIN', 'CRRT'))
        
        kable(summarytools::descr(HighConc50cont, stats = c("q1","med","q3","min","max","n.valid"),
                                  transpose = TRUE), digits=1, booktabs =T, caption ="Cmin above 50 mg/L")
        st(HighConc50cont,summ = list(
                c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                  'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
           summ.names = list(
                   c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                   c('Count','Percent')))
        table(HighConc50cat$ADMIN)
        table(HighConc50cat$CRRT)
        
        #Dose (not in the HighConc50 dataset because only sampling events there)
        #Select pte
        HighConc50Pte <- subset(FlucTotIV, ID %in% c(117, 122, 126, 139, 144))
        #Set AMT to numeric
        HighConc50Pte$AMT <- as.numeric(as.character(HighConc50Pte$AMT))
        #Dosis (niet enkel dosis die bij hoge spiegel hoort maar algemeen, anders is te vervelend om te selecteren en om maar idee te hebben)
        #Dose (not only dose that belongs to high mirror but general, otherwise it is too annoying to select and to have an idea)?
        summary(HighConc50Pte$AMT)
        
      

#HOGE SPIEGELS OP BASIS VAN INTERACTIEVE PLOT-------------------------------------------------------------------------------------------------------------------------------
 #HIGH MIRRORS/REFLECTION BASED ON INTERACTIVE PLOT?
        
    #Create interactive plots to see who is outlier
          #Plot, group by column
          DataDV = subset(FlucTotIV, EVID == 0) #only concentrations 
        
          #Everything must be numeric
          DataDV$DV <- as.numeric(as.character(DataDV$DV)) #DV to Numeric
          DataDV$TAD<- as.numeric(as.character(DataDV$TAD)) #AMT to numeric
          
          #Concentration depending on TAD
            p1 <- ggplot(DataDV, aes(TAD, as.numeric(DV),colour = as.factor(HOSPITAL), data_id=ID)) +
              geom_point()+
              scale_y_continuous(name="Fluconazole conc [mg/L]") +
              scale_x_continuous(name="TAD (min)",limits= c(0, 24) ) + #should be hour here as well
              PLOTSTYLE
            ggplotly(p1) #to make it interactive (if you want to see ID in this, you have to put data_id=ID in aes) #plotly interactive plot - interesting
            
          #Concentration/dose depending on TAD
                #Select the EVID==0 with row above 
                  #DataDVAMT1 <- FlucTotIV[which(FlucTotIV$EVID == 0) + c(-1:0), ] --> not necessary if you work with lag's formula
                  DataDVAMT1 <- FlucTotIV
                
                #Set up database properly
                  DataDVAMT1$CA = NA #create new column for concentration/dose
                  DataDVAMT1$DV <- as.numeric(as.character(DataDVAMT1$DV)) #DV to Numeric
                  DataDVAMT1$AMT<- as.numeric(as.character(DataDVAMT1$AMT)) #AMT to numeric
                  
                #Dosissen doortrekken zodat er op elke rij een dosis staat (anders heb je de spiegels die niet direct na een dosis afgenonen zijn en waar dus minstens nog 1 spiegel tussen zit niet mee)
                  #Extend doses so that there is a dose on each row (otherwise you will not include the mirrors that were not measured immediately after a dose and that therefore have at least 1 mirror in between)?
                  
                  #First replace the dot with NA, otherwise it seems that there is already a dose everywhere  #Replace the dot 'NA' in the AMT column with the most recent non-'NA' prior to it using na.locf function
                  for (i in unique(DataDVAMT1$ID)){
                    DataDVAMT1$AMT[DataDVAMT1$ID==i] = na.locf(DataDVAMT1$AMT[DataDVAMT1$ID==i],na.rm=F, fromLast = F) #na.locf could be used to replace user-written function
                  }
                  #na.locf: Generic function for replacing each NA with the most recent non-NA prior to it     #change the NONMEM format data to R format data
                  
                #Calculate the concentration/dose (dose is one row above)
                  DataDVAMT2 <- DataDVAMT1 %>% mutate(CA = DV / AMT) #now that we have extended the AMT, it no longer has to be with the row above it, but it is allowed with the row at the same height because it is drawn from top to bottom
                        #DataDVAMT2 <- DataDVAMT1 %>% mutate(CA = DV / lag(AMT)) #formula, the lag function points to going back 1 row
                  test <- subset(DataDVAMT2, select = c('ID', 'DV', 'AMT', 'CA')) #check if it worked
                
                #Enkel de spiegels
                  #Just the mirrors?
                DataDVCA = subset(DataDVAMT2, EVID == 0) #only concentrations 
                
                #Everything must be numeric
                DataDVCA$CA <- as.numeric(as.character(DataDVCA$CA)) #CA to numeric
                DataDVCA$TAD<- as.numeric(as.character(DataDVCA$TAD)) #AMT to numeric
                
                #Interactive plot
                p2 <- ggplot(DataDVCA, aes(TAD, as.numeric(CA),colour = as.factor(HOSPITAL), data_id=ID)) +
                  geom_point()+
                  scale_y_continuous(name="Conc/Dose") +
                  scale_x_continuous(name="TAD (min)",limits= c(0, 24) ) +
                  PLOTSTYLE
                ggplotly(p2) #to make it interactive (if you want to see ID in this, you have to put data_id=ID in aes)
              
            
          #Concentration depending on TIME
            p3 <- ggplot(DataDV, aes(TIME, as.numeric(DV),colour = as.factor(HOSPITAL), data_id=ID)) +
              scale_y_continuous(name="Fluconazole conc [mg/L]") +
              scale_x_continuous(name="TIME (min)") +
              geom_point()+
              PLOTSTYLE
            ggplotly(p3)
    
              
    #Hoge spiegels selecteren op basis van p1
            ##Select high mirrors based on p1 p=plot
                  
          #Patients with high concentrations --> based on interactive plot
              #Hospital 1 (105, 114, 117, 118, 126, 139, 144)
              #Hospital 5 (510, 512, 513, 517, 520, 525)
              #Hospital 6 (620)
            
          #Convert DV to numeric without changing the values
              DataDV = subset(FlucTotIV, EVID == 0)
              DataDV$DV <- as.numeric(as.character(DataDV$DV))
              
          #Select patients with high concentration
              #Selection
                  HighConcInPlot1 = subset(DataDV, ID %in% c(105, 114, 117, 118, 126, 139, 144, 510, 512, 513, 517, 520, 525, 620))
              
              #Covariates
                  HighConcInPlot1cont <- subset(HighConcInPlot1, select=c('DV', 'AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI'))
                  HighConcInPlot1cat <- subset(HighConcInPlot1, select=c('CRRT','ARCAlg', 'SEX'))
                  
                  kable(summarytools::descr(HighConcInPlot1, stats = c("q1","med","q3","min","max","n.valid"),
                                            transpose = TRUE), digits=1, booktabs =T, caption ="High conc")
                  st(HighConcInPlot1cont,summ = list(
                          c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                            'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
                     summ.names = list(
                             c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                             c('Count','Percent')))
                  table(HighConcInPlot1cat$CRRT)
                  table(HighConcInPlot1cat$ARCAlg)
              
              #Dose (not in the HighConcInPlot dataset because there are only sampling events)
                  #Select pte
                  HighConcInPlot1Pte <- subset(FlucTotIV, ID %in% c(105, 114, 117, 118, 126, 139, 144, 510, 512, 513, 517, 520, 525, 620))
                  #Set AMT to numeric
                  HighConcInPlot1Pte$AMT <- as.numeric(as.character(HighConcInPlot1Pte$AMT))
                  #Dose (not only dose that belongs to high mirror but general, otherwise it is too annoying to select and to have an idea)
                  summary(HighConcInPlot1Pte$AMT)
                  
   
          #Do not select high mirrors (for comparison)
              #Selection
                 NotHighConcInPlot1 <- DataDV[!(DataDV$ID %in% c("105", "114", "117", "118", "126", "139", "144", "510", "512", "513", "517", "520", "525", "620")),]
              
              #Covariates
                  NotHighConcInPlot1cont <- subset(NotHighConcInPlot1, select=c('DV','AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI'))
                  NotHighConcInPlot1Cat <- subset(NotHighConcInPlot1, select=c('CRRT','ARCAlg', 'SEX'))
                          
                  kable(summarytools::descr(NotHighConcInPlot1cont, stats = c("q1","med","q3","min","max","n.valid"),
                                                    transpose = TRUE), digits=1, booktabs =T, caption ="Non-high conc")
                  st(NotHighConcInPlot1cont,summ = list(
                          c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                            'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
                     summ.names = list(
                             c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                             c('Count','Percent')))
                  table(NotHighConcInPlot1Cat$CRRT)
                  table(NotHighConcInPlot1Cat$ARCAlg)
              
              #Dose
                  #Select pte
                  NotHighConcInPlot1Pte <- FlucTotIV[!(FlucTotIV$ID %in% c("105", "114", "117", "118", "126", "139", "144", "510", "512", "513", "517", "520", "525", "620")),]
                  #Set AMT to numeric
                  NotHighConcInPlot1Pte$AMT <- as.numeric(as.character(NotHighConcInPlot1Pte$AMT))
                  #Dose (not only dose that belongs to high mirror but general, otherwise it is too annoying to select and to have an idea)
                  summary(NotHighConcInPlot1Pte$AMT)
          
              
                  
                  
    #Hoge spiegels selecteren op basis van p2
                  #Select high mirrors based on p2
                  
      #Patients with high concentrations --> based on interactive plot
            #Hospital 1 (110, 117, 118, 120, 126, 144)
            #Hospital 5 (506, 510, 520)
            #Hospital 6 (620)
            #Hospital 8 (813, 838)
                  
            #DV naar numeric zetten zonder dat de waarden veranderen
                DataDV = subset(FlucTotIV, EVID == 0)
                DataDV$DV <- as.numeric(as.character(DataDV$DV))
                  
            #Select patients with high concentration
               #Selection
                HighConcInPlot2 = subset(DataDV, ID %in% c(110, 117, 118, 120, 126, 144, 506, 510, 520, 620, 813, 838))
                  
              #Covariats
                HighConcInPlot2cont <- subset(HighConcInPlot2, select=c('DV', 'AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI'))
                HighConcInPlot2cat <- subset(HighConcInPlot2, select=c('CRRT','ARCAlg', 'SEX'))
                
                kable(summarytools::descr(HighConcInPlot2cont, stats = c("q1","med","q3","min","max","n.valid"),
                                          transpose = TRUE), digits=1, booktabs =T, caption ="High conc")
                st(HighConcInPlot2cont,summ = list(
                        c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                          'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
                   summ.names = list(
                           c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                           c('Count','Percent')))
                table(HighConcInPlot2cat$CRRT)
                table(HighConcInPlot2cat$ARCAlg)
                  
              #Dose (not in the HighConcInPlot dataset because there are only sampling events)
                #Select pte
                HighConcInPlot2Pte <- subset(FlucTotIV, ID %in% c(110, 117, 118, 120, 126, 144, 506, 510, 520, 620, 813, 838))
                #Set AMT to numeric
                HighConcInPlot2Pte$AMT <- as.numeric(as.character(HighConcInPlot2Pte$AMT))
                #Dose (not only dose that belongs to high level but general, otherwise it is too annoying to select and to have an idea)
                summary(HighConcInPlot2Pte$AMT)
                  
                  
        #Do not select high mirrors (for comparison)
              #Selection
                NotHighConcInPlot2 <- DataDV[!(DataDV$ID %in% c("110", "117", "118", "120", "126", "144", "506", "510", "520", "620", "813", "838")),]
                  
              #Covariates
                NotHighConcInPlot2cont <- subset(NotHighConcInPlot2, select=c('DV','AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI'))
                NotHighConcInPlot2Cat <- subset(NotHighConcInPlot2, select=c('CRRT','ARCAlg', 'SEX'))
                
                kable(summarytools::descr(NotHighConcInPlot2cont, stats = c("q1","med","q3","min","max","n.valid"),
                                          transpose = TRUE), digits=1, booktabs =T, caption ="Non-high conc")
                st(NotHighConcInPlot2cont,summ = list(
                        c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)',
                          'pctile(x)[75]','max(x)'), c('notNA(x)','mean(x)')),
                   summ.names = list(
                           c('N','Mean','SD','Min','Pctl.25','Median','Pctl.75','Max'),
                           c('Count','Percent')))
                table(NotHighConcInPlot2Cat$CRRT)
                table(NotHighConcInPlot2Cat$ARCAlg)
                  
              #Dose
                #Select pte
                NotHighConcInPlot2Pte <- FlucTotIV[!(FlucTotIV$ID %in% c("110", "117", "118", "120", "126", "144", "506", "510", "520", "620", "813", "838")),]
                #Set AMT to numeric
                NotHighConcInPlot2Pte$AMT <- as.numeric(as.character(NotHighConcInPlot2Pte$AMT))
                #Dose (not only dose that belongs to high mirror but general, otherwise it is too annoying to select and to have an idea)
                summary(NotHighConcInPlot2Pte$AMT)
                  
              
#AFWIJKENDE SPIEGELS IN GOF------------------------------------------------------------------------------------------------------------------------------------
                #DIFFERENT MIRRORS IN GOF  #GOF - Goodness-of-fit
                
    #The overestimated pte in the GOF
            #Loading the sdtab003 into R
              setwd("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Nonmem/Runs/Runs_220726")
              sdtab <- read.nm.tables('sdtab034') 
            #Plot alle data
              p3 <- ggplot(sdtab, aes(x= PRED, y= DV)) + #GOF met Cobs tov PRED
                geom_text(aes(label=ID))+ 
                PLOTSTYLE
              ggplotly(p3) #interactief maken
                  #missing values zijn DV=. denk ik dus als geen concentratie gemeten wordt (staan in deze plot bij maar bij gewone GOF via Pirananiet)
            #Plot stuk dat probleem vormt en de plot naar onder trekt
                p4 <- ggplot(sdtab, aes(x= PRED, y= DV)) + #GOF met Cobs tov PRED
                  geom_text(aes(label=ID)) +
                  xlim(35,80) + #ik wil kijken naar de IDs met een veel hogere concentratie dan verwacht, dwz een hoge waarde op x-as maar lage waarde op-as --> trekt GOF naar beneden
                  ylim(0,30) +
                  PLOTSTYLE
                ggplotly(p3) #interactief maken
            #Pte die in dit afwijken: 
                #vooral: 134, 302, 303, 520, 529  
                #Als telkens meest afwijkende eruit neemt tot redelijk goede GOF plot: 134, 302, 303, 304,520, 529, 823, 841

            #Outlier pte selecteren
              HogeGOF <- subset(FlucTotIV, ID %in% c(134, 302, 303, 304,520, 529, 823, 841))
              HogeGOF$DV <- as.numeric(as.character(HogeGOF$DV)) #naar numeriek en de '.' wordt dan MA, wat de bedoeling is
              HogeGOFDV = subset(HogeGOF, EVID == 0) #Enkel stalen selecteren
              
              HogeGOFcont <- subset(HogeGOFDV, select=c('DV','AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI', 'FLUID'))
              HogeGOFcat <- subset(HogeGOFDV, select=c('CRRT','IHD', 'ARCAlg', 'SEX'))
              
              kable(summarytools::descr(HogeGOFcont, stats = c("med","q1","q3",'mean',"min","max","n.valid"),
                                        transpose = TRUE), digits=2, booktabs =T, caption ="GOF outliers")
              table(HogeGOFcat$CRRT)
              
              HogeGOF$AMT <- as.numeric(as.character(HogeGOF$AMT)) #naar numeriek en de '.' wordt dan MA, wat de bedoeling is
              summary(HogeGOF$AMT)
            
            #Controlegroup (alle andere pte)
              NietHogeGOF <- FlucTotIV[!(FlucTotIV$ID %in% c("134", "302", "303","304","520","529", "823", "841")),]
              NietHogeGOF$DV <- as.numeric(as.character(NietHogeGOF$DV)) #naar numeriek en de '.' wordt dan MA, wat de bedoeling is
              NietHogeGOFDV = subset(NietHogeGOF, EVID == 0) #Enkel stalen selecteren
              
              NietHogeGOFcont <- subset(NietHogeGOFDV, select=c('DV','AGE', 'BW','BMI','BSA','CREAT','CKDEPI','CG', 'AST', 'GGT', 'BILI','FLUID'))
              NietHogeGOFcat <- subset(NietHogeGOFDV, select=c('CRRT','IHD', 'ARCAlg', 'SEX'))
              
              kable(summarytools::descr(NietHogeGOFcont, stats = c("q1","med","q3",'mean', "min","max","n.valid"),
                                        transpose = TRUE), digits=1, booktabs =T, caption ="GOF others")
              table(NietHogeGOFcat$CRRT)
              
              NietHogeGOF$AMT <- as.numeric(as.character(NietHogeGOF$AMT)) #naar numeriek en de '.' wordt dan MA, wat de bedoeling is
              summary(NietHogeGOF$AMT)
              