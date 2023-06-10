#VOORBEREIDING---------------------------------------------------------------------------------

    #Laden packages
    library(Hmisc)
    library(zoo)
    library(ggplot2) #deel tidyverse
    library(dplyr) #deel tidyverse
    library(tidyr) #deel tidyverse
    library(stats)
    library(xpose4) #tabel van nonmem importeren
    library(naniar) #een waarde in dataset vervangen door een andere waarde
    library(purrr) #for map_dfr
    
    
    #Plotstyle instellen
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


#TOTALE DATASET LADEN---------------------------------------------------------------------------------
  
    #Aparte datasets laden...........................................................................................
      #UZL
      DatabankFlucUZL <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Databanken/UZL/DatabankFlucUZL_nonmem_finaal.csv")
      #Radboudumc
      DatabankFlucRAD <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/Radboudumc/DatabankFlucRAD_nonmem_finaal.csv")
      #Bergner
      DatabankFlucBERGN <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/Bergner/DatabankFlucBERGN_nonmem_finaal.csv")
      #Buijk
      DatabankFlucBUIJK <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/Buijk/DatabankFlucBUIJK_nonmem_finaal.csv")
      #Sandaradura
      DatabankFlucSAN <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/Sandaradura/DatabankFlucSAN_nonmem_finaal.csv")
      #RobertsDALI
      DatabankFlucDALI <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/RobertsDALI/DatabankFlucDALI_nonmem_finaal.csv")
      #RobertsObese
      DatabankFlucOBS <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK//Databanken/RobertsObese/DatabankFlucOBS_nonmem_finaal.csv")
      #Alffenaar
      DatabankFlucALF <- read.csv("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Databanken/Alffenaar/DatabankFlucALF_nonmem_finaal.csv")
      
      
    #Datasets aan elkaar binden tot uiteindelijke finale dataset (FlucTot3)...........................................................................................
      #Verschillende datasets samen voegen...........................................................................................
      FlucTot1 <- rbind(DatabankFlucUZL,DatabankFlucRAD, DatabankFlucBERGN, DatabankFlucBUIJK, DatabankFlucSAN, DatabankFlucDALI, DatabankFlucOBS, DatabankFlucALF) 
      
      #Kolom van OCC2 verwijderen (= kolom36 in FlucTot1) ...........................................................................................
      FlucTot2  = FlucTot1 [-36]
      
      #FlucTot2 met FlucConc als numeriek maar dan is "." naar NA
      FlucTot2$DV <- as.numeric(as.character(FlucTot2$DV))


#BMI and BSABEREKENEN----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  #Niet berekend in dataset dus zelf berekenen       
  
      #Alle -99 naar NA, anders rekent hij met -99
      FlucTot2$BW[FlucTot2$BW==-99]= NA
      FlucTot2$LENGHT[FlucTot2$LENGHT==-99]= NA
      
      #Formules
      FlucTot2$BMI =  (FlucTot2$BW)/(FlucTot2$LENGHT^2)
      FlucTot2$BSA =  0.007184 * (FlucTot2$BW^0.425) * ((FlucTot2$LENGHT*100)^0.725)


#CG RENAL FUNCTION BEREKENEN----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  #Niet berekend in dataset dus zelf berekenen  

      #Nieuwe tijdelijke databank hiervoor aanmaken
      CGCalc <- FlucTot2
      
      #Alle '-99' CREATs moeten even terug naar NA omgezet worden anders neemt R de waarde -99 voor het berekenen van CG
      CGCalc$CREAT[CGCalc$CREAT==-99]= NA
      
      #De groepen maken waarvoor eenzelfde formule voor CG (namelijk man en vrouw)
      #Dit schrijven in een nieuwe kolom, namelijk CGx, andere x voor elke databank
      CGMan <- CGCalc %>% 
        filter(SEX == 1)
        CGMan$CG1= ((140-CGMan$AGE) * (CGMan$BW))/(72 * CGMan$CREAT)
      
      CGVrouw <- CGCalc %>% 
        filter(SEX == 2)
        CGVrouw$CG2= ((140-CGVrouw$AGE) * (CGVrouw$BW))/(72 * CGVrouw$CREAT) * 0.85

      
      #Alle rijen samenvoegen = 452 rijen = oorspronkelijke dataset
      combinedCGCalc2 <- bind_rows(CGMan,CGVrouw)
      
      #Je wil nu 1 kolom maken van de CG1, CG2, CG3 en CG4
      #Als je gewoon optelt, komt de NA erbij --> die wil je eerst weg
      #Krijg foutmelding omdat nieuwste versie van tidyverse niet meer automatisch omzet naar character dus moet je eerst zelf doen
      combinedCGCalc2$CG1 <- as.character(combinedCGCalc2$CG1) %>% 
        replace_na("")
      combinedCGCalc2$CG2 <- as.character(combinedCGCalc2$CG2) %>% 
        replace_na("")
   
      #Dan optellen => in CG zit nu alles samen 
      combinedCGCalc2$CG = paste(combinedCGCalc2$CG1 , combinedCGCalc2$CG2)
      combinedCGCalc2$CG <- as.numeric(as.character(combinedCGCalc2$CG))
      
      #De overbodige kolommen CGx verwijderen
      combinedCGCalc3 = combinedCGCalc2[-42]
      combinedCGCalc4 = combinedCGCalc3[-42] #getal blijft altijd 42 omdat er een kolom weggaat telkens
      

      #Juist ordenen
      combinedCGCalc4$CG <- as.numeric(as.character(combinedCGCalc4$CG))
      combinedCGCalc5 <- arrange(combinedCGCalc4, ID, OCC, TIME)

      #Gelijkstellen aan Fluc3
      FlucTot3 <- combinedCGCalc5
      FlucTot3$CG <- as.numeric(as.character(FlucTot3$CG))
      
      #Formules CG (https://www.kidney.org/professionals/kdoqi/gfr_calculatorcoc)
        #Bij mannen:
            #CCr={((140–age) x weight)/(72xSCr)}
        #Bij vrouwen:
            #CCr={((140–age) x weight)/(72xSCr)}x 0.85
      

      
#LogDV van DV invoegen (om te proberen, Roger had dat gedaan in studie maar achteraf zag ik dat hij die kolom wel gedropt heeft)-------------------------------------------------------------------------------------------
      FlucTot3$LOGDV = log10(FlucTot3$DV)
      
           
#Enkel IV databank maken ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      #Databank met enkel PO om te zien welke patienten geexcludeerd moeten worden
      DataPO = subset(FlucTot3, CMT==1) #PO toedieningen (stalen zijn sowieso CMT=2 want altijd IV genomen)
      PatientenPO<-unique(DataPO$ID) #lengte kijken --> 16
      PatientenPO #103 109 112 130 131 143 402 406 408 409 410 411 412 413 414 602  
      
      #Databank met enkel IV --> exclusie van bovenstaande pte
      FlucTot4 <- FlucTot3[FlucTot3$ID != 103 & FlucTot3$ID != 109 & FlucTot3$ID != 112 & FlucTot3$ID != 130 & FlucTot3$ID != 131 & FlucTot3$ID != 143 & FlucTot3$ID != 402 & FlucTot3$ID != 406 & FlucTot3$ID != 408 & FlucTot3$ID != 409 & FlucTot3$ID != 410 & FlucTot3$ID != 411 & FlucTot3$ID != 412 & FlucTot3$ID != 413 & FlucTot3$ID != 414 & FlucTot3$ID != 602, ]  
      table(FlucTot4$ADMIN) #checken dat geen PO bijzit
      

            
#In de nonmem dataset de -99 van de continue variabelen vervangen naar mediaan van die parameter-------------------------------------------------------------------------------------------
  #anders moet je met formules in nonmem werken en dat maakt script ingewikkelder
      #Nieuwe dataset maken omdat enkel voor nonmem moet --> Fluc4
      FlucTot5 = FlucTot4
      FlucTot5 <- FlucTot5 %>% replace_with_na_all(condition = ~.x == -99)
      FlucTot5DV <- subset(FlucTot5, EVID==0)

      
      #BW
      FlucTot5$BW[FlucTot5$BW==-99]= NA  #eerste alle -99 naar NA, anders berekent hij de mediaan met -99 als getal
      FlucTot5$BW <- as.numeric(as.character(FlucTot5$BW)) #BW als numeriek zetten om mee te kunnen rekenen
      median(FlucTot5$BW, na.rm = TRUE) #mediaan berekenen (om te kijken of dit klopt)
      FlucTot5$BW[is.na(FlucTot5$BW)] <- median(FlucTot5$BW, na.rm = TRUE)  #mediaan invullen ipv NA
      
      FlucTot5$BMI[FlucTot5$BMI==-99]= NA 
      FlucTot5$BMI <- as.numeric(as.character(FlucTot5$BMI))
      median(FlucTot5$BMI, na.rm = TRUE)
      FlucTot5$BMI[is.na(FlucTot5$BMI)] <- median(FlucTot5$BMI, na.rm = TRUE)
      
      FlucTot5$BSA[FlucTot5$BSA==-99]= NA 
      FlucTot5$BSA <- as.numeric(as.character(FlucTot5$BSA))
      median(FlucTot5$BSA, na.rm = TRUE)
      FlucTot5$BSA[is.na(FlucTot5$BSA)] <- median(FlucTot5$BSA, na.rm = TRUE)
      
      FlucTot5$LENGHT[FlucTot5$LENGHT==-99]= NA 
      FlucTot5$LENGHT <- as.numeric(as.character(FlucTot5$LENGHT))
      median(FlucTot5$LENGHT, na.rm = TRUE)
      FlucTot5$LENGHT[is.na(FlucTot5$LENGHT)] <- median(FlucTot5$LENGHT, na.rm = TRUE)
      FlucTot5$LENGHT = FlucTot5$LENGHT * 100 #in model Muilwijk is in cm
      
      FlucTot5$AGE[FlucTot5$AGE==-99]= NA 
      FlucTot5$AGE <- as.numeric(as.character(FlucTot5$AGE))
      median(FlucTot5$AGE, na.rm = TRUE)
      FlucTot5$AGE[is.na(FlucTot5$AGE)] <- median(FlucTot5$AGE, na.rm = TRUE)
      
      FlucTot5$CREAT[FlucTot5$CREAT==-99]= NA 
      FlucTot5$CREAT <- as.numeric(as.character(FlucTot5$CREAT))
      median(FlucTot5$CREAT, na.rm = TRUE)
      FlucTot5$CREAT[is.na(FlucTot5$CREAT)] <- median(FlucTot5$CREAT, na.rm = TRUE)
      
      FlucTot5$CG[FlucTot5$CG==-99]= NA 
      FlucTot5$CG <- as.numeric(as.character(FlucTot5$CG))
      median(FlucTot5$CG, na.rm = TRUE)
      FlucTot5$CG[is.na(FlucTot5$CG)] <- median(FlucTot5$CG, na.rm = TRUE)

      FlucTot5$CKDEPI[FlucTot5$CKDEPI==-99]= NA 
      FlucTot5$CKDEPI <- as.numeric(as.character(FlucTot5$CKDEPI))
      median(FlucTot5$CKDEPI, na.rm = TRUE)
      FlucTot5$CKDEPI[is.na(FlucTot5$CKDEPI)] <- median(FlucTot5$CKDEPI, na.rm = TRUE)
    
      FlucTot5$BILI[FlucTot5$BILI==-99]= NA 
      FlucTot5$BILI <- as.numeric(as.character(FlucTot5$BILI))
      median(FlucTot5$BILI, na.rm = TRUE)
      FlucTot5$BILI[is.na(FlucTot5$BILI)] <- median(FlucTot5$BILI, na.rm = TRUE)
      
      FlucTot5$AST[FlucTot5$AST==-99]= NA 
      FlucTot5$AST <- as.numeric(as.character(FlucTot5$AST))
      median(FlucTot5$AST, na.rm = TRUE)
      FlucTot5$AST[is.na(FlucTot5$AST)] <- median(FlucTot5$AST, na.rm = TRUE)
      
      FlucTot5$GGT[FlucTot5$GGT==-99]= NA 
      FlucTot5$GGT <- as.numeric(as.character(FlucTot5$GGT))
      median(FlucTot5$GGT, na.rm = TRUE)
      FlucTot5$GGT[is.na(FlucTot5$GGT)] <- median(FlucTot5$GGT, na.rm = TRUE)
      
      FlucTot5$FLUID[FlucTot5$FLUID==-99]= NA 
      FlucTot5$FLUID <- as.numeric(as.character(FlucTot5$FLUID))
      median(FlucTot5$FLUID, na.rm = TRUE)
      FlucTot5$FLUID[is.na(FlucTot5$FLUID)] <- median(FlucTot5$FLUID, na.rm = TRUE)
      
      FlucTot5$SOFA[FlucTot5$SOFA==-99]= NA 
      FlucTot5$SOFA <- as.numeric(as.character(FlucTot5$SOFA))
      median(FlucTot5$SOFA, na.rm = TRUE)
      FlucTot5$SOFA[is.na(FlucTot5$SOFA)] <- median(FlucTot5$SOFA, na.rm = TRUE)


#ARC WEERGEVEN (na invullen mediaan)--------------------------------------------------------------------------------- ---------------------------------------------------------------------------------      
      FlucTot5$ARCCKD <- ifelse((FlucTot5$CKDEPI > 96.5) ,1,0)
      FlucTot5$ARCAlg[FlucTot5$ARC24 == 0 | FlucTot5$ARCCKD == 0] = 0 
      FlucTot5$ARCAlg[FlucTot5$ARC24 == 1 | FlucTot5$ARCCKD == 1] = 1 
      
    
        
#DOSIS TOEVOEGEN ALS COVARIAAT --------------------------------------------------------------------------------- ---------------------------------------------------------------------------------
      #DOSE als covariate in dataset zodat ook bij EVID=0 (sampling) en niet enkel bij EVID=1 (doseerevent)
      #Nieuwe kolom maken
      FlucTot5$DOSE = FlucTot5$AMT
      #DOSE numeriek zetten en dan '.' naar NA
      FlucTot5$DOSE <- as.numeric(as.character(FlucTot5$DOSE))
      #Dosissen doortrekken naar onder zodat er op elke rij een dosis staat (anders heb je de spiegels die niet direct na een dosis afgenonen zijn en waar dus minstens nog 1 spiegel tussen zit niet mee)
      for (i in unique(FlucTot5$ID)){
        FlucTot5$DOSE[FlucTot5$ID==i] = na.locf(FlucTot5$DOSE[FlucTot5$ID==i],na.rm=F, fromLast = F)
      }
      

#ADDITIONAL ROW FOR AUC CALCULATION--------------------------------------------------------------------------------- ---------------------------------------------------------------------------------
  #Je wil een rij vlak voor de volgende dosis dus TAD = TIME volgende dosis - 0,1 --> dan ben je zeker dat laatste moment voor volgende dosis hebt
  #En dan in een volgende stap, wil je de 24u en eventueel 12u AUC bepalen, en dan kan je die lijnen selecteren met EVID=2 en een TAD rond 24u
      #Maak nieuwe dataset
      FlucTot7 = FlucTot5
      #TAD en TIME naar numeriek zetten
      FlucTot7$TAD <- as.numeric(as.character(FlucTot7$TAD))
      FlucTot7$TIME <- as.numeric(as.character(FlucTot7$TIME))
      #Voeg een nieuwe kolom toe om onderscheid te maken in nieuw toegevoegde rijen
      FlucTot7$DUM = 0
      #Voeg een rij toe op het einde van elke OCC
      FlucTot7 <- FlucTot7 %>%
        group_by(ID, OCC) %>%
        group_modify(~ add_row(., EVID=0, MDV=1, DUM=1)) %>% #EVID 0 en MDV 1 for dummy line for calculation en DUM=1 om aan te geven dat over dummy row gaat
        ungroup %>%
        slice(-n())
      #Voeg een rij toe op in het begin van elke ID (want anders is eerste berekening nieuw ID nog van vorige pte)
      FlucTot7 <- FlucTot7 %>%
        group_by(ID) %>%
        group_modify(~ add_row(., .before=0, EVID=0, MDV=1, DUM=2)) %>% #EVID 0 en MDV 1 for dummy line for calculation en DUM=1 om aan te geven dat over dummy row gaat
        ungroup %>%
        slice(-n())
      #TIME berekenen van DUM=1
          #Nieuwe kolom toevoegen TIME2 met TIME van dosering (EVID=1) en dat doortrekken naar BOVEN --> TIME2 = TIME van VOLGENDE dosis
          FlucTot7$TIME2 = NA
          FlucTot7$TIME2[FlucTot7$EVID==1] = FlucTot7$TIME[FlucTot7$EVID==1] 
          for (i in unique(FlucTot7$ID)){
            FlucTot7$TIME2[FlucTot7$ID==i] = na.locf(FlucTot7$TIME2[FlucTot7$ID==i],na.rm=F, fromLast = T)
          }
          #TIME bij DUM1 is nu nog leeg ==> TIME bij DUM1 invullen door TIME2 (= TIME van volgende toediening) - 0.01
          FlucTot7$TIME<- ifelse(FlucTot7$DUM==1, (FlucTot7$TIME2 - 0.01), FlucTot7$TIME) #als DUM=1 (nieuwe rij), dan TIME=TIME2-0.01, anders TIME=TIME
          #TIME bij DUM2 is nu nog leeg ==> TIME bij DUM2 = 0
          FlucTot7$TIME[FlucTot7$DUM==2] = 0
          FlucTot7$OCC[FlucTot7$DUM==2] = 0
          #De laatste DUM nieuwe rij is nu nog over van elke pte 
              #Nieuwe kolom toevoegen TIME3 met TIME van dosering en dat doortrekken naar ONDER --> TIME3 = TIME van VORIGE dosis (al toegediend)
              FlucTot7$TIME3 = NA
              FlucTot7$TIME3[FlucTot7$EVID==1] = FlucTot7$TIME[FlucTot7$EVID==1] 
              for (i in unique(FlucTot7$OCC)){
                FlucTot7$TIME3[FlucTot7$OCC==i] = na.locf(FlucTot7$TIME3[FlucTot7$OCC==i],na.rm=F, fromLast = F)
              }
              #TIME moet de TIME zijn van de laatste dosis (TIME3) + 24 (geen dosis erna meer dus kan kiezen wanneer)
              FlucTot7$TIME3[FlucTot7$DUM==2] = 0
              FlucTot7$TIME <- ifelse(is.na(FlucTot7$TIME), (FlucTot7$TIME3 + 24), FlucTot7$TIME)
        #TAD berekenen van DUM=1 --> verschil tussen TIME (moment DUM row) and TIME vorige dosis (TIME3)
          FlucTot7$TAD <- ifelse(FlucTot7$DUM==1, (FlucTot7$TIME - FlucTot7$TIME3), FlucTot7$TAD)
          FlucTot7$TAD[FlucTot7$EVID==1] <- "."
        #TAD berekenen van DUM=0 --> 0
          FlucTot7$TAD[FlucTot7$DUM==2] <- "."
        #Overige data in deze rijen invullen
          FlucTot7$AMT[FlucTot7$DUM==1 | FlucTot7$DUM==2] <- "."
          FlucTot7$RATE[FlucTot7$DUM==1| FlucTot7$DUM==2] <- "."
          FlucTot7$DV[FlucTot7$DUM==1| FlucTot7$DUM==2] <- "."
          FlucTot7$DV[FlucTot7$EVID==1| FlucTot7$DUM==2] <- "."
          FlucTot7$LOGDV[FlucTot7$DUM==1| FlucTot7$DUM==2] <- "."
          FlucTot7$LOGDV[FlucTot7$EVID==1| FlucTot7$DUM==2] <- "."
          FlucTot7$CMT[FlucTot7$DUM==1| FlucTot7$DUM==2] <- 2
          FlucTot7$SS[FlucTot7$DUM==1| FlucTot7$DUM==2] <- 0
          FlucTot7$II[FlucTot7$DUM==1| FlucTot7$DUM==2] <- 0
          
          for (i in unique(FlucTot7$ID)){
            FlucTot7$OCC3[FlucTot7$ID==i] = na.locf(FlucTot7$OCC3[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$OCC[FlucTot7$ID==i] = na.locf(FlucTot7$OCC[FlucTot7$ID==i],na.rm=F, fromLast = F)
          }

          for (i in unique(FlucTot7$ID)){
           FlucTot7$ADMIN[FlucTot7$ID==i] = na.locf(FlucTot7$ADMIN[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$HOSPITAL[FlucTot7$ID==i] = na.locf(FlucTot7$HOSPITAL[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$APACHE[FlucTot7$ID==i] = na.locf(FlucTot7$APACHE[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$BW[FlucTot7$ID==i] = na.locf(FlucTot7$BW[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$LENGHT[FlucTot7$ID==i] = na.locf(FlucTot7$LENGHT[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$AGE[FlucTot7$ID==i] = na.locf(FlucTot7$AGE[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$SEX[FlucTot7$ID==i] = na.locf(FlucTot7$SEX[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$RACE[FlucTot7$ID==i] = na.locf(FlucTot7$RACE[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$CREAT[FlucTot7$ID==i] = na.locf(FlucTot7$CREAT[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$CKDEPI[FlucTot7$ID==i] = na.locf(FlucTot7$CKDEPI[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$CL24[FlucTot7$ID==i] = na.locf(FlucTot7$CL24[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$GGT[FlucTot7$ID==i] = na.locf(FlucTot7$GGT[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$AFT[FlucTot7$ID==i] = na.locf(FlucTot7$AFT[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$AST[FlucTot7$ID==i] = na.locf(FlucTot7$AST[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$ALT[FlucTot7$ID==i] = na.locf(FlucTot7$ALT[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$BILI[FlucTot7$ID==i] = na.locf(FlucTot7$BILI[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$ALB[FlucTot7$ID==i] = na.locf(FlucTot7$ALB[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$SOFA[FlucTot7$ID==i] = na.locf(FlucTot7$SOFA[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$CRRT[FlucTot7$ID==i] = na.locf(FlucTot7$CRRT[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$UF[FlucTot7$ID==i] = na.locf(FlucTot7$UF[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$IHD[FlucTot7$ID==i] = na.locf(FlucTot7$IHD[FlucTot7$ID==i],na.rm=F, fromLast = T)
           FlucTot7$ECMO[FlucTot7$ID==i] = na.locf(FlucTot7$ECMO[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$FLUID[FlucTot7$ID==i] = na.locf(FlucTot7$FLUID[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$OCC3[FlucTot7$ID==i] = na.locf(FlucTot7$OCC3[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$ARCCKD[FlucTot7$ID==i] = na.locf(FlucTot7$ARCCKD[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$ARC24[FlucTot7$ID==i] = na.locf(FlucTot7$ARC24[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$ARCAlg[FlucTot7$ID==i] = na.locf(FlucTot7$ARCAlg[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$BMI[FlucTot7$ID==i] = na.locf(FlucTot7$BMI[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$BSA[FlucTot7$ID==i] = na.locf(FlucTot7$BSA[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$CG[FlucTot7$ID==i] = na.locf(FlucTot7$CG[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$BMI[FlucTot7$ID==i] = na.locf(FlucTot7$BMI[FlucTot7$ID==i],na.rm=F, fromLast = T) 
           FlucTot7$DOSE[FlucTot7$ID==i] = na.locf(FlucTot7$DOSE[FlucTot7$ID==i],na.rm=F, fromLast = T) 
          }
          
          for (i in unique(FlucTot7$ID)){
            FlucTot7$ADMIN[FlucTot7$ID==i] = na.locf(FlucTot7$ADMIN[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$HOSPITAL[FlucTot7$ID==i] = na.locf(FlucTot7$HOSPITAL[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$APACHE[FlucTot7$ID==i] = na.locf(FlucTot7$APACHE[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$BW[FlucTot7$ID==i] = na.locf(FlucTot7$BW[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$LENGHT[FlucTot7$ID==i] = na.locf(FlucTot7$LENGHT[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$AGE[FlucTot7$ID==i] = na.locf(FlucTot7$AGE[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$SEX[FlucTot7$ID==i] = na.locf(FlucTot7$SEX[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$RACE[FlucTot7$ID==i] = na.locf(FlucTot7$RACE[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$CREAT[FlucTot7$ID==i] = na.locf(FlucTot7$CREAT[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$CKDEPI[FlucTot7$ID==i] = na.locf(FlucTot7$CKDEPI[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$CL24[FlucTot7$ID==i] = na.locf(FlucTot7$CL24[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$GGT[FlucTot7$ID==i] = na.locf(FlucTot7$GGT[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$AFT[FlucTot7$ID==i] = na.locf(FlucTot7$AFT[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$AST[FlucTot7$ID==i] = na.locf(FlucTot7$AST[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$ALT[FlucTot7$ID==i] = na.locf(FlucTot7$ALT[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$BILI[FlucTot7$ID==i] = na.locf(FlucTot7$BILI[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$ALB[FlucTot7$ID==i] = na.locf(FlucTot7$ALB[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$SOFA[FlucTot7$ID==i] = na.locf(FlucTot7$SOFA[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$CRRT[FlucTot7$ID==i] = na.locf(FlucTot7$CRRT[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$UF[FlucTot7$ID==i] = na.locf(FlucTot7$UF[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$IHD[FlucTot7$ID==i] = na.locf(FlucTot7$IHD[FlucTot7$ID==i],na.rm=F, fromLast = F)
            FlucTot7$ECMO[FlucTot7$ID==i] = na.locf(FlucTot7$ECMO[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$FLUID[FlucTot7$ID==i] = na.locf(FlucTot7$FLUID[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$OCC3[FlucTot7$ID==i] = na.locf(FlucTot7$OCC3[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$ARCCKD[FlucTot7$ID==i] = na.locf(FlucTot7$ARCCKD[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$ARC24[FlucTot7$ID==i] = na.locf(FlucTot7$ARC24[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$ARCAlg[FlucTot7$ID==i] = na.locf(FlucTot7$ARCAlg[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$BMI[FlucTot7$ID==i] = na.locf(FlucTot7$BMI[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$BSA[FlucTot7$ID==i] = na.locf(FlucTot7$BSA[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$CG[FlucTot7$ID==i] = na.locf(FlucTot7$CG[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$BMI[FlucTot7$ID==i] = na.locf(FlucTot7$BMI[FlucTot7$ID==i],na.rm=F, fromLast = F) 
            FlucTot7$DOSE[FlucTot7$ID==i] = na.locf(FlucTot7$DOSE[FlucTot7$ID==i],na.rm=F, fromLast = F) 
          }
          
      #Tijdelijke kolommen verwijderen
      FlucTot7 = FlucTot7[-46]
      FlucTot7 = FlucTot7[-46]
      FlucTot7= arrange(FlucTot7, ID, TIME)
      
      #Maar als je finale model runt met deze dataset zijn de waarden wel ongeveer zelfde maar de GOF is heel vreemd (ik denk omdat hij de dummy rows ziet als staalnames en dat is bij al deze rijen 0)
        #Toch EVID=2 zetten bij DUM=1 en DUM=2 (zoals S. Wicha eerder zei)
        FlucTot7$EVID[FlucTot7$DUM==1 | FlucTot7$DUM==2] <- 2
      
      
#DV staat nu als NA als niet gekend is en moet naar punt --------------------------------------------------------------------------------- ---------------------------------------------------------------------------------
        FlucTot4$DV[is.na(FlucTot4$DV)] <- "." 
        FlucTot5$DV[is.na(FlucTot5$DV)] <- "."
        FlucTot7$DV[is.na(FlucTot7$DV)] <- "."
        FlucTot4$LOGDV[is.na(FlucTot4$LOGDV)] <- "." 
        FlucTot5$LOGDV[is.na(FlucTot5$LOGDV)] <- "."
        FlucTot7$LOGDV[is.na(FlucTot7$LOGDV)] <- "."
        
#Overgebleven NA naar -99 (categorische die NA waren, de continue zijn ingevuld door mediaan)
        FlucTot4$IHD[is.na(FlucTot4$IHD)] <- -99
        FlucTot4$UF[is.na(FlucTot4$UF)] <- -99
        FlucTot5$IHD[is.na(FlucTot5$IHD)] <- -99
        FlucTot5$UF[is.na(FlucTot5$UF)] <- -99
        FlucTot7$IHD[is.na(FlucTot7$IHD)] <- -99
        FlucTot7$UF[is.na(FlucTot7$UF)] <- -99
        
        
#CSV FILE MAKEN van finale databank ------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
      #Enkel IV
          #Dataset IV - Mediaan NIET ingevuld --> voor dataexploratie
          setwd("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Databanken/Totaal")
          FlucTot4 = arrange(FlucTot4, ID, OCC3, TIME)
          write.csv(FlucTot4, "DatabankFluc_Tot_IV_finaal.csv", row.names=F, quote = F)
          
          #Dataset IV - Mediaan WEL ingevuld --> voor Nonmem
          FlucTot6 <- subset(FlucTot5, select=c('ID','OCC3', 'TIME', 'TAD', 'AMT', 'RATE', 'DV','LOGDV','CMT', 'MDV', 'EVID', 'SS', 'II', 'ADMIN', 'HOSPITAL','BW','BMI','BSA','CREAT','CKDEPI', 'CG', 'ARCAlg','SOFA', 'CRRT', 'IHD', 'FLUID', 'BILI', 'AST', 'GGT', 'UF', 'DOSE', 'SEX', 'LENGHT', 'AGE'))
          setwd("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Nonmem/Runs/Runs_220913/Dataset")
          FlucTot6= arrange(FlucTot6, ID,  OCC3, TIME)
          write.csv(FlucTot6, "DatabankFluc_Tot_IV_nonmem_finaal.csv", row.names=F, quote = F)
          
          #Dataset IV - Mediaan WEL ingevuld --> voor Nonmem EN lijn voor AUC calculatie op einde van elk doseerinterval
          FlucTot7 <- subset(FlucTot7, select=c('ID','OCC3', 'TIME', 'TAD', 'AMT', 'RATE', 'DV','LOGDV','CMT', 'MDV', 'EVID', 'SS', 'II', 'ADMIN', 'HOSPITAL','BW','BMI','BSA','CREAT','CKDEPI', 'CG', 'ARCAlg','SOFA', 'CRRT', 'IHD', 'FLUID', 'BILI', 'AST', 'GGT', 'UF', 'DOSE', 'SEX', 'LENGHT', 'AGE', 'DUM', 'OCC'))
          setwd("C:/Users/Ruth/Ruth/FluconazoleMulticentrischPopPK/Nonmem/Simulations/Nonmem SIM/Simulations/Dataset")
          FlucTot7= arrange(FlucTot7, ID, TIME)
          write.csv(FlucTot7, "DatabankFluc_Tot_IV_nonmem_finaal_AUCTAD.csv", row.names=F, quote = F)
    
     
     
   