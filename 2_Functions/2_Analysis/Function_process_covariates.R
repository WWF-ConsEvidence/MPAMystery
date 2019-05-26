#---- Preprocess matching covariates function ----
# author: Louise Glew, louise.glew@gmail.com
# modified: --

process_covariates <- 
  function(HH.data, DE.data) {
 
#---- Import look up tables ----
    
    ethnic.lkp<- import("x_Flat_data_files/1_Social/Inputs/master_ethnic_lookup_2017_117.xlsx")
    education.lkp <- import("x_Flat_data_files/1_Social/Inputs/education_lkp_BHS.xlsx")
    
# ---Create functions
    # Function to remove all white space in string variables
    trim <- function(x) gsub("^\\s+|\\s+$","",x)
    
    # Function to clean string variables (lower case, remove punctuation)
    str_clean <- function(strings) {
      require(dplyr)
      require(tm)
      strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
        trim()
    }
    

#----Define function
    
    # Age
    age.bin<-c(0,20,30,40,50,60,70,990)
    
    HH.age<-subset(DE.data, select=c("HouseholdID","RelationHHH","IndividualAge"), RelationHHH==0)
    HH.monitoring.year <-subset(HH.data, select=c("HouseholdID","MonitoringYear"))
    HH.age <-left_join(HH.age,(subset(HH.data, select=c("HouseholdID","MonitoringYear"))),by="HouseholdID")
    HH.age$IndividualAge[HH.age$IndividualAge >= 990] <- 990 #recode blind values
    
    t0.age <- subset(HH.age, MonitoringYear=="Baseline")
    t2.age <- subset(HH.age, MonitoringYear=="2 Year Post")
    t4.age <- subset(HH.age, MonitoringYear=="4 Year Post")
    
    t0.age$IndividualAge <- .bincode(t0.age$IndividualAge,age.bin,TRUE,TRUE)
    t2.age$IndividualAge <- .bincode((t2.age$IndividualAge-2),age.bin,TRUE,TRUE)
    t4.age$IndividualAge <- .bincode((t4.age$IndividualAge-4),age.bin,TRUE,TRUE)
    
    HH.age <- rbind(t0.age, t2.age, t4.age)
    HH.age <-unique(subset(HH.age, select=c("HouseholdID","IndividualAge")))
    
    rm(t0.age,t2.age,t4.age,HH.monitoring.year,age.bin)

    
    # Gender of Household Head
    gender.HHH <- unique(subset(DE.data, select=c("HouseholdID","IndividualGender"), RelationHHH==0))

    # Residency
    resident.bin<-c(0,10,20,30,40,50,60,990)

    HH.residency<-subset(HH.data,select=c("HouseholdID","YrResident", "MonitoringYear")) %>% transmute(HouseholdID=HouseholdID,
                                                                                                       YearsResident=YrResident,
                                                                                                       MonitoringYear=MonitoringYear)
    HH.residency$YearsResident[HH.residency$YearsResident >= 990] <- 990

    t0.residency <- subset(HH.residency, MonitoringYear=="Baseline")
    t2.residency <- subset(HH.residency, MonitoringYear=="2 Year Post")
    t4.residency <- subset(HH.residency, MonitoringYear=="4 Year Post")
    
    t0.residency$YearsResident <- .bincode(t0.residency$YearsResident,resident.bin,TRUE,TRUE)
    t2.residency$YearsResident <- ifelse(t2.residency$YearsResident>2,(.bincode((t2.residency$YearsResident-2),resident.bin,TRUE,TRUE)),1)
    t4.residency$YearsResident <- ifelse(t4.residency$YearsResident>4,(.bincode((t4.residency$YearsResident-4),resident.bin,TRUE,TRUE)),1)
   
    HH.residency <-rbind(t0.residency,t2.residency,t4.residency)
    HH.residency <- na.omit(HH.residency)
    HH.residency$MonitoringYear<-NULL

    rm(resident.bin, t0.residency, t2.residency,t4.residency)
    
    
    # Dominant ethnicity
    
    HH.eth <- subset(HH.data, select=c("HouseholdID","PaternalEthnicity", "MonitoringYear", "SettlementID"))
    HH.eth$PaternalEthnicity <-str_clean(HH.eth$PaternalEthnicity)
    HH.eth<- left_join(HH.eth,ethnic.lkp, by=c("PaternalEthnicity"="std.eth.str"))

    max.eth <- HH.eth %>%
      group_by(MonitoringYear,SettlementID,eth.iso)%>%
      dplyr::summarise(freq.eth=n()) %>%
      top_n(1, freq.eth)
    
    HH.eth <-left_join(HH.eth,max.eth, by=c("SettlementID" = "SettlementID", "MonitoringYear"="MonitoringYear"))
    HH.eth$dom.eth <- ifelse(HH.eth$eth.iso.x==HH.eth$eth.iso.y,1,0)
    HH.eth <-subset(HH.eth, select=c("HouseholdID","dom.eth"))
    x <-HH.eth %>%  #quick bodge to get rid of duplicates where ethnicities tied. 
      group_by(HouseholdID) %>%
      top_n(1,dom.eth)
    
    HH.eth <-unique(x)
    
    rm(max.eth,x)

    # Education level of household head
    HH.ed <- subset(DE.data, select=c("HouseholdID","IndividualEducation"), RelationHHH==0)
    HH.ed <- left_join(HH.ed, education.lkp, by=c("IndividualEducation"))
    HH.ed$IndividualEducation <-NULL
 
    # Children in Household
    DE.data$Child<-ifelse(DE.data$IndividualAge<19,1,0) #  create new variable, child/adult
    N.Child<-DE.data%>%
      group_by(HouseholdID) %>% 
      summarise(n.child=sum(Child))
    
    # Market distance
    market.distance<-subset(HH.data,select=c("HouseholdID","TimeMarket", "MonitoringYear","SettlementID"))
    market.distance$TimeMarket[market.distance$TimeMarket >=990] <- 990

    market.mean <-market.distance %>% 
      group_by(SettlementID,MonitoringYear)%>%
      summarise (mean=mean(TimeMarket[TimeMarket!=990])) # subsequent rows handle blind codes, and missing data
    
    market.mean$mean[is.na(market.mean$mean)]<- ave(market.mean$mean, 
                                     market.mean$SettlementID, 
                                     FUN=function(x)mean(x,na.rm = T))[is.na(market.mean$mean)] 
    
    impute.market <- filter(market.distance,TimeMarket==990)
    impute.market <-inner_join(subset(impute.market, select=c("HouseholdID","MonitoringYear", "SettlementID")),market.mean, by=c("MonitoringYear", "SettlementID"))
    colnames(impute.market) <-c("HouseholdID","MonitoringYear", "SettlementID", "TimeMarket")
    market.distance <-rbind((subset(market.distance, TimeMarket!=990)),impute.market)
    
    rm(market.mean, impute.market)
    
    # Site and treatment
    MPA <-left_join((subset (HH.data, select=c("HouseholdID", "MPAID","SettlementID","MonitoringYear"))),(subset(SE.data, select=c("SettlementID","Treatment"))),by="SettlementID")
    
    #Compile match covariate
    match.covariate <-
      left_join(MPA,market.distance[,c("HouseholdID","TimeMarket")],by="HouseholdID") %>%
      left_join(N.Child,by="HouseholdID") %>%
      left_join(HH.ed,by="HouseholdID") %>%
      left_join(HH.eth,by="HouseholdID") %>%
      left_join(HH.residency,by="HouseholdID") %>%
      left_join(gender.HHH,by="HouseholdID") %>%
      left_join(HH.age,by="HouseholdID") %>%
      .[!duplicated(.),]
      
    
rm(MPA,market.distance,N.Child,HH.ed, HH.eth,HH.residency,gender.HHH, HH.age)




    covariate.means <- 
      match.covariate %>%
      group_by(SettlementID,MPAID,MonitoringYear) %>%
      summarise(mean.age=mean(IndividualAge,na.rm=T),
                mean.year.res=mean(YearsResident,na.rm=T),
                mean.time.market=mean(TimeMarket,na.rm=T)) %>%
      mutate(mean.time.market=ifelse(MPAID==1 & MonitoringYear=="Baseline",
                                     mean.time.market[MPAID==1 & MonitoringYear=="2 Year Post"],
                                     ifelse(MPAID==2 & MonitoringYear=="Baseline",
                                            mean.time.market[MPAID==2 & MonitoringYear=="2 Year Post"],
                                            mean.time.market)))
    
    match.covariate <-
      left_join(match.covariate,covariate.means,by=c("SettlementID","MPAID","MonitoringYear")) %>%
      transmute(HouseholdID=HouseholdID,
                MPAID=MPAID,
                SettlementID=SettlementID,
                MonitoringYear=MonitoringYear,
                Treatment=Treatment,
                TimeMarket=ifelse(is.na(TimeMarket),
                                  mean.time.market,
                                  as.numeric(TimeMarket)),
                n.child=ifelse(is.na(n.child),
                               0,as.numeric(n.child)),
                ed.level=ifelse(is.na(ed.level),
                                990,
                                as.numeric(ed.level)),
                dom.eth=dom.eth,
                YearsResident=ifelse(is.na(YearsResident),
                                     mean.year.res,
                                     as.numeric(YearsResident)),
                IndividualGender=IndividualGender,
                IndividualAge=ifelse(is.na(IndividualAge),
                                     mean.age,
                                     as.numeric(IndividualAge)))
    
    
return (match.covariate)
  }
#rm()

