#---- Preprocess matching covariates function ----
# author: Louise Glew, louise.glew@gmail.com
# modified: --

process_covariates <- 
  function(HH.data, DE.data,t0.t2.pairs, t0.t4.pairs) {
 
#---- Import look up tables ----
    
    ethnic.lkp<- read.delim("x_Flat_data_files/1_Social/Inputs/BHS/eth_output_kc_2017_1217.txt")
    education.lkp <- read.delim("x_Flat_data_files/1_Social/Inputs/BHS/education_lkp.txt")
    
# ---Create functions
    # Function to remove all white space in string variables
    trim <- function(x) gsub("^\\s+|\\s+$","",x)
    
    # Function to clean string variables (lower case, remove punctuation)
    str_clean <- function(strings) {
      require(dplyr)
      require(tm)
      strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = FALSE) %>% stripWhitespace() %>% 
        trim()
    }
    

#----Define function
    
    # Age
    age.bin<-c(0,20,30,40,50,60,70,990)
    
    HH.age<-subset(DE.data, select=c("HouseholdID","DemographicCode","IndividualAge"), DemographicCode==1)
    HH.monitoring.year <-subset(HH.data, select=c("HouseholdID","MonitoringYear"))
    HH.age <-left_join(HH.age,(subset(HH.data, select=c("HouseholdID","MonitoringYear"))),by="HouseholdID")
    HH.age$IndividualAge[HH.age$IndividualAge >= 990] <- 990 #recode blind values
    
    t0.age <- subset(HH.age, MonitoringYear=="t0")
    t2.age <- subset(HH.age, MonitoringYear=="t2")
    t4.age <- subset(HH.age, MonitoringYear=="t4")
    
    t0.age$IndividualAge <- .bincode(t0.age$IndividualAge,age.bin,TRUE,TRUE)
    t2.age$IndividualAge <- .bincode((t2.age$IndividualAge-2),age.bin,TRUE,TRUE)
    t4.age$IndividualAge <- .bincode((t4.age$IndividualAge-4),age.bin,TRUE,TRUE)
    
    HH.age <- rbind(t0.age, t2.age, t4.age)
    HH.age <-unique(subset(HH.age, select=c("HouseholdID","IndividualAge")))
    
    rm(t0.age,t2.age,t4.age,HH.monitoring.year,age.bin)

    
    # Gender of Household Head
    gender.HHH <- unique(subset(DE.data, select=c("HouseholdID","IndividualGender"), DemographicCode==1))
    
    # Residency
    resident.bin<-c(0,10,20,30,40,50,60,990)

    HH.residency<-subset(HH.data,select=c("HouseholdID","YearsResident", "MonitoringYear"))
    HH.residency$YearsResident[HH.residency$YearsResident >= 990] <- 990

    t0.residency <- subset(HH.residency, MonitoringYear=="t0")
    t2.residency <- subset(HH.residency, MonitoringYear=="t2")
    t4.residency <- subset(HH.residency, MonitoringYear=="t4")
    
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
      summarise(freq.eth=n()) %>%
      top_n(1, freq.eth)
    
    HH.eth <-left_join(HH.eth,max.eth, by=c("SettlementID" = "SettlementID", "MonitoringYear"="MonitoringYear"))
    HH.eth$dom.eth <- ifelse(HH.eth$eth.iso.x==HH.eth$eth.iso.y,1,0)
    HH.eth <-subset(HH.eth, select=c("HouseholdID","dom.eth"))
    x <-HH.eth %>%  #quick bodge to get rid of duplicates where ethnicities tied. 
      group_by(HouseholdID)%>%
      top_n(1,dom.eth)
    
    HH.eth <-unique(x)
    
    rm(max.eth,x)

    # Education level of household head
    HH.ed <- subset(DE.data, select=c("HouseholdID","IndividualEducation"), DemographicCode==1)
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
    MPA <-left_join((subset (HH.data, select=c("HouseholdID", "MPAID","SettlementID"))),(subset(SE.data, select=c("SettlementID","Treatment"))),by="SettlementID")
    
    # Longitudinal pairs
    t0.t2.pairs <-melt(t0.t2.matched, id.vars="match.id",measure.vars=c("HouseholdID.t0","HouseholdID.t2"))
    t0.t2.pairs<-subset(t0.t2.pairs,select=c("match.id","value"))
    colnames(t0.t2.pairs) <-c("t0.t2.pair","HouseholdID")
    
    t0.t4.pairs <-melt(t0.t4.matched, id.vars="match.id",measure.vars=c("HouseholdID.t0","HouseholdID.t4"))
    t0.t4.pairs<-subset(t0.t4.pairs,select=c("match.id","value"))
    colnames(t0.t4.pairs) <-c("t0.t4.pair","HouseholdID")
    
    
    #Compile match covariate
    match.covariate <-list(MPA,market.distance,N.Child,HH.ed, HH.eth,HH.residency,gender.HHH, HH.age,t0.t2.pairs,t0.t4.pairs) %>%
      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="HouseholdID"), .)
    
match.covariate$t0.t2.pair[is.na(match.covariate$t0.t2.pair)]<-99999
match.covariate$t0.t4.pair[is.na(match.covariate$t0.t4.pair)]<-99999
    
rm(MPA,market.distance,N.Child,HH.ed, HH.eth,HH.residency,gender.HHH, HH.age)

return (match.covariate)
  }
#rm()