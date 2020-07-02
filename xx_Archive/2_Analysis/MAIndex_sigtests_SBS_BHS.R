pacman::p_load(Kendall,rio)

HHData$MAIndex <- ifelse(HHData$RemoveMA=="No",
                         rowSums(HHData[,c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                                           "BoatInboard", "PhoneCombined", "TV", "Entertain", "Satellite", 
                                           "Generator")],
                                 na.rm=TRUE),
                         NA)

HHData$MAIndex.noTV <- ifelse(HHData$RemoveMA=="No",
                              rowSums(HHData[,c("CarTruck", "Bicycle", "Motorcycle", "BoatNoMotor", "BoatOutboard", 
                                                "BoatInboard", "PhoneCombined", "Entertain", "Satellite", 
                                                "Generator")],
                                      na.rm=TRUE),
                              NA)


Sett.Means <-
  HHData %>%
  group_by(SettlementID,SettlementName,MPAID,MonitoringYear,Treatment) %>%
  summarise(MAIndex=mean(MAIndex,na.rm=T),
            MAIndex.noTV=mean(MAIndex.noTV,na.rm=T))
  

# ---- FLOTIM ----
Flotim.Sett.Means <- Sett.Means %>% filter(MPAID==16 & MonitoringYear=="3 Year Post")
Flotim.HHData <- HHData %>% filter(MPAID==16 & MonitoringYear=="3 Year Post")
Flotim.sett.names <- factor(Flotim.Sett.Means$SettlementName)
Flotim.Trend.HHData <- HHData %>% filter(MPAID==16)

Flotim.even.number.setts.function <- 
  mapply(a=Flotim.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Flotim.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Flotim.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Flotim.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Flotim.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Flotim.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Flotim.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Flotim.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Flotim.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Flotim.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Flotim.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Flotim.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Flotim.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Flotim.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Flotim.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Flotim.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Flotim.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Flotim.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Flotim.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Flotim.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Flotim.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Flotim.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Flotim.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Flotim.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Flotim.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Flotim.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Flotim.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Flotim.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Flotim.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Flotim.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Flotim.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Flotim.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Flotim.median.setts <- 
  mapply(i=Flotim.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Flotim.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Flotim.sett.names)%%2!=0,
                                      as.character(Flotim.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Flotim.even.number.setts.function[j])),
                               levels=levels(Flotim.sett.names))})


Flotim.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Flotim.sett.names[which(Flotim.sett.names!=Flotim.median.setts[a])]),
                                                               as.character(Flotim.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Flotim.sett.names[which(Flotim.sett.names!=Flotim.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Flotim.HHData[Flotim.HHData$SettlementName==i | Flotim.HHData$SettlementName==Flotim.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Flotim.HHData[Flotim.HHData$SettlementName==i | Flotim.HHData$SettlementName==Flotim.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Flotim.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Flotim.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Flotim.Trend.HHData$InterviewYear==unique(Flotim.Trend.HHData$InterviewYear)[1]],
                                    i[Flotim.Trend.HHData$InterviewYear==unique(Flotim.Trend.HHData$InterviewYear)[2]]))
                    }))


Flotim.sigvals.Sett <- 
  cbind.data.frame(Flotim.non.parametric.test.settlements[order(Flotim.non.parametric.test.settlements$MAIndex.SettlementName),
                                                   c("MAIndex.SettlementName","MAIndex.p.value")],
                   Flotim.non.parametric.test.settlements[order(Flotim.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                   c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)

# ---- TELMA ----
Telma.Sett.Means <- Sett.Means %>% filter(MPAID==1 & MonitoringYear=="4 Year Post")
Telma.HHData <- HHData %>% filter(MPAID==1 & MonitoringYear=="4 Year Post")
Telma.sett.names <- factor(Telma.Sett.Means$SettlementName)
Telma.Trend.HHData <- HHData %>% filter(MPAID==1)

Telma.even.number.setts.function <- 
  mapply(a=Telma.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Telma.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Telma.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Telma.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Telma.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Telma.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Telma.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Telma.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Telma.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Telma.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Telma.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Telma.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Telma.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Telma.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Telma.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Telma.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Telma.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Telma.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Telma.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Telma.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Telma.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Telma.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Telma.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Telma.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Telma.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Telma.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Telma.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Telma.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Telma.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Telma.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Telma.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Telma.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Telma.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Telma.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Telma.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Telma.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Telma.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Telma.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Telma.median.setts <- 
  mapply(i=Telma.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Telma.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Telma.sett.names)%%2!=0,
                                      as.character(Telma.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Telma.even.number.setts.function[j])),
                               levels=levels(Telma.sett.names))})


Telma.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Telma.sett.names[which(Telma.sett.names!=Telma.median.setts[a])]),
                                                               as.character(Telma.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Telma.sett.names[which(Telma.sett.names!=Telma.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Telma.HHData[Telma.HHData$SettlementName==i | Telma.HHData$SettlementName==Telma.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Telma.HHData[Telma.HHData$SettlementName==i | Telma.HHData$SettlementName==Telma.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))

Telma.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Telma.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Telma.Trend.HHData$InterviewYear==unique(Telma.Trend.HHData$InterviewYear)[1]],
                                    i[Telma.Trend.HHData$InterviewYear==unique(Telma.Trend.HHData$InterviewYear)[2]]))
                    }))


Telma.sigvals.Sett <- 
  cbind.data.frame(Telma.non.parametric.test.settlements[order(Telma.non.parametric.test.settlements$MAIndex.SettlementName),
                                                          c("MAIndex.SettlementName","MAIndex.p.value")],
                   Telma.non.parametric.test.settlements[order(Telma.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                          c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)


# ---- TNTC ----
TNTC.Sett.Means <- Sett.Means %>% filter(MPAID==2 & MonitoringYear=="4 Year Post")
TNTC.HHData <- HHData %>% filter(MPAID==2 & MonitoringYear=="4 Year Post")
TNTC.sett.names <- factor(TNTC.Sett.Means$SettlementName)
TNTC.Trend.HHData <- HHData %>% filter(MPAID==2)


TNTC.even.number.setts.function <- 
  mapply(a=TNTC.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=TNTC.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- TNTC.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[TNTC.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[TNTC.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[TNTC.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[TNTC.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[TNTC.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- TNTC.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[TNTC.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[TNTC.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[TNTC.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[TNTC.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[TNTC.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[TNTC.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[TNTC.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[TNTC.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- TNTC.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[TNTC.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[TNTC.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[TNTC.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[TNTC.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[TNTC.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[TNTC.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[TNTC.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[TNTC.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[TNTC.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[TNTC.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[TNTC.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[TNTC.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[TNTC.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

TNTC.median.setts <- 
  mapply(i=TNTC.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(TNTC.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(TNTC.sett.names)%%2!=0,
                                      as.character(TNTC.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(TNTC.even.number.setts.function[j])),
                               levels=levels(TNTC.sett.names))})


TNTC.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(TNTC.sett.names[which(TNTC.sett.names!=TNTC.median.setts[a])]),
                                                               as.character(TNTC.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=TNTC.sett.names[which(TNTC.sett.names!=TNTC.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       TNTC.HHData[TNTC.HHData$SettlementName==i | TNTC.HHData$SettlementName==TNTC.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=TNTC.HHData[TNTC.HHData$SettlementName==i | TNTC.HHData$SettlementName==TNTC.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


TNTC.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=TNTC.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[TNTC.Trend.HHData$InterviewYear==unique(TNTC.Trend.HHData$InterviewYear)[1]],
                                    i[TNTC.Trend.HHData$InterviewYear==unique(TNTC.Trend.HHData$InterviewYear)[2]]))
                    }))


TNTC.sigvals.Sett <- 
  cbind.data.frame(TNTC.non.parametric.test.settlements[order(TNTC.non.parametric.test.settlements$MAIndex.SettlementName),
                                                           c("MAIndex.SettlementName","MAIndex.p.value")],
                   TNTC.non.parametric.test.settlements[order(TNTC.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                           c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)



# ---- KAIMANA ----
Kaimana.Sett.Means <- Sett.Means %>% filter(MPAID==3 & MonitoringYear=="4 Year Post")
Kaimana.HHData <- HHData %>% filter(MPAID==3 & MonitoringYear=="4 Year Post")
Kaimana.sett.names <- factor(Kaimana.Sett.Means$SettlementName)
Kaimana.Trend.HHData <- HHData %>% filter(MPAID==3)


Kaimana.even.number.setts.function <- 
  mapply(a=Kaimana.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Kaimana.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kaimana.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Kaimana.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kaimana.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Kaimana.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kaimana.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kaimana.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kaimana.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Kaimana.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kaimana.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kaimana.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kaimana.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Kaimana.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Kaimana.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kaimana.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Kaimana.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kaimana.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kaimana.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kaimana.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Kaimana.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kaimana.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kaimana.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kaimana.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Kaimana.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Kaimana.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Kaimana.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Kaimana.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kaimana.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Kaimana.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Kaimana.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Kaimana.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Kaimana.median.setts <- 
  mapply(i=Kaimana.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Kaimana.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Kaimana.sett.names)%%2!=0,
                                      as.character(Kaimana.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Kaimana.even.number.setts.function[j])),
                               levels=levels(Kaimana.sett.names))})


Kaimana.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Kaimana.sett.names[which(Kaimana.sett.names!=Kaimana.median.setts[a])]),
                                                               as.character(Kaimana.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Kaimana.sett.names[which(Kaimana.sett.names!=Kaimana.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Kaimana.HHData[Kaimana.HHData$SettlementName==i | Kaimana.HHData$SettlementName==Kaimana.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Kaimana.HHData[Kaimana.HHData$SettlementName==i | Kaimana.HHData$SettlementName==Kaimana.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Kaimana.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Kaimana.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Kaimana.Trend.HHData$InterviewYear==unique(Kaimana.Trend.HHData$InterviewYear)[1]],
                                    i[Kaimana.Trend.HHData$InterviewYear==unique(Kaimana.Trend.HHData$InterviewYear)[2]]))
                    }))


Kaimana.sigvals.Sett <- 
  cbind.data.frame(Kaimana.non.parametric.test.settlements[order(Kaimana.non.parametric.test.settlements$MAIndex.SettlementName),
                                                          c("MAIndex.SettlementName","MAIndex.p.value")],
                   Kaimana.non.parametric.test.settlements[order(Kaimana.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                          c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)



# ---- KOFIAU ----
Kofiau.Sett.Means <- Sett.Means %>% filter(MPAID==4 & MonitoringYear=="4 Year Post")
Kofiau.HHData <- HHData %>% filter(MPAID==4 & MonitoringYear=="4 Year Post")
Kofiau.sett.names <- factor(Kofiau.Sett.Means$SettlementName)
Kofiau.Trend.HHData <- HHData %>% filter(MPAID==4)


Kofiau.even.number.setts.function <- 
  mapply(a=Kofiau.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Kofiau.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kofiau.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Kofiau.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kofiau.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Kofiau.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kofiau.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kofiau.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kofiau.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Kofiau.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kofiau.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kofiau.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kofiau.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Kofiau.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Kofiau.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kofiau.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Kofiau.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kofiau.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kofiau.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kofiau.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Kofiau.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kofiau.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kofiau.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kofiau.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Kofiau.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Kofiau.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Kofiau.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Kofiau.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kofiau.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Kofiau.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Kofiau.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Kofiau.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Kofiau.median.setts <- 
  mapply(i=Kofiau.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Kofiau.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Kofiau.sett.names)%%2!=0,
                                      as.character(Kofiau.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Kofiau.even.number.setts.function[j])),
                               levels=levels(Kofiau.sett.names))})


Kofiau.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Kofiau.sett.names[which(Kofiau.sett.names!=Kofiau.median.setts[a])]),
                                                               as.character(Kofiau.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Kofiau.sett.names[which(Kofiau.sett.names!=Kofiau.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Kofiau.HHData[Kofiau.HHData$SettlementName==i | Kofiau.HHData$SettlementName==Kofiau.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Kofiau.HHData[Kofiau.HHData$SettlementName==i | Kofiau.HHData$SettlementName==Kofiau.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Kofiau.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Kofiau.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Kofiau.Trend.HHData$InterviewYear==unique(Kofiau.Trend.HHData$InterviewYear)[1]],
                                    i[Kofiau.Trend.HHData$InterviewYear==unique(Kofiau.Trend.HHData$InterviewYear)[2]]))
                    }))


Kofiau.sigvals.Sett <- 
  cbind.data.frame(Kofiau.non.parametric.test.settlements[order(Kofiau.non.parametric.test.settlements$MAIndex.SettlementName),
                                                           c("MAIndex.SettlementName","MAIndex.p.value")],
                   Kofiau.non.parametric.test.settlements[order(Kofiau.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                           c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)



# ---- DAMPIER ----
Dampier.Sett.Means <- Sett.Means %>% filter(MPAID==5 & MonitoringYear=="4 Year Post")
Dampier.HHData <- HHData %>% filter(MPAID==5 & MonitoringYear=="4 Year Post")
Dampier.sett.names <- factor(Dampier.Sett.Means$SettlementName)
Dampier.Trend.HHData <- HHData %>% filter(MPAID==5)


Dampier.even.number.setts.function <- 
  mapply(a=Dampier.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Dampier.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Dampier.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Dampier.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Dampier.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Dampier.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Dampier.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Dampier.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Dampier.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Dampier.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Dampier.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Dampier.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Dampier.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Dampier.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Dampier.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Dampier.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Dampier.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Dampier.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Dampier.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Dampier.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Dampier.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Dampier.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Dampier.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Dampier.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Dampier.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Dampier.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Dampier.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Dampier.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Dampier.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Dampier.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Dampier.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Dampier.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Dampier.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Dampier.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Dampier.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Dampier.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Dampier.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Dampier.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Dampier.median.setts <- 
  mapply(i=Dampier.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Dampier.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Dampier.sett.names)%%2!=0,
                                      as.character(Dampier.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Dampier.even.number.setts.function[j])),
                               levels=levels(Dampier.sett.names))})


Dampier.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Dampier.sett.names[which(Dampier.sett.names!=Dampier.median.setts[a])]),
                                                               as.character(Dampier.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Dampier.sett.names[which(Dampier.sett.names!=Dampier.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Dampier.HHData[Dampier.HHData$SettlementName==i | Dampier.HHData$SettlementName==Dampier.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Dampier.HHData[Dampier.HHData$SettlementName==i | Dampier.HHData$SettlementName==Dampier.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Dampier.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Dampier.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Dampier.Trend.HHData$InterviewYear==unique(Dampier.Trend.HHData$InterviewYear)[1]],
                                    i[Dampier.Trend.HHData$InterviewYear==unique(Dampier.Trend.HHData$InterviewYear)[2]]))
                    }))


Dampier.sigvals.Sett <- 
  cbind.data.frame(Dampier.non.parametric.test.settlements[order(Dampier.non.parametric.test.settlements$MAIndex.SettlementName),
                                                         c("MAIndex.SettlementName","MAIndex.p.value")],
                   Dampier.non.parametric.test.settlements[order(Dampier.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                         c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)




# ---- MISOOL ----
Misool.Sett.Means <- Sett.Means %>% filter(MPAID==6 & MonitoringYear=="4 Year Post")
Misool.HHData <- HHData %>% filter(MPAID==6 & MonitoringYear=="4 Year Post")
Misool.sett.names <- factor(Misool.Sett.Means$SettlementName)
Misool.Trend.HHData <- HHData %>% filter(MPAID==6)


Misool.even.number.setts.function <- 
  mapply(a=Misool.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Misool.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Misool.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Misool.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Misool.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Misool.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Misool.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Misool.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Misool.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Misool.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Misool.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Misool.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Misool.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Misool.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Misool.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Misool.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Misool.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Misool.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Misool.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Misool.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Misool.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Misool.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Misool.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Misool.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Misool.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Misool.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Misool.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Misool.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Misool.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Misool.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Misool.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Misool.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Misool.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Misool.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Misool.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Misool.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Misool.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Misool.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Misool.median.setts <- 
  mapply(i=Misool.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Misool.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Misool.sett.names)%%2!=0,
                                      as.character(Misool.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Misool.even.number.setts.function[j])),
                               levels=levels(Misool.sett.names))})


Misool.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Misool.sett.names[which(Misool.sett.names!=Misool.median.setts[a])]),
                                                               as.character(Misool.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Misool.sett.names[which(Misool.sett.names!=Misool.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Misool.HHData[Misool.HHData$SettlementName==i | Misool.HHData$SettlementName==Misool.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Misool.HHData[Misool.HHData$SettlementName==i | Misool.HHData$SettlementName==Misool.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Misool.trend.non.parametric.test.byMPA  <- 
  data.frame(mapply(i=Misool.Trend.HHData[,c("MAIndex","MAIndex.noTV")],
                    function(i){
                      MannKendall(c(i[Misool.Trend.HHData$InterviewYear==unique(Misool.Trend.HHData$InterviewYear)[1]],
                                    i[Misool.Trend.HHData$InterviewYear==unique(Misool.Trend.HHData$InterviewYear)[2]]))
                    }))


Misool.sigvals.Sett <- 
  cbind.data.frame(Misool.non.parametric.test.settlements[order(Misool.non.parametric.test.settlements$MAIndex.SettlementName),
                                                           c("MAIndex.SettlementName","MAIndex.p.value")],
                   Misool.non.parametric.test.settlements[order(Misool.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                           c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)


# ---- ALOR ----
Alor.Sett.Means <- Sett.Means %>% filter(MPAID==15 & MonitoringYear=="Baseline")
Alor.HHData <- HHData %>% filter(MPAID==15 & MonitoringYear=="Baseline")
Alor.sett.names <- factor(Alor.Sett.Means$SettlementName)

Alor.even.number.setts.function <- 
  mapply(a=Alor.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Alor.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Alor.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Alor.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Alor.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Alor.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Alor.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Alor.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Alor.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Alor.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Alor.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Alor.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Alor.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Alor.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Alor.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Alor.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Alor.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Alor.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Alor.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Alor.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Alor.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Alor.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Alor.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Alor.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Alor.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Alor.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Alor.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Alor.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Alor.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Alor.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Alor.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Alor.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Alor.median.setts <- 
  mapply(i=Alor.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Alor.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Alor.sett.names)%%2!=0,
                                      as.character(Alor.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Alor.even.number.setts.function[j])),
                               levels=levels(Alor.sett.names))})


Alor.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Alor.sett.names[which(Alor.sett.names!=Alor.median.setts[a])]),
                                                               as.character(Alor.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Alor.sett.names[which(Alor.sett.names!=Alor.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Alor.HHData[Alor.HHData$SettlementName==i | Alor.HHData$SettlementName==Alor.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Alor.HHData[Alor.HHData$SettlementName==i | Alor.HHData$SettlementName==Alor.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Alor.sigvals.Sett <- 
  cbind.data.frame(Alor.non.parametric.test.settlements[order(Alor.non.parametric.test.settlements$MAIndex.SettlementName),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   Alor.non.parametric.test.settlements[order(Alor.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                        c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)



# ---- KOON ----
Koon.Sett.Means <- Sett.Means %>% filter(MPAID==18 & MonitoringYear=="Baseline")
Koon.HHData <- HHData %>% filter(MPAID==18 & MonitoringYear=="Baseline")
Koon.sett.names <- factor(Koon.Sett.Means$SettlementName)

Koon.even.number.setts.function <- 
  mapply(a=Koon.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Koon.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Koon.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Koon.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Koon.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Koon.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Koon.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Koon.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Koon.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Koon.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Koon.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Koon.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Koon.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Koon.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Koon.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Koon.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Koon.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Koon.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Koon.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Koon.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Koon.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Koon.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Koon.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Koon.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Koon.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Koon.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Koon.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Koon.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Koon.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Koon.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Koon.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Koon.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Koon.median.setts <- 
  mapply(i=Koon.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Koon.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Koon.sett.names)%%2!=0,
                                      as.character(Koon.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Koon.even.number.setts.function[j])),
                               levels=levels(Koon.sett.names))})


Koon.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Koon.sett.names[which(Koon.sett.names!=Koon.median.setts[a])]),
                                                               as.character(Koon.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Koon.sett.names[which(Koon.sett.names!=Koon.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Koon.HHData[Koon.HHData$SettlementName==i | Koon.HHData$SettlementName==Koon.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Koon.HHData[Koon.HHData$SettlementName==i | Koon.HHData$SettlementName==Koon.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Koon.sigvals.Sett <- 
  cbind.data.frame(Koon.non.parametric.test.settlements[order(Koon.non.parametric.test.settlements$MAIndex.SettlementName),
                                                          c("MAIndex.SettlementName","MAIndex.p.value")],
                   Koon.non.parametric.test.settlements[order(Koon.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                          c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)


# ---- KEI ----
Kei.Sett.Means <- Sett.Means %>% filter(MPAID==17 & MonitoringYear=="Baseline")
Kei.HHData <- HHData %>% filter(MPAID==17 & MonitoringYear=="Baseline")
Kei.sett.names <- factor(Kei.Sett.Means$SettlementName)

Kei.even.number.setts.function <- 
  mapply(a=Kei.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Kei.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Kei.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Kei.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Kei.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Kei.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kei.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kei.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Kei.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Kei.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kei.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kei.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kei.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Kei.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Kei.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Kei.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Kei.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kei.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Kei.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Kei.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Kei.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kei.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Kei.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Kei.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Kei.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Kei.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Kei.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Kei.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kei.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Kei.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Kei.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Kei.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kei.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Kei.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Kei.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Kei.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Kei.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Kei.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Kei.median.setts <- 
  mapply(i=Kei.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Kei.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Kei.sett.names)%%2!=0,
                                      as.character(Kei.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Kei.even.number.setts.function[j])),
                               levels=levels(Kei.sett.names))})


Kei.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Kei.sett.names[which(Kei.sett.names!=Kei.median.setts[a])]),
                                                               as.character(Kei.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Kei.sett.names[which(Kei.sett.names!=Kei.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Kei.HHData[Kei.HHData$SettlementName==i | Kei.HHData$SettlementName==Kei.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Kei.HHData[Kei.HHData$SettlementName==i | Kei.HHData$SettlementName==Kei.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Kei.sigvals.Sett <- 
  cbind.data.frame(Kei.non.parametric.test.settlements[order(Kei.non.parametric.test.settlements$MAIndex.SettlementName),
                                                        c("MAIndex.SettlementName","MAIndex.p.value")],
                   Kei.non.parametric.test.settlements[order(Kei.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                        c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)


# ---- YAMDENA ----
Yamdena.Sett.Means <- Sett.Means %>% filter(MPAID==19 & MonitoringYear=="Baseline")
Yamdena.HHData <- HHData %>% filter(MPAID==19 & MonitoringYear=="Baseline")
Yamdena.sett.names <- factor(Yamdena.Sett.Means$SettlementName)

Yamdena.even.number.setts.function <- 
  mapply(a=Yamdena.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         b=Yamdena.HHData[,c("MAIndex","MAIndex.noTV")],
         function(a,b){
           med <- median(a,na.rm=T)
           equal <- c(a[which(a==med)])
           upper <- c(a[which(a>med)])
           upper <- min(upper,na.rm=T)
           lower <- c(a[which(a<med)]) 
           lower <- max(lower,na.rm=T)
           upper.sett <- Yamdena.Sett.Means$SettlementName[a==upper]
           upper.sett <- ifelse(length(upper.sett)>1 & length(upper.sett)<3,
                                ifelse((sd(b[Yamdena.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                         (sd(b[Yamdena.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[2] & !is.na(b)]))),
                                       as.character(upper.sett[1]),as.character(upper.sett[2])),
                                ifelse(length(upper.sett)>1 & length(upper.sett)<4,
                                       ifelse((sd(b[Yamdena.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[2] & !is.na(b)]))) &
                                                (sd(b[Yamdena.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                              as.character(upper.sett[1]),
                                              ifelse((sd(b[Yamdena.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.HHData$SettlementName==upper.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Yamdena.HHData$SettlementName==upper.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.HHData$SettlementName==upper.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett[3] & !is.na(b)]))),
                                                     as.character(upper.sett[2]),
                                                     as.character(upper.sett[3]))),
                                       as.character(upper.sett)))
           lower.sett <- Yamdena.Sett.Means$SettlementName[a==lower]
           lower.sett <- ifelse(length(lower.sett)>1 & length(lower.sett)<3,
                                ifelse((sd(b[Yamdena.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                         (sd(b[Yamdena.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[2] & !is.na(b)]))),
                                       as.character(lower.sett[1]),as.character(lower.sett[2])),
                                ifelse(length(lower.sett)>1 & length(lower.sett)<4,
                                       ifelse((sd(b[Yamdena.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[2] & !is.na(b)]))) &
                                                (sd(b[Yamdena.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[1] & !is.na(b)])))<
                                                (sd(b[Yamdena.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                              as.character(lower.sett[1]),
                                              ifelse((sd(b[Yamdena.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.HHData$SettlementName==lower.sett[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[1] & !is.na(b)]))) &
                                                       (sd(b[Yamdena.HHData$SettlementName==lower.sett[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[2] & !is.na(b)])))<
                                                       (sd(b[Yamdena.HHData$SettlementName==lower.sett[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett[3] & !is.na(b)]))),
                                                     as.character(lower.sett[2]),
                                                     as.character(lower.sett[3]))),
                                       as.character(lower.sett)))
           sett.equal.med <- Yamdena.Sett.Means$SettlementName[a==equal]
           sett.equal.med <- ifelse(length(sett.equal.med)>1 & length(sett.equal.med)<3,
                                    ifelse((sd(b[Yamdena.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                             (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))),
                                           as.character(sett.equal.med[1]),as.character(sett.equal.med[2])),
                                    ifelse(length(sett.equal.med)>2 & length(sett.equal.med)<4,
                                           ifelse((sd(b[Yamdena.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[2] & !is.na(b)]))) &
                                                    (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[1] & !is.na(b)])))<
                                                    (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                  as.character(sett.equal.med[1]),
                                                  ifelse((sd(b[Yamdena.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[1]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[1] & !is.na(b)]))) &
                                                           (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[2]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[2] & !is.na(b)])))<
                                                           (sd(b[Yamdena.HHData$SettlementName==sett.equal.med[3]],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==sett.equal.med[3] & !is.na(b)]))),
                                                         as.character(sett.equal.med[2]),
                                                         as.character(sett.equal.med[3]))),
                                           ifelse(is.na(sett.equal.med),
                                                  NA,
                                                  as.character(sett.equal.med))))
           
           
           median.sett <- ifelse(!is.na(sett.equal.med),
                                 as.character(sett.equal.med),
                                 ifelse((sd(b[Yamdena.HHData$SettlementName==upper.sett],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==upper.sett & !is.na(b)])))<
                                          (sd(b[Yamdena.HHData$SettlementName==lower.sett],na.rm=T)/sqrt(length(b[Yamdena.HHData$SettlementName==lower.sett & !is.na(b)]))),
                                        as.character(upper.sett),
                                        as.character(lower.sett)))
         })

Yamdena.median.setts <- 
  mapply(i=Yamdena.Sett.Means[,c("MAIndex","MAIndex.noTV")],
         j=names(Yamdena.even.number.setts.function),
         function(i,j){
           med <- median(i,na.rm=T)
           med.setts <- factor(ifelse(length(Yamdena.sett.names)%%2!=0,
                                      as.character(Yamdena.Sett.Means$SettlementName[which(i==med)]),
                                      as.character(Yamdena.even.number.setts.function[j])),
                               levels=levels(Yamdena.sett.names))})


Yamdena.non.parametric.test.settlements <- 
  data.frame(mapply(a=c("MAIndex","MAIndex.noTV"),
                    function(a){
                      results <- 
                        list(cbind.data.frame(SettlementName=c(as.character(Yamdena.sett.names[which(Yamdena.sett.names!=Yamdena.median.setts[a])]),
                                                               as.character(Yamdena.median.setts[a])),
                                              
                                              rbind.data.frame(t(data.frame(mapply(i=Yamdena.sett.names[which(Yamdena.sett.names!=Yamdena.median.setts[a])],
                                                                                   function(i){
                                                                                     var <- 
                                                                                       Yamdena.HHData[Yamdena.HHData$SettlementName==i | Yamdena.HHData$SettlementName==Yamdena.median.setts[a],a]
                                                                                     
                                                                                     test <- 
                                                                                       wilcox.test(var~SettlementName,
                                                                                                   data=Yamdena.HHData[Yamdena.HHData$SettlementName==i | Yamdena.HHData$SettlementName==Yamdena.median.setts[a],],
                                                                                                   exact=F)
                                                                                   }))["p.value",]),
                                                               "median"))
                        )}))


Yamdena.sigvals.Sett <- 
  cbind.data.frame(Yamdena.non.parametric.test.settlements[order(Yamdena.non.parametric.test.settlements$MAIndex.SettlementName),
                                                       c("MAIndex.SettlementName","MAIndex.p.value")],
                   Yamdena.non.parametric.test.settlements[order(Yamdena.non.parametric.test.settlements$MAIndex.noTV.SettlementName),
                                                       c("MAIndex.noTV.SettlementName","MAIndex.noTV.p.value")]) %>%
  select(MAIndex.SettlementName,MAIndex.p.value,MAIndex.noTV.p.value)


# ---- OUTPUT TO CSV ----

sigvals.Sett <- 
  rbind.data.frame(cbind.data.frame(MPAID=1,Telma.sigvals.Sett),
                   cbind.data.frame(MPAID=2,TNTC.sigvals.Sett),
                   cbind.data.frame(MPAID=3,Kaimana.sigvals.Sett),
                   cbind.data.frame(MPAID=4,Kofiau.sigvals.Sett),
                   cbind.data.frame(MPAID=5,Dampier.sigvals.Sett),
                   cbind.data.frame(MPAID=6,Misool.sigvals.Sett),
                   cbind.data.frame(MPAID=15,Alor.sigvals.Sett),
                   cbind.data.frame(MPAID=16,Flotim.sigvals.Sett),
                   cbind.data.frame(MPAID=17,Kei.sigvals.Sett),
                   cbind.data.frame(MPAID=18,Koon.sigvals.Sett),
                   cbind.data.frame(MPAID=19,Yamdena.sigvals.Sett))

sigvals.MPAtrend <-
  rbind.data.frame(cbind.data.frame(MPAID=1,Telma.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=2,TNTC.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=3,Kaimana.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=4,Kofiau.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=5,Dampier.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=6,Misool.trend.non.parametric.test.byMPA["sl",]),
                   cbind.data.frame(MPAID=16,Flotim.trend.non.parametric.test.byMPA["sl",]))


export(sigvals.Sett,"x_Flat_data_files/1_Social/Outputs/MAIndex.sigtests.xlsx",sheetName="Settlement_level_status")
export(sigvals.MPAtrend,"x_Flat_data_files/1_Social/Outputs/MAIndex.sigtests.trend.xlsx",sheetName="MPA_level_trend")
