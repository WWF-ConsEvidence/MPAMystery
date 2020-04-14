# look for variable names
grep('market',names(HHData),value=T, ignore.case = T)
# set directory
outputdir <- "C:/Users/dgill6/Documents/Data analysis/BHS/"

# Correlogram
plot.cor <- function (x){
  M <- cor(x,method='spearman',use="pairwise.complete.obs")
  #arrange by hierarchical clusters
  ord <-   corrMatOrder(M, order="hclust")
  M1 <- M[ord, ord]
  #get correlation p-values, in same order
  pval <- psych::corr.test(x, adjust="none",use = "pairwise",method="spearman")$p[ord, ord]
  #psych::corr.test(Mgt_latest2[,c('STF','BGT')], adjust="none",use = "pairwise",method="spearman")$p
  
  # corrplot(M1, tl.pos="lt",method = "number",type='lower',order = "original",title = "",
  #          p.mat = pval , insig = "blank")
  # corrplot(M1, tl.pos="n",diag=F,add=T,method = "circle",type='upper',order = "original",title = "",
  #          p.mat = pval , insig = "blank",cl.pos="n")
   corrplot.mixed(M,order='hclust')
  }
# Xlabels (need fixing)
xlabl <- function (x,y){
  quo_y <- enquo(y)  # needed to use variable names in function
  A <- x %>%
    dplyr::select(MPAName:N) %>% 
    spread(key = !!quo_y, value=N)
  A <- A %>% 
    mutate(xlabel=paste0(MonitoringYear,"\n",'(',A[,3],',',A[,4],')')) %>% 
    select(MPAName, MonitoringYear,xlabel)
  A <- left_join(x,A,by=c('MPAName'='MPAName', 'MonitoringYear'='MonitoringYear'))
  
  pd <- position_dodge(width=.3) # move them .05 to the left and right
  myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
  colnames(myColors) <- unique(A[,2])
  colScale <- scale_colour_manual(name = "Group",values = rev(myColors),breaks=unique(A[,2]))
  return (A)
}  

##################################
# Explore tenure at each
tenure <- HHData %>% 
  dplyr::select(MonitoringYear,RightsAccessClean:RightsTransferClean)
names(tenure) <- gsub('Rights','',names(tenure))
names(tenure) <- gsub('Clean','',names(tenure))
is.na(tenure[,2:6])<-tenure[,2:6]>1
tenure <- tenure[complete.cases(tenure),]
summary(tenure)

jpeg(paste0(outputdir,'tenure_corr_time.jpg'), width = 1024, height = 480, units = "px")
par(mfrow=c(1,3))
corrplot.mixed(cor(tenure %>% 
             filter(MonitoringYear=="Baseline") %>% 
             dplyr::select(-MonitoringYear),method='spearman',use="pairwise.complete.obs"),
             title="Baseline")
corrplot.mixed(cor(tenure %>% 
             filter(MonitoringYear=="2 Year Post") %>% 
             dplyr::select(-MonitoringYear),method='spearman',use="pairwise.complete.obs"),
             title="two year")
corrplot.mixed(cor(tenure %>% 
             filter(MonitoringYear=="4 Year Post") %>% 
             dplyr::select(-MonitoringYear),method='spearman',use="pairwise.complete.obs"),
             title="four year")
dev.off()

# Group and year summaries
#-------------
pd <- position_dodge(width=.3) # move them .05 to the left and right

# Food security trends
p.foodsec <- ggplot(HHData %>% 
 left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
 left_join(MPANames,by='MPAID') %>%
 filter(!is.na(FSIndex)) %>% 
 Rmisc::summarySE(measurevar="FSIndex", groupvars=c('MPAName',"MonitoringYear"),na.rm = T),
aes(x=MonitoringYear, y=FSIndex)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=FSIndex-se, ymax=FSIndex+ se), width=0.2, position = pd ) +
  # scale_y_continuous(limits = c(1.5,4.2)) +
  ylab("Food Security Index") +
  xlab("MonitoringYear") +
  facet_grid(.~MPAName) 
p.foodsec

# Tenure trends
p.tenure <- ggplot(HHData %>% 
   left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
   left_join(MPANames,by='MPAID') %>%
   filter(!is.na(MTIndex) & MTIndex<=5) %>% 
   Rmisc::summarySE(measurevar="MTIndex", groupvars=c('MPAName',"MonitoringYear"),na.rm = T),
 aes(x=MonitoringYear, y=MTIndex)) + 
  geom_bar(stat="identity", position =pd, fill='blue')+ theme_bw() +
  geom_line( position = pd) +
  geom_errorbar(aes(ymin=MTIndex-se, ymax=MTIndex+ se), width=0.2, position = pd ) +
  # scale_y_continuous(limits = c(1.5,4.2)) +
  ylab("Marine Tenure Index") +
  xlab("MonitoringYear") +
  facet_grid(.~MPAName) 
p.tenure
  
# Export
plot_grid(p.foodsec,p.tenure,
          labels=letters[1:2], ncol = 1, nrow = 2)
ggsave(paste0(outputdir,"general_tenure_foodsecurity.MPA.png"),width = 14,height = 7)
##############
# Fishing dependency
table(HHData$PrimaryLivelihoodClean,HHData$MonitoringYear)
# Food security different based on primary occupation?
prim.occ.data <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>%
  filter(!is.na(PrimaryLivelihoodClean)) %>% 
  mutate(lvlhds=recode(PrimaryLivelihoodClean,'1'='farming','2'='forest_harvesting','3'='fishing','4'='aquaculture',
                       '5'='mining','6'='tourism','7'='wage_labor','996'="other"))

# Plot
prim.occ.data2 <- Rmisc::summarySE(prim.occ.data, measurevar="FSIndex", groupvars=c('MPAName','lvlhds',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right

p.priocc <-  ggplot(prim.occ.data2, aes(x=MonitoringYear, y=FSIndex)) + 
              geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
              geom_line( position = pd ) +
              geom_point(size=2, position =pd)+ theme_bw() +
              ylab("FSIndex") +
              xlab("MonitoringYear") +
              facet_grid(lvlhds ~ MPAName) +
              colScale

 #-------------
# Food security different based on income?
prim.inc <- HHData %>% 
 left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
 left_join(MPANames,by='MPAID') %>% 
 filter(!is.na(PercentIncFishClean) & PercentIncFishClean!=0) %>% 
 mutate(fish.income=ifelse(PercentIncFishClean>=3,'majority','minority'))

# Plot
prim.inc2 <- Rmisc::summarySE(prim.inc, measurevar="FSIndex", groupvars=c('MPAName','fish.income',"MonitoringYear"),na.rm = T)
xlabl(prim.inc2,fish.income)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("majority","minority")
colScale <- scale_colour_manual(name = "Group",values = rev(myColors),breaks=c("majority","minority"))

prim.inc <- ggplot(prim.inc2, aes(x=MonitoringYear, y=FSIndex,colour=fish.income)) + 
             #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
             geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
             geom_line( position = pd ) +
             geom_point(size=2, position =pd)+ theme_bw() +
             ylab("FSIndex") +
             xlab("MonitoringYear") +
             facet_grid(. ~ MPAName) +
             colScale
ggsave(paste0(outputdir,"fishincome_foodsecurity.png"),width = 15,height = 3)

#-------------
 # Those who rely heavily on fish for protein differ in food security?
most.prot <- HHData %>% 
 left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
 left_join(MPANames,by='MPAID') %>% 
 filter(!is.na(FreqEatFishClean)) %>% 
 mutate(reliance=ifelse(PercentProteinFishClean>3,'reliant','less reliant'))

# Plot
most.prot2 <- Rmisc::summarySE(most.prot, measurevar="FSIndex", groupvars=c('reliance',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("reliant","less reliant")
colScale <- scale_colour_manual(name = "Group",values = myColors,breaks=c("reliant","less reliant"))

ggplot(most.prot2, aes(x=MonitoringYear, y=FSIndex,colour=reliance)) + 
 #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
 geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
 geom_line( position = pd ) +
 geom_point(size=2, position =pd)+ theme_bw() +
 ylab("FSIndex") +
 xlab("MonitoringYear") +
 colScale
# + geom_text(x=5.5, y=2, label="a)",col="black")

#---------------
# Those who eat fish regularly differ in food security?
table(HHData$FreqEatFishClean,HHData$PercentProteinFishClean)
plot.cor(dplyr::select(HHData,FreqEatFishClean,PercentProteinFishClean))

eat.fish <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(!is.na(FreqEatFishClean)) %>% 
  mutate(reliance=ifelse(FreqEatFishClean>3,'weekly','less often'))

# Plot all MPAs
eat.fish2 <- Rmisc::summarySE(eat.fish, measurevar="FSIndex", groupvars=c('reliance',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("weekly","less often")
colScale <- scale_colour_manual(name = "Frequency",values = myColors,breaks=c("weekly","less often"))

freq.sec.all <- ggplot(eat.fish2, aes(x=MonitoringYear, y=FSIndex,colour=reliance)) + 
                #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
                geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
                geom_line( position = pd ) +
                geom_point(size=2, position =pd)+ theme_bw() +
                ylab("FSIndex") +
                xlab("MonitoringYear") +
                colScale

# Plot by MPA
eat.fish3 <- Rmisc::summarySE(eat.fish, measurevar="FSIndex", groupvars=c('MPAName','reliance',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("weekly","less often")
colScale <- scale_colour_manual(name = "Frequency",values = myColors,breaks=c("weekly","less often"))

freq.sec <-ggplot(eat.fish3, aes(x=MonitoringYear, y=FSIndex,colour=reliance)) + 
              #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
              geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
              geom_line( position = pd ) +
              geom_point(size=2, position =pd)+ theme_bw() +
              ylab("FSIndex") +
              xlab("MonitoringYear") +
              facet_grid(. ~ MPAName) +
              colScale

# Has the number of people eating fish changed?
table(HHData$FreqEatFishClean,HHData$MonitoringYear)
eat.fish4 <- Rmisc::summarySE(eat.fish, measurevar="FreqEatFishClean", groupvars=c('MPAName',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right

freq.eat <- ggplot(eat.fish4, aes(x=MonitoringYear, y=FreqEatFishClean)) + 
              #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
              geom_errorbar(aes(ymin=FreqEatFishClean- se, ymax=FreqEatFishClean+ se), width=0, position = pd ) +
              geom_line( position = pd ) +
              geom_point(size=2, position =pd)+ theme_bw() +
              ylab("Freq. fish consumption") +
              xlab("MonitoringYear") +
              facet_grid(. ~ MPAName) +
              colScale

plot_grid(freq.sec + theme(legend.position="top"),
          freq.eat, labels=c("a", "b"), ncol = 1, nrow = 2)
ggsave(paste0(outputdir,"eatfreq_foodsecurity.png"),width = 15,height = 5)


#-------------
# Food security different based on market distance?
mrkt.dist <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(!is.na(TimeMarketClean)) %>% 
  mutate(mrktdist=ifelse(TimeMarketClean>=2,'far','near'))

# Plot
mrkt.dist2 <- Rmisc::summarySE(mrkt.dist, measurevar="FSIndex", groupvars=c('MPAName','mrktdist',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("far","near")
colScale <- scale_colour_manual(name = "Group",values = rev(myColors),breaks=c("far","near"))

mrkt.dist <- ggplot(mrkt.dist2, aes(x=MonitoringYear, y=FSIndex,colour=mrktdist)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale
ggsave(paste0(outputdir,"mrktdist_foodsecurity.png"),width = 15,height = 3)

#--------------------------------------------------------
# Food security differ by tenure?
table(HHData$RightsAccessCoded)

t.access <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(RightsAccessCoded%in%c(0,1)) %>% 
  mutate(access=ifelse(RightsAccessCoded==1,'yes','no'))

# Plot all MPAs
t.access2 <- Rmisc::summarySE(t.access, measurevar="FSIndex", groupvars=c('access',"MonitoringYear"),na.rm = T)
pd <- position_dodge(width=.3) # move them .05 to the left and right
myColors <- matrix(c('deepskyblue3','darkblue'),nrow=1)
colnames(myColors) <- c("yes","no")
colScale <- scale_colour_manual(name = "Access",values = myColors,breaks=c("yes","no"))

p.access <- ggplot(t.access2, aes(x=MonitoringYear, y=FSIndex,colour=access)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  colScale

# Plot by MPA
t.access3 <- Rmisc::summarySE(t.access, measurevar="FSIndex", groupvars=c('MPAName','access',"MonitoringYear"),na.rm = T)

p.access.MPA <- ggplot(t.access3, aes(x=MonitoringYear, y=FSIndex,colour=access)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale

##Harvest rights
t.harvest <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(RightsHarvestClean%in%c(0,1)) %>% 
  mutate(harvest=ifelse(RightsHarvestClean==1,'yes','no'))

# Plot all MPAs
t.harvest2 <- Rmisc::summarySE(t.harvest, measurevar="FSIndex", groupvars=c('harvest',"MonitoringYear"),na.rm = T)
colScale <- scale_colour_manual(name = "harvest",values = myColors,breaks=c("yes","no"))

p.harvest <- ggplot(t.harvest2, aes(x=MonitoringYear, y=FSIndex,colour=harvest)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  colScale

# Plot by MPA
t.harvest3 <- Rmisc::summarySE(t.harvest, measurevar="FSIndex", groupvars=c('MPAName','harvest',"MonitoringYear"),na.rm = T)

p.harvest.MPA <- ggplot(t.harvest3, aes(x=MonitoringYear, y=FSIndex,colour=harvest)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale
## Managment rights
t.manage <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(RightsManageClean%in%c(0,1)) %>% 
  mutate(manage=ifelse(RightsManageClean==1,'yes','no'))

# Plot all MPAs
t.manage2 <- Rmisc::summarySE(t.manage, measurevar="FSIndex", groupvars=c('manage',"MonitoringYear"),na.rm = T)
colScale <- scale_colour_manual(name = "manage",values = myColors,breaks=c("yes","no"))

p.manage <- ggplot(t.manage2, aes(x=MonitoringYear, y=FSIndex,colour=manage)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  colScale

# Plot by MPA
t.manage3 <- Rmisc::summarySE(t.manage, measurevar="FSIndex", groupvars=c('MPAName','manage',"MonitoringYear"),na.rm = T)

p.manage.MPA <- ggplot(t.manage3, aes(x=MonitoringYear, y=FSIndex,colour=manage)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale

##Exclude rights
t.exclude <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(RightsExcludeClean%in%c(0,1)) %>% 
  mutate(exclude=ifelse(RightsExcludeClean==1,'yes','no'))

# Plot all MPAs
t.exclude2 <- Rmisc::summarySE(t.exclude, measurevar="FSIndex", groupvars=c('exclude',"MonitoringYear"),na.rm = T)
colScale <- scale_colour_manual(name = "exclude",values = myColors,breaks=c("yes","no"))

p.exclude <- ggplot(t.exclude2, aes(x=MonitoringYear, y=FSIndex,colour=exclude)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  colScale

# Plot by MPA
t.exclude3 <- Rmisc::summarySE(t.exclude, measurevar="FSIndex", groupvars=c('MPAName','exclude',"MonitoringYear"),na.rm = T)

p.exclude.MPA <- ggplot(t.exclude3, aes(x=MonitoringYear, y=FSIndex,colour=exclude)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale

##Transfer rights
t.transfer <- HHData %>% 
  left_join(dplyr::select(BigFive,HouseholdID,MAIndex:SERate),by='HouseholdID')  %>% 
  left_join(MPANames,by='MPAID') %>% 
  filter(RightsTransferClean%in%c(0,1)) %>% 
  mutate(transfer=ifelse(RightsTransferClean==1,'yes','no'))

# Plot all MPAs
t.transfer2 <- Rmisc::summarySE(t.transfer, measurevar="FSIndex", groupvars=c('transfer',"MonitoringYear"),na.rm = T)
colScale <- scale_colour_manual(name = "transfer",values = myColors,breaks=c("yes","no"))

p.transfer <- ggplot(t.transfer2, aes(x=MonitoringYear, y=FSIndex,colour=transfer)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  colScale

# Plot by MPA
t.transfer3 <- Rmisc::summarySE(t.transfer, measurevar="FSIndex", groupvars=c('MPAName','transfer',"MonitoringYear"),na.rm = T)

p.transfer.MPA <- ggplot(t.transfer3, aes(x=MonitoringYear, y=FSIndex,colour=transfer)) + 
  #scale_x_discrete(limits = rev(levels(MPAdata2$No_take))) +
  geom_errorbar(aes(ymin=FSIndex- se, ymax=FSIndex+ se), width=0, position = pd ) +
  geom_line( position = pd ) +
  geom_point(size=2, position =pd)+ theme_bw() +
  ylab("FSIndex") +
  xlab("MonitoringYear") +
  facet_grid(. ~ MPAName) +
  colScale


plot_grid(p.access,p.harvest,p.manage,p.exclude,p.transfer+ theme(legend.position="right"),
           labels=letters[1:5], ncol = 2, nrow = 3)
ggsave(paste0(outputdir,"tenure_foodsecurity.png"),width = 10,height = 7)


