# 
# code: MPA Mystery, BHS two and four year social impact data


# ---- inputs ---- 
# ---- code sections ----



# ---- 1.1 Consolidate 2 year impact data for "Big Five" ----

FS.impact.2yr <- hfoodsec.ATT[!duplicated(hfoodsec.ATT$HouseholdID),]
colnames(FS.impact.2yr) <- c(colnames(FS.impact.2yr[1:5]),
                             paste("HFS",colnames(FS.impact.2yr[6:8]),sep="."))

MA.impact.2yr <- assets.ATT[!duplicated(assets.ATT$HouseholdID),]
colnames(MA.impact.2yr) <- c(colnames(MA.impact.2yr[1:5]),
                             paste("assets",colnames(MA.impact.2yr[6:8]),sep="."))

PA.impact.2yr <- attach.ATT[!duplicated(attach.ATT$HouseholdID),]
colnames(PA.impact.2yr) <- c(colnames(PA.impact.2yr[1:5]),
                             paste("attach",colnames(PA.impact.2yr[6:8]),sep="."))

MT.impact.2yr <- tenure.ATT[!duplicated(tenure.ATT$HouseholdID),]
colnames(MT.impact.2yr) <- c(colnames(MT.impact.2yr[1:5]),
                             paste("tenure",colnames(MT.impact.2yr[6:8]),sep="."))

SE.impact.2yr <- enrol.ATT[!duplicated(enrol.ATT$HouseholdID),]
SE.impact.2yr[,2:8] <- SE.impact.2yr[,2:8]/100
colnames(SE.impact.2yr) <- c(colnames(SE.impact.2yr[1:5]),
                             paste("enrol",colnames(SE.impact.2yr[6:8]),sep="."))

impact.2yr <- 
  full_join(FS.impact.2yr,MA.impact.2yr,by="HouseholdID") %>%
  full_join(PA.impact.2yr,by="HouseholdID") %>%
  full_join(MT.impact.2yr,by="HouseholdID") %>%
  full_join(SE.impact.2yr,by="HouseholdID") %>%
  left_join(HHData[,1:2],by="HouseholdID")


# ---- 1.2 Consolidate 4 year impact data for "Big Five" ----

hfoodsec.ATT.t4 <- read.csv("2_Social/FlatDataFiles/BHS/hfoodsec.ATT.t4.csv",header=T)
assets.ATT.t4 <- read.csv("2_Social/FlatDataFiles/BHS/assets.ATT.t4.csv",header=T)
attach.ATT.t4 <- read.csv("2_Social/FlatDataFiles/BHS/attach.ATT.t4.csv",header=T)
tenure.ATT.t4 <- read.csv("2_Social/FlatDataFiles/BHS/tenure.ATT.t4.csv",header=T)
enrol.ATT.t4 <- read.csv("2_Social/FlatDataFiles/BHS/enrol.ATT.t4.csv",header=T)




FS.impact.4yr <- hfoodsec.ATT.t4[!duplicated(hfoodsec.ATT.t4$HouseholdID),]
colnames(FS.impact.4yr) <- c(colnames(FS.impact.4yr[1]),
                             paste("HFS",colnames(FS.impact.4yr[2:8]),sep="."))

MA.impact.4yr <- assets.ATT.t4[!duplicated(assets.ATT.t4$HouseholdID),]
colnames(MA.impact.4yr) <- c(colnames(MA.impact.4yr[1]),
                             paste("assets",colnames(MA.impact.4yr[2:8]),sep="."))

PA.impact.4yr <- attach.ATT.t4[!duplicated(attach.ATT.t4$HouseholdID),]
colnames(PA.impact.4yr) <- c(colnames(PA.impact.4yr[1]),
                             paste("attach",colnames(PA.impact.4yr[2:8]),sep="."))

MT.impact.4yr <- tenure.ATT.t4[!duplicated(tenure.ATT.t4$HouseholdID),]
colnames(MT.impact.4yr) <- c(colnames(MT.impact.4yr[1]),
                             paste("tenure",colnames(MT.impact.4yr[2:8]),sep="."))

SE.impact.4yr <- enrol.ATT.t4[!duplicated(enrol.ATT.t4$HouseholdID),]
colnames(SE.impact.4yr) <- c(colnames(SE.impact.4yr[1]),
                             paste("enrol",colnames(SE.impact.4yr[2:8]),sep="."))

impact.4yr <- 
  full_join(FS.impact.4yr,MA.impact.4yr,by="HouseholdID") %>%
  full_join(PA.impact.4yr,by="HouseholdID") %>%
  full_join(MT.impact.4yr,by="HouseholdID") %>%
  full_join(SE.impact.4yr,by="HouseholdID") %>%
  left_join(HHData[,1:2],by="HouseholdID")

# 4 Year impact p values

p.vals.4yr <- read.csv('2_Social/FlatDataFiles/BHS/pvals_t4_2018_0104.csv',header=T)

p.vals.dampier.4yr <- p.vals.4yr[p.vals.4yr$MPAID==5,2:3]
p.vals.kaimana.4yr <- p.vals.4yr[p.vals.4yr$MPAID==3,2:3]
p.vals.kofiau.4yr <- p.vals.4yr[p.vals.4yr$MPAID==4,2:3]
p.vals.mayalibit.4yr <- p.vals.4yr[p.vals.4yr$MPAID==1,2:3]
p.vals.misool.4yr <- p.vals.4yr[p.vals.4yr$MPAID==6,2:3]
p.vals.tntc.4yr <- p.vals.4yr[p.vals.4yr$MPAID==2,2:3]

# ---- 1.3 Import standardized 2 year impact data from flat files ----

std.impact.2yr <- read.csv('2_Social/FlatDataFiles/BHS/MPA_2yr_std_impacts&se.csv',header=TRUE)
std.impact.2yr <- std.impact.2yr[,1:4]
std.impact.2yr$Domain <- factor(ifelse(std.impact.2yr$Domain=="Material assets","Economic Well-Being",
                                       ifelse(std.impact.2yr$Domain=="Food security","Health",
                                              ifelse(std.impact.2yr$Domain=="Marine tenure","Political Empowerment",
                                                     ifelse(std.impact.2yr$Domain=="Place attachment","Culture",
                                                            ifelse(std.impact.2yr$Domain=="School enrollment","Education","NA"))))),
                                levels=rev(c("Economic Well-Being","Health","Political Empowerment","Education","Culture")),
                                ordered=T)
colnames(std.impact.2yr) <- c("Domain","MPA","Std.impact","Std.se")

std.impact.2yr$impact.direction <- factor(ifelse(std.impact.2yr$Std.impact>0,"Positive","Negative"),
                                          levels=c("Positive","Negative"),
                                          ordered=T)

seascape.impact.2yr <- read.csv('2_Social/FlatDataFiles/BHS/BHS_2yr_std_impacts.csv',header=TRUE)
seascape.impact.2yr$Domain <- factor(ifelse(seascape.impact.2yr$Domain=="Economic well-being","Economic Well-Being",
                                            ifelse(seascape.impact.2yr$Domain=="Health","Health",
                                                   ifelse(seascape.impact.2yr$Domain=="Political empowerment","Political Empowerment",
                                                          ifelse(seascape.impact.2yr$Domain=="Culture","Culture",
                                                                 ifelse(seascape.impact.2yr$Domain=="Education","Education","NA"))))),
                                     levels=rev(c("Economic Well-Being","Health","Political Empowerment","Education","Culture")),
                                     ordered=T)
colnames(seascape.impact.2yr) <- c("Domain","Std.impact","Lower.CI","Upper.CI")


# ---- 1.4 Import standardized 4 year impact data from flat files ----

# std.impact.4yr <- read.csv('2_Social/FlatDataFiles/BHS/MPA_4yr_std_impacts&se.csv',header=TRUE)
# std.impact.4yr <- std.impact.4yr[,1:4]

fs.std.impact.4yr <- read.delim('2_Social/FlatDataFiles/BHS/FS_smd_ci_t4.txt',sep=",")
fs.std.impact.4yr$MPAID <- 1:6
fs.std.impact.4yr$Domain <- "Health"

ma.std.impact.4yr <- read.delim('2_Social/FlatDataFiles/BHS/MA_smd_ci_t4.txt',sep=",")
ma.std.impact.4yr$MPAID <- 1:6
ma.std.impact.4yr$Domain <- "Economic Well-Being"

mt.std.impact.4yr <- read.delim('2_Social/FlatDataFiles/BHS/MT_smd_ci_t4.txt',sep=",")
mt.std.impact.4yr$MPAID <- 1:6
mt.std.impact.4yr$Domain <- "Marine Tenure"

pa.std.impact.4yr <- read.delim('2_Social/FlatDataFiles/BHS/PA_smd_ci_t4.txt',sep=",")
pa.std.impact.4yr$MPAID <- 1:6
pa.std.impact.4yr$Domain <- "Culture"

se.std.impact.4yr <- read.delim('2_Social/FlatDataFiles/BHS/SE_smd_ci_t4.txt',sep=",")
se.std.impact.4yr$MPAID <- 1:6
se.std.impact.4yr$Domain <- "Education"


FullMPANames <- data.frame(MPAID=1:6,
                           MPA=c("Teluk Mayalibit MPA","Teluk Cenderawasih National Park","Kaimana MPA",
                                 "Kofiau dan Pulau Boo MPA","Selat Dampier MPA","Misool Selatan Timur MPA"))


std.impact.4yr <- 
  rbind.data.frame(fs.std.impact.4yr,ma.std.impact.4yr) %>%
  rbind.data.frame(mt.std.impact.4yr) %>%
  rbind.data.frame(pa.std.impact.4yr) %>%
  rbind.data.frame(se.std.impact.4yr) %>%
  left_join(FullMPANames,by="MPAID")

colnames(std.impact.4yr) <- c("Lower.CI","Std.impact","Upper.CI","MPAID","Domain","MPA")


std.impact.4yr$Domain <- factor(std.impact.4yr$Domain,
                                levels=rev(c("Economic Well-Being","Health","Marine Tenure","Education","Culture")),
                                ordered=T)

std.impact.4yr$impact.direction <- factor(ifelse(std.impact.4yr$Std.impact>0,"Positive","Negative"),
                                          levels=c("Positive","Negative"),
                                          ordered=T)


seascape.impact.4yr <- read.csv('2_Social/FlatDataFiles/BHS/BHS_4yr_std_impacts.csv',header=TRUE)
seascape.impact.4yr$Domain <- factor(seascape.impact.4yr$Domain,
                                     levels=rev(c("Economic Well-Being","Health","Political Empowerment","Education","Culture")),
                                     ordered=T)
colnames(seascape.impact.4yr) <- c("Domain","Lower.CI","Std.impact","Upper.CI")
