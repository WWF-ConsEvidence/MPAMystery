# 
# code: MPA Mystery, BHS two year social impact data


# ---- inputs ---- 
# ---- code sections ----

# --- Consolidate 2yr impact data for "Big Five"
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
colnames(SE.impact.2yr) <- c(colnames(SE.impact.2yr[1:5]),
                             paste("enrol",colnames(SE.impact.2yr[6:8]),sep="."))

impact.2yr <- full_join(FS.impact.2yr,MA.impact.2yr,by="HouseholdID")
impact.2yr <- full_join(impact.2yr,PA.impact.2yr,by="HouseholdID")
impact.2yr <- full_join(impact.2yr,MT.impact.2yr,by="HouseholdID")
impact.2yr <- full_join(impact.2yr,SE.impact.2yr,by="HouseholdID")
impact.2yr <- left_join(impact.2yr,HHData[,1:2],by="HouseholdID")


# ---- 1.4 Import standardized 2 year impact data from flat files ----

std.impact.2yr <- read.csv('MPAMystery/Social/FlatDataFiles/BHS/MPA_2yr_std_impacts&se.csv',header=TRUE)
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

seascape.impact.2yr <- read.csv('MPAMystery/Social/FlatDataFiles/BHS/BHS_2yr_std_impacts.csv',header=TRUE)
seascape.impact.2yr$Domain <- factor(ifelse(seascape.impact.2yr$Domain=="Economic well-being","Economic Well-Being",
                                            ifelse(seascape.impact.2yr$Domain=="Health","Health",
                                                   ifelse(seascape.impact.2yr$Domain=="Political empowerment","Political Empowerment",
                                                          ifelse(seascape.impact.2yr$Domain=="Culture","Culture",
                                                                 ifelse(seascape.impact.2yr$Domain=="Education","Education","NA"))))),
                                     levels=rev(c("Economic Well-Being","Health","Political Empowerment","Education","Culture")),
                                     ordered=T)
colnames(seascape.impact.2yr) <- c("Domain","Std.impact","Lower.CI","Upper.CI")

