# This code runs a loop through each FS variable to calculate the ATT for each food security variable
# ======
# 4.4 Disaggregated food security
Allfoodsec <- subset(MPA.time.pairs, select=c("HouseholdID"))

for (i in 2:7){
# Pre-process data
food.outcome <- hfoodsec[c(1,i)]
food.MPA.t2 <- subset(MPA.time.pairs, select=c("HouseholdID"))
food.MPA.t2 <- left_join(food.MPA.t2,food.outcome,by=c("HouseholdID"))

# Time outcome
food.time <-Variable_outcome(MPA.time.pairs,food.outcome)

# Treatment outcome
food.tr <-Variable_outcome(MPA.rpt.xsection.pairs,food.outcome)

# Interaction outcome
food.int <-Variable_outcome(MPA.int.xsection.pairs,food.outcome)

# Join three outcome sets
food.ATT <- left_join(food.time,food.tr, by="HouseholdID")
food.ATT <- left_join(food.ATT, food.int, by="HouseholdID")

# Join to MPA at t2 data
food.ATT <- left_join(food.ATT,food.MPA.t2, by="HouseholdID")
colnames(food.ATT) <- c("HouseholdID", "food.mpa.t0","food.c.t2","food.c.t0", "food.mpa.t2")

# Compute outcomes
food.ATT <- mutate(food.ATT, MPA.outcome=(food.mpa.t2 -food.mpa.t0))
food.ATT <- mutate(food.ATT, control.outcome=(food.c.t2 -food.c.t0))

# Compute treatment effect
food.ATT <- mutate(food.ATT, ATT= (food.mpa.t2 - food.mpa.t0)-(food.c.t2 - food.c.t0))
food.ATT <- na.omit (food.ATT)
names(food.ATT)[8] <- paste0(names(hfoodsec)[i],'.ATT')

Allfoodsec <- left_join(Allfoodsec,food.ATT[,c(1,8)],by="HouseholdID")

}
head(Allfoodsec)
#rm(food.time, food.tr, food.int, food.MPA.t2, food.outcome)
