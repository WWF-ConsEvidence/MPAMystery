## # ---
# code:  Compute MPA outcomes
# author: Louise Glew, louise.glew@gmail.com; Eric Spurgeon eric.spurgeon@gmail.com
# created: June 2016
# modified:

Variable_outcome <-function(df_pairs,df_outcome){

# Extract MPA at t2
MPA.t2.outcome <- subset(df_pairs, select=c("HouseholdID"))
MPA.t2.outcome <- left_join(MPA.t2.outcome,df_outcome,by=c("HouseholdID"))
# Extract MPA at baseline
MPA.t0.outcome <- data.frame(df_pairs[,2])
colnames(MPA.t0.outcome) <-c("paired_id")
MPA.t0.outcome <- left_join(MPA.t0.outcome,df_outcome,by=c("paired_id"="HouseholdID"))
# Rejoin pairs
MPA.time.outcome <- cbind(MPA.t2.outcome, MPA.t0.outcome)
colnames(MPA.time.outcome)<-c("HouseholdID", "outcome_var.t2", "paired_id", "outcome_var.t0")
# Mutate
#MPA.time.outcome <- mutate(MPA.time.outcome, df_outcome.outcome=outcome_var.t2 -outcome_var.t0)
# Average
MPA.time.outcome <- ddply(MPA.time.outcome,.(HouseholdID), summarise, mean(outcome_var.t0))
}

