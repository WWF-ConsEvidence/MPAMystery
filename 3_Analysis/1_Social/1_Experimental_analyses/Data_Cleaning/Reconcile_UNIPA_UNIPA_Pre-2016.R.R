# ---- 1.1 Load libraries & data ----
library(data.table)
library(janitor)
library(glue)
library(vetr)
library(diffdf)
library(arsenal)
library(xlsx)
library(tidyverse)
pacman::p_load(rio, plyr, foreach, dplyr, openxlsx, chron)


# - UNIPA DATA
unipa_hh <- 
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/UNIPA_data/HH_tbl_HOUSEHOLD_UNIPA.xlsx')  %>%
  filter(!is.na(HouseholdID))

#add index column to data frame
unipa_hh$index <- 1:nrow(unipa_hh)

# - UNIPA-pre-2016 DATA
unipa_hh_pre2016 <-
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/UNIPA_data/HH_tbl_HOUSEHOLD_UNIPA_Pre-2016.xlsx')  %>%
  filter(!is.na(HouseholdID))
unipa_hh_pre2016$index <- 1:nrow(unipa_hh_pre2016)

# check dimensions of the two datasets
dim(unipa_hh)
dim(unipa_hh_pre2016)

#set all column names to lower case
colnames(unipa_hh) <- tolower(colnames(unipa_hh))
colnames(unipa_hh_pre2016) <- tolower(colnames(unipa_hh_pre2016))

#check for differences in columns
setdiff(colnames(unipa_hh), colnames(unipa_hh_pre2016))
setdiff(colnames(unipa_hh_pre2016), colnames(unipa_hh))

#rename columns in the datasets due to inconsistency/misspelling
names(unipa_hh)[names(unipa_hh) == "mpaid"] <- "mpa"
names(unipa_hh)[names(unipa_hh) == "settlementid"] <- "settlement"
names(unipa_hh)[names(unipa_hh) == "freqfish"] <- "freqfishtime"
names(unipa_hh)[names(unipa_hh) == "assetsattellite"] <- "assetsatellite"
names(unipa_hh_pre2016)[names(unipa_hh_pre2016) == "assetsattellite"] <- "assetsatellite"

#check for differences in columns again
setdiff(colnames(unipa_hh), colnames(unipa_hh_pre2016))
setdiff(colnames(unipa_hh_pre2016), colnames(unipa_hh))

#remove irrelevant/unmatched columns
drops_unipa_hh <- c("countryid","4","f133","f134")
drops_unipa_hh_pre2016 <- c("primarymarketname","secondarymarketname",
               "timesecondarymarket","primaryfishtechnique",
               "secondaryfishtechnique","tertiaryfishtechnique",
               "entrycomputeridentifier","entryhouseholdid","baseline_t2_pairs")

unipa_hh <- unipa_hh[,!(names(unipa_hh) %in% drops_unipa_hh)]
unipa_hh_pre2016 <- unipa_hh_pre2016[,!(names(unipa_hh_pre2016) %in% drops_unipa_hh_pre2016)]

#check for differences in columns again
setdiff(colnames(unipa_hh), colnames(unipa_hh_pre2016))
setdiff(colnames(unipa_hh_pre2016), colnames(unipa_hh))

#compare two cleaned dataframes --> some mismatches in variable class
compare_df_cols(unipa_hh, unipa_hh_pre2016, return = "mismatch")
#compare_df_cols(unipa_hh, unipa_hh_pre2016, return = "match")

#compare 2 df and examine differences
unipa_hh$newid <- paste(unipa_hh$mpa, unipa_hh$settlement, unipa_hh$interviewyear, unipa_hh$respondent, sep = "/")
unipa_hh_pre2016$newid <- paste(unipa_hh_pre2016$mpa, unipa_hh_pre2016$settlement, unipa_hh_pre2016$interviewyear, unipa_hh_pre2016$respondent, sep = "/")

comparison <- comparedf(unipa_hh, unipa_hh_pre2016, by="newid",
                        int.as.num = TRUE)

comparison_summary <- summary(comparison)
n.diffs(comparison) #19048

all_diffs <- diffs(comparison)
diffs_summary <- as.data.frame(diffs(comparison, by.var = TRUE))

#export diffs as csv
#export(diffs_summary, "diffs_by_nums_of_var_2hhversions_newid.csv")
#export(all_diffs, "diffs_by_var_values_2hhversions_newid.csv")

#check mismatches
diffdf(unipa_hh, unipa_hh_pre2016)



