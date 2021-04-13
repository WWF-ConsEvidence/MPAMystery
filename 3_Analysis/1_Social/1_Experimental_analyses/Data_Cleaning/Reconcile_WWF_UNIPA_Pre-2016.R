
# ---- 1.1 Load libraries & data ----
library(data.table)
library(janitor)
library(glue)
library(vetr)
library(diffdf)
library(arsenal)
library(xlsx)
pacman::p_load(rio, plyr, foreach, dplyr, openxlsx, chron)

# - WWF DATA
WWF_household <- 
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/WWF_data/household-20210113.csv')  %>%
  filter(!is.na(householdid))

# - UNIPA DATA
# compared with HH_tbl_HOUSEHOLD_UNIPA before
UNIPA_household <- 
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/UNIPA_data/HH_tbl_HOUSEHOLD_UNIPA_Pre-2016.xlsx')  %>%
  filter(!is.na(HouseholdID))

dim(WWF_household)
dim(UNIPA_household)

#set column names in WWF and UNIPA to lower case
colnames(WWF_household) <- tolower(colnames(WWF_household))
colnames(UNIPA_household) <- tolower(colnames(UNIPA_household))

#rename the columns in b/UNIPA dataset
#colnames(UNIPA_household)[4] = "MPA"
#colnames(UNIPA_household)[5] = "Settlement"
#colnames(UNIPA_household)[41] = "FreqFishTime"

#names(UNIPA_household)[names(UNIPA_household) == "mpaid"] <- "MPA"
#names(UNIPA_household)[names(UNIPA_household) == "settlementid"] <- "Settlement"
#names(UNIPA_household)[names(UNIPA_household) == "freqfish"] <- "FreqFishTime"
names(UNIPA_household)[names(UNIPA_household) == "assetsattellite"] <- "assetsatellite"

#set all column names to lower case
a <- colnames(WWF_household)
b <- tolower(colnames(UNIPA_household))

#check for differences in columns
setdiff(a, b)
setdiff(b, a)

#remove irrelevant/unmatched columns
#drops_WWF <- c("entrycomputeridentifier","entryhouseholdid",
#               "baseline_t2_pairs","primaryfishtechnique",
#               "secondaryfishtechnique","tertiaryfishtechnique",
#               "primarymarketname","secondarymarketname","timesecondarymarket")
#drops_UNIPA <- c("countryid","4","f133","f134")

#WWF_household <- WWF_household[,!(names(WWF_household) %in% drops_WWF)]
#UNIPA_household <- UNIPA_household[,!(names(UNIPA_household) %in% drops_UNIPA)]

#check for dimension differences for columns in the two updated datasets
dim(WWF_household)
dim(UNIPA_household)

#compare two cleaned dataframes --> some mismatch in variable class
compare_df_cols(WWF_household, UNIPA_household, return = "mismatch")
#compare_df_cols(WWF_household, UNIPA_household, return = "match")

#construct error message
compare_df_cols(WWF_household, UNIPA_household, return = "mismatch") %>%
  mutate(message = glue("Column `{column_name}`: expected class {WWF_household}, got class {UNIPA_household}")) %>%
  pull(message)


#compare 2 df and examine differences
comparison <- comparedf(WWF_household, UNIPA_household, by="householdid",
                        int.as.num = TRUE)

comparison_summary <- summary(comparison)
n.diffs(comparison)

all_diffs <- diffs(comparison)
diffs_summary <- as.data.frame(diffs(comparison, by.var = TRUE))

#export diffs as csv
#export(diffs, "diffs_WWF_UNIPA_Pre-2016.csv")
#export(all_diffs, "all_diffs.csv")


#check mismatches
alike(WWF_household, UNIPA_household)
compare(WWF_household, UNIPA_household)
#expect_equal(WWF_household, UNIPA_household)
diffdf(WWF_household, UNIPA_household)







