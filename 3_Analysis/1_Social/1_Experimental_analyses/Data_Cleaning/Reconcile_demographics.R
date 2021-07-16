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
WWF_demo <- 
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/WWF_data/demographic-20210113.csv')  %>%
  filter(!is.na(household))

# - UNIPA DATA
UNIPA_demo <- 
  import('./BHS_UNIPA_WWF_2016-19_Data_for_Comparison/UNIPA_data/HH_tbl_DEMOGRAPHIC_UNIPA_Pre-2016.xlsx')  %>%
  filter(!is.na(Household))

dim(WWF_demo)
dim(UNIPA_demo)

#add index column to data frame
WWF_demo$index <- 1:nrow(WWF_demo)
UNIPA_demo$index <- 1:nrow(UNIPA_demo)

#set column names in UNIPA to lower case
colnames(UNIPA_demo) <- tolower(colnames(UNIPA_demo))

a <- colnames(WWF_demo)
b <- colnames(UNIPA_demo)

#check for differences in columns
setdiff(a, b)
setdiff(b, a)

#compare two dataframes --> variable class integer (WWF) vs numeric/logical (UNIPA)
compare_df_cols(WWF_demo, UNIPA_demo, return = "mismatch")

#compare 2 df and examine differences
# create newid as unique identifier
WWF_demo$newid <- paste(WWF_demo$demographicid, WWF_demo$individualname, sep = "/")
UNIPA_demo$newid <- paste(UNIPA_demo$demographicid, UNIPA_demo$individualname, sep = "/")

comparison <- comparedf(WWF_demo, UNIPA_demo, by="newid",
                        int.as.num = TRUE)

comparison_summary <- summary(comparison)
n.diffs(comparison)

all_diffs <- diffs(comparison)
diffs_summary <- as.data.frame(diffs(comparison, by.var = TRUE))

#export diffs as csv
#export(diffs_summary, "diffs_by_nums_of_var_demographics_newid.csv")
#export(all_diffs, "diffs_by_var_values_demographics_newid.csv")






