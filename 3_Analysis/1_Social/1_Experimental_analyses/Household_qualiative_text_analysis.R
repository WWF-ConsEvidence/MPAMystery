#---- Text Analysis: Processing Household Responses to Economics Status and Trend ----
# author: Duong Le, David Gill
# November 15, 2019

# This script imports raw excel translation and performs/outputs word counts for all meaningful words (i.e. remove all stop words) detected in the household responses
# Identifying themes relevant to each household answer based on pre-specified word criteria
# Plot Word Frequency graphs
#---- Import  tables ----
Econ.reason <- import("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Economic_StatusReason_20190914-coding.xlsx")

pacman::p_load(cowplot, Matrix, stargazer, broom, foreign, writexl, tidytext, tidyverse)

#--------------------------------------------------------------------------------------------------#
#--Import data, clean, analyze word frequency with tidy text
#--------------------------------------------------------------------------------------------------#
# 1.---Create text cleaning functions
# 1a. Function to remove all white space in string variables
trim <- function(x) gsub("^\\s+|\\s+$","",x)

# 1b.Function to clean string variables (lower case, remove punctuation)
str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}

Econ.reason.words.df <- Econ.reason %>% 
  select(EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish))


# 2a. Count frequencies of words for EconomicStatusReasonEnglish
econ.reason.words.rank <- Econ.reason.words.df %>% 
  unnest_tokens(word, EconomicStatusReasonEnglish) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

# 2b. Export word frequencies to txt and xls file
write.table(econ.reason.text.df, "R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Word_frequency_result.txt", sep="\t")
write_xlsx(econ.reason.text.df, path = "R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Word_frequency_result", col_names = TRUE, format_headers = TRUE)



#--------------------------------------------------------------------------------------------------------------#
# Marley has worked with the Word_frequency_result excel output above to manually categorize each word into our identified set of important categories
# The ultimate goal is to get word association: generate a pool of relevant words to each defined category
# Next step (go to econ_reason_categories.R) is to use these word pools to do a "grepl" to categorize each household response (full sentence) into the right categories of economic reasons
#--------------------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#
#--Word categorization exercise
#--------------------------------------------------------------------------------------------------#
pcdir <- "//nsoe-files.nicholas.duke.edu/NSOE/Research/Gill/research/ind-soc-impacts"
inputdir <- paste0(pcdir, "/Economic_StatusReason/")
outputdir <- paste0(pcdir, "/Economic_StatusReason/")
options(scipen=999,stringsAsFactors = FALSE)
today.date <- gsub("-","",Sys.Date())
last.file <- function(dir.nam,nam){
  import(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))}
#source(paste0(codedir,"my_summary_plot_functions.R"))


input.data <- import("C:/Users/marle/OneDrive/Desktop/Independent Study/Economic_StatusReason_20190914-coding.xlsx")
word.data <- last.file(inputdir, "Word Frequency")

head(input.data)
head(word.data)
names(word.data);names(input.data)

# word list
unclear <- unique(word.data$Unclear)
income <- unique(word.data$Income)
expend <- unique(word.data$Expenditure)
marine <- unique(word.data$Marine)
agr <- unique(word.data$Agriculture)
forestry <- unique(word.data$Forestry)
tourism <- unique(word.data$Tourism)
gov.assist <- unique(word.data$Gov.assistance)
ngo.assist <- unique(word.data$NGO.assistance)
remit <- unique(word.data$Remittances)
asset <- unique(word.data$Assets.change)
educ <- unique(word.data$Education)
social <- unique(word.data$Social)
religion <- unique(word.data$Religious)
family <- unique(word.data$family)
climate <- unique(word.data$climate)
market <- unique(word.data$market)


# --- preparing data file for text analysis (word frequency and word distinction)
# --- Modify: only focus on 6 BHS MPAs now
classification.out <- input.data %>% 
  select(HouseholdID:EconomicStatusReasonEnglish) %>% 
  left_join(select(HHData,HouseholdID, Treatment, yearsPost), by="HouseholdID") %>% 
  filter(MPAID<=6) %>% 
    mutate(unclear=ifelse(grepl(paste(unclear,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         income=ifelse(grepl(paste(income,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         expend=ifelse(grepl(paste(expend,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         marine=ifelse(grepl(paste(marine,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         agr=ifelse(grepl(paste(agr,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         forestry=ifelse(grepl(paste(forestry,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         tourism=ifelse(grepl(paste(tourism,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         gov.assist=ifelse(grepl(paste(gov.assist,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         ngo.assist=ifelse(grepl(paste(ngo.assist,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         remit=ifelse(grepl(paste(remit,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         asset=ifelse(grepl(paste(asset,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         educ=ifelse(grepl(paste(educ,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         social=ifelse(grepl(paste(social,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         religion=ifelse(grepl(paste(religion,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         family=ifelse(grepl(paste(family,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         climate=ifelse(grepl(paste(climate,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0),
         market=ifelse(grepl(paste(market,collapse = '|'),EconomicStatusReasonEnglish,ignore.case = T,perl=T),1,0))


export(classification.out,paste0(outputdir,"Economic_StatusReason_Classification_OUtput_20191126.csv"))


#--------------------------------------------------------------------------------------------------#
#--Word Frequency  Analysis Plots 
#--------------------------------------------------------------------------------------------------#

library(tidytext)

# Function to clean string variables (lower case, remove punctuation)
trim <- function(x) gsub("^\\s+|\\s+$","",x)

str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}

data(stop_words)

#--- 1. Plots by Economic Changes (Econ status: 1->5; what are the top words/top distinct words for each)
econStatus.out <- classification.out %>% 
  select(EconomicStatusTrend, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish))

words.by.status.n <- econStatus.out %>% 
  group_by(EconomicStatusTrend) %>% 
  summarise(status.count=length(EconomicStatusTrend)) 

# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.status <- econStatus.out %>% 
  filter(EconomicStatusTrend<=5) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByStatus, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByStatus %in% stop_words$word) %>% 
  filter(!wordsByStatus %in% unclear) %>% 
  filter(!wordsByStatus %in% c("due","stable","cost", "2")) %>% 
  count(EconomicStatusTrend, wordsByStatus, sort = TRUE) %>% 
  left_join(words.by.status.n,by="EconomicStatusTrend") %>% 
  mutate(EconomicStatusTrend_name = ifelse(EconomicStatusTrend==1, paste0("1-Decreasing (n=",status.count,")"),
                                           ifelse(EconomicStatusTrend==2,paste0("2-Slightly Decreasing (n=",status.count,")"),
                                                  ifelse(EconomicStatusTrend==3,paste0("3-Stable (n=",status.count,")"),
                                                         ifelse(EconomicStatusTrend==4,paste0("4-Slightly Increasing (n=",status.count,")"),
                                                                ifelse(EconomicStatusTrend==5,paste0("5-Increasing (n=",status.count,")"), "")))))) 

# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.status <- words.by.status %>%
  bind_tf_idf(wordsByStatus, EconomicStatusTrend, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.status %>%
  filter(!(EconomicStatusTrend==3)) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(wordsByStatus = factor(wordsByStatus, levels = rev(unique(wordsByStatus)))) %>% 
  group_by(EconomicStatusTrend_name) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByStatus, tf_idf, fill = EconomicStatusTrend_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~EconomicStatusTrend_name, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByEconStatus_distinct.pdf"),width = 12, height = 6)


# 2. top all words (not distinct) by econ status change
words.by.status %>%
  filter(!(EconomicStatusTrend==3)) %>% 
  arrange(desc(n)) %>%
  mutate(wordsByStatus = factor(wordsByStatus, levels = rev(unique(wordsByStatus)))) %>% 
  group_by(EconomicStatusTrend_name) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByStatus, n, fill = EconomicStatusTrend_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~EconomicStatusTrend_name, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByEconStatus.pdf"),width = 12, height = 6)


#---------------------------------------------------------------------------------------------------------#

#--- 2. Plots by MPA (8 MPAs; what are the top words/top distinct words for each)
MPAID.out <- classification.out %>% 
  select(MPAID, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish))

words.by.MPA.n <- MPAID.out %>% 
  group_by(MPAID) %>% 
  summarise(MPA.count=length(MPAID)) 

# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.MPA <- MPAID.out %>% 
  filter(MPAID <= 6) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByMPA, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByMPA %in% stop_words$word) %>% 
  filter(!wordsByMPA %in% unclear) %>% 
  filter(!wordsByMPA %in% c("due","stable","cost", "2")) %>% 
  count(MPAID, wordsByMPA, sort = TRUE) %>% 
  left_join(words.by.MPA.n,by="MPAID") %>% 
  mutate(MPAName_short = ifelse(MPAID==1,paste0("1-Telma (n=",MPA.count,")"),
                                ifelse(MPAID==2,paste0("2-TNTC (n=",MPA.count,")"),
                                       ifelse(MPAID==3,paste0("3-Kaimana (n=",MPA.count,")"),
                                              ifelse(MPAID==4,paste0("4-Kofiau (n=",MPA.count,")"),
                                                     ifelse(MPAID==5,paste0("5-Dampier (n=",MPA.count,")"),
                                                            ifelse(MPAID==6,paste0("6-Misool (n=",MPA.count,")"),
                                                                   ifelse(MPAID==15,paste0("15-Selat Pantar (n=",MPA.count,")"),
                                                                          ifelse(MPAID==16,paste0("16-Flores Timur (n=",MPA.count,")"),""))))))))) 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.MPA <- words.by.MPA %>%
  bind_tf_idf(wordsByMPA, MPAID, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.MPA %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsByMPA = factor(wordsByMPA, levels = rev(unique(wordsByMPA)))) %>% 
  group_by(MPAName_short) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByMPA, tf_idf, fill = MPAName_short)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~MPAName_short, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByMPA_distinct.pdf"),width = 12, height = 8)


# 2. top all words (not distinct) by econ status change
words.by.MPA %>%
  arrange(desc(n)) %>%
  mutate(wordsByMPA = factor(wordsByMPA, levels = rev(unique(wordsByMPA)))) %>% 
  group_by(MPAName_short) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByMPA, n, fill = MPAName_short)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~MPAName_short, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByMPA.pdf"),width = 12, height = 8)







#---------------------------------------------------------------------------------------------------------#

#--- 3. Plots by occupation (7 occupations; what are the top words/top distinct words for each)
job.out <- classification.out %>% 
  select(`Primary Livelihood` , EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish),
         job = `Primary Livelihood` ) %>% 
  select(job, EconomicStatusReasonEnglish)

words.by.job.n <- job.out %>% 
  group_by(job) %>% 
  summarise(job.count=length(job)) 
#only 3 obs Aquaculture and 2 obs Marine Extraction --> remove those plots


# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.job <- job.out %>% 
  filter(job <= 7 & !(job==4 |job==5)) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByJob, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByJob %in% stop_words$word) %>% 
  filter(!wordsByJob %in% unclear) %>% 
  filter(!wordsByJob %in% c("due","stable","cost", "2")) %>% 
  count(job, wordsByJob, sort = TRUE) %>% 
  left_join(words.by.job.n,by="job") %>% 
  mutate(job_type = ifelse(job==1,paste0("1-Farming (n=",job.count,")"),
                                ifelse(job==2,paste0("2-Forestry (n=",job.count,")"),
                                       ifelse(job==3,paste0("3-Fishing (n=",job.count,")"),
                                              ifelse(job==4,paste0("4-Aquaculture (n=",job.count,")"),
                                                     ifelse(job==5,paste0("5-Marine Resource Extraction (n=",job.count,")"),
                                                            ifelse(job==6,paste0("4-Marine Tourism (n=",job.count,")"),
                                                                   ifelse(job==7,paste0("5-Other Wage Labor (n=",job.count,")"),"")))))))) 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.job <- words.by.job %>%
  bind_tf_idf(wordsByJob, job_type, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.job %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsByJob = factor(wordsByJob, levels = rev(unique(wordsByJob)))) %>% 
  group_by(job_type) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByJob, tf_idf, fill = job_type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~job_type, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByJob_distinct.pdf"),width = 12, height = 8)


# 2. top all words (not distinct) by econ status change
words.by.job %>%
  arrange(desc(n)) %>%
  mutate(wordsByJob = factor(wordsByJob, levels = rev(unique(wordsByJob)))) %>% 
  group_by(job_type) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByJob, n, fill = job_type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~job_type, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/new_20191126/ByJob.pdf"),width = 12, height = 8)








#---------------------------------------------------------------------------------------------------------#

#--- 4. Plots by yearsPost (3 yearsPost; what are the top words/top distinct words for each)
year.out <- classification.out %>% 
  select(yearsPost, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  select(yearsPost, EconomicStatusReasonEnglish) %>% 
  mutate(yearsPost= ifelse(yearsPost==0, "Baseline",
                           ifelse(yearsPost==2|yearsPost==3, "Postline 1", 
                                  ifelse(yearsPost>3 & yearsPost<10, "Postline 2", " "))))

# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.year <- year.out %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByYear, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByYear %in% stop_words$word) %>% 
  filter(!wordsByYear %in% unclear) %>% 
  count(yearsPost, wordsByYear, sort = TRUE) 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.year <- words.by.year %>%
  bind_tf_idf(wordsByYear, yearsPost, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.year %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsByYear = factor(wordsByYear, levels = rev(unique(wordsByYear)))) %>% 
  group_by(yearsPost) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(wordsByYear, tf_idf, fill = yearsPost)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~yearsPost, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/ByYear_distinct.pdf"),width = 12, height = 8)


# 2. top all words (not distinct) by econ status change
words.by.job %>%
  arrange(desc(n)) %>%
  mutate(wordsByYear = factor(wordsByYear, levels = rev(unique(wordsByYear)))) %>% 
  group_by(yearsPost) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(wordsByYear, n, fill = yearsPost)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~yearsPost, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/ByYear.pdf"),width = 12, height = 8)



#---------------------------------------------------------------------------------------------------------#

#--- 5. Marine-related occupation only (Job 3->6): Plots by MPA (8 MPAs; what are the top words/top distinct words for each)
MPAID.out <- classification.out %>% 
  select(MPAID, `Primary Livelihood`, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish))

# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.MPA <- MPAID.out %>% 
  filter(MPAID <= 16) %>% 
  filter(`Primary Livelihood` %in% 3:6) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByMPA, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByMPA %in% stop_words$word) %>% 
  filter(!wordsByMPA %in% unclear) %>% 
  count(MPAID, wordsByMPA, sort = TRUE) %>% 
  mutate(MPAName_short = ifelse(MPAID==1,"1 - Telma",
                                ifelse(MPAID==2,"2 - TNTC",
                                       ifelse(MPAID==3,"3 - Kaimana",
                                              ifelse(MPAID==4,"4 - Kofiau",
                                                     ifelse(MPAID==5,"5 - Dampier",
                                                            ifelse(MPAID==6,"6 - Misool",
                                                                   ifelse(MPAID==15,"7 - Selat Pantar",
                                                                          ifelse(MPAID==16,"8 - Flores Timur",""))))))))) 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.MPA <- words.by.MPA %>%
  bind_tf_idf(wordsByMPA, MPAID, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.MPA %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsByMPA = factor(wordsByMPA, levels = rev(unique(wordsByMPA)))) %>% 
  group_by(MPAName_short) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByMPA, tf_idf, fill = MPAName_short)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~MPAName_short, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/MarineOnly_ByMPA_distinct.pdf"),width = 12, height = 8)


# 2. top all words (not distinct) by econ status change
words.by.MPA %>%
  arrange(desc(n)) %>%
  mutate(wordsByMPA = factor(wordsByMPA, levels = rev(unique(wordsByMPA)))) %>% 
  group_by(MPAName_short) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsByMPA, n, fill = MPAName_short)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~MPAName_short, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/MarineOnly_ByMPA.pdf"),width = 12, height = 8)



#---------------------------------------------------------------------------------------------------------#

#--- 6. Marine-related occupation only (Job 3->6): Plots by yearsPost (3 yearsPost; what are the top words/top distinct words for each)
year.out <- classification.out %>% 
  select(`Primary Livelihood`, yearsPost, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  select(yearsPost, EconomicStatusReasonEnglish) %>% 
  mutate(yearsPost= ifelse(yearsPost==0, "Baseline",
                           ifelse(yearsPost==2|yearsPost==3, "Postline 1", 
                                  ifelse(yearsPost>3 & yearsPost<10, "Postline 2", " "))))

# get all single words count per econ change status
# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.year <- year.out %>% 
  filter(`Primary Livelihood` %in% 3:6) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsByYear, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsByYear %in% stop_words$word) %>% 
  filter(!wordsByYear %in% unclear) %>% 
  count(yearsPost, wordsByYear, sort = TRUE) 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.year <- words.by.year %>%
  bind_tf_idf(wordsByYear, yearsPost, n)

#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.year %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsByYear = factor(wordsByYear, levels = rev(unique(wordsByYear)))) %>% 
  group_by(yearsPost) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(wordsByYear, tf_idf, fill = yearsPost)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~yearsPost, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/MarineOnly_ByYear_distinct.pdf"),width = 12, height = 8)


# 2. top all words (not distinct) by econ status change
words.by.job %>%
  arrange(desc(n)) %>%
  mutate(wordsByYear = factor(wordsByYear, levels = rev(unique(wordsByYear)))) %>% 
  group_by(yearsPost) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(wordsByYear, n, fill = yearsPost)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~yearsPost, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Plots/MarineOnly_ByYear.pdf"),width = 12, height = 8)



