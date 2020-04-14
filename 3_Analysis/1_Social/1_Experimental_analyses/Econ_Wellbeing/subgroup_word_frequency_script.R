##----------04/13/2020-----------------##
##----------Word Frequency by Subgroups-----------------##

#setwd("D:/Dropbox/MPA_research/MPAMystery/")
pacman::p_load(lfe,cowplot,stargazer,broom,qvalue,psych,factoextra,ineq, sf, rio, tidytext, plotly, tidyverse)
#resultPath <- "D:/Dropbox/MPA_research/"
resultPath <- "C:/Users/dtl20/Dropbox/MPA_research/"

subgroup.EconResponse.df <- rio::import("R:/Gill/research/ind-soc-impacts/Economic_StatusReason/Economic_StatusReason_04142020.xlsx")

# Function to clean string variables (lower case, remove punctuation)
trim <- function(x) gsub("^\\s+|\\s+$","",x)

str_clean <- function(strings) {
  require(dplyr)
  require(tm)
  strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
    trim()
}

unclear <- unique(subgroup.EconResponse.df$Unclear)
#---------------------------------------------------------------------------------------------------------#
#--- 1. Plots by Gender (what are the top words/top distinct words for each)
Gender.out <- subgroup.EconResponse.df %>% 
  select(Male, EconomicStatusReasonEnglish) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish), 
         Gender=ifelse(Male==1, "Male", "Female"))

words.by.Gender.n <- Gender.out %>% 
  group_by(Gender) %>% 
  summarise(Gender.count=n()) 

# find all words apear by each status, remove stop_words, count replication of each remaining words
words.by.Gender <- Gender.out %>% 
  # filter(MPAID <= 6) %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish)) %>% 
  unnest_tokens(output=wordsBySubGroup, input=EconomicStatusReasonEnglish) %>% 
  filter(!wordsBySubGroup %in% stop_words$word) %>% 
  filter(!wordsBySubGroup %in% unclear) %>% 
  #filter(!wordsBySubGroup %in% c("due","stable","cost", "2")) %>% 
  count(Gender, wordsBySubGroup, sort = TRUE) %>% 
  left_join(words.by.Gender.n,by="Gender")  
 
# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.Gender <- words.by.Gender %>%
  bind_tf_idf(wordsBySubGroup, Gender, n)

##---------------------------------------------------------------------#
#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change
words.by.Gender %>%
  arrange(desc(tf_idf)) %>%
  mutate(wordsBySubGroup = factor(wordsBySubGroup, levels = rev(unique(wordsBySubGroup)))) %>% 
  group_by(Gender) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsBySubGroup, tf_idf, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_distinct.pdf'),width = 12, height = 8)



# 2. top all words (not distinct) by econ status change
words.by.Gender %>%
  arrange(desc(n)) %>%
  #mutate(wordsBySubGroup = factor(wordsBySubGroup, levels = rev(unique(wordsBySubGroup)))) %>% 
  group_by(Gender) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(wordsBySubGroup, n, fill = Gender)) +
  #ggplot(aes(x = reorder(f.name, -age), y = age))
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word Counts") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()
ggsave(paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_full.pdf'),width = 12, height = 8)

export(words.by.Gender,paste0(resultPath,'Paper 1-MPA and Equity/results/plots/subGroup_word_frequency/ByGender_full.xlsx'))



test <- words.by.Gender %>%
  arrange(desc(n)) %>%
  group_by(Gender) %>% 
  top_n(10)
