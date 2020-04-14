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

# create cleaned word dataframe
words.df <- subgroup.EconResponse.df %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish), 
         Gender=ifelse(Male==1, "Male", "Female")) %>% 
  # split sentences to words, filter out unimportant words
  unnest_tokens(output=word.val, input=EconomicStatusReasonEnglish) %>% 
  filter(!word.val %in% stop_words$word) %>% 
  filter(!word.val %in% unclear) 

head(words.df)

#---------------------------------------------------------------------------------------------------------#
#--- 1. Plots by Gender (what are the top words/top distinct words for each)
words.by.Gender <- words.df %>% 
  group_by(Gender) %>% 
  mutate(sum.gender=n_distinct(HouseholdID)) %>% 
  group_by(Gender,word.val,sum.gender) %>% 
  summarise(Gender.count=n_distinct(HouseholdID),
            Gender.pct=round(Gender.count/mean(sum.gender), digits = 2)) %>% 
  arrange(-Gender.count)

# use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
words.by.Gender <- words.by.Gender %>%
  bind_tf_idf(word.val, Gender, Gender.count)

words.by.Gender.top10 <- words.by.Gender %>% 
  group_by(Gender) %>% 
  top_n(10,Gender.count)
##---------------------------------------------------------------------#
#visualize by plotting 1) most words by status and 2) most distinct words by status
# 1. top distinct words by econ status change

ggplot(words.by.Gender.top10, aes(word.val, reorder(Gender.count, -Gender.count) , fill = Gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Word importance (tf-idf)") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()

ggplot(words.by.Gender.top10, aes(word.val, Gender.pct, fill = Gender)) +
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
