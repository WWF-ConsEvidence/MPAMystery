##----------04/13/2020-----------------##
##----------Word Frequency by Subgroups-----------------##

#setwd("D:/Dropbox/MPA_research/MPAMystery/")
pacman::p_load(lfe,cowplot,stargazer,broom,qvalue,psych,factoextra,ineq, sf, rio, tidytext, plotly, tidyverse)
#resultPath <- "D:/Dropbox/MPA_research/"
#resultPath <- "C:/Users/dtl20/Dropbox/MPA_research/"

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
unclear_more <- c("income","results", "NA", "depend", "depends", "depending", " ", "999","997", "change", "increased", "additional", "this_is_number")

library(stringr)

# create cleaned word dataframe
words.df <- subgroup.EconResponse.df %>% 
  mutate(EconomicStatusReasonEnglish=str_clean(EconomicStatusReasonEnglish), 
         Gender=ifelse(Male==1, "Male", "Female"), 
         Livelihood=ifelse(Fisher==1, "Fisher", "Non-fisher"), 
         Residence=ifelse(yrResident.above==1, "Indigenous", "Non-indigenous"), 
         Age=ifelse(age.retired==1, "Retired", "Working")) %>% 
  # split sentences to words, filter out unimportant words
  unnest_tokens(output=word.val, input=EconomicStatusReasonEnglish) %>% 
  filter(!word.val %in% stop_words$word) %>% 
  mutate(word.val = ifelse(str_detect(word.val, "[0-9]+")==FALSE, word.val,"this_is_number")) %>%  #change all numbers into "this_is_numbers"
  filter(!word.val %in% unclear) %>% 
  filter(!word.val %in% unclear_more)  %>% 
  na.omit()

head(words.df)

#---------------------------------------------------------------------------------------------------------#
#--- 1. Plots by Gender (what are the top words/top distinct words for gender group; for 
#---a) MPA region-before; b) MPA region-after; c) non-MPA region-before; d) non-MPA region-after)

#---a) MPA region-before;
words10.Gender.MPA.before <- words.df %>% 
  filter(Treatment==1, yearsPost==0) %>% 
  group_by(Gender) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Gender, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Gender) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=0)

#---b) MPA region-after;
words10.Gender.MPA.after <- words.df %>% 
  filter(Treatment==1, yearsPost>0) %>% 
  group_by(Gender) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Gender, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Gender) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=1)

#---c) nonMPA region-before;
words10.Gender.nonMPA.before <- words.df %>% 
  filter(Treatment==0, yearsPost==0) %>% 
  group_by(Gender) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Gender, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Gender) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=0)

#---d) nonMPA region-after;
words10.Gender.nonMPA.after <- words.df %>% 
  filter(Treatment==0, yearsPost>0) %>% 
  group_by(Gender) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Gender, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Gender) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=1)


#---combine);
words10.Gender <- rbind(words10.Gender.MPA.before, words10.Gender.MPA.after, words10.Gender.nonMPA.before, words10.Gender.nonMPA.after)

# # use the bind_tf_idf to get all important/meaning words in each econ status (put less weight on common words appear in all MPAs such as "the", "a", "because" etc)
# words.by.Gender <- words.by.Gender %>%
#   bind_tf_idf(word.val, Gender, Gender.count)

##---------------------------------------------------------------------#
#visualize by plotting  most words by status 
MPA.pre <- ggplot(filter(words10.Gender, Treatment==1, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Gender)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel A: MPA-Before") + scale_color_grey() +
  facet_wrap(~Gender, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

MPA.post <- ggplot(filter(words10.Gender, Treatment==1, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Gender)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel B: MPA-After") + scale_color_grey() +
  facet_wrap(~Gender, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.pre <- ggplot(filter(words10.Gender, Treatment==0, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Gender)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel C: Control-Before") + scale_color_grey() +
  facet_wrap(~Gender, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.post <- ggplot(filter(words10.Gender, Treatment==0, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Gender)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel D: Control-After") + scale_color_grey() +
  facet_wrap(~Gender, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

##combine the 4 plots
plot_grid(MPA.pre, .MPA.post, nonMPA.pre, nonMPA.post, ncol=2)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/wellbeing_words/Gender_words.png"), width=14, height=10)

#ggsave("D:/Dropbox/MPA_research/Paper 1-MPA and Equity/results/2020/plots/wellbeing_words/Gender_words.png", width=14, height=10)




#---------------------------------------------------------------------------------------------------------#
#--- 2. Plots by Residence (what are the top words/top distinct words for gender group; for 
#---a) MPA region-before; b) MPA region-after; c) non-MPA region-before; d) non-MPA region-after)

#---a) MPA region-before;
words10.Livelihood.MPA.before <- words.df %>% 
  filter(Treatment==1, yearsPost==0) %>% 
  group_by(Livelihood) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Livelihood, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Livelihood) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=0)

#---b) MPA region-after;
words10.Livelihood.MPA.after <- words.df %>% 
  filter(Treatment==1, yearsPost>0) %>% 
  group_by(Livelihood) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Livelihood, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Livelihood) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=1)

#---c) nonMPA region-before;
words10.Livelihood.nonMPA.before <- words.df %>% 
  filter(Treatment==0, yearsPost==0) %>% 
  group_by(Livelihood) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Livelihood, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Livelihood) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=0)

#---d) nonMPA region-after;
words10.Livelihood.nonMPA.after <- words.df %>% 
  filter(Treatment==0, yearsPost>0) %>% 
  group_by(Livelihood) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Livelihood, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Livelihood) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=1)

#---combine);
words10.Livelihood <- rbind(words10.Livelihood.MPA.before, words10.Livelihood.MPA.after, words10.Livelihood.nonMPA.before, words10.Livelihood.nonMPA.after)

##---------------------------------------------------------------------#
#visualize by plotting  most words by status 
MPA.pre <- ggplot(filter(words10.Livelihood, Treatment==1, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Livelihood)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel A: MPA-Before") + scale_color_grey() +
  facet_wrap(~Livelihood, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

MPA.post <- ggplot(filter(words10.Livelihood, Treatment==1, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Livelihood)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel B: MPA-After") + scale_color_grey() +
  facet_wrap(~Livelihood, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.pre <- ggplot(filter(words10.Livelihood, Treatment==0, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Livelihood)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel C: Control-Before") + scale_color_grey() +
  facet_wrap(~Livelihood, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.post <- ggplot(filter(words10.Livelihood, Treatment==0, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Livelihood)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel D: Control-After") + scale_color_grey() +
  facet_wrap(~Livelihood, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

##combine the 4 plots
plot_grid(MPA.pre, MPA.post, nonMPA.pre, nonMPA.post, ncol=2)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/wellbeing_words/Livelihood_words.png"), width=14, height=10)





#---------------------------------------------------------------------------------------------------------#
#--- 3. Plots by Residence (what are the top words/top distinct words for gender group; for 
#---a) MPA region-before; b) MPA region-after; c) non-MPA region-before; d) non-MPA region-after)

#---a) MPA region-before;
words10.Residence.MPA.before <- words.df %>% 
  filter(Treatment==1, yearsPost==0) %>% 
  group_by(Residence) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Residence, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Residence) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=0)

#---b) MPA region-after;
words10.Residence.MPA.after <- words.df %>% 
  filter(Treatment==1, yearsPost>0) %>% 
  group_by(Residence) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Residence, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Residence) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=1)

#---c) nonMPA region-before;
words10.Residence.nonMPA.before <- words.df %>% 
  filter(Treatment==0, yearsPost==0) %>% 
  group_by(Residence) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Residence, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Residence) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=0)

#---d) nonMPA region-after;
words10.Residence.nonMPA.after <- words.df %>% 
  filter(Treatment==0, yearsPost>0) %>% 
  group_by(Residence) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Residence, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Residence) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=1)

#---combine);
words10.Residence <- rbind(words10.Residence.MPA.before, words10.Residence.MPA.after, words10.Residence.nonMPA.before, words10.Residence.nonMPA.after)

##---------------------------------------------------------------------#
#visualize by plotting  most words by status 
MPA.pre <- ggplot(filter(words10.Residence, Treatment==1, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Residence)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel A: MPA-Before") + scale_color_grey() +
  facet_wrap(~Residence, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

MPA.post <- ggplot(filter(words10.Residence, Treatment==1, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Residence)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel B: MPA-After") + scale_color_grey() +
  facet_wrap(~Residence, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.pre <- ggplot(filter(words10.Residence, Treatment==0, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Residence)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel C: Control-Before") + scale_color_grey() +
  facet_wrap(~Residence, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.post <- ggplot(filter(words10.Residence, Treatment==0, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Residence)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel D: Control-After") + scale_color_grey() +
  facet_wrap(~Residence, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

##combine the 4 plots
plot_grid(MPA.pre, MPA.post, nonMPA.pre, nonMPA.post, ncol=2)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/wellbeing_words/Residence_words.png"), width=14, height=10)



#---------------------------------------------------------------------------------------------------------#
#--- 4. Plots by Age (what are the top words/top distinct words for gender group; for 
#---a) MPA region-before; b) MPA region-after; c) non-MPA region-before; d) non-MPA region-after)

#---a) MPA region-before;
words10.Age.MPA.before <- words.df %>% 
  filter(Treatment==1, yearsPost==0) %>% 
  group_by(Age) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Age, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Age) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=0)

#---b) MPA region-after;
words10.Age.MPA.after <- words.df %>% 
  filter(Treatment==1, yearsPost>0) %>% 
  group_by(Age) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Age, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Age) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=1, Post=1)

#---c) nonMPA region-before;
words10.Age.nonMPA.before <- words.df %>% 
  filter(Treatment==0, yearsPost==0) %>% 
  group_by(Age) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Age, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Age) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=0)

#---d) nonMPA region-after;
words10.Age.nonMPA.after <- words.df %>% 
  filter(Treatment==0, yearsPost>0) %>% 
  group_by(Age) %>% 
  mutate(sum=n_distinct(HouseholdID)) %>% 
  group_by(Age, word.val,sum) %>% 
  summarise(count=n_distinct(HouseholdID),
            pct.full=count/mean(sum)) %>% 
  mutate(pct=round(pct.full, digits = 2)) %>% 
  arrange(-count) %>% 
  group_by(Age) %>% 
  top_n(10, count) %>% 
  mutate(word.rank= rank(-pct, ties.method = 'first'),
         Treatment=0, Post=1)

#---combine);
words10.Age <- as.data.frame()
words10.Age <- rbind(words10.Age.MPA.before, words10.Age.MPA.after, words10.Age.nonMPA.before, words10.Age.nonMPA.after)

##---------------------------------------------------------------------#
#visualize by plotting  most words by status 
MPA.pre <- ggplot(filter(words10.Age, Treatment==1, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Age)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel A: MPA-Before") + scale_color_grey() +
  facet_wrap(~Age, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

MPA.post <- ggplot(filter(words10.Age, Treatment==1, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Age)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel B: MPA-After") + scale_color_grey() +
  facet_wrap(~Age, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.pre <- ggplot(filter(words10.Age, Treatment==0, Post==0), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Age)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel C: Control-Before") + scale_color_grey() +
  facet_wrap(~Age, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

nonMPA.post <- ggplot(filter(words10.Age, Treatment==0, Post==1), aes(x=reorder(word.val, -word.rank),  y=pct,  color = Age)) +
  geom_col(show.legend = FALSE, fill="white",  width = 0.5, size=1) +
  labs(x = NULL, y = "Word Frequency", title="Panel D: Control-After") + scale_color_grey() +
  facet_wrap(~Age, ncol = 2, scales = "free") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

##combine the 4 plots
plot_grid(MPA.pre, MPA.post, nonMPA.pre, nonMPA.post, ncol=2)
ggsave(paste0(resultPath,"Paper 1-MPA and Equity/results/2020/plots/wellbeing_words/Age_words.png"), width=14, height=10)
