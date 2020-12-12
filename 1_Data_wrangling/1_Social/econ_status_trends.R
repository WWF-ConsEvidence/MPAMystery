
#setwd("D:/Dropbox/MPA_research/MPAMystery/")
pacman::p_load(cowplot,stargazer,psych, sf, rio, janitor,tidytext, plotly, tidyverse)
#resultPath <- "D:/Dropbox/MPA_research/"
#resultPath <- "C:/Users/dtl20/Dropbox/MPA_research/"

# input.directory 
input.dir <- "C:/Users/marle/OneDrive/Desktop/Independent Study/"
input.dir <- "C:/Users/dag/Dropbox/data/analysis/BHS/"

# please clean names in original excel sheet ahead of time
econ.dat <- rio::import(paste0(input.dir,"Copy of Economic_StatusReason_subGroup_coding_withGender_14799.xlsx"),
                                        skip=2) %>% 
  rename(ngo_livelihood="Livelihood...15",
         ngo_education="Education...16",
         ngo_general="General...17",
         gov_livelihood="Livelihood...19",
         gov_education="Education...20",
         gov_general="General...21") %>% 
  clean_names() %>% 
  mutate(economic_status_trend=ifelse(economic_status_trend>5,NA,economic_status_trend),
         gender=as.factor(ifelse(male==1,"male","female")),
         fisher=ifelse(primary_livelihood==3,1,0)) # check

names(econ.dat)

# Econ status trends
summary(econ.dat$economic_status_trend)

# by gender (can get other values as well, just using average and median as example)
econ.dat %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>%
  summarise(avg=mean(economic_status_trend,na.rm = T),
            median=median(economic_status_trend,na.rm = T))

# by fisher
econ.dat %>% 
  filter(!is.na(fisher)) %>% 
  group_by(fisher) %>%
  summarise(avg=mean(economic_status_trend,na.rm = T),
            median=median(economic_status_trend,na.rm = T))


#---- Drivers
econ.dat.gp <- econ.dat %>%
  gather("topic", "response", ngo_livelihood:exogenous_general) %>% # convert columns to rows
  filter(!is.na(response)) %>% 
  mutate(response=as.factor(response)) %>% 
  select(household_id_rand:secondary_livelihood,fisher,economic_status_trend,topic,response)

head(econ.dat.gp)  

# now there is a row for every topic selected. We can then group, sum and filter as we wish

# top responses ( 1s only)
topic.gp.1 <- econ.dat.gp %>% 
  filter(response==1) %>% 
  group_by(topic,response) %>% 
  count() %>% 
  arrange(-n)

topic.gp.1 %>% view()

# ugly plot
ggplot(topic.gp.1, aes(x = topic, y=n)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "top")
# better plot (you can also trim it to the top 10 responses or group by higher level categories)
ggplot(topic.gp.1, aes(x = reorder(topic,n), y=n)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "top") +
  coord_flip()

# top responses for 1s and 2s
topic.gp <- econ.dat.gp %>% 
  filter(response%in%c(1,2)) %>% 
  group_by(topic,response) %>% 
  count() %>% 
  arrange(-n)

topic.gp %>% view()

ggplot(topic.gp, aes(x = reorder(topic,n), y=n)) +
  geom_bar(aes(fill=response), stat = "identity") +
  theme(legend.position = "top") +
  coord_flip()

# as percentages
topic.gp.pct <- econ.dat.gp %>% 
  filter(response%in%c(1,2)) %>% 
  group_by(topic,response) %>% 
  count() %>% 
  group_by(topic) %>% 
  mutate(n.topic=sum(n),pct=n/n.topic)

topic.gp.pct %>% view()

cols <- c("1" = "skyblue2", "2" = "orangered")
ggplot(topic.gp.pct, aes(x = reorder(topic,n), y=pct)) +
  geom_bar(aes(fill=response), stat = "identity") +
  theme(legend.position = "top") +
  scale_fill_manual(values = cols) +
  coord_flip()


# other plots
topic.gender.gp <-  econ.dat.gp %>% 
                      filter(response%in%c(1,2) & !is.na(gender)) %>% 
                      group_by(topic,gender) %>% 
                      count() %>% 
                      arrange(-n)

topic.gender.gp %>% view()

ggplot(topic.gender.gp, aes(x = reorder(topic,n), y=n)) +
  geom_bar(aes(fill=gender), stat = "identity") +
  theme(legend.position = "top") +
  coord_flip()

# e.g. total NGO responses (please do this for all individual topics so you can tally by groups)
ngo.topics <- c("ngo_livelihood","ngo_general","ngo_education")

econ.dat.gp <- econ.dat.gp %>% 
  mutate(ngo=ifelse(topic %in% c(ngo.topics),1,0))
econ.dat.gp %>% 
  filter(ngo==1) %>% 
    group_by(topic,response) %>% 
    count()








# # Function to clean string variables (lower case, remove punctuation)

# trim <- function(x) gsub("^//s+|//s+$","",x)
# 
# str_clean <- function(strings) {
#   require(dplyr)
#   require(tm)
#   strings %>% tolower() %>% removePunctuation(preserve_intra_word_dashes = TRUE) %>% stripWhitespace() %>% 
#     trim()
# }
# 
# unclear <- unique(econ.dat$Unclear)
# unclear_more <- c("income","results", "NA", "depend", "depends", "depending", " ", "999","997", "change", "increased", "additional", "this_is_number")
# 
# #library(stringr)
# 
# # create cleaned word dataframe
# words.df <- econ.dat %>% 
#   mutate(economic_status_reason_english=str_clean(economic_status_reason_english), 
#          Gender=ifelse(male==1, "Male", "Female"), 
#          Livelihood=ifelse(primary_livelihood==3, "Fisher", "Non-fisher"), 
#        #  Residence=ifelse(yrResident.above==1, "Indigenous", "Non-indigenous"), 
#        #  Age=ifelse(age.retired==1, "Retired", "Working")
#        ) %>% 
#   # split sentences to words, filter out unimportant words
#   unnest_tokens(output=word.val, input=economic_status_reason_english) %>% 
#   filter(!word.val %in% stop_words$word) %>% 
#   mutate(word.val = ifelse(str_detect(word.val, "[0-9]+")==FALSE, word.val,"this_is_number")) %>%  #change all numbers into "this_is_numbers"
#   filter(!word.val %in% c(unclear,unclear_more)) %>% 
#   filter(!is.na(word.val))  
# 
# head(words.df)

# words.df %>% 
# group_by(economic_status_reason_english, Gender, Livelihood) %>% 
# summarize(n=count(economic_status_trend), mean=mean(economic_status_trend), median=median(economic_status_trend))
# 
# 
# summary.stat.df <- econ.dat %>%
#   group_by(as.integer(male)) %>%
#   summarise(mean = mean(economic_status_trend,  na.rm = TRUE),
#             median = median(economic_status_trend,  na.rm = TRUE),
#             count = n())  
# 
# summary.stat.df
# 
# 
# econ.dat %>%
# Livelihood <- Livelihood=ifelse(Fisher==1, "Fisher", Non-fisher")
# group_by(factor(Livelihood)) %>%
#   summarise(mean = mean(EconomicStatusTrend,  na.rm = TRUE),
#             median = median(EconomicStatusTrend,  na.rm = TRUE),
#             count = count(EconomicStatusTrend,  na.rm = TRUE))  %>%
#   mutate(Livelihood=ifelse(Fisher==1, "Fisher", Non-fisher")) 
# 
# summary.stat.df

