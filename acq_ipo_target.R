#----------------------------------------------------------------------------------------------------------
#-THE FOLLOWING STARTS WITH THE OUTPUTS OF 'exploration_v2'.R AND 'clc_cats_words.R'
#----------------------------------------------------------------------------------------------------------
word_cats.1 <- words_cats %>% select(-c("org_description", "categories", "keywords_homepage", "keywords_news", "tweet_words"))

org_full_data.1 <- org_full_data %>% left_join(word_cats.1)

#--Bring in acq & ipo data & merge--
#--NOTE: you need to change the file path to wherever you have saved the data
ipo<-read_csv("YOUR_PATH/ipos.csv")
acq<-read_csv("YOUR_PATH/acquisitions.csv")

org_full_data.2 <- org_full_data.1 %>%
  left_join(ipo) %>%
  left_join(acq)

#--CREATE TARGET VARS--
org_full_data.3 <- org_full_data.2 %>% 
  mutate(acquired = ifelse(is.na(acq_announced_on), 0, 1),
         ipo = ifelse(is.na(ipo_went_public_on), 0, 1),
         acq_ipo = ifelse(acquired==1 | ipo==1, 1, 0))

#--CLEAN UP--
rm(word_cats.1, org_full_data.1, org_full_data.2, acq, ipo)

mean(org_full_data.3$acquired, na.rm=T)
mean(org_full_data.3$ipo, na.rm=T)
mean(org_full_data.3$acq_ipo, na.rm=T)


write_rds(org_full_data.3, "~/Dropbox/Analytics_Capstone/VC/org_full_data.3.RDS")
