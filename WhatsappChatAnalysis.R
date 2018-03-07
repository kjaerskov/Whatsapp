#Whatsapp word analysis
#Load library
library("dplyr")
library("tidytext")
library("ggplot2")
library("tidyr")
library("scales")
library("stringr")
library("wordcloud") # word-cloud generator 
library("lubridate")
library("purrr")
library("broom")


#Clean up
rm(list=ls())
closeAllConnections()

#Read file
tmp <- read.csv("Whatsappchat.csv",   header = FALSE, sep = ";",
                na.strings="", stringsAsFactors = FALSE, fill=TRUE, encoding = "UTF-8")

#Construct data frame
dfRawText <- data_frame(line = 1:nrow(tmp),
                        Text = as.character(gsub("^.*\\w{1}[:][[:blank:]](.*$)","\\1",tmp$V1)),
                        Year = as.character(gsub("^.*((\\d+){4}?).*","\\1",tmp$V1)),
                        Month = as.character(gsub("^.*\\d+{2}[/](\\d+{2})[/]\\d+{4}.*","\\1",tmp$V1)),
                        Date = as.character(gsub("^(\\d+{2}[/]\\d+{2}).*","\\1",tmp$V1)),
                        Time = as.numeric(gsub("^.*((\\d+){2}[.]\\d+).*$","\\1",tmp$V1)),
                        Sender = gsub("^.*[-][[:blank:]]((\\w+){2}).*$","\\1",tmp$V1),
                        Timestamp = gsub("(^.*)[[:blank:]][-].*$","\\1",tmp$V1)
                        )

#Correct timestamps
dfRawText$Timestamp <- gsub("\\/","-",dfRawText$Timestamp)
dfRawText$Timestamp <- gsub(",","",dfRawText$Timestamp)
dfRawText$Timestamp <- gsub("\\.",":",dfRawText$Timestamp)

#Alter common redundant words
dfRawText$Text <- gsub("henter","hente",dfRawText$Text)


#Identify problem rows and replace these rows with data from previous row
problemIdx <- !(dfRawText$Sender %in% c("Frederik", "Hlín"))

for (i in which(problemIdx==TRUE, arr.ind=TRUE)) {
  dfRawText$Year[i] <- dfRawText$Year[i-1]
  dfRawText$Month[i] <- dfRawText$Month[i-1]
  dfRawText$Time[i] <- dfRawText$Time[i-1]
  dfRawText$Sender[i] <- dfRawText$Sender[i-1]
  dfRawText$Date[i] <- dfRawText$Date[i-1]
  dfRawText$Timestamp[i] <- dfRawText$Timestamp[i-1]}

#Concert timestamps to date-time object
dfRawText$Timestamp <- dmy_hm(dfRawText$Timestamp)

#Create data frame with words as tokens
dfTidyText <- dfRawText %>% unnest_tokens(word, Text)

#Read Danish stop words
tmp <- read.csv("DanishStopWords.txt",   header = FALSE, sep = "", encoding = "UTF-8")
StopWords <- data_frame(word = as.character(tmp[,1]))

#Remove stop words from text
dfTidyText <- dfTidyText %>% anti_join(StopWords)

#Visualize word counts per year and sender
dfTidyText %>% 
  count(Year, Sender, word, sort=TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n>50) %>%
  ggplot(aes(word,n, fill=Sender)) +
  facet_wrap(~Year, ncol=3) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() 

#Calculate frequencies grouped by sender
frequency <- dfTidyText %>% 
  mutate(word = str_extract(word, "\\w+")) %>%
  group_by(Sender) %>% 
  count(word, sort = TRUE) %>% 
  left_join(dfTidyText %>% 
              group_by(Sender) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#Spread by Sender
frequency <- frequency %>% 
  select(Sender, word, freq) %>% 
  spread(Sender, freq) %>%
  arrange(Hlín, Frederik)

#Plot
ggplot(frequency, aes(Hlín, Frederik)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

#Calculate the logration of words used by sender
word_ratios <- dfTidyText %>%
  mutate(word = str_extract(word, "[[:alpha:]]+")) %>%
  count(word, Sender) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(Sender, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Hlín / Frederik)) %>%
  arrange(desc(logratio))

#Plot
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(20, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Frederik/Hlín)") +
  scale_fill_discrete(name = "", labels = c("Frederik", "Hlín"))

#filter(!str_detect(word, "^@")) %>%

words_by_time <- dfTidyText %>%
  filter(str_detect(word, "[[:alpha:]]+")) %>%
  filter(Year %in% c(2016, 2017)) %>%
  mutate(time_floor = floor_date(Timestamp, unit = "1 month")) %>%
  count(time_floor, Sender, word) %>%
  ungroup() %>%
  group_by(Sender, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

nested_data <- words_by_time %>%
  nest(-word, -Sender) 

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., family = "binomial")))

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

top_slopes

words_by_time %>%
  inner_join(top_slopes, by = c("word", "Sender")) %>%
  filter(Sender == "Frederik") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency (Frederik)")

words_by_time %>%
  inner_join(top_slopes, by = c("word", "Sender")) %>%
  filter(Sender == "Hlín") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency (Hlín)")

words_by_time %>%
  inner_join(top_slopes, by = c("word", "Sender")) %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  facet_wrap(~ Sender, scales = "free_y", ncol=1) +
  geom_line(size = 1.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  labs(x = NULL, y = "Word frequency")

#Plot word clouds
######

#Calculate frequencis and proportions
frequency <- dfTidyText %>% 
  count(Year, Sender, word, sort=TRUE) 

wordcloud(words = frequency$word[which(frequency$Sender=="Hlín" & frequency$Year==2016)],
          freq = frequency$n[which(frequency$Sender=="Hlín" & frequency$Year==2016)],
          min.freq = 2,
          max.words=150, random.order=FALSE, random.color=FALSE, rot.per=0, fixed.asp = TRUE,
          colors=brewer.pal(12, "Paired"))
title("Hlín - 2016")
wordcloud(words = frequency$word[which(frequency$Sender=="Hlín" & frequency$Year==2017)],
          freq = frequency$n[which(frequency$Sender=="Hlín" & frequency$Year==2017)],
          min.freq = 2,
          max.words=150, random.order=FALSE, random.color=FALSE, rot.per=0, fixed.asp = TRUE,
          colors=brewer.pal(12, "Paired"))
title("Hlín - 2017")
wordcloud(words = frequency$word[which(frequency$Sender=="Frederik" & frequency$Year==2016)],
          freq = frequency$n[which(frequency$Sender=="Frederik" & frequency$Year==2016)],
          min.freq = 2,
          max.words=150, random.order=FALSE, random.color=FALSE, rot.per=0, fixed.asp = TRUE,
          colors=brewer.pal(12, "Paired"))
title("Frederik - 2016")
wordcloud(words = frequency$word[which(frequency$Sender=="Frederik" & frequency$Year==2017)],
          freq = frequency$n[which(frequency$Sender=="Frederik" & frequency$Year==2017)],
          min.freq = 2,
          max.words=150, random.order=FALSE, random.color=FALSE, rot.per=0, fixed.asp = TRUE,
          colors=brewer.pal(12, "Paired"))
title("Frederik - 2017")

#Find TF IDF, term frequencies as inverse document frequencies
#Count frequncies of word pr. "document"
dfTF_IDF <- dfTidyText %>% 
  count(Sender, word, sort=TRUE)
#Apply TF IDF transform
dfTF_IDF <- dfTF_IDF %>%
  bind_tf_idf(word, Sender, n) %>%
  arrange(desc(tf_idf))

#Plot inverse document frequencies
dfTF_IDF %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(Sender) %>% 
    top_n(25) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = Sender)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Sender, ncol = 2, scales = "free") +
    coord_flip()
