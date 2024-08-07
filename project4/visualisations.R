library(tidyverse)
library(ggplot2)
#read file
youtube_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQwoRnDeTtF_TG-3CzQtA2AfZB5ENLBiczTeqnuQ_qZQBIbyRUOGE2V0Nlsnkwa4ylimJ3CdOglRmWi/pub?output=csv")
#make categorized data excludinf nulls
category_data <- youtube_data %>%
  mutate(category = case_when(
    str_detect(tolower(title), "goals") ~ "Goal review",
    str_detect(tolower(title), "assists") ~ "Assist review",
    str_detect(tolower(title), "highlight") ~ "Highlights",
    str_detect(tolower(title), "matchday") ~ "Highlights",
    str_detect(tolower(title), "matchday") ~ "Highlights",
    str_detect(tolower(title), "analysis") ~ "player analysis",
    str_detect(tolower(title), "skills &") ~ "player analysis",
    str_detect(tolower(title), "so far") ~ "player analysis",
    str_detect(tolower(title), "goals 2") ~ "player analysis",
    str_detect(tolower(title), "power") ~ "player analysis",
    str_detect(tolower(title), "top") ~ "top moments",
  ))%>%
  filter(!is.na(category))

#plot showing median views for every categories
median_view_by_category <- ggplot(data = category_data,
       aes(x = (viewCount), 
           y = reorder(category, -viewCount),
           colour = category)) +
  geom_jitter(height = 0.2) +
  geom_boxplot() +
  labs(title = "How do the views of \nvideos differ by categories?", y = "category", x = "view count") 


category_by_channel <- category_data %>%
  group_by(channelName, category) %>%
  summarise(n = n())%>%
  arrange(desc(n))

#plot showing different categories and their count 
category_count_by_channel <- ggplot()+
  geom_col(data = category_by_channel,
             aes(x = n,
                 y = category,
                 fill= channelName))+
  labs(title = "What kind of videos do \nthey usually create?",
       x = "number",
       y = "categories")

highlight_data <- category_data %>%
  filter(category == "Highlights")  

#plot comparing highlights duration
mean_highlight_duration <- highlight_data %>%
  group_by(channelName) %>%
  summarise(mean_duration = mean(duration)) %>%
  ggplot() +
  geom_bar(aes(x = channelName, y = mean_duration, fill = channelName),
           stat = "identity",) +
  labs(title = "Which league channel has \na longer Highlights?",
       x = "Channel",
       y = "Mean Duration")
