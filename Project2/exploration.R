library(tidyverse)      # data manipulation & plotting

learning_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTq_JQND-AjtOoTETwiWWdiC0x3KL9QQoRXmxhYV03PqzpaUtX5H99suyJoYP9Vg26qBsLGz7gnMUQP/pub?output=csv")
renamed_learning_data <- learning_data %>%
  rename(rest_activity = 2,
         mean_rest = 3,
         rest_period = 4,
         change_to_rest = 5,
         rest_rate =6,
         #prefix "next_" means that these data are from section 2 of the form
         next_rest_activity = 7,
         next_mean_rest = 8,
         next_rest_period = 9,
         next_change_to_rest = 10,
         next_rest_rate = 11)
  

#Bar chart1: most common activity category in week3 and week4
#how to get word frequency from text: https://afit-r.github.io/tidy_text and https://www.codementor.io/@alexander-k/r-word-frequency-in-dataframe-165jgfxxqe
text_tb <- tibble(text = renamed_learning_data$rest_activity)
next_text_tb <- tibble(text = renamed_learning_data$next_rest_activity)
#week3 dataframe
word_tb <- text_tb %>%
  unnest_tokens(word, text)
#week4 dataframe
next_word_tb <- next_text_tb %>%
  unnest_tokens(word, text)

#how to categorize elements in table: https://stackoverflow.com/questions/43696227/mutate-with-case-when-and-contains
# how to make a function: https://www.dataquest.io/blog/write-functions-in-r/
# I had to make a categorizing function to avoid duplicating categorizing code but I kept faiing so I couldn.t avoid duplicating.
word_tb <- word_tb %>%
    mutate(category = case_when(
      #how to check if value is in Dataframe: https://sparkbyexamples.com/r-programming/usage-of-in-operator-in-r/
      word %in% c('watching', 'watch') ~ 'watching_video',
      word %in% c('game', 'games', 'lol', 'fifa', 'boardgame', 'sudoku') ~ 'playing_game',
      word %in% c('friend', 'friends', 'gf', 'girlfriend', 'family', 'bf', 'boyfriend') ~ 'spend_time_with_friend_or_family',
      word %in% c('read', 'reading', 'write', 'writing') ~ 'reading_and_writing',
      word %in% c('sport', 'sports', 'exercise', 'workout', 'gym', 'football', 'basketball', 'ball', 'walk', 'walking', 'park', 'travel', 'run', 'running', 'parc', 'baseball', 'soccer', 'golf', 'volleyball', 'swimming', 'jog', 'jogging') ~ "going_outside_or_exercise",
      word %in% c('sns', 'internet', 'instagram', 'facebook', 'online', 'phone') ~ 'internet_browsing_and_sns',
      word %in% c('studying', 'learn', 'studied') ~ 'studying',
      word %in% c('sleeping', 'bed', 'nap', 'sleep', 'slept') ~ 'sleeping',
      word %in% c('listening', 'listen', 'piano', 'guitar', 'singing') ~ 'listening_music_or_playing_instrument',
      TRUE ~ NA_character_
    ))

next_word_tb <- next_word_tb %>%
  mutate(category = case_when(
    #how to check if value is in Dataframe: https://sparkbyexamples.com/r-programming/usage-of-in-operator-in-r/
    word %in% c('watching', 'watch') ~ 'watching_video',
    word %in% c('game', 'games', 'lol', 'fifa', 'boardgame', 'sudoku') ~ 'playing_game',
    word %in% c('friend', 'friends', 'gf', 'girlfriend', 'family', 'bf', 'boyfriend') ~ 'spend_time_with_friend_or_family',
    word %in% c('read', 'reading', 'write', 'writing') ~ 'reading_and_writing',
    word %in% c('sport', 'sports', 'exercise', 'workout', 'gym', 'football', 'basketball', 'ball', 'walk', 'walking', 'park', 'travel', 'run', 'running', 'parc', 'baseball', 'soccer', 'golf', 'volleyball', 'swimming', 'jog', 'jogging') ~ "going_outside_or_exercise",
    word %in% c('sns', 'internet', 'instagram', 'facebook', 'online', 'phone') ~ 'internet_browsing_and_sns',
    word %in% c('studying', 'learn', 'studied') ~ 'studying',
    word %in% c('sleeping', 'bed', 'nap', 'sleep', 'slept') ~ 'sleeping',
    word %in% c('listening', 'listen', 'piano', 'guitar', 'singing') ~ 'listening_music_or_playing_instrument',
    TRUE ~ NA_character_
  ))

#3rd week activity plot 
count_category_tb <- word_tb %>%
  count(category, sort = TRUE)
activity_plot <- count_category_tb %>%
  filter(category != "N/A") %>%
  #get the first top five categories
  head(5) %>%
  #reference of reorder function: https://stackoverflow.com/questions/37480949/re-ordering-bars-in-rs-barplot
  ggplot(aes(y = reorder(category, n), x = n, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Category", title = "Top 5 Most Common Categories in 3rd week of March") +
  theme_minimal()

next_count_category_tb <- next_word_tb %>%
  count(category, sort = TRUE)
next_activity_plot <- next_count_category_tb %>%
  filter(category != "N/A") %>%
  #get the first top five categories
  head(5) %>%
  #reference of reorder function: https://stackoverflow.com/questions/37480949/re-ordering-bars-in-rs-barplot
  ggplot(aes(y = reorder(category, n), x = n, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Count",y='', title = "Top 5 Most Common Categories in 4th week of March") +
  theme_minimal()

next_count_category_tb
next_activity_plot
activity_plot

#Bar chart 2: common rest period
separated_rest_period <- renamed_learning_data[4] %>%
  separate_rows(rest_period, sep= ", ") %>%
  count(rest_period, sort = TRUE)

next_separated_rest_period <- renamed_learning_data[9] %>%
  separate_rows(next_rest_period, sep= ", ") %>%
  count(next_rest_period, sort = TRUE)
#week3 period plot
period_plot <- separated_rest_period %>%
  ggplot(aes( y = reorder(rest_period, n), x = n, fill = rest_period))+
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Rest_period", title = "Common Rest Period in 3rd week") +
  theme_minimal()
#week4 period plot
next_period_plot <- next_separated_rest_period %>%
  ggplot(aes( y = reorder(next_rest_period, n), x = n, fill = next_rest_period))+
  geom_bar(stat = "identity") +
  labs(x = "Count", y = "Rest_period", title = "Common Rest Period in 4th week") +
  theme_minimal()


#summary values: common rest time, mean_rate, min_change max_change and no_change
mean_rate <- round(mean(renamed_learning_data$rest_rate),2)
next_mean_rate <- round(mean(renamed_learning_data$next_rest_rate),2)
mean_change <- round(mean(renamed_learning_data$change_to_rest),2)
next_meam_change <- round(mean(renamed_learning_data$next_change_to_rest),2)
no_change <- renamed_learning_data %>%
  filter(change_to_rest == 0) %>%
  count()
next_no_change <- renamed_learning_data %>%
  filter(next_change_to_rest == 0) %>%
  count()

