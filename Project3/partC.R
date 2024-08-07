library(tidyverse)
library(rvest)
keywords <- c("sustainability", "sports", "employment")

search1 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[1])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[1],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))
#search2
search2 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[1])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[2],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))
#search3
search3 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[1])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[3],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))

#use bind_rows() and your search1, search2, and search3 data frames to create a combined data frame named combined_search.
combined_search <- bind_rows(search1, search2, search3)

#read a csv file
speeches_governments <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRHTFJcFmsIkjaFUCEwFWASaBOAR4X2Upx66C5Bhgc_WNc2JxxdRbbvyoewmvt_EjNdCNZZzzkENwLg/pub?gid=0&single=true&output=csv")

#Use an appropriate function from {tidyverse}, specifically the {dplyr} package, to join the two data frames combined_search and speeches_governments by the variable year, and name this new data frame speech_data.
speech_data <- left_join(speeches_governments, combined_search, by = "year")


# Create the bar plot
ggplot(speech_data, aes(x = factor(year), y = num_speeches, fill = keyword)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Speeches Each Year by Keyword",
    x = "Year",
    y = "Number of Speeches",
    fill = "Keyword"
  ) +
  theme_minimal() +
  #changing x text to vertical
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
