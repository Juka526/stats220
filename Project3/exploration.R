library(tidyverse)
library(jsonlite)

json_data <- fromJSON("pixabay_data.json")
pixabay_photo_data <- json_data$hits
names(pixabay_photo_data)
quantiles <- pixabay_photo_data %>%
  pull(imageSize) %>%
  quantile()
#select(previewURL, pageURL, selected_photos, tags)
selected_photos <- pixabay_photo_data %>%
  mutate(sizeLevel = ifelse(imageSize <= quantiles[1], "small", 
                            ifelse(imageSize > quantiles[1] & imageSize <= quantiles[2], "mid", "large")),
         pageOrientation = ifelse(previewWidth >= previewHeight, "landscape", "portrait"),
         likesProportion = round((likes/views)*100,3)) %>%
  select(previewURL, pageURL, tags, likesProportion, pageOrientation, sizeLevel, views)%>%
  filter(likesProportion > quantile(likesProportion, 0.75))  # Adjust this condition as per your requirement
write_csv(selected_photos, "selected_photos.csv")
  
meanLikesProportion <- selected_photos$likesProportion %>% mean(na.rm=TRUE)
meanViews <- selected_photos$views %>% mean(na.rm=TRUE)
boat_photos <- sum(str_detect(selected_photos$tags, "boat"))

total_photos <- nrow(selected_photos)


selected_summaries <- selected_photos%>%
  group_by(pageOrientation)%>%
  summarise(n())

animation <- selected_photos %>%
  pull(previewURL)%>%
  image_read() %>%
  image_animate(fps = 5)
animation

image_write(animation, "my_photos.gif")
#seperate rows with "," to get a single tag from tags 
tags_new <- selected_photos%>%
  separate_rows(tags, sep = ", ")
tags_new
# count number of each tags
tags_count <- tags_new%>%
  group_by(tags)%>%
  summarise(n())
#rename "n()" to "freq" 
tags_count<- tags_count %>%
  rename(freq = 2)
# Filter the top 5 tags from tags_count
top_tags_counts <- tags_count %>%
  filter(freq %in% head(sort(freq, decreasing = TRUE), 5))

  
# Plot the bar plot
ggplot(top_tags_counts, aes(x = tags, y = freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Tags", y = "Frequency", title = "Top 5 Most Common Tags") 
  
  
  
  
 
