library(magick)
library(ggplot2)
#save plots as images
ggsave("img_category_count.png", category_count_by_channel)

ggsave("highlights_duration.png", mean_highlight_duration)

ggsave("median_views.png", median_view_by_category)

category_count <- image_read("img_category_count.png") %>%
  image_scale(500)

highlights_duration <- image_read("highlights_duration.png") %>%
                                    image_scale(500)

median_views <- image_read("median_views.png") %>%
  image_scale(500)

#color themes for frames
colors = c("#F189C9", "#FFBEBA", "#00C4FF", "#F3EED9")

#default background
background<- image_blank(width = 1200, 
            height = 400,
            color =colors[4])

#images for first and last frame
epl_bundesliga <- image_read("https://cdn.pmnewsnigeria.com/wp-content/uploads/2023/08/premier-league-bundesliga.jpg.webp")%>%
  image_scale(500)

epl_bundes_players <- image_read("https://e0.365dm.com/20/05/2048x1152/skysports-graphic-bundesliga_4991005.jpg?20200515114225") %>%
  image_scale(600)

#first title frame
title<- background %>%
  image_annotate(text="Comparisons of Bundesliga\n\nand\n\nPremier League YouTube Channels", 
                 color = colors[1],
                 size = 35,
                 font = "Helvetica",
                 gravity = "center",
                 location = "+300+5")%>%
  image_composite(epl_bundesliga, offset = "+50+80")


#second frame
category_frame <- background %>%
  image_annotate(text="There are five main categories of videos and \nthis column plot represents that \n@premierleague mostly crete highlights videos while  \n@bundesliga mostly create goal review videos.\nvideo type, Top moments, was a category of videos that only \n@premierleague creates and Assist review \nwas only created  by @bundesliga.", 
                 color = colors[3],
                 size = 23,
                 font = "Helvetica",
                 gravity = "center",
                 location = "+260+5")%>%
  image_composite(category_count, offset = "+20+20")


#third frame
median_frame <- background %>%
  image_annotate(text="Highlights was the most popular video category among them \nand followed by Goal review, but it looks there were \nnot enought data to compare with other categories.\nLike highlights had a widely variable distribution of viewCount, \ngoal review had a similar distribution relatively", 
                 color = colors[3],
                 size = 23,
                 font = "Helvetica",
                 gravity = "center",
                 location = "+260+5")%>%
  image_composite(median_views, offset = "+20+20")


#fourth frame
highlights_frame <- background %>%
  image_annotate(text="I wanted to compare the duration of highlights \nbetween two channels because I felt @premierleague's \nhighlights were too short.\nHowever, it was interesting that @bundesliga \nhad a shorter highlights", 
                 color = colors[1],
                 size = 25,
                 font = "Helvetica",
                 gravity = "center",
                 location = "+300+5")%>%
  image_composite(highlights_duration, offset = "+20+20")


#fifith frame
conclusion_frame <- background %>%
  image_annotate(text="Overall, I could observe that they have different \nmain categories of video, and highlights were the \nmost common one for both of them.\nMoreover, it was happy to know that my favorite \nfootball league which is premier league\n has a longer highlights than bundesliga.", 
                 color = colors[1],
                 size = 23,
                 font = "Helvetica",
                 gravity = "center",
                 location = "+300+5")%>%
  image_composite(epl_bundes_players, offset = "+27+30")


frames <- c(title, category_frame, median_frame, highlights_frame, conclusion_frame)
animation <- image_animate(frames, fps=1, delay=500, loop=3)
image_write(animation, "data_story.gif")
