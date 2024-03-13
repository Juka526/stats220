# Load the magick library
library(magick)

serious_dog <- image_read("https://i.pinimg.com/originals/b1/7e/21/b17e218b08f84b12d346128691cfe02c.jpg") %>%
  image_scale(200)



happy_dog <- image_read("https://static.boredpanda.com/blog/wp-content/uploads/2015/09/10974198_764347496952795_4957312736937536277_o__700.jpg") %>%
  image_scale(200)

orange_square <- image_blank(width = 200,
                             height = 100,
                             color = "#e08d3c") %>%
  image_annotate(text = "but.. \nstill have more",
                 color = "#FFFFFF",
                 size = 28,
                 font = "Impact",
                 gravity = "center")

gif_square <- image_blank(width = 200,
                             height = 100,
                             color = "#e08d3c") %>%
  image_annotate(text = "I can't believe it.",
                 color = "#FFFFFF",
                 size = 28,
                 font = "Impact",
                 gravity = "center")

first_square <- image_blank(width = 200,
            height = 100,
            color = "#448488") %>%
  image_annotate(text = "Finally submit \n an assignment!!",
                  color = "#FFFFFF",
                  size = 28,
                  font = "impact",
                  gravity = "center")
first_row <- c(happy_dog, first_square) %>%
  image_append(stack=TRUE)
second_row <- c(image_crop(serious_dog, "200x200"), orange_square) %>%
  image_append(stack = TRUE)
my_meme <- c(first_row, second_row) %>%
  image_append()

image_write(my_meme, "my_meme.png")


first_frame <- image_scale(first_row)
second_frame <- image_scale(image_read("https://media.makeameme.org/created/check-canvas-0e47437a4f.jpg"))
third_frame <- image_scale(image_read("https://media.istockphoto.com/id/166223622/photo/school-dog.jpg?s=612x612&w=0&k=20&c=OyEvTWUaPczUz6Cx-wv83XTj_d3frjJiPRgypixVbbk="))
fourth_frame <- image_scale(c(image_crop(serious_dog, "200x200"), gif_square) %>%
                              image_append(stack = TRUE))

frames <- image_resize(c(first_frame, second_frame, third_frame, fourth_frame), '300x300!')

animation <- image_animate(frames, fps = 1)
image_write(animation, "my_animation.gif")






