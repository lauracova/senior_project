

#https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/

library(keras)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

img_dir <- "data/VOCdevkit/VOC2007/JPEGImages"
annot_file <- "data/pascal_train2007.json"

annotations <- fromJSON(file = annot_file)
str(annotations, max.level = 1)

imageinfo <- annotations$images %>% {
  tibble( # tibble constructs a data frame with image characteristics and location
    id = map_dbl(., "id"),
    file_name = map_chr(., "file_name"),
    image_height = map_dbl(., "height"),
    image_width = map_dbl(., "width")
  )
}

classes <- c(
  "aeroplane",
  "bicycle",
  "bird",
  "boat",
  "bottle",
  "bus",
  "car",
  "cat",
  "chair",
  "cow",
  "diningtable",
  "dog",
  "horse",
  "motorbike",
  "person",
  "pottedplant",
  "sheep",
  "sofa",
  "train",
  "tvmonitor"
)

boxinfo <- annotations$annotations %>% {    #dataset for boxes
  tibble(
    image_id = map_dbl(., "image_id"),
    category_id = map_dbl(., "category_id"),
    bbox = map(., "bbox")
  )
}

boxinfo <- boxinfo %>%                                                    #unlisting and splitting boxinfo$bbox
  mutate(bbox = unlist(map(.$bbox, function(x) paste(x, collapse = " "))))
boxinfo <- boxinfo %>% 
  separate(bbox, into = c("x_left", "y_top", "bbox_width", "bbox_height"))
boxinfo <- boxinfo %>% mutate_all(as.numeric)

                                                                        #calculating y_bottom and x_right
boxinfo <- boxinfo %>% 
  mutate(y_bottom = y_top + bbox_height - 1, x_right = x_left + bbox_width - 1) #subract one because bbox_height is the number of pixels and includes the starting and ending pixel
                                            
                                                      
catinfo <- annotations$categories %>%  {               #getting dataset for class id's and names from annotations
  tibble(id = map_dbl(., "id"), name = map_chr(., "name"))
}

                                                        #joining three datasets together
imageinfo <- imageinfo %>%
  inner_join(boxinfo, by = c("id" = "image_id")) %>%
  inner_join(catinfo, by = c("category_id" = "id"))

#actual size of image we will pass to network
target_height <- 224
target_width <- 224

                                    #scale bounding box according to actual image size (images are different sizes)
imageinfo <- imageinfo %>% mutate(
  x_left_scaled = (x_left / image_width * target_width) %>% round(), #155/500*224
  x_right_scaled = (x_right / image_width * target_width) %>% round(),
  y_top_scaled = (y_top / image_height * target_height) %>% round(),
  y_bottom_scaled = (y_bottom / image_height * target_height) %>% round(),
  bbox_width_scaled =  (bbox_width / image_width * target_width) %>% round(),
  bbox_height_scaled = (bbox_height / image_height * target_height) %>% round()
)

img_data <- imageinfo[4,] #fourth row of imageinfo
img <- image_read(file.path(img_dir, img_data$file_name))
img <- image_draw(img)
rect(
  img_data$x_left,
  img_data$y_bottom,
  img_data$x_right,
  img_data$y_top,
  border = "purple",
  lwd = 2
)
text(
  img_data$x_right,
  img_data$y_top,
  img_data$name,
  pos = 2,
  cex = 1.5,
  col = "purple"
)
dev.off()


#calculate area of bounding boxes
imageinfo <- imageinfo %>% mutate(area = bbox_width_scaled * bbox_height_scaled)

#choose boxes that are the biggest for each image
imageinfo_maxbb <- imageinfo %>%
  group_by(id) %>%
  filter(which.max(area) == row_number())


#creating training and test set
n_samples <- nrow(imageinfo_maxbb) # i had to write this line
train_indices <- sample(1:n_samples, 0.8 * n_samples)
train_data <- imageinfo_maxbb[train_indices,]
validation_data <- imageinfo_maxbb[-train_indices,]


feature_extractor <-  #Error: The h5py Python package is required to use pre-built Keras models
  application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3),
    pooling = "avg"
  )













'''
https://blogs.rstudio.com/tensorflow/posts/2018-12-18-object-detection-concepts/
'''

#selects columns
imageinfo4ssd <- imageinfo %>% 
  select(category_id,
         file_name,
         name,
         x_left,
         y_top,
         x_right,
         y_bottom,
         ends_with("scaled"))

#groups by image and adds objects and their dimensions to a string for each image
imageinfo4ssd <- imageinfo4ssd %>%
  group_by(file_name) %>%
  summarise(
    categories = toString(category_id),
    name = toString(name),
    xl = toString(x_left_scaled),
    yt = toString(y_top_scaled),
    xr = toString(x_right_scaled),
    yb = toString(y_bottom_scaled),
    xl_orig = toString(x_left),
    yt_orig = toString(y_top),
    xr_orig = toString(x_right),
    yb_orig = toString(y_bottom),
    cnt = n()
  )

example <- imageinfo4ssd[5, ]
img <- image_read(file.path(img_dir, example$file_name))
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

img <- image_draw(img)
for (i in 1:example$cnt) {
  rect(x_left[i],
       y_bottom[i],
       x_right[i],
       y_top[i],
       border = "white",
       lwd = 2)
  text(
    x = as.integer(x_right[i]),
    y = as.integer(y_top[i]),
    labels = name[i],
    offset = 1,
    pos = 2,
    cex = 1,
    col = "white"
  )
}
dev.off()
print(img)




