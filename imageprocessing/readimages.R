
'''
The 10 classes to predict are:

c0: safe driving
c1: texting - right
c2: talking on the phone - right
c3: texting - left
c4: talking on the phone - left
c5: operating the radio
c6: drinking
c7: reaching behind
c8: hair and makeup
c9: talking to passenger
'''

############################################
# read in images
############################################
#This video shows what I did in python in 5 minutes
#https://campus.datacamp.com/courses/convolutional-neural-networks-for-image-processing/image-processing-with-neural-networks?ex=1

library(magick)
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html

#read in one pic

image <- image_read("data/train/c0/img_100026.jpg")


#read in many

images <- list.files("data/train/c0")

  
'''

file_list <- list.files("names/")

my_list <-do.call("rbind",map(file_list,function(x) 
  read_table(here::here(paste0("Case_Study_12/analysis/names/",x)), col_names = FALSE) %>% 
    separate("X1", into=c("State", "Gender", "Year", "Name", "Count"), remove=TRUE)
))

'''

####################################################
# Work with Saunders on 4/24
####################################################
class(image)
?image_read
image
image[[1]]
class(image[[1]])
image[[1]][1,,] #reds
image[[1]][,,1] #blues
tmp <- image[[1]]
tmp
tmp <- image
tmp
class(tmp[[1]][1,,])  #these are hex values
class(tmp[[1]][1,1,1])
?raw
View(tmp[[1]][1,,])
View(tmp[[1]][2,,])

#saunders said to just clasify whether the driver is safe or texting in right hand

#attempts to change one rgb color to zero:

> tmp[[1]][1,,] <- 0
Error in tmp[[1]][1, , ] <- 0 : 
  incompatible types (from double to raw) in subassignment type fix
> tmp[[1]][1,,] <- "0"
Error in tmp[[1]][1, , ] <- "0" : 
  incompatible types (from character to raw) in subassignment type fix
> tmp[[1]][1,,] <- "00"
Error in tmp[[1]][1, , ] <- "00" : 
  incompatible types (from character to raw) in subassignment type fix
> tmp[[1]][1,,] <- ff
Error: object 'ff' not found
> tmp[[1]][1,,] <- "ff"
Error in tmp[[1]][1, , ] <- "ff" : 
  incompatible types (from character to raw) in subassignment type fix



###################################################
# Turn images into data. bitmap, rbg, and raw data
###################################################

library(imager) 
#imager package lets you plot image with x and y coordinates and show image without r, g, or b

image2 <- load.image("data/train/c0/img_10003.jpg")
plot(image2) #drivers arms are in different positions???
image2
class(image2)
str(image2)  #shows the structure

image2[,,,]
image2[1,,,] #first r, g, b for 480 pixels
image2[2,,,] #second r, g, b for 480 pixels
image2[,1,,] #first r, g, b for 640 pixels
image2[4,4,,]*255 #r, g, b for specific pixel location
#multiply by 255 to get r,g,b from decimal to rgb value

dat1 <- image2[,,,]*255
dat1.red <- dat1[,,1]
dat1.green <- dat1[,,2]
dat1.blue <- dat1[,,3]
View(dat1.red)

#in tidy format with all rgb
data <- as.data.frame(image2)
data %>% mutate(value= value*255)
table(data$cc)
#data$x = 640, data$y = 480, data$cc = r, g, or b, data$value = value of rgb
plot(grayscale(image2))

#turn off r, g, or b
cscale <- function(r,g,b) rgb(0,g,b)
plot(image2, colourscale=cscale,rescale=FALSE)
cscale <- function(r,g,b) rgb(r,0,b)
plot(image2, colourscale=cscale,rescale=FALSE)
cscale <- function(r,g,b) rgb(r,g,0)
plot(image2, colourscale=cscale,rescale=FALSE)

#detect edges
image2.g <- grayscale(image2)
grayscale.values <- image2.g[,,,]
nrow(grayscale.values)
ncol(grayscale.values)

gr <- imgradient(image2.g,"xy")
gr
plot(gr)
grmag <- imgradient(image2,"xy") %>% enorm %>% plot(main="Gradient magnitude")
grmag <- imgradient(image2.g,"xy") %>% enorm %>% plot(main="Gradient magnitude")
class(grmag)


dat2 <- image2.g[,,,] #still 4 dimensions but one color channel
View(dat2)

#raster image
library(raster)
rst.blue <- raster(image2[,,1])
rst.green <- raster(image2[,,2])
rst.red <- raster(image2[,,3])
rst.blue
head(rst.blue)





#another package got the rgb values

library(jpeg)
img <- readJPEG("data/train/c0/img_10003.jpg")
class(img)
img
dim(img)
(img[,,])
dat2 <- (img[,,])*255
dat2.red <- dat2[,,1]
dat2.green <- dat2[,,2]
dat2.blue <- dat2[,,3]
View(dat2.red)


