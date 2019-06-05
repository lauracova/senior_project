
#https://medium.freecodecamp.org/an-introduction-to-web-scraping-using-r-40284110c848


#Behind the Name
address <- "https://www.behindthename.com/name/sarah"


#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
library(rvest)

my.html <- read_html(address)
mysection <- html_nodes(my.html, "section")
mystring <- html_text(mysection)[1]

library(stringr)

meaning <- str_split(mystring, "\n")[[1]][3] 
##str_remove(meaning, "\\\\")
meaning




