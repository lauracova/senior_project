

#Behind the Name
address <- "https://www.behindthename.com/name/garrett"

#install.packages("selectr")
#install.packages("xml2")
#install.packages("rvest")
library(rvest)

my.html <- read_html(address)
mysection <- html_nodes(my.html, "section")
html_text(mysection)[1]


#https://medium.freecodecamp.org/an-introduction-to-web-scraping-using-r-40284110c848




y[1:(n-1)]-y[2:n]

