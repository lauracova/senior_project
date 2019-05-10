
to.read = file("data/mnist/train-images.idx3-ubyte", "rb")
readBin(to.read, integer(), n=28*28, endian = "big")


m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28)
image(m)

par(mfrow=c(5,5))
par(mar=c(0,0,0,0))
for(i in 1:25){m = matrix(readBin(to.read,integer(), size=1, n=28*28, endian="big"),28,28);image(m[,28:1])}





library(readr)
pixels <- read_csv("data/mnist/train.csv")
