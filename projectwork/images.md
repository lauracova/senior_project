
magick package:
seems like it is more for editing pictures than turning them into data

imager: https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html

Extracting rgb:
https://stackoverflow.com/questions/16163611/extract-rgb-channels-from-a-jpeg-image-in-r


R provides many functions to examine features of vectors and other objects, for example
class() - what kind of object is it (high-level)?
typeof() - what is the object’s data type (low-level)?
length() - how long is it? What about two dimensional objects?
attributes() - does it have any metadata?



Class:
magick-image: 

Data type: 
Bitmaps:http://paulbourke.net/dataformats/bitmaps/
Bitmaps are defined as a regular rectangular mesh of cells called pixels, each pixel containing a colour value. They are characterised by only two parameters, the number of pixels and the information content (colour depth) per pixel. There are other attributes that are applied to bitmaps but they are derivations of these two fundamental parameters (I think rgb is a derivation of the second parameter). Note that bitmaps are always orientated horizontally and vertically. Pixels should be considered square although they may have other aspect ratios in practice. In the majority of situations bitmaps are used to represent images on the computer. For example the following is a bitmap which has 397 pixels horizontally, 294 pixels vertically, and each pixel contains a grey value from a possible 256 different greys.
Bitmap is the image collection of pixels. 




rgb:  https://www.youtube.com/watch?v=15aqFQQVBWU   https://www.scantips.com/basics1b.html
Each pixel has an RGB value. Each pixel's color sample has three numerical RGB components (Red, Green, Blue) to represent the color of that tiny pixel area. These three RGB components are three 8-bit numbers for each pixel. Three 8-bit bytes (one byte for each of RGB) is called 24 bit color. Each 8 bit RGB component can have 256 possible values, ranging from 0 to 255. For example, three values like (64, 224, 208), meaning (Red=64, Green=224, Blue=208) to denote one turquoise pixel. However, computers store rgb values in binary. It turns on and off the bits. There are 8 bits which make one byte for each RGB. So turquoise would be R:01000000 G: 11100000 B: 11010000. Those are the 24 digits or bits that represent the color. Hexadecimal used by digital artists. It is shorter and has 6 digits. The first two are for red, second two for green, and last two for blue. 


raw: 
class(tmp[[1]][1,1,1]) and class(tmp[[1]][1,,1]) and > class(tmp[[1]][1,1,]) are examples of "raw"
the "raw" have hex values in common
https://stat.ethz.ch/R-manual/R-devel/library/base/html/raw.html: The raw type is intended to hold raw bytes. It is possible to extract subsequences of bytes, and to replace elements (but only by elements of a raw vector). A raw vector is printed with each byte separately represented as a pair of hex digits.



cimg: ?cimg
cimg is a class for storing image or video/hyperspectral data. Images have up to 4 dimensions, labelled x,y,z,c. x and y are the usual spatial dimensions, z is a depth dimension (which would correspond to time in a movie *just 1 if it is a pic*), and c is a colour dimension.
An object of class cimg is actually just a thin interface over a regular 4D array:



imager_array: cmig is made of an array of up to 4 dimensions


numeric: simply numeric, what I've been using

class:
array:

