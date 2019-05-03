
#image.darknet and yolo

library(image.darknet)

yolo_tiny_voc <- image_darknet_model(type = "detect", 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

x <- image_darknet_detect(file = "data/train/c1/img_10011.jpg", 
                          object = yolo_tiny_voc,
                          threshold = 0.19)



'''
Sources:
https://heartbeat.fritz.ai/object-detection-in-just-3-lines-of-r-code-using-tiny-yolo-b5a16e50e8a0
chrome-extension://bjfhmglciegochdpefhhlphglcehbmek/content/web/viewer.html?file=https%3A%2F%2Farxiv.org%2Fpdf%2F1506.02640.pdf
https://pjreddie.com/media/files/papers/YOLOv3.pdf
https://towardsdatascience.com/yolo-v3-object-detection-53fb7d3bfe6b
'''