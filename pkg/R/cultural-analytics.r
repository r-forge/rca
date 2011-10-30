##    cultural-analytics.r - Statistical analysis and plotting of images
##    Copyright (C) 2011  Rob Myers <rob@robmyers.org>
##
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.


################################################################################
## Libraries used
################################################################################

## source("http://bioconductor.org/biocLite.R")
## biocLite("EBImage")
library("EBImage")

##install.packages("colorspace")
library("colorspace")

## install.packages("plyr") # for ddply
library(plyr)


################################################################################
## Image palettes
################################################################################

## The representation used by palettes and histograms

imageToDataFrame<-function(image){
  data.frame(red=as.numeric(imageData(channel(image, "red"))),
             green=as.numeric(imageData(channel(image, "green"))),
             blue=as.numeric(imageData(channel(image, "blue"))))
}

## Convert the image's pixels to colorspace RGB objects

imageToRgb<-function(image){
  RGB(as.numeric(imageData(channel(image, "red"))),
      as.numeric(imageData(channel(image, "green"))),
      as.numeric(imageData(channel(image, "blue"))))
}

## Convert the RGB data.frame to an RGB objects collection

rgbToHsv<-function(rgbs){
  as(rgbs, "HSV")
}

## Quantize the RGB colours in an image, extracting an RGB colour palette
## Return the colours in ascending order of brightness

dataFramePalette<-function(dataframe, count){
  ## Cluster r,g,b values as points in RGB space
  clusters<-kmeans(dataframe, count)
  ## The centre of each cluster is its average colour
  averageColours<-clusters$centers
  ## Return the colours in brightness order
  sortPalette(averageColours)
}

## Sort a palette's entries in rough order of brightness

sortPalette<-function(palette){
  colourValues<-apply(palette, 1, sum)
  palette[order(colourValues),]
}

## Sort a vector of palettes in rough order of brightness

sortPalettes<-function(palettes){
  ## Sum the pixel values and divide them by the number of pixels
  paletteValues<-sapply(palettes,
                        function(palette){sum(palette) / length(palette)})
  palettes[order(paletteValues)]
}

## Convert palette colours to R colors suitable for use in plotting

paletteToR<-function(palette){
  apply(palette, 1, function(colour){rgb(colour[1], colour[2], colour[3])})
}

## Convert the colour image to greyscale, or intensity

imageToIntensity<-function(image){
  channel(image, "grey")
}

## Plot a list of palettes as a matrix of colours, possibly with labels

plotPalettes<-function(palettes, names=FALSE){
  colourCount<-length(palettes[[1]])
  palettesColours<-sapply(palettes, paletteToR, USE.NAMES=FALSE)
  image(matrix(1:(length(palettes) * colourCount), colourCount,
               length(palettes)), col=palettesColours, axes=FALSE)
  if(names != FALSE){
    axis(2, at=seq(0.0, 1.0, 1.0 / (length(palettes) - 1)),
         labels=names, las=2, tick=0)
  }
}


################################################################################
## Image histograms
################################################################################

## FIXME: make these act like hist()!

## Plot a simple brightness histogram

plotIntensityHistogram<-function(image, bins=255, main="", xlab="Value",
                                 col=grey(0)){
  hist(imageData(image), main=main, breaks=0:bins/bins, xlim=c(0, 1),
       xlab=xlab, col=col, border=NULL)
}

## Colour histogram
## http://cns.bu.edu/~gsc/ColorHistograms.html

rgbImageHist<-function(rgbDataFrame, binResolution=25){
  scaledRGBs<-floor(rgbDataFrame * binResolution)
  counts<-ddply(scaledRGBs, c("red", "green", "blue"), nrow)
  names(counts)[4]<-"count"
  attr(counts, "binResolution")<-binResolution
  counts
}

## Plot colour histogram

plotRgbHistogram<-function(colourHist){
  values<-apply(colourHist, 1, function(row){sum(row[c(1, 2, 3)])})
  valuesOrder<-order(values)
  countsOrdered<-colourHist$count[valuesOrder]
  binResolution<-attr(colourHist, "binResolution")
  colours<-apply(colourHist, 1, function(row){rgb(row[1] / binResolution,
                                                  row[2] / binResolution,
                                                  row[3] / binResolution)})
  coloursOrdered<-colours[valuesOrder]
  barplot(countsOrdered, col=coloursOrdered, border=NA)
}


################################################################################
## Image colour clouds
################################################################################

## Plot colour cloud
## http://cns.bu.edu/~gsc/ColorHistograms.html

# These are internal utility functions, not to be exported

sigmoid<-function(x){
  1 / (1 + exp(-x))
}

colourCloudX<-function(r,g,b){
  sigmoid((g - r + 1) / 2)
}

colourCloudY<-function(r,g,b){
  eps<-length(colours)
  (b + eps)/(r + g + b + 3 * eps)
}

randomPointInRadius<-function(x, y, radius){
  q<-runif(1) * (pi * 2)
  r<-radius * sqrt(runif(1))
  h<-r * cos(q)
  v<-r * sin(q)
  c(x + h, y + v)
}

randomizedBinPoints<-function(xs, ys, radius){
  coords<-array(dim=c(length(xs), 2))
  for(index in 1:length(xs)){
    point<-randomPointInRadius(xs[index], ys[index], radius)
    coords[index, 1]<-point[1]
    coords[index, 2]<-point[2]
  }
  coords
}

# This is the main function, to be exported

plotColourCloud<-function(hist, title="", radius=0.005, dotSize=1){
  binResolution<-attr(hist, "binResolution")
  bins<-hist[c(1,2,3)] / binResolution
  ## Rep each item by the count that applies to its bin, this allows us to
  ## process the repeated item lists directly rather than nest loops & counters
  xs<-rep(apply(bins, 1, function(col){colourCloudX(col[1], col[2], col[3])}),
          hist$count)
  ys<-rep(apply(bins, 1, function(col){colourCloudY(col[1], col[2], col[3])}),
          hist$count)
  hexes<-rep(apply(bins, 1, function(c){rgb(c[1], c[2], c[3])}),
             hist$count)
  coords<-randomizedBinPoints(xs, ys, radius)
  plot(coords[,1], coords[,2], bg=hexes, main=title, cex=dotSize,
       col=NULL, pch=21, xlab="", ylab="", xaxt="n", yaxt="n")
}
 
## Calculate entropy from a histogram

imageEntropy<-function(histogram){
  nonzeroCounts<-histogram$counts[histogram$counts > 0]
  probs<-nonzeroCounts / sum(nonzeroCounts)
  -sum(probs * log2(probs))
}

##TODO: Brightness box plot?

################################################################################
## RGB statistics
################################################################################

## Calculate the median values for the RGB coordinates
## The colour returned is not a colour in the image,
## it just contains the median values
  
medianRgb<-function(rgbcoords){
  RGB(median(rgbcoords[,"R"]), median(rgbcoords[,"G"]),
      median(rgbcoords[,"B"]))
}
  
## Calculate the minimum and maximum values for the RGB coordinates
## Returns a vector of colours, the first containing low values,
## the second containing high values
## These are not colours that appear in the image, they just contain the values
  
rangeRgb<-function(rgbcoords){
  hrange<-range(rgbcoords[,"R"])
  srange<-range(rgbcoords[,"G"])
  vrange<-range(rgbcoords[,"B"])
  list(min=RGB(hrange[1], srange[1], vrange[1]),
       max=RGB(hrange[2], srange[2], vrange[2]))
}

## Calculate the standard deviation for the RGB coordinates
## The colour returned is not a colour in the image,
## it just contains the sd for each value

sdRgb<-function(rgbcoords){
  rsd<-sd(rgbcoords[,"R"])
  gsd<-sd(rgbcoords[,"G"])
  bsd<-sd(rgbcoords[,"B"])
  list(R=rsd[1], G=gsd[1], B=bsd[1])
}

## A good way of getting the min, max, median and other useful values

summaryRgb<-function(rgbcoords){
  list(R=summary(rgbcoords[,"R"]),
       G=summary(rgbcoords[,"G"]),
       B=summary(rgbcoords[,"B"]))
}


################################################################################
## HSV statistics
################################################################################

## Calculate the median values for the HSV coordinates
## The colour returned is not a colour in the image,
## it just contains the median values
  
medianHsv<-function(hsvcoords){
  HSV(median(hsvcoords[,"H"]), median(hsvcoords[,"S"]), median(hsvcoords[,"V"]))
}
  
## Calculate the minimum and maximum values for the HSV coordinates
## Returns a vector of colours, the first containing low values,
## the second containing high values
## These are not colours that appear in the image, they just contain the values
  
rangeHsv<-function(hsvcoords){
  hrange<-range(hsvcoords[,"H"])
  srange<-range(hsvcoords[,"S"])
  vrange<-range(hsvcoords[,"V"])
  list(min=HSV(hrange[1], srange[1], vrange[1]),
       max=HSV(hrange[2], srange[2], vrange[2]))
}

## Calculate the standard deviation for the HSV coordinates
## The colour returned is not a colour in the image,
## it just contains the sd for each value

sdHsv<-function(hsvcoords){
  hsd<-sd(hsvcoords[,"H"])
  ssd<-sd(hsvcoords[,"S"])
  vsd<-sd(hsvcoords[,"V"])
  list(H=hsd[1], S=ssd[1], V=vsd[1])
}

## A good way of getting the min, max, median and other useful values

summaryHsv<-function(hsvcoords){
  list(H=summary(hsvcoords[,"H"]),
       S=summary(hsvcoords[,"S"]),
       V=summary(hsvcoords[,"V"]))
}

################################################################################
## Scatter plot
################################################################################

## Labels

plotLabels<-function(labelValues, xValues, yValues, labelCol="black",
                     labelSize=1){
  ## Position the labels above the images
  text(xValues, yValues, labelValues, col=labelCol, cex=labelSize, pos=3)
}

## Image utilities

## Left X value for image
## image parameter accepted to give these calls a regular signature

imageXLeft<-function(image, valueX){
  valueX
}

## Right X value for image
## image parameter accepted to give these calls a regular signature

imageXRight<-function(image, valueX, thumbnailWidth){
  valueX + thumbnailWidth
}

## Get the height of the image scaled to the new width

imageHeightScaled<-function(image, scaledWidth){
  scale<-dim(image)[1] / scaledWidth
  dim(image)[2] / scale
}

## Bottom Y value for image

imageYBottom<-function(image, valueY, thumbnailWidth){
  valueY - imageHeightScaled(image, thumbnailWidth)
}

## Top Y value for image

imageYTop<-function(image, valueY){
  valueY
}

## End image utilities

## Image scatter plot

plotImages<-function(images, xValues, yValues, thumbnailWidth=0.3){
  for(i in 1:length(images)){
    image<-images[[i]]
    x<-xValues[i]
    y<-yValues[i]
    ## Does the image really have to be rotated???
    rasterImage(rotate(image), imageXLeft(image, x), imageYTop(image, y),
                imageXRight(image, x, thumbnailWidth),
                imageYBottom(image, y, thumbnailWidth))
  }
}

## Lines

plotLines<-function(xValues, yValues, lineCol="black", lineWidth=1){
  lines(xValues, yValues, col=lineCol, lwd=lineWidth)
}

## Points

plotPoints<-function(xValues, yValues, pointCol="black", pointStyle=19){
    points(xValues, yValues, pch=pointStyle, col=pointCol)
  }

## Plot everything in an order that is good for visibility

plotImageScatter<-function(images, xValues, yValues, labelValues,
                           lineCol="black", lineWidth=1.0,
                           pointStyle=19, pointCol="black",
                           thumbnailWidth=0.3,
                           labelCol="black", labelSize=1,
                           shouldDrawLines=TRUE, shouldDrawPoints=TRUE,
                           shouldDrawImages=TRUE, shouldDrawLabels=TRUE){
  if(shouldDrawLines){
    plotLines(xValues, yValues, lineCol=lineCol, lineWidth=lineWidth)
  }
  if(shouldDrawPoints){
    plotPoints(xValues, yValues, pointStyle=pointStyle, pointCol=pointCol)
  }
  if(shouldDrawImages){
    plotImages(images, xValues, yValues, thumbnailWidth)
  }
  if(shouldDrawLabels){
    plotLabels(labelValues, xValues, yValues, labelCol=labelCol,
               labelSize=labelSize)
  }
}


###############################################################################
## Feature analysis
################################################################################

## http://staff.science.uva.nl/~asalah/buter11deviantart.pdf


##edgeToPixelRatio<-function(biOpsImagedata, sigma=0.7){
##  cannyImage<-imgCanny(biOpsImagedata, sigma)
##  (1 / numPixels(cannyImage)) * sum(cannyImage.pixels)
##}

##cornerToPixelRatio<-function(biOpsImagedata, sigma=0.7){
## 
##  (1 / numPixels(harrisImage)) * sum(harrisImage.pixels)
##}

### HSV and RGB are already covered

##TODO: averageIntensityFeature ?

## Median intensity is already covered

#intensityVariance<-function(greyscaleImage){
#  (1 / numPixels(greyscaleImage)) * sum(greyscaleImage.pixels)
#}

## intensityHistogram is already covered

##TODO: intensityEntropy is already covered

## 3x3 grid versions? (Make n*n grid versions, combine w/Mathematica stuff...)

## Itti, Koch, Niebur model

## OpenCV

##faceCount<-function(image, ...){
##}

##eyeCount<-function(image, ...){
##}

##cornerCount<-function(image, ...){
##}

##SEE: table 1

## Classifiers work better with normalised features,
## so rescale all data to 0..1

## They used knn, naive bayes, nearest mean, SVM

## inter-intra distance?
## http://stackoverflow.com/questions/4786665/kmeans-inter-and-intra-cluster-ordering
## Or the function they provide

## Confusion matrix

## Precision

## Recall

## F-measure

##meanImage<-function(images){
  ## average the images onto an image of max(width, height) square
  ## centering the images (Or aligning to top right?_
##}


################################################################################
## More from series_palette.r
################################################################################

## SD of properties of list of images
## Mean of properties of list of images
## Summary of properties of list of images

## Clustering images based on properties
