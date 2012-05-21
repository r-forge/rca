##    cultural-analytics.r - Statistical analysis and plotting of images
##    Copyright (C) 2011, 2012  Rob Myers <rob@robmyers.org>
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


##install.packages("colorspace")
library("colorspace")

## install.packages("plyr") # for ddply
library(plyr)


################################################################################
## Image palettes
################################################################################

## The representation used by palettes and histograms

imageToDataFrame<-function(image){
  data.frame(red=as.numeric(image[,,1]),
             green=as.numeric(image[,,2]),
             blue=as.numeric(image[,,3]))
}

## Convert the image's pixels to colorspace RGB objects

imageToRgb<-function(image){
  RGB(as.numeric(image[,,1]),
      as.numeric(image[,,2]),
      as.numeric(image[,,3]))
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

imageToIntensity<-function(image, method="perceptual"){
  if(method == "mean"){
    (image[,,1] + image[,,2] + image[,,3]) / 3
  } else if(method == "perceptual") {
    (image[,,1] * .3) + (image[,,2] * .59) + (image[,,3] * .11)
  } else {
    simpleError(paste("Unknown imageToIntensity method:", method))
  }
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
  hist(image, main=main, breaks=0:bins/bins, xlim=c(0, 1),
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
  
medianRgb<-function(rgbs){
  rgbcoords<-coords(rgbs)
  RGB(median(rgbcoords[,"R"]), median(rgbcoords[,"G"]),
      median(rgbcoords[,"B"]))
}
  
## Calculate the minimum and maximum values for the RGB coordinates
## Returns a vector of colours, the first containing low values,
## the second containing high values
## These are not colours that appear in the image, they just contain the values
  
rangeRgb<-function(rgbs){
  rgbcoords<-coords(rgbs)
  hrange<-range(rgbcoords[,"R"])
  srange<-range(rgbcoords[,"G"])
  vrange<-range(rgbcoords[,"B"])
  list(min=RGB(hrange[1], srange[1], vrange[1]),
       max=RGB(hrange[2], srange[2], vrange[2]))
}

## Calculate the standard deviation for the RGB coordinates
## The colour returned is not a colour in the image,
## it just contains the sd for each value

sdRgb<-function(rgbs){
  rgbcoords<-coords(rgbs)
  rsd<-sd(rgbcoords[,"R"])
  gsd<-sd(rgbcoords[,"G"])
  bsd<-sd(rgbcoords[,"B"])
  list(R=rsd[1], G=gsd[1], B=bsd[1])
}

## A good way of getting the min, max, median and other useful values

summaryRgb<-function(rgbs){
  rgbcoords<-coords(rgbs)
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
  
medianHsv<-function(hsvs){
  hsvcoords<-coords(hsvs)
  HSV(median(hsvcoords[,"H"]), median(hsvcoords[,"S"]), median(hsvcoords[,"V"]))
}
  
## Calculate the minimum and maximum values for the HSV coordinates
## Returns a vector of colours, the first containing low values,
## the second containing high values
## These are not colours that appear in the image, they just contain the values
  
rangeHsv<-function(hsvs){
  hsvcoords<-coords(hsvs)
  hrange<-range(hsvcoords[,"H"])
  srange<-range(hsvcoords[,"S"])
  vrange<-range(hsvcoords[,"V"])
  list(min=HSV(hrange[1], srange[1], vrange[1]),
       max=HSV(hrange[2], srange[2], vrange[2]))
}

## Calculate the standard deviation for the HSV coordinates
## The colour returned is not a colour in the image,
## it just contains the sd for each value

sdHsv<-function(hsvs){
  hsvcoords<-coords(hsvs)
  hsd<-sd(hsvcoords[,"H"])
  ssd<-sd(hsvcoords[,"S"])
  vsd<-sd(hsvcoords[,"V"])
  list(H=hsd[1], S=ssd[1], V=vsd[1])
}

## A good way of getting the min, max, median and other useful values

summaryHsv<-function(hsvs){
  hsvcoords<-coords(hsvs)
  list(H=summary(hsvcoords[,"H"]),
       S=summary(hsvcoords[,"S"]),
       V=summary(hsvcoords[,"V"]))
}

## Slowly but surely build a data.frame of values

imageSummaries<-function(images){
    t(sapply(images,
           function(img){
             hsvs<-rgbToHsv(imageToRgb(img))
             sds<-sdHsv(hsvs)
             c(unlist(summaryHsv(hsvs)), H.sd=sds$H, S.sd=sds$S, V.sd=sds$V)
           }))
}

################################################################################
## Scatter plot
################################################################################

## Get the factors required to scale the images to an area of 1.0

imageAreaNormalised<-function(images){
  #FIXME
  sapply(images, function(img){ 1.0 / sqrt(dim(img)[1] * dim(img)[2]) })
}

## Convert an x position or a width in pixels into user co-ordinates

pixelsToUserX<-function(size){
  ## How many pixels per inch across the device?
  ppi<-dev.size("px")[1] / dev.size("in")[1]
  ## How many pixels across the plot?
  pinPixels<-par("pin")[1] * ppi
  ## What proportion of the user co-ordinates width is the size?
  (size / pinPixels) * (par("usr")[2] - par("usr")[1])
}

## Convert a y position or a height in pixels into user co-ordinates

pixelsToUserY<-function(size){
  ## How many pixels per inch down the device?
  ppi<-dev.size("px")[2] / dev.size("in")[2]
  ## How many pixels down the plot?
  pinPixels<-par("pin")[2] * ppi
  ## What proportion of the user co-ordinates height is the size?
  (size / pinPixels) * (par("usr")[4] - par("usr")[3])
}

## Calculate the aspect ratio of an image

imageAspectRatio<-function(img){
  dim(img)[1] / dim(img)[2]
}

## Image scatter plot
## rasterImage plots in user space co-ordinates, plot() or par(usr) affects this
## So we calculate the aspect ratio

plotImages<-function(xValues, yValues, images, thumbnailWidth=72, cex=NULL){
  # Scale the default size by cex, which may be 1 or n values
  if(! is.null(cex)){
    thumbnailWidth<-thumbnailWidth * cex
  }
  ## We always need a thumbnailWidth list equal to length(images)
  if(length(thumbnailWidth) < length(images)) {
    thumbnailWidth<-rep(thumbnailWidth, length.out=length(images))
  }
  for(i in 1:length(images)){
    image<-images[[i]]
    x<-xValues[i]
    y<-yValues[i]
    rasterImage(image, x,
                y - pixelsToUserY(thumbnailWidth[i]) * imageAspectRatio(image),
                x + pixelsToUserX(thumbnailWidth[i]), y)
  }
}

## Public interface to the plotImages routine

images<-function(x, y=NULL, images=NULL, thumbnailWidth=72, cex=NULL){
  xy<-xy.coords(x, y)
  plotImages(xy$x, xy$y, images, thumbnailWidth, cex)
}

## Plot everything in an order that is good for visibility
## This breaks the naming scheme. It's a reference to another piece of software.

imagePlot<-function(x, y=NULL, images=NULL, labels=NULL,
                    main="", sub="",
                    xlim=NULL, ylim=NULL,
                    axes=TRUE, ann=par("ann"),
                    text.adj=par("adj"),
                    thumbnailWidth=72,
                    ...){
  xy<-xy.coords(x, y)
  if(is.null(xlim)){
    xlim<-range(xy$x[is.finite(xy$x)])
  }
  if(is.null(ylim)){
    ylim<-range(xy$y[is.finite(xy$y)])
  }
  opar<-par(no.readonly=TRUE)
  on.exit(par(opar))
  plot.new()
  plot.window(xlim, ylim, ...)
  par(xpd=NA)
  lines(xy$x, xy$y, ...)
  points(xy$x, xy$y, ...)
  plotImages(xy$x, xy$y, images, thumbnailWidth)
  text(xy$x, xy$y, labels, adj=text.adj, ...)
  if(axes){
    axis(1, ...)
    axis(2, ...)
  }
  if (ann){
    ## Take x/ylab from parameters to let user set explicitly
    title(main=main, sub=sub, ...) # xlab=xy$xlab, ylab=xy$ylab,
  }
}


################################################################################
## Image tiling
################################################################################

## Extract a section of a larger image as an image of the same colorMode

subImage<-function(image, x, y, w, h){
  xx<-x + w - 1
  yy<-y + h - 1
  image[x:xx, y:yy, ]
}

## Divide image into smaller tiles
## If the image dosn't divide exactly into that number of rows or columns,
## not all images may be the same size

divideImage<-function(image, columns, rows){
  tiles<-vector("list", columns * rows)
  width<-dim(image)[1] / columns
  height<-dim(image)[2] / rows
  x<-1
  for(column in 1:columns){
    y<-1
    for(row in 1:rows){
      sub<-subImage(image, x, y, width, height)
      tiles[[((column - 1) * rows ) + row]]<-sub
      y<-y + height
    }
    x<-x + width
  }
  matrix(tiles, nrow=rows, ncol=columns)
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

intensityVariance<-function(intensityImage){
  (1 / length(intensityImage)) * sum(intensityImage)
}

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

##### People can do these easily enough, just have them as recipes in Vignettes?
## SD of properties of list of images
## Mean of properties of list of images
## Summary of properties of list of images
## Clustering images based on properties

################################################################################
## Historical aesthetic evaluation functions
################################################################################

# A modern version of Birkhoff's aesthetic measure
# From http://gilab.udg.edu/publ/container/publications/jaume-rigau/2007/Conceptualizing%20Birkhoffs%20aesthetic%20measure%20using%20.pdf

birkhoff<-function(image){
  histogram<-hist(imageToIntensity(image), breaks=0:255/255,
                  plot=FALSE)
  order<-imageEntropy(histogram)
  # Use gzip, like PNG
  intensity<-imageToIntensity(image)
  rawSize<-length(intensity)
  compressedSize<-length(memCompress(as.raw(as.character(intensity)),
  			 type="gzip"))
  complexity<-compressedSize * rawSize
  order * complexity
}


