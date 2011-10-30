\name{plotRgbHistogram}
\alias{plotRgbHistogram}
\title{
  Plot a colour histogram of an image
}
\description{
  This function plots an RGB histogram on the current graphics device.
}
\usage{
plotRgbHistogram(colourHist)
}
\arguments{
  \item{colourHist}{
    A colour histogram created by calling \code{rgbImageHist()} .
  }
}
\value{
  None
}
\references{
  See: http://cns.bu.edu/~gsc/ColorHistograms.html
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbdf<-imageToDataFrame(img)
rgbhist<-rgbImageHist(rgbdf)
plotRgbHistogram(rgbhist)
}
