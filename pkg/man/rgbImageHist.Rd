\name{rgbImageHist}
\alias{rgbImageHist}
\title{
  Calculate a colour histogram for an RGB image data.frame
}
\description{
  This function calculates a colour histogram from a data.frame
  representing the pixels of an image as generated by \code{imageToDataFrame} .
}
\usage{
rgbImageHist(rgbDataFrame, binResolution=25)
}
\arguments{
  \item{rgbDataFrame}{
    A data.frame representing the pixels of an image as generated by \code{imageToDataFrame} .
  }
  \item{binResolution}{
    How many sections to divide each dimension of colour space into.
  }
}
\value{
  A colour histogram.
}
\references{
  See: http://cns.bu.edu/~gsc/ColorHistograms.html
}

\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
rgbdf<-imageToDataFrame(img)
rgbhist<-rgbImageHist(rgbdf)
}
