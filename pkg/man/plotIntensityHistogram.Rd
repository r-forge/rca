\name{plotIntensityHistogram}
\alias{plotIntensityHistogram}
\title{
Plots a histogram of the intensity of an image's pixels
}
\description{
  This function calculates a histogram for the pixel values of an intensity
  map (a greyscale image) and plots it. It should not be called on a colour image.
}
\usage{
plotIntensityHistogram(image, bins = 255, main = "", xlab = "Value", col = grey(0))
}
\arguments{
  \item{image}{
    An intensity map. A greyscale image created using \code{imageToIntensity} .
  }
  \item{bins}{
    The number of bins to use for the histogram.
  }
  \item{main}{
    The main title label.
  }
  \item{xlab}{
    The X axis label.
  }
  \item{col}{
    The R color to use for the histogram.
  }
}
\value{
  The histogram (generated by \code{hist()} ).
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
intensity<-imageToIntensity(img)
plotIntensityHistogram(intensity)
}
