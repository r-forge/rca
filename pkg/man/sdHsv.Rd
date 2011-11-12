\name{sdHsv}
\alias{sdHsv}
\title{
  Calculate the standard deviation of HSV colour values
}
\description{
  This function calculates the standard deviation of the hue, saturation
  and value of colorspace library HSV objects.
}
\usage{
sdHsv(hsvs)
}
\arguments{
  \item{hsvs}{
    colorspace library HSV values.
  }
}
\value{
  A vector with named values for the H, S and V standard deviations.
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
rgbs<-imageToRgb(img)
hsvs<-rgbToHsv(rgbs)
sdhsv<-sdHsv(hsvs)
}
