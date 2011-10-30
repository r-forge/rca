\name{sdHsv}
\alias{sdHsv}
\title{
  Calculate the standard deviation of HSV colour values
}
\description{
  This function calculates the standard deviation of the hue, saturation
  and value of the coords of colorspace library HSV objects.
}
\usage{
sdHsv(hsvcoords)
}
\arguments{
  \item{hsvcoords}{
    The results of calling coords() on colorspace library HSV objects.
  }
}
\value{
  A vector with named values for the H, S and V standard deviations.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
hsvs<-rgbToHsv(rgbs)
hsvcoords<-coords(hsvs)
sdhsv<-sdHsv(hsvcoords)
}
