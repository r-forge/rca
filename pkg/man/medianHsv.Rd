\name{medianHsv}
\alias{medianHsv}
\title{
  Calculate the median HSV colour of an image.
}
\description{
  Calculate the median values for the HSV coordinates of the image.
  The colour returned is not necessarily a colour in the image, it just contains the median values.
}
\usage{
medianHsv(hsvs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hsvs}{
    colorspace library HSV values.
  }
}
\value{
  A colorspace library HSV object representing the median HSV colour of the
  image. This may not match a colour actually in the image.
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
rgbs<-imageToRgb(img)
hsvs<-rgbToHsv(rgbs)
hsvMed<-medianHsv(hsvs)
}
