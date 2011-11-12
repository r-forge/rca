\name{sdRgb}
\alias{sdRgb}
\title{
  Calculate the standard deviation of RGB colour values.
}
\description{
  This function calculates the standard deviation of the hue, saturation
  and value of colorspace library RGB objects.
}
\usage{
sdRgb(rgbs)
}
\arguments{
  \item{rgbs}{
    colorspace library RGB valuess.
  }
}
\value{
  A vector with named values for the R, G and B standard deviations.
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
rgbs<-imageToRgb(img)
sdrgb<-sdRgb(rgbs)
}
