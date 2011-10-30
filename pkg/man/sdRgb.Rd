\name{sdRgb}
\alias{sdRgb}
\title{
  Calculate the standard deviation of RGB colour values.
}
\description{
  This function calculates the standard deviation of the hue, saturation
  and value of the coords of colorspace library RGB objects.
}
\usage{
sdRgb(rgbcoords)
}
\arguments{
  \item{rgbcoords}{
    The results of calling coords() on colorspace library RGB objects.
  }
}
\value{
  A vector with named values for the R, G and B standard deviations.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
rgbcoords<-coords(rgbs)
sdrgb<-sdRgb(rgbcoords)
}
