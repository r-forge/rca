\name{medianRgb}
\alias{medianRgb}
\title{
Calculate the median HSV colour of an image.
}
\description{
  Calculate the median values for the RGB coordinates of the image.
  The colour returned is not necessarily a colour in the image, it just contains the median values.
}
\usage{
medianRgb(rgbcoords)
}
\arguments{
  \item{rgbcoords}{
    RGB co-ordinates
  }
}
\value{
  A colorspace library RGB object representing the median RGB colour of the
  image. This may not match a colour actually in the image.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
rgbcoords<-coords(rgbs)
rgbMed<-medianRgb(rgbcoords)
}
