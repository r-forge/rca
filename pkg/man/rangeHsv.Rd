\name{rangeHsv}
\alias{rangeHsv}
\title{
  Calculate the range for each component of an HSV colour
}
\description{
  This function calculates the ranges for the components of an HSV
  colorspace library objects and returns them as named values in a list.
}
\usage{
rangeHsv(hsvcoords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hsvcoords}{
    The coordinates of colorspace library HSV objects, as returned by
    \code{coords()} .
  }
}
\value{
  A list with the range for the H, S and V values under those names.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
hsvs<-rgbToHsv(rgbs)
hsvcoords<-coords(hsvs)
hsvrange<-rangeHsv(hsvcoords)
}
