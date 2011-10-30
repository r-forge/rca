\name{rangeRgb}
\alias{rangeRgb}
\title{
  Calculate the range for each component of an RGB colour
}
\description{
  This function calculates the ranges for the components of an RGB
  colorspace library objects and returns them as named values in a list.
}
\usage{
rangeRgb(rgbcoords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rgbcoords}{
    The coordinates of colorspace library RGB objects, as returned by
    \code{coords()} .
  }
}
\value{
  A list with the range for the R, G and B values under those names.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
rgbcoords<-coords(rgbs)
rgbrange<-rangeRgb(rgbcoords)
}
