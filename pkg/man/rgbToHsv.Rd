\name{rgbToHsv}
\alias{rgbToHsv}
\title{
  Convert RGB values to HSV.
}
\description{
  This function converts colorspace library RGB values to their HSV equivalents.
}
\usage{
rgbToHsv(rgbs)
}
\arguments{
  \item{rgbs}{
    Colorspace library RGB values.
  }
}
\value{
  Colorspace library HSV values.
}
\examples{
rgbs<-RGB(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6), c(0.7, 0.8, 0.9))
hsvs<-rgbToHsv(rgbs)
}
