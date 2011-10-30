\name{summaryRgb}
\alias{summaryRgb}
\title{
  Summarise the statistical properties of RGB colours.
}
\description{
  Use the summary function to analyse the red, green and blue components
  of colorspace library RGB values.
}
\usage{
summaryRgb(rgbcoords)
}
\arguments{
  \item{rgbcoords}{
    The coords() of colorspace library RGB values
  }
}
\value{
  A list with summaries for the R, G and B values.
}
\examples{
library(colorspace)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
rgbcoords<-coords(rgbs)
summaryrgb<-summaryRgb(rgbcoords)
}
