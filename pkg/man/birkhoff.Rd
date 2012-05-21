\name{birkhoff}
\alias{birkhoff}
\title{
Birkhoff's aesthetic measure for images.
}
\description{
  Birkhoff's aesthetic measure is expressed as \(M=O/C\)
  Where M is the aesthetic measure, O is the order of the image and C is its
  complexity.
  For this function we use the entropy of the image as the order and the ratio
  of its uncompressed and compressed sizes (using the gzip compression
  algorithm) as its complexity.
}
\usage{
birkhoff(image)
}
\arguments{
  \item{image}{
  An image.
  }
}
\value{
  A number representing the aesthetic measure of the image.
}
\references{
  See: http://gilab.udg.edu/publ/container/publications/jaume-rigau/2007/Conceptualizing%20Birkhoffs%20aesthetic%20measure%20using%20.pdf
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
measure<-birkhoff(img)
}
