\name{imageToDataFrame}
\alias{imageToDataFrame}
\title{
  Convert an image's pixels to a data.frame of RGB values.
}
\description{
  This function converts the channels of a raster image to a
  data.frame with red, green and blue columns. Each row of the
  data.frame represents one pixel in the image. Rows of the image can be
  accessed as ranges of rows in the data.frame.

  This format is used by the palette and colour histogram functions.
}
\usage{
imageToDataFrame(image)
}
\arguments{
  \item{image}{
    An RGB raster image.
  }
}
\value{
    A data frame with columns named``red'',``green'' and ``blue''.
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
rgbs<-imageToDataFrame(img)
}