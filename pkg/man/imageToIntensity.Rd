\name{imageToIntensity}
\alias{imageToIntensity}
\title{
  imageToIntensity
}
\description{
  Convert an image to an intensity map.
}
\usage{
imageToIntensity(image, method="perceptual")
}
\arguments{
  \item{image}{
    An RGB raster image.
  }
  \item{method}{
    A string naming the method to use for colour conversion.
    "mean" - Use the mean of the red, green and blue channels.
    "perceptual"- Weight the red, green and blue channels perceptually.
  }
}
\details{
  Many algorithms require an intensity map of the image rather than the
  full-colour version of the image. An intensity map is a greyscale version of the image.
}
\value{
  A greyscale image matrix.
}
\examples{
library(CulturalAnalytics)
library(jpeg)
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readJPEG(imgs[1])
intensity<-imageToIntensity(img)
}