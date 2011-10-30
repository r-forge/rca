\name{imageToRgb}
\alias{imageToRgb}
\title{
  Convert an image's pixels to a vector of RGB objects.
}
\description{
  This function converts the channels of an EBImage image to a vector of
  colorspace library RGB objects. These objects can be processed using
  colorspace functions and vector processing functions.
}
\usage{
imageToRgb(image)
}
\arguments{
  \item{image}{
    An RGB EBImage image
  }
}
\value{
  A vector of colorspace RGB objects, one for each pixel in the
  image. Rows can be extracted from this as ranges within the vector.
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
}