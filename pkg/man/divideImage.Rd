\name{divideImage}
\alias{divideImage}
\title{
  divideImage
}
\description{
  Divide an image into a number of smaller images
}
\usage{
divideImage(image, columns, rows)
}
\arguments{
  \item{image}{
    An EBImage Image object.
  }
  \item{columns}{
    The number of columns to divide the image into.
  }
  \item{rows}{
    The number of rows to divide the image into.
  }
}
\details{
  It can be useful to examine sections of an image separately. This
  function allows images to be broken down for such analysis.
  Note that if the image cannot be divided exactly into the number of
  rows and columns specified, some images may have different dimensions
  from others.
}
\value{
  A matrix of EBImages. Elements of the matrix must be accessed using [[.
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
tiles<-divideImage(img, 3, 3)
t13<-tiles[[1,3]]
}
