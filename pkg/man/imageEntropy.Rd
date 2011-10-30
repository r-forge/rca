\name{imageEntropy}
\alias{imageEntropy}
\title{
  Calculate the entropy of the image
}
\description{
  A function that calculates the entropy of the image based on its histogram.
}
\usage{
imageEntropy(histogram)
}
\arguments{
  \item{histogram}{
    A histogram of the image pixel values, as calculated by the
    \code{hist()} function.
  }
}
\details{
  The entropy of an image is a useful indicator of how visually complex the image is.
}
\value{
  A value representing the entropy of the image.
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
histogram<-hist(imageData(channel(img, "grey")), breaks=0:255/255,
                plot=FALSE)
entropy<-imageEntropy(histogram)
}
