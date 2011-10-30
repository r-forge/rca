\name{imageToIntensity}
\alias{imageToIntensity}
\title{
  imageToIntensity
}
\description{
  Convert an image to an intensity map.
}
\usage{
imageToIntensity(image)
}
\arguments{
  \item{image}{
    An EBImage Image object.
  }
}
\details{
  Many algorithms require an intensity map of the image rather than the
  full-colour version of the image. An intensity map is just a greyscale version of the image.
}
\value{
  A greyscale EBImage (the ``grey'' channel of the EBImage).
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
intensity<-imageToIntensity(img)
}