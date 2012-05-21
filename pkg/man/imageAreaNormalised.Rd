\name{imageAreaNormalised}
\alias{imageAreaNormalised}
\title{
  Get the factors required to scale the images to an area of 1.0
}
\description{
  Calcluate the factors required to scale each image so that it has an
  area of 1.0 . This is useful for trying to use the areas of
  differently proportioned images to indicate values. The resulting
  values can be used as cex arguments.
}
\usage{
imageAreaNormalised(images)
}
\arguments{
  \item{images}{
    The images to generate the scale factors for.
  }
}
\value{
  The scale factors required to scale the images to an area of 1.0 .
}
\examples{
library(CulturalAnalytics)
library(jpeg)

# Get the images

imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
dirimgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
imgs<-lapply(dirimgs, function(imgpath){readJPEG(imgpath)})

# Calculate the scale factors required to draw them sized 1.0 each
imageAreaNormalised(imgs)

}
