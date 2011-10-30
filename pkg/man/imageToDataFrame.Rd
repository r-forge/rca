\name{imageToDataFrame}
\alias{imageToDataFrame}
\title{
  Convert an image's pixels to a data.frame of RGB values.
}
\description{
  This function converts the channels of an EBImage image to a
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
    An EBImage RGB image.
  }
}
\value{
    A data frame with columns named``red'',``green'' and ``blue''.
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToDataFrame(img)
}