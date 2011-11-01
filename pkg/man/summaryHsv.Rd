\name{summaryHsv}
\alias{summaryHsv}
\title{
  Summarise the statistical properties of HSV colours.
}
\description{
  Use the summary function to analyse the hue, saturation and value components
  of colorspace library HSV values.
}
\usage{
summaryHsv(hsvs)
}
\arguments{
  \item{hsvs}{
    colorspace library HSV values
  }
}
\value{
  A list containing summary objects for the H, S and V values.
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
imgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
img<-readImage(imgs[1])
rgbs<-imageToRgb(img)
hsvs<-rgbToHsv(rgbs)
summaryhsv<-summaryHsv(hsvs)
}