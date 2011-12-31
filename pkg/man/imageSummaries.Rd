\name{imageSummaries}
\alias{imageSummaries}
\title{
  Produce summaries of image properties
}
\description{
  This function produces a matrix of summaries for a list of images.
}
\usage{
imageSummaries(images)
}
\arguments{
  \item{images}{
    A list of raster images
  }
}
\value{
  A matrix of summary values for the images. Each row represents a
  single image. See the column names for what the values represent, they
  are the summaryHsv and sdHsv values.
}
\examples{
library(CulturalAnalytics)
library(jpeg)

stripExtension<-function(filename){
  sub("\\\\.[^.]+$", "", filename)
}

underscoresToSpaces<-function(source){
  gsub("_", " ", source)
}

cleanFilename<-function(filepath){
  filename<-basename(filepath)
  name<-stripExtension(filename)
  underscoresToSpaces(name)
}

imgdir<-paste(system.file(package = "CulturalAnalytics"),
              "images/Claude_Monet/Haystacks", sep = "/")
imgPaths<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
imgs<-lapply(imgPaths, function(path){readJPEG(path)})
imgNames<-cleanFilename(imgPaths)
imageSummariesData<-data.frame(name=imgNames,
                               imageSummaries(imgs),
                               stringsAsFactors=FALSE)
}