\name{paletteToR}
\alias{paletteToR}
\title{
  Convert a palette to R colours
}
\description{
  This function converts a palette represented as a data.frame into a vector of R colours ready for plotting.
}
\usage{
paletteToR(palette)
}
\arguments{
  \item{palette}{
    A data.frame with red, green and blue columns representing a palette..
  }
}
\value{
  A vector of R colors.
}

\examples{
library(colorspace)
palette<-data.frame(red=c(0.1, 0.2, 0.3), green=c(0.4, 0.5, 0.6), blue=c(0.7, 0.8, 0.9))
rColours<-paletteToR(palette)
}
