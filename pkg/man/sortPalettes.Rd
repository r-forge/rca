\name{sortPalettes}
\alias{sortPalettes}
\title{
Sort palettes in order of brightness
}
\description{
This function sorts a vector of palettes in rough order of brightness
(using the sum of each component in a palette as its brightness).
}
\usage{
sortPalettes(palettes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{palettes}{
    A list of palettes.
  }
}
\value{
  A sorted list of palettes, in brighness order.
}

\examples{
palettes<-list(data.frame(red=c(0.1, 0.2, 0.9), green=c(0.4, 0.5, 0.9),
blue=c(0.7, 0.8, 0.9)),
data.frame(red=c(0.1, 0.2, 0.3), green=c(0.4, 0.5, 0.6),
blue=c(0.7, 0.8, 0.9)),
data.frame(red=c(0.1, 0.2, 0.1), green=c(0.4, 0.5, 0.1),
blue=c(0.7, 0.8, 0.1)))
sorted<-sortPalettes(palettes)
}
