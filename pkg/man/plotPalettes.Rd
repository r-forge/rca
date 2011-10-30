\name{plotPalettes}
\alias{plotPalettes}
\title{
  Plot a palettes with optional labels
}
\description{
  Plot a list of palettes as a series of rows of coloured squares,
  optionally with labels rendered to the left of each palette.
}
\usage{
plotPalettes(palettes, names = FALSE)
}
\arguments{
  \item{palettes}{
    A list of palettes.
  }
  \item{names}{
    A vector of labels for the palettes.
  }
}
\value{
  None.
}
\examples{
palettes<-list(data.frame(red=c(0.1, 0.5, 0.9), green=c(0.4, 0.5, 0.9),
blue=c(0.7, 0.8, 0.9)),
data.frame(red=c(0.1, 0.4, 0.3), green=c(0.4, 0.5, 0.6),
blue=c(0.6, 0.8, 0.9)),
data.frame(red=c(0.1, 0.3, 0.1), green=c(0.4, 0.5, 0.1),
blue=c(0.5, 0.8, 0.1)))
paletteNames<-c("First Palette", "Second Palette", "Third Palette")
## You'll need to allow space for palette names when added
par(mar=c(4, 8, 4, 4))
plotPalettes(palettes, paletteNames)
}
