\name{CulturalAnalytics-package}
\alias{CulturalAnalytics-package}
\alias{CulturalAnalytics}
\docType{package}
\title{
Functions for analysis and plotting of image properties
}
\description{
CulturalAnalytics provides functions for obtaining statistics such as
the standard deviation and mean in the RGB and HSV colour spaces, for
image entropy and histograms in greyscale (intensity) and colour, and
for plotting colour clouds and image scatter charts.

This library uses several different representations for colours and
images as required by the libraries it is based on and to make its own
functions more efficient. Functions to convert between these
representations are provided.

There are several main groups of functions:
Functions containing the word ``to'' convert between representations.
Functions with names beginning in ``plot'' will create a plot.
Other functions calculate and operate on statistical properties and
collections of properties.
}
\details{
  \tabular{ll}{
    Package: \tab CulturalAnalytics\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2011-10-31\cr
    License: \tab GPL (>=3)\cr
    LazyLoad: \tab yes\cr
  }
  To use the library load it using \code{library("CulturalAnalytics")}.
  
  To load images for use with algorithms from the library you will need to
  use the EBImage library. 
}
\author{
  Rob Myers
  
  Maintainer: Rob Myers <rob@robmyers.org>
}
\references{
  The term ``Cultural Analytics'' was coined by Lev Manovich.

  The colour histogram and colour cloud algorithms were adapted from
  work described by Sai Chaitanya Gaddam.
  
  See: http://cns.bu.edu/~gsc/ColorHistograms.html

  The image scatter plot was adapted from the documented behaviour of ImagePlot.

  See: http://lab.softwarestudies.com/p/imageplot.html

  Some other algorithms were adapted from work described by Butler
  et al.

  See: http://staff.science.uva.nl/~asalah/buter11deviantart.pdf
}
\keyword{ package }
