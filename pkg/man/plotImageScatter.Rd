\name{plotImageScatter}
\alias{plotImageScatter}
\title{
  Construct a scatter plot using various elements including images.
}
\description{
  This function constructs a scatter plot using graphical elements:
  points, lines, images and labels. The order and position of these
  elements has been chosen for legibility, in particular the positioning
  of images down and to the right of the co-ordinate they represent.
  Which elements are plotted can be controlled using the should... parameters.
}
\usage{
plotImageScatter(images, xValues, yValues, labelValues,
lineCol = par("col"), lineWidth = 1, pointStyle = 19, pointCol = par("col"),
thumbnailWidth = 0.3, labelCol = par("col"), labelSize = 1,
shouldDrawLines = TRUE, shouldDrawPoints = TRUE, shouldDrawImages = TRUE,
shouldDrawLabels = TRUE)
}
\arguments{
  \item{images}{
    A list of EBImage images. 
  }
  \item{xValues}{
    The x co-ordinates at which to plot the elements.
  }
  \item{yValues}{
    The y co-ordinates at which to plot the elements.
  }
  \item{labelValues}{
    The labels for the x,y co-ordinates.
  }
  \item{lineCol}{
    The R color for lines.
  }
  \item{lineWidth}{
    The line width.
  }
  \item{pointStyle}{
    The R point style.
  }
  \item{pointCol}{
    The R color for points.
  }
  \item{thumbnailWidth}{
    The width to standardize images to.
  }
  \item{labelCol}{
    The R color for labels.
  }
  \item{labelSize}{
    The size for labels.
  }
  \item{shouldDrawLines}{
    Whether to draw lines or not.
  }
  \item{shouldDrawPoints}{
    Whether to draw points or not.
  }
  \item{shouldDrawImages}{
    Whether to draw the images or not
  }
  \item{shouldDrawLabels}{
    Whether to draw the labels or not.
  }
}
\value{
  None.
}
\references{
  Based on the documented behaviour of ImagePlot: http://lab.softwarestudies.com/p/imageplot.html
}
\examples{
imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
dirimgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
imgs<-lapply(dirimgs, function(imgpath){readImage(imgpath)})
summaries<-lapply(imgs,
  function(img){rgbs<-imageToRgb(img)
    hsvs<-rgbToHsv(rgbs)
    summaryHsv(hsvs)})
vMedian<-as.numeric(lapply(summaries, function(summary){summary$V[["Median"]]}))
sMean<-as.numeric(lapply(summaries,
function(summary){summary$S[["Mean"]]}))
png(filename="catest.png", width=4, height=2, units="in", res=72)
plot.new()
title(main="plotImageScatter", xlab="Value Median", ylab="Saturation Mean")
plotImageScatter(imgs, vMedian, sMean, dirimgs)
axis(1)
axis(2)
dev.off()
}
