\name{imagePlot}
\alias{imagePlot}
\title{
  Construct a scatter plot using various elements including images
}
\description{
  This function constructs a scatter plot using graphical elements:
  points, lines, images and labels. The order and position of these
  elements has been chosen for legibility, in particular the positioning
  of images down and to the right of the co-ordinate they represent.
}
\usage{
imagePlot(x, y=NULL, images=NULL, labels=NULL,
          main="", sub="",
          xlim=NULL, ylim=NULL,
          axes=TRUE, ann=par("ann"),
          text.adj=par("adj"),
          thumbnailWidth=72,
          ...)
}
\arguments{
  \item{x}{
    The x co-ordinates at which to plot the elements (as per xy.coords() .
    Note that the names of the x and y axes are always taken from ...,
    not from xy.coords).
  }
  \item{y}{
    The y co-ordinates at which to plot the elements (as per xy.coords() ).
  }
  \item{images}{
    A list of EBImage images.
  }
  \item{labels}{
    The labels for the x,y co-ordinates.
  }
  \item{main}{
    The main title for the plot
  }
  \item{sub}{
    The subtitle for the plot.
  }
  \item{xlim}{
    A vector of the minimum and maximum x values for the axes. If NULL,
    calculated from x.
  }
  \item{ylim}{
    A vector of the minimum and maximum y values for the axes. If NULL,
    calculated from y.
  }
  \item{axes}{
    Whether to plot the axes or not.
  }
  \item{ann}{
    Whether to annotate the plot or not.
  }
  \item{text.adj}{
    The adj argument for text(). This can't be set in ... as we need to
    specify the two-argument vector version.
  }
  \item{thumbnailWidth}{
    The width to standardize images to, in device independent
    pixels.
  }
  \item{...}{
    Other arguments to pass to the plotting functions. These can be used
    to set colour and other visual properties, and to set labels.
  }
}
\value{
  None.
}
\references{
  Based on the documented behaviour of ImagePlot: http://lab.softwarestudies.com/p/imageplot.html
}
\examples{
# Utility code to transform file names into labels

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

# ^ And then ".*- ([^(]+).*" regex to use standard format of top level samples
# And add text positioning to the plot parameters, defaulting to above right

# Get the images

imgdir<-paste(system.file(package = "CulturalAnalytics"), "images", sep = "/")
dirimgs<-paste(imgdir, dir(path = imgdir, pattern = ".jpg"), sep = "/")
imgs<-lapply(dirimgs, function(imgpath){readImage(imgpath)})

# Get the image property summaries

summaries<-lapply(imgs,
  function(img){rgbs<-imageToRgb(img)
    hsvs<-rgbToHsv(rgbs)
    summaryHsv(hsvs)})
vMedian<-as.numeric(lapply(summaries, function(summary){summary$V[["Median"]]}))
sMean<-as.numeric(lapply(summaries,
function(summary){summary$S[["Mean"]]}))

# Start drawing to file
png(filename="catest.png", width=4, height=2, units="in", res=72)

# Set the background colour
par(bg="gray40")

# Draw the image scatter (and lines, points and labels)
# Note setting of graphics parameters using ...
imagePlot(vMedian, sMean, imgs, cleanFilename(dirimgs),
                 col="white", col.main="white", col.axis="white",
                 col.lab="white", cex=0.6, text.adj=c(0, -0.5),
                 main="imagePlot() Example",
                 xlab="Value Median", ylab="Saturation Median")

dev.off()
}
