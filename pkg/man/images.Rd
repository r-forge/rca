\name{images}
\alias{images}
\title{
  Construct a scatter plot of images
}
\description{
  The image scatter plot function used by imagePlot, made public so it
  can be used in plots created using other systems.
}
\usage{
images(x, y=NULL, images=NULL, thumbnailWidth=72)
}
\arguments{
  \item{x}{
    The x co-ordinates at which to plot the elements (as per xy.coords() ).
  }
  \item{y}{
    The y co-ordinates at which to plot the elements (as per xy.coords() ).
  }
  \item{images}{
    A list of EBImage images.
  }
  \item{thumbnailWidth}{
    The width to standardize images to, in device independent
    pixels.
  }
}
\value{
  None.
}
\examples{

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
png(filename="images-test.png", width=6, height=6, units="in", res=72)

plot.new()

# Set the background colour
par(bg="gray40")

# Draw the image scatter (and lines, points and labels)
# Note setting of graphics parameters using ...
images(vMedian, sMean, imgs)

dev.off()
}
