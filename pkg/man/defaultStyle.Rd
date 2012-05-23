\name{defaultStyle}
\alias{defaultStyle, overlayColor, foregroundColor, backgroundColor,
  lineLwd, pointPch, pointCex, labelCex}
\title{
Default graphics parameters.
}
\description{
  A function to set up a default style for plots, and useful values for
  maintaining that style.

  defaultStyle() sets the background, foreground, line width and point
  style.

  The foreground color, which should be used for plotting, is available
  as foregroundColor. The background color is available as
  backgroundColor. And the colour for axes, legends and titles is
  available as overlayColor. You'll need to specify col.ticks as
  overlayColor manually, unfortunately.

  The width for lines is available as lineLwd, the point style as
  pointPch, the point size as pointCex, and the label size as labelCex.

  In the example, note the specification of frame.plot=FALSE and the
  manual specification of the axis col and col.ticks.
}
\usage{
defaultStyle()
}
\arguments{
}
\value{
}
\references{
}
\examples{
library(CulturalAnalytics)
defaultStyle()
plot(1:10, 1:10, type="n", xlim=c(0,10), ylim=c(0,10), frame.plot=FALSE,
     main="Main Title", sub="Subtitle", xlab="X Axis", ylab="Y Axis")
points(c(1, 2, 5, 8), c(9, 5, 3, 1), cex=pointCex)
axis(1, 0:10, col=overlayColor, col.ticks=overlayColor,
     col.axis=overlayColor)
axis(2, 0:10, col=overlayColor, col.ticks=overlayColor,
     col.axis=overlayColor)
}
