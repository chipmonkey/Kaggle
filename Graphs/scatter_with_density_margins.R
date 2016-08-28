#
# Stolen with love from: http://people.duke.edu/~csm29/ggplot2/ggplot2.html
#

library(ggplot2)
library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

scatterWithMargins <- function(plotData, x, y, color){
  xy <- ggplot(plotData, aes_string(x=x, y=y, col = color))+
    geom_point()+
    geom_density2d()+
    theme_classic()
  xDen <- ggplot(plotData, aes_string(x=x, fill = color, col=color))+
    geom_density(alpha=0.1)+
    theme_classic()+
    xlab("")
  yDen <- ggplot(plotData, aes_string(x=y, fill = color, col=color))+
    geom_density(alpha=0.1)+
    coord_flip()+
    theme_classic()+
    xlab("")
  leg <- g_legend( xDen )
  grid.arrange(xDen+theme(legend.position = "none"),
               leg,
               xy+theme(legend.position = "none"),
               yDen+theme(legend.position = "none"),
               nrow = 2, ncol = 2,
               widths = unit(c(2,1), c("null", "null")),
               heights = unit(c(1,2), c("null", "null"))
  )
}

scatterWithMargins(iris, "Sepal.Length", "Sepal.Width", "Species")
