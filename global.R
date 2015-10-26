# load packages
library(RSocrata)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# read data, .json format failing
d <- read.socrata("https://data.cms.gov/resource/yuq5-65xt.csv")

# convert date to POSIXct from POSIXlt
d$Agreement.Start.Date <- ymd(d$Agreement.Start.Date)
colnames(d)[1] <- "ACO_Name"

# filter to just ACOs in target states
d <- slice(d, grep("Rhode Island|Massachusetts|Connecticut", d$States.Where.Beneficiaries.Reside))

# read measure descritption lookup file
mdesc <- read.csv("aco_quality_measure_desc.csv", colClasses = "character", strip.white = TRUE)

# take just quality measure columns
qm <- select(d, ACO_Name, ACO.1:ACO.33)

# rename columns
colnames(qm)[2:ncol(qm)] <- mdesc$measure

# gather data to long format
qm <- gather(qm, measure, value, -ACO_Name)

avail.measures <- unique(mdesc$measure)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# qmpd <- filter(qm, measure == "ACO.9")
# 
# ggplot(qmpd, aes(ACO_Name, value, colour = value, label = round(value, 2))) +
#   geom_point(size = 10) +
#   geom_text(colour = "black", size = 4) +
#   scale_colour_gradient(low = "red", high = "green") +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") +
#   #geom_hline(yintercept = median(d1$ACO.1), linetype = "dashed") +
#   coord_flip()