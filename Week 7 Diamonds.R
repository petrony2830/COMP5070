# ##################################### QPLOT (FROM GGPLOT2) DEMONSTRATION USING THE DIAMONDS DATA SET #####################################
#
# Quote: "ggplot2 is an R package for producing statistical, or data, graphics, but it is unlike most other graphics packages because it 
# has a deep underlying grammar. This grammar, based on the Grammar of Graphics (Wilkinson, 2005), is composed of a set 
# of independent components."
#
# "ggplot2 is designed to work in a layered fashion, starting with a layer showing the raw data then adding layers of annotation 
# and statistical summaries"  - H.Wickham, ggplot2, Use R, DOI 10.1007/978-0-387-98141_1, © Springer Science+Business Media, LLC 2009
#
# When using ggplot2, plots are built up in layers, first the default data is specified and then the graphical objects, geoms and statistical views, stats are put on top. Many different layers can be added as # required and each layer can use data from a different data frame.


# load ggplot2 - make sure it's already installed!
library(ggplot2)

# Check the available in-built data sets in the loaded packages
data()

# Check the available in-built data sets in all installed packages
data(package = .packages(all.available = TRUE))

# Check the dimensions of the Diamonds data set
dim(diamonds)

# Use head() to look at the first few rows - what variable types do we see?
head(diamonds)


# To help with the analysis, sometimes it's a good idea to randomly draw a smaller sample
# from the larger data set.  You can use the set.seed() function - pick your own number to use.

set.seed(23)
dsmall <- diamonds[sample(nrow(diamonds),100),]

# Check the dimensions and first few rows of dsmall
dim(dsmall)
head(dsmall)

#The ggplot2 package implements a system for creating graphics in R based on a com- prehensive and coherent grammar. This provides a consistency
# to graph creation often lacking in R, and allows the user to create graph types that are innovative and novel.
# The simplest approach for creating graphs in ggplot2 is through the qplot() or quick plot function. The format is
# 
# qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, facets=, xlim=, ylim=, xlab=, ylab=, main=, sub=)

# ########################################## CREATING SCATTERPLOTS ##########################################

# Create a scatterplot of carat vs price.  Use qplot()
qplot(carat, price, data = diamonds)

# I don't like black dots.  I'd like seagreen3 dots.  Look at what happens here:
qplot(carat, price, data = diamonds,colour="seagreen3")

# Colour is handled counterintuitively in ggplot2
# To specify it, access geom_point() and set the colour in there 
ggplot(diamonds, aes(x=carat, y=price)) + 
    geom_point(size=3, colour="seagreen3")         # seagreen 3

# We can use ggplot to produce other types of scatterplots
# E.g. a line plot broken down by group (clarity)
ggplot(dsmall, aes(x=carat,y=price)) + 
    geom_line(aes(colour=clarity, group=clarity)) + # colour, group both depend on cond2
    geom_point(aes(colour=clarity),               # colour depends on cond2
               size=3)  

# Or a line plot, broken down by group (clarity)
ggplot(dsmall, aes(x=carat,y=price)) + 
    geom_line(aes(colour=clarity, group=clarity)) + # colour, group both depend on cond2
    geom_point(aes(colour=clarity, shape=clarity))  # larger points, different shape

# qplot() accepts transformations - e.g. log transform for curvature
qplot(log(carat), log(price), data = diamonds)

# Playing with colour a little bit more, this time using data = dsmall to pick the colours
# We can also change the shapes of the symbols plotted, based on cut category
qplot(carat, price, data = dsmall, colour = color,shape = cut)   # qplot automatically picks the colours

# Let's compare 3 plots side-by-side
par(mfrow=c(1,3))

# In large data sets, it is often a nice idea to make data points transparent, to facilitate reading
# In ggplot2 we can use the alpha option
# What does I mean? (try it without I())

qplot(carat, price, data = diamonds, alpha = I(0.1))

# Or could write this as
ggplot(diamonds,aes(x=carat, y=price)) + 
	geom_point(alpha = 0.1)

# Why don't we have one plot in a space for 3?  This is not implemented in ggplo2.  Instead we need a workaround using grid.arrange
# We thus need to install the library gridExtra

library(gridExtra)
q1 <- qplot(carat, price, data = diamonds, alpha = I(1/10))
q2 <- qplot(carat, price, data = diamonds, alpha = I(1/50))
q3 <- qplot(carat, price, data = diamonds, alpha = I(1/80))
grid.arrange(q1, q2, q3, ncol=3)


# Note that qplot is not limited to scatterplots, but can produce almost any kind of plot by varying the geom option.  
# The following geoms enable you to investigate two-dimensional relationships:
#	geom = "point" draws points to produce a scatterplot. This is the default.
#	geom = "smooth" fits a smoother to the data.
#   geom = "boxplot" produces a boxplot.
#	geom = "path" and geom = "line" draw lines between the data points. Traditionally these are used to explore relationships between time and another variable, but lines may be used to join observations connected in some other way. 

# For a single continuous variable:
#  geom = "histogram" draws a histogram (default when supplying only one variable to qplot())
#  geom = "density" creates a density plot 

# For a single discrete variable:
#  geom = "bar" makes a bar chart.

# Try adding smoothers to the carat vs price scatterplot - do this for the dsmall and diamonds data:
q1<-  qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
q2 <- qplot(carat, price, data = diamonds, geom = c("point", "smooth"))
grid.arrange(q1, q2,ncol=2)


# Try drawing a histogram of clarity, using the cut to produce appropriate colours
qplot(clarity, data=diamonds, fill= cut, geom="bar")

# Or you can map colours using ggplot
ggplot(dsmall, aes(x=clarity, fill=cut)) + geom_bar()

# Want more control? Use ggplot()
# green fill, blue outlines
ggplot(dsmall, aes(x=clarity)) + geom_bar(fill="seagreen3", colour="darkblue")


# Try drawing a histogram of carat, using the color (of the diamonds) to produce appropriate colours
qplot(carat, data = diamonds, geom = "histogram", fill = color)

# Histogram bin width is important 
q1 <- qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, xlim = c(0,3))
q2 <- qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))
q3 <- qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim = c(0,3))
grid.arrange(q1, q2,q3, ncol=3)


# Compare density with histogram plots	
q1<-  qplot(carat, data = diamonds, geom = "density", colour = color) 
q2 <- qplot(carat, data = diamonds, geom = "histogram", fill = color)
grid.arrange(q1, q2,ncol=2)


# Plot the carat data as histograms with a binwidth of 0.1 and have one histogram per colour
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3),fill=color)


# ##################################### GGPLOT2 EXTRA DEMONSTRATION USING THE DIAMONDS DATA SET #####################################

p1 <- 
    ggplot(diamonds, aes(x=carat, y=price, colour=color, group=cut)) +
    geom_line() +
    ggtitle("Price by cut and colour")

# Second plot
p2 <- 
    ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Price by colour")

# Third plot
p3 <- 
    ggplot(subset(diamonds, table==58), aes(x=carat, colour=color)) +
    geom_density() +
    ggtitle("Price by Carat (Table == 58)")

# Fourth plot
p4 <- 
    ggplot(subset(diamonds, table==58), aes(x=carat, fill=color)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(color ~ .) +
    ggtitle("Price, by colour") +
    theme(legend.position="none")        # No legend (redundant in this graph)    

#Once the plot objects are set up, we can render them with multiplot. This will make two columns of graphs: 
source("multiplot.r")
multiplot(p1, p2, p3, p4, cols=2)

# ##################################### END OF GGPLOT2 DEMONSTRATION USING THE DIAMONDS DATA SET #####################################
