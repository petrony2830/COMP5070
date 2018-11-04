
# ################################## Introductory Visualisation using mtcars (in-built data set) ##################################

# A nice command to run if you want to check which packages are loaded
# Note that R pre-loads packages for you, so as soon as you run R, the command will return a result!
(.packages())

# Check the available in-built data sets in the loaded packages
data()

# Check the available in-built data sets in all installed packages
data(package = .packages(all.available = TRUE))


# For this practical you will need the packages:
# lattice
# vcd
# ggplot2

# to install a package try
install.packages("ggplot2")

# attach the mtcars data using attach()
attach(mtcars)

# Explore the data (use the head() function)
head(mtcars)

# Create categorical variables for gear and cyl using factor()
# For gear, use the levels 3, 4 and 5.
# For cyl, use the levels 4,6 and 8.

gear <- factor(gear, levels = c(3, 4, 5), 
    labels = c("3 gears", "4 gears", "5 gears"))

cyl <- factor(cyl, levels = c(4, 6, 8), 
    labels = c("4 cylinders", "6 cylinders", "8 cylinders"))


# Load the vcd packge  (Visualising Categorical Data)
# install.packages("vcd")
library(vcd)   # needs :colorspace

# Load the ggplot2 package
library(ggplot2)   # needs: digest, gtable, proto, reshape2, stringr, Rcpp
			 # scales, munsell

# Load the lattice package
library(lattice)


# Using table(), produce cell counts for the categories in cyl
counts <- table(cyl)
counts

par(mfrow=c(1,1))

# ########################################### BARPLOTS ##############################################


# Produce a simple bar plot using barplot
# Specify a main title, axis labels and pick your favourite colour 
barplot(counts, 
        main = "Simple Bar Plot", 
        xlab = "Cylinders", 
        ylab = "Frequency",
        col = 'coral')

# Make the barplot horizontal using the horiz= option
barplot(counts, 
        main = "Horizontal Bar Plot", 
        xlab = "Frequency", 
        ylab = "Cylinders", 
        col='coral', 
        horiz = TRUE)

# Now produce a two-way table of cyl vs gear, using table()
# Store the result in counts2
counts2 <- table(cyl,gear)
counts2

# Produce a stacked barplot of gears, broken down by cylinders
barplot(counts2, main = "Stacked Bar Plot", xlab = "Gears", 
    ylab = "Frequency", col = c("coral", "darkgoldenrod1", "darkolivegreen1"), 
    legend = rownames(counts2))
    
# Now produce a grouped barplot
barplot(counts2, main = "Grouped Bar Plot", xlab = "Gears", 
    ylab = "Frequency", col = c("coral", "darkgoldenrod1", "darkolivegreen1"), 
    legend = rownames(counts2), 
    beside = TRUE)


# How to produce a stacked barplot of cylinders broken down by gears?
counts2 <- table(gear,cyl)
counts2
barplot(counts2, main = "Stacked Bar Plot", xlab = "Cylinders", 
    ylab = "Frequency", col = c("coral", "darkgoldenrod1", "darkolivegreen1"), 
    legend = rownames(counts2))


# How to fix the legend problem?  Try using the args.legend option
# (there are better ways but this is one)
barplot(counts2, main = "Stacked Bar Plot", xlab = "Cylinders", 
    ylab = "Frequency", col = c("coral", "darkgoldenrod1", "darkolivegreen1"),
    legend=TRUE, args.legend=list(x="topright"))
    

# We don't have to plot just counts ... we can plot all sorts of statistics, e.g. means.

# Compute the means of mpg, aggregated by cyl
# !!!! NOTE: mpg is overshadowed by another mpg - demo of why attach()
# is dangerous!!!
means <- aggregate(mtcars$mpg, by=list(cyl), FUN=mean)

# Order the means in ascending order.  What is $x in this case?!?
means <- means[order(means$x), ]
means

# Create a barplot of the means of the data barplot()
barplot(means$x, 
        names.arg = means$Group.1,
        col = c("coral", "darkgoldenrod1", "darkolivegreen1"))
title("Mean Miles per Gallon by Cylinders")


# We can fit labels in bar plots using the par() functions
# mar plays with margins
par(mar = c(5, 8, 4, 2))

# las plays with the orientation of the tick mark labels 
# and any other text added to a plot after its initialisation
# the options are as follows: always parallel to the axis (the default, 0), 
# always horizontal (1), always perpendicular to the axis (2), and always vertical (3).
par(las = 1)

# Now produce a barplot of the data in counts (Cars by Cylinders)
barplot(counts, main = "Cars by Cylinders", horiz = TRUE, 
    cex.names = 0.8, names.arg = rownames(cyl), col='coral')
    
    
# ######################################### HISTOGRAMS #############################################

# Create a graphics space that allows for 4 plots, 2 in 2 rows.  Use the par() command

par(mfrow = c(2, 2))

# Use the hist() command to produce a histogram of the mpg data 
# Note: need to use mtcars$mpg other in trouble
# This is a cheeky fix :)
mpg <- mtcars$mpg
hist(mpg)

# Now make the histogram more impressive: 12 bins, a nicer colour, x-axis label and main title
hist(mpg, breaks = 12, col = "coral", 
    xlab = "Miles Per Gallon", 
    main = "Coloured histogram with 12 bins")

# Now make the histogram even more impressive
# Add a rug plot using rug() and superimpose a curve using lines() and density()
hist(mpg, probability = TRUE, breaks = 12, col = "coral", 
    xlab = "Miles Per Gallon", 
    main = "Histogram, rug plot, density curve")
rug(jitter(mpg))   #adds tick marks
lines(density(mpg), col = "blue", lwd = 2)


# Now make the histogram even more impressive with a superimposed Normal curve and a box
# around the entire picture
h <- hist(mpg, breaks = 12, col = "coral", 
    xlab = "Miles Per Gallon", 
    main = "Histogram with normal curve and box")
xfit <- seq(min(mpg), max(mpg), length = 40)
yfit <- dnorm(xfit, mean = mean(mpg), sd = sd(mpg))
yfit <- yfit * diff(h$mids[1:2]) * length(mpg)
lines(xfit, yfit, col = "blue", lwd = 2)
box()

# restore original graphic parameters using par()
par(mfrow = c(1, 1))

# Try a very different sort of density plot using qplot from ggplot2
#qplot(mpg, data=mtcars, geom=c("density"),facets=cyl~., fill=cyl)


# ############################### BOXPLOTS ################################################

# Create a 1x2 space using par()

# Start with a 'simple' boxplot of mpg, broken down by cyl
par(mfrow = c(1, 2))
boxplot(mpg ~ cyl, data = mtcars, 
    main = "Car Mileage Data", 
    xlab = "Number of Cylinders", 
    ylab = "Miles Per Gallon",col='seagreen3')

# Change the style of boxplots using notch=TRUE
boxplot(mpg ~ cyl, data = mtcars, notch = TRUE, 
    varwidth = TRUE, col = "coral", 
    main = "Car Mileage Data", 
    xlab = "Number of Cylinders", 
    ylab = "Miles Per Gallon")

par(mfrow = c(1, 1))

# Restore the original graphics space using par()
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_violin(aes(fill = factor(cyl)))


# This is more impressive (requires the lattice package to be loaded)
# Using bwplot() produce a boxplot broken down by cyl and gears.  Each boxplot will contain 3 boxes (one per cylinder)
# and there will be three boxplots frames in total (one per gear)
# Now produce the plot using bwplot()
bwplot(cyl ~ mpg | gear, 
    main = "Box Plots by Cylinders and Gears", 
    xlab = "Miles per Gallon", ylab = "Cylinders")

bwplot(gear ~ mpg | cyl, 
       main = "Box Plots by Cylinders and Gears", 
       xlab = "Miles per Gallon", ylab = "Cylinders")

# What is the formula  y ~ x | A*B    ???
# We just used that above in bwplot().  The variables y and x on the left side of the vertical bar are called the primary variables.
# The variables on the right are the conditioning variables.
# Primary variables map variables to the axes in each panel.  Here, y~x describes the variables to place on the vertical and horizontal axes, respectively.  For single-variable plots, replace y~x with ~x.

# On the right hand side we use *, which produces combinations of factors.  In the above example we only consider gear which is precisely the formulation
# y~x|A 

# Following this logic, we have:
# ~x|A displays the numeric variable x for each level of factor A. 
# y~x|A*B displays the relationship between numeric variables y and x separately for every combination of factor A and B levels. 
# A~x displays categorical variable A on the vertical axis and numeric variable x on the horizontal axis. 
# ~x displays the numeric variable x alone. 



# Create factors from the original cylinder data (use mtcars$cyl)
cylinder <- as.factor(mtcars$cyl)

# Create a boxplot using qplot() from the ggplot2 library
qplot(cylinder, mpg, data=mtcars, geom=c("boxplot", "jitter"),
    fill=cylinder,
    main="Box plots with superimposed data points",
    xlab= "Number of Cylinders",
    ylab="Miles per Gallon")


# ######################### DOTCHARTS ###############################################

# Create a dotchart of the mpg data.  Set the title and xlabel and use cex = 0.7 (or experiment!)
dotchart(mpg, labels = row.names(mtcars), 
    cex = 0.7, 
    main = "Gas Milage for Car Models", 
    xlab = "Miles Per Gallon")


# Colourise & sort the grouped dot chart!
# Plot mpg however factor the cars by groups formed by cyl
x <- mtcars[order(mpg), ]
x$cyl <- factor(x$cyl)

x$colour[x$cyl == 4] <- "coral"
x$colour[x$cyl == 6] <- "blue"
x$colour[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, labels = row.names(x), cex = 0.7, 
    pch = 19, groups = x$cyl, 
    gcolor = "black", color = x$colour, 
    main = "Gas Milage for Car Models\ngrouped by cylinder", 
    xlab = "Miles Per Gallon")
    
# Using dotplot() from the lattice library, create dotplots of the mpg data broken down by cylinder (in each plot)
# with 3 plots overall (corresponding to gear)    
dotplot(cyl ~ mpg | gear, 
    main = "Dot Plots by Number of Gears and Cylinders", 
    xlab = "Miles Per Gallon")


# ############################################## SCATTERPLOTS ####################################################

# Create a scatterplot using xyplot() from the lattice package, with mpg broken down by car weight (wt), cylinder
# and gear (hard to explain but let's see what happens!)
xyplot(mpg ~ wt | cyl * gear, 
    main = "Scatter Plots by Cylinders and Gears", 
    xlab = "Car Weight", ylab = "Miles per Gallon")

# We can also create scatterplots in a 3D cube using cloud()
# In this example, plot mpg as the cube 'height', and wt and qsec as the remaining dimensions (length, width)
cloud(mpg ~ wt * qsec | cyl, 
    main = "3D Scatter Plots by Cylinders")

 
 
 
# How about inserting regression lines?
# Use qplot() from ggplot2 and analyse the transmission data by creating factors from am
transmission <- factor(am, levels = c(0, 1), 
    labels = c("Automatic", "Manual"))
    
# produce the scatterplot with regression lines of wt vs mpg broken down by transmission type
qplot(wt, mpg, data = mtcars, 
    color = transmission, shape = transmission, 
    geom = c("point", "smooth"), 
    method = "lm", formula = y ~ x, 
    xlab = "Weight",  ylab = "Miles Per Gallon", 
    main = "Regression Example")


# ################################## UNLOAD PACKAGES ####################################

detach(mtcars)
# unload all packages that were manually loaded
pkgs = names(sessionInfo()$otherPkgs)
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
