
# The source for the data for this practical is:
#
# http://stat-computing.org/dataexpo/2009/the-data.html
#
# There are other years available for analysis, from 1987-2008.
#
# Other links that might be of interest are:
#
# http://apps.bts.gov/xml/ontimesummarystatistics/src/index.xml
# http://www.transtats.bts.gov/Fields.asp?Table_ID=236
#
# These links provide summaries of the data.


# load ff and ffbase
library(ff)
library(ffbase)


# ############################### How to create airline.csv (from the lecture notes )  ##############################

# The following code is a suggestion as to how to download and read the .csv files from the host website
# Data Expo '09 http://stat-computing.org/dataexpo/2009/the-data.html


# Process each year (1987-2008)
# for (year in 1987:2008) {
    # file.name <- paste(year, "csv.bz2", sep = ".")
    # if ( !file.exists(file.name) ) {
        # url.text <- paste("http://stat-computing.org/dataexpo/2009/", year, ".csv.bz2", sep = "")
        # cat("Downloading missing data file ", file.name, "\n", sep = "")
        # download.file(url.text, file.name)
    # }
# } 


# ## Read sample file (any one will do) to get column names and types
# d <- read.csv("2008.csv.bz2")
# integer.columns <- sapply(d, is.integer)
# factor.columns  <- sapply(d, is.factor)
# factor.levels   <- lapply(d[, factor.columns], levels)
# n.rows <- 0L
# ## Process each file determining the factor levels
#
# for (year in 1987:2008) {        
    # file.name <- paste(year, "csv.bz2", sep = ".")
    # cat("Processing ", file.name, "\n", sep = "")
    # d <- read.csv(file.name)
    # n.rows <- n.rows + NROW(d)
    # new.levels <- lapply(d[, factor.columns], levels)
    # for ( i in seq(1, length(factor.levels)) ) {
        # factor.levels[[i]] <- c(factor.levels[[i]], new.levels[[i]])
    # }
# rm(d) } 
# # save(integer.columns, factor.columns, factor.levels, file = "factors.RData")
# # Now convert all factors to integers so we can create a bigmatrix of the d

# col.classes <- rep("integer", length(integer.columns))
# col.classes[factor.columns] <- "character"
# cols  <- which(factor.columns)
# first <- TRUE

# csv.file <- "airlines.csv"   # Write combined integer-only data to this file
# csv.con  <- file(csv.file, open = "w")
# for (year in 1987:2008) {
    # file.name <- paste(year, "csv.bz2", sep = ".")
    # cat("Processing ", file.name, "\n", sep = "")
    # d <- read.csv(file.name, colClasses = col.classes)
    # ## Convert the strings to integers
    # for ( i in seq(1, length(factor.levels)) ) {
        # col <- cols[i]
        # d[, col] <- match(d[, col], factor.levels[[i]])
    # }
    # write.table(d, file = csv.con, sep = ",",
                # row.names = FALSE, col.names = first)
    # first <- FALSE
# }
# close(csv.con)

# You will now have the full data set in one file (airlines.csv).
# This is the file with over 120 million rows of data, discussed in the lecture notes


# ##########################################################################################################


# For the practical session, we will focus on just 1 year of data (2008)

# Read in the data.  This took just over 3 minutes on my computer.
air08 <- read.csv.ffdf(file='2008.csv.bz2',header=T)

# Check object size
print(object.size(air08),units="Mb")

# Check other information
class(air08)
head(air08)

# Create an empty list called result.
# We will use this list to store unique tail numbers.
result <- list()

# Find the unique tail numbers using unique()
result$tailnum <- unique(air08$TailNum)

# How many tail numbers have we found?
length(result$tailnum)

# Produce a variable containing the unique tail numbers and flight numbers.  
# This took around 20 seconds on my computer
result$tailnum.flightnum <- unique(air08[c('TailNum','FlightNum')])

# How many duplicates were there?
# This took around 20 seconds on my computer
sum(duplicated(air08[c('TailNum','FlightNum')]))


# How many NAs are there in the data.  Let's look at ActualElapsedTime
sum(!is.na(air08$ActualElapsedTime))


# Compute a frequency table of Unique Carriers and a visual summary
# Create the table
result$UniqueCarrier <- table.ff(air08$UniqueCarrier, exclude=NA)

# Set an option that does not favour scientific notation
options(scipen = 999)

# Now produce a barplot from the table
barplot(result$UniqueCarrier[order(result$UniqueCarrier)], col = 'darkolivegreen2', horiz = FALSE, cex.names=0.6, main='Unique Carrier Frequency table')

# How does with() work?  This took around 7 seconds.
result$OriginAirport <- with(data=air08[c('Origin')], expr = as.character(Origin))

# Create a two-way table (around 5 seconds on my computer)
result$CarrierOrigin <- table.ff(air08$UniqueCarrier, air08$Origin, exclude=NA)

# Produce a barplot of this table
barplot(result$CarrierOrigin, col = heat.colors(length(rownames(result$CarrierOrigin))), width = 2, xlab='Origin Airport', ylab = 'Frequency', main = 'Flight Carrier by Origin Airport',legend.text = TRUE,args.legend = list(x = "topright", bty = "n", inset=c(0.05, 0)))


# Summary descriptive statistics using ffbase
mean(air08$DepDelay,na.rm=TRUE)

# This will also work
mean(air08[,5],na.rm=TRUE)

# This will not work!!!
mean(air08[,5:8],na.rm=TRUE)

# However we can get around this :)
sapply(air08[,5:8],mean,na.rm=TRUE)

# And we have some flexibility too :)
sapply(air08[,c(5:8,12:16)],mean,na.rm=TRUE)

# This won't work (see the comments below)
sd(air08$DepDelay,na.rm=TRUE)

# Error: is.atomic(x) is not TRUE
#
# This error arises because sd() is not overloaded for ff objects, while mean() is.
#
# Remember, the statistical functions that are overloaded for ff are: sum, min, max, range,
# quantile, hist.
#
# If you want the standard deviation, you'll need to write code :)

# So what else can we do?
sapply(air08[,5:8],range,na.rm=TRUE)
sapply(air08[,5:8],quantile,na.rm=TRUE)

# We can specify particular quantiles, e.g.:
sapply(air08[,5:8],quantile,probs = c(0.33, 0.66, 0.99),na.rm=TRUE)

sapply(air08[,5:8],min,na.rm=TRUE)
sapply(air08[,5:8],max,na.rm=TRUE)

# Produce a histogram of a cumulative sum.
# This code will NOT work! :)
hist(cumsum.ff(air08$DepDelay), col = 'darkolivegreen2')


# However this will :)
hist(cumsum.ff(!is.na(air08$DepDelay)), col = 'darkolivegreen2',xlab='Departure Delay Cumulative Sums')

# maximum cumulative sum? (is it worth it?)
max(cumsum.ff(!is.na(air08$DepDelay)))


# how to apply a function to a group of data?
air08$DepDelayGrouped <- with(data=air08[c('DayOfWeek')], expr = as.character(DayOfWeek))

# Can we analyse by groups?
library(doBy)
result$delaybyday <- ffdfdply(air08[c('DepDelay','DayOfWeek')], split = air08$DepDelayGrouped, FUN=function(x){ 
  summaryBy(DepDelay ~ DayOfWeek, data=x, FUN=sum, keep.names=FALSE, na.rm=TRUE)
 }, trace=FALSE)

# Sort by day of week 
idx <- ffdforder(result$delaybyday[c('DayOfWeek')])
result$delaybydaysort <- result$delaybyday[idx, ]
result$delaybydaysort

barplot(result$delaybydaysort[,2], names.arg = c('Mon','Tues','Wed','Thurs','Fri','Sat','Sun'), col = 'darkolivegreen3', horiz = FALSE, cex.names=0.9, main='Departure Delay by Day of Week')

# We can use boolean indexing, but it's not as straightforward as for data frames
# E.g. select all flights which were NOT cancelled and were delayed by more than 15 minutes, for either the
# departure or arrival
idx <- ffwhich(air08, air08$Cancelled == 0 & (air08$DepDelay >= 15 | air08$ArrDelay >= 15))

# Extract this subset of the data
# Question: what does [,] do? Try without it!
airSubset <- air08[idx,][,]

dim(airSubset)

# Compute the overall ratio of delayed flights
nrow(airSubset)/nrow(air08)

# Compare a specific carrier against others.
nrow(airSubset[airSubset$UniqueCarrier == c("AA"),]) / nrow(air08[idx,][,])

# What was the largest departure delay overall?
max(air08$DepDelay,na.rm=TRUE)

# Summarise Flight Delays by Hour
airSubset$FlightDelayed <- rep(1,nrow(airSubset))
airSubset$DepHour <- airSubset$DepTime/100
df <- summary(sum(FlightDelayed) ~ DepHour, object = airSubset)
head(df)

# ################################################# Bigmemory Analysis ###############################################


library(bigmemory)
library(biganalytics)

# The default big.matrix is not shared across processes and is limited to available RAM. 
# shared big.matrix has identical size constraints as the basic big.matrix, but may be shared across separate R instances 
# A file-backed big.matrix may exceed available RAM by using hard drive space, and may also be shared across processes. 

# We don't need to load the data this way (I've already done all the hard work ;)
# x <- read.big.matrix('2008.csv', type='integer', header=TRUE, backingfile='airline.bin', descriptorfile='airline.desc', extraCols='Age')

xdesc<- dget('airline.desc') ## we do not need to read all data from csv again.
x<-attach.big.matrix(xdesc)

class(x)
head(x)
colnames(x)

# Some basic data access operations
x[1:6,1:6]
a <- x[,1]
a <- x[,2]
colnames(x)
tail(x, 5) # last 5 rows
colmin(x, 1)
colrange(x,5,na.rm=TRUE)

# A histogram of the data.  Note that we can't use $ with bigmemory objects.
# We need to access by column (name or index).
# Setting xlim just a bit above 2400 will show the full x-axis
options(scipen=1)
par(bg = 'cornsilk')  # See what this does :)
hist(x[,"ArrTime"],col='lightgreen',xlab="Arrival Time (24 Hour Clock)",main="Flight Volume (Arrivals) by Hour",cex.main=1.2,font.main=4,col.main='mediumblue',xlim=c(0,2410))


# Some familiar operations
# The number of flights on a Saturday
sum(x[,'DayOfWeek'] == 6)

# Produce counts of flights by day of week 
dayCount <- integer(7)

# Using a for loop over each day of the week
for (i in 1:7) {
    dayCount[i] <- sum(x[,'DayOfWeek'] == i)
}


library(foreach)
# The same calculation, but usually more efficient
dayCount <- foreach(i = 1:7, .combine=c)  %do% {
 sum(x[,'DayOfWeek'] == i)
 }


# Define a function to compute the 'Birth Month' of a plane
birthmonth <- function(y) { 
     minYear <- min(y[,'Year'], na.rm=TRUE) 
     these <- which(y[,'Year']==minYear) 
     minMonth <- min(y[these,'Month'], na.rm=TRUE) 
     return(12*minYear + minMonth - 1)
}


library(bigtabulate)

# Create a list of Tail Numbers
acindices <- bigsplit(x, 'TailNum')

# Apply the birthmonth function to each plane, as identified by Tail Number
acStart <- sapply(acindices, function(i) birthmonth(x[i, c('Year','Month'), drop= FALSE]))

# Now try to approximate the age - this will not work!
x[,'Age'] <- x[,'Year']*as.integer(12) + x[,'Month'] - as.integer(acStart[x[,'TailNum']])


# A bit of a pity, but we can still produce interesting tables :)
descr_stats <- bigtabulate(x, ccols = "DayOfWeek", summary.cols = "ArrDelay", summary.na.rm = TRUE)

# Prettify the output
stat.names <- dimnames(descr_stats$summary[[1]])[2][[1]]
day.names <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

descr_stats.p <- cbind(matrix(unlist(descr_stats$summary), byrow = TRUE, nrow = length(descr_stats$summary), ncol = length(stat.names), dimnames = list(day.names, stat.names)),ValidObs = descr_stats$table)
print(descr_stats.p)

# Plot the mean delay by day
plot(descr_stats.p[, "mean"], ylab="Average arrival delay", xlab="Day of Week", xaxt="n",cex.lab=1.0,font.lab=3)
axis(side=1,at=c(1:7), labels=day.names, las=2)
lines(descr_stats.p[, "mean"], col="green4", lty="dotted", ylab="Average arrival delay")
points(descr_stats.p[, "mean"], col="limegreen", pch = 16)
title(main="Plot of Average Arrival Delay, by Day",cex.main=1.2,font.main=4)


# What else can we do?

# We can use split-apply-combine
# Divide the vector defined by 1:nrow(x) by the days of the week
dow <- split(1:nrow(x),x[,'DayOfWeek'])

# How do we know what the different levels are?
# We're not interested in the head() part of this command, but the levels in the output
head(factor(x[,"DayOfWeek"]))


# Now we know we have 7 levels (possibly not the biggest surprise ...)
# Give names to the days of the week
names(dow) <- day.names

# Tempting but you'll be sorry ...
head(dow)

# However accessing by name yields to the controls we are used to
dow$Sat[1:10]

# We can use dow to our advantage and speed up calculations using a foreach loop
dayCount <- foreach(dayInds = dow, .combine=c)  %do% {
 length(dayInds)
 }


# We can also split on other levels (e.g. hours) and find e.g. finding longest delays/best time to fly

# Convert to the 24 hour clock (almost)
depHours <- floor(x[,'CRSDepTime']/100)

# Departure hours listed as 24 need to be recoded to 0
depHours[depHours == 24] <- 0

# Split the data
hourInds <- split(1:length(depHours),depHours)

# Check the levels to make sure they're OK
head(factor(depHours))

# Compute delay according to selected quantiles.  I.e. we are asking the question:
# "How long were the longest 1% of flight delays for a given hour?" 
#
# This can be interpreted by a frequent flyer as:
#
# "How long could my longest delay be, for 99% of my flights?"
#
# We can also try this for other quantiles too.
# 
qnts <- c(0.5, 0.75, 0.9, 0.99)

# Find the quantiles for each hour
delayQnts <- foreach( hour = hourInds, .combine=cbind) %do% {
	quantile(x[hour,'DepDelay'], qnts, na.rm=TRUE)
}

# Have a look at the output
delayQnts

# Assign more meaningful column names :)
colnames(delayQnts) <- names(hourInds)

# Much better!
delayQnts


# We can speed things up even more (parallel processing!)
library(parallel)
library(doMC)
library(doSNOW)

# Need to determine how many parallel cores in the computer
numParallelCores <- max(1,detectCores()-1)

# E.g. I have 7 
numParallelCores

# Now we need to 'register' these cores, before we can take advantage 
# of the parallel capabilities
cl <- makeCluster(rep('localhost',numParallelCores,type='SOCK'))
registerDoSNOW(cl)

# Now try the calculation  Notice the use of require() and attach.big.matrix
# This is necessary because we are using parallel processing
delayQnts <- foreach( hour = hourInds, .combine=cbind) %dopar% {
	require(bigmemory)
    x<-attach.big.matrix('airline.desc')
	quantile(x[hour,'DepDelay'], qnts, na.rm=TRUE)
}

# Again, assign meaningful column names
colnames(delayQnts) <- names(hourInds)

# Take a look
delayQnts

# Deregister the parallel processor
stopCluster(cl)

# Plot the results of the delay Quantiles
library(ggplot2)
library(reshape2)

# Convert delayQnts to a structure we can plot
# This will give us 'long' format
dq <- melt(delayQnts)

# Assign meaningful names!
names(dq) <- c('quantile','hour','delay')

# Plot delay by hour - which hour of the day experienced the longest delays (per quantile)?
qplot(hour,delay,data=dq,color=quantile,geom='line')

