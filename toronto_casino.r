# The data for this code was originally downloaded from 
#
# http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=26136f951f6dd310VgnVCM10000071d60f89RCRD&vgnextchannel=1a66e03bb8d1e310VgnVCM10000071d60f89RCRD&vgnextfmt=default

# The blog post is at http://rforwork.info/2013/05/02/do-torontonians-want-a-new-casino-survey-analysis-part-1/


library(ff)
library(ffbase)
library(stringr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)
library(RgoogleMaps)

# The author created 2 copies of the same data set so he could can convert one and have the original for its text values
# Not the best approach!
casino = read.csv("casino_survey_results20130325.csv",stringsAsFactors=TRUE)
casino.orig = read.csv("casino_survey_results20130325.csv")

# Take a look at the data
dim(casino)
head(casino)

# He then performed numerical recoding here which is also not the best approach.
# Instead, see the alternative code just below this commented section.
# 
#casino$Q1_A = ifelse(casino.orig$Q1_A == "Neutral or Mixed Feelings", 3,
#              ifelse(casino.orig$Q1_A == "Somewhat in Favour", 4,
#              ifelse(casino.orig$Q1_A == "Somewhat Opposed", 2,
#              ifelse(casino.orig$Q1_A == "Strongly in Favour", 5,
#              ifelse(casino.orig$Q1_A == "Strongly Opposed", 1,NA)))))

# Numerical re-coding of Q1 and Q2.
# First, take a look at the factor levels for Q1
# Print just the levels found from applying factor() to the casino data
# The ordering c(6,4,2,3,5,1) prints the levels in ordinal order
# (try without [c(6,4,2,3,5,1)] and you'll see the difference!)
levels(factor(casino$Q1_A, levels(casino$Q1_A)[c(6,4,2,3,5,1)]))

# We can access these one at a time as well, e.g.
levels(factor(casino$Q1_A, levels(casino$Q1_A)[c(6,4,2,3,5,1)]))[1]

# If we try this for [1], then [2], ... up to [6] we'll see an empty string ""
levels(factor(casino$Q1_A, levels(casino$Q1_A)[c(6,4,2,3,5,1)]))[6]

# We will need to take care of this one, since it indicates no response!

# Now, let's recode the data.  The values are taken in the order
# 6,4,2,3,5,1 so that we have "no response" followed by a more
# meaningful ordering of the categories of responses, from
# Strongly opposed through to Strongly in Favour.
casino$Q1_A <- as.numeric(factor(casino$Q1_A,levels(casino$Q1_A)[c(6,4,2,3,5,1)]))

# Now take care of those empty responses! (They were in [6])
casino$Q1_A[which(casino$Q1_A==6)] <- NA

# We can repeat this for casino$Q2_A, except we will have to do this a bit
# differently as the responses were different
# First we need to work out the ordering of the levels
levels(factor(casino$Q2_A,levels(casino$Q2_A)))

# I'd suggest the ordering should be [c(3,4,5,2,1)] and that
# the "" response (position 1) should be replaced by NA.
# This ordering is suggested by looking at the file casino_survey_readme.xls
# Which can be downloaded from the first url given above

casino$Q2_A <- as.numeric(factor(casino$Q2_A,levels(casino$Q2_A)[c(3,4,5,2,1)]))
casino$Q2_A[which(casino$Q2_A==1)] <- NA

# Now recode Q3_A through to Q3_Q (columns 8:24)
for (i in 8:24) {
  casino[,i] = ifelse(casino.orig[,i] == "Not Important At All", 1,
  ifelse(casino.orig[,i] == "Somewhat Important", 2,
  ifelse(casino.orig[,i] == "Very Important", 3,NA)))}


# Now recode "Q7_A_StandAlone", "Q7_A_Integrated", Q7_B_StandAlone"   "Q7_B_Integrated"
# "Q7_C_StandAlone" and "Q7_C_Integrated" in columns 31:32,47,48,63,64.

for (i in c(31:32,47,48,63,64)) {
  casino[,i] = ifelse(casino.orig[,i] == "Highly Suitable",5,
                ifelse(casino.orig[,i] == "Neutral or Mixed Feelings",3,
                ifelse(casino.orig[,i] == "Somewhat Suitable",4,
                ifelse(casino.orig[,i] == "Somewhat Unsuitable",2,
                ifelse(casino.orig[,i] == "Strongly Unsuitable",1,NA)))))}

# There tended to be blank responses in the original dataset.  When seeking to 
# plot the responses in their original text option format, the author either removed them
# or coded them in "Did not disclose" in others.

casino.orig$Q1_A[casino.orig$Q1_A == ""] = NA
casino.orig$Q1_A = factor(casino.orig$Q1_A, levels=c("Strongly Opposed","Somewhat Opposed","Neutral or Mixed Feelings","Somewhat in Favour","Strongly in Favour"))

# Here's the graph showing how people feel about a new casino
ggplot(subset(casino.orig, !is.na(Q1_A)), aes(x=Q1_A,y=..count../sum(..count..))) + geom_bar(fill="forest green") + coord_flip() + ggtitle("How do you feel about having a new casino in Toronto?") + scale_x_discrete(name="") + theme_wsj() + theme(title=element_text(size=14),plot.title=element_text(hjust=.8)) + stat_bin(aes(label = sprintf("%.02f %%", ..count../sum(..count..)*100)), geom="text") + scale_y_continuous(labels=percent)

# How does the casino fit into your image of toronto...
ggplot(subset(casino.orig, Q2_A!= ''), aes(x=Q2_A,y=..count../sum(..count..))) + geom_bar(fill="forest green") + coord_flip() + ggtitle("How does a new casino in Toronto fit your image of the City of Toronto?") + scale_x_discrete(name="") + theme_wsj() + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + stat_bin(aes(label = sprintf("%.02f %%", ..count../sum(..count..)*100)),geom="text") + scale_y_continuous(labels=percent)

# Where you'd prefer to see it located
ggplot(subset(casino.orig, Q6!= ''), aes(x=Q6,y=..count../sum(..count..))) + geom_bar(fill="forest green") + coord_flip() + ggtitle("If a casino is built, where would you prefer to see it located?") + scale_x_discrete(name="") + theme_wsj() + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + stat_bin(aes(label = sprintf("%.02f %%", ..count../sum(..count..)*100)), geom="text") + scale_y_continuous(labels=percent)


# Reshaping the downtown ratings data for graphing..
stand.and.integrated.ratings.downtown = cbind(prop.table(as.matrix(table(casino.orig$Q7_A_StandAlone)[1:5])),
                                     prop.table(as.matrix(table(casino.orig$Q7_A_Integrated)[1:5])))

# This *will* work :)
stand.and.integrated.ratings.downtown = cbind(prop.table(as.matrix(table(casino.orig$Q7_A_StandAlone)[2:6])),
                                     prop.table(as.matrix(table(casino.orig$Q7_A_Integrated)[2:6])))


colnames(stand.and.integrated.ratings.downtown) = c("Standalone Casino","Integrated Entertainment Complex")

stand.and.integrated.ratings.downtown.long = melt(stand.and.integrated.ratings.downtown, varnames=c("Rating","Casino Type"), value.name="Percentage")

# Graphing ratings of casino suitability for the downtown location
ggplot(stand.and.integrated.ratings.downtown.long,  aes(x=stand.and.integrated.ratings.downtown.long$"Casino Type", fill=Rating, y=Percentage,label=sprintf("%.02f %%", Percentage*100))) + geom_bar(position="dodge",stat="identity") + coord_flip() + ggtitle("Ratings of Casino Suitability \nin Downtown Toronto by Casino Type") + scale_x_discrete(name="") + theme(title=element_text(size=14),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent) + geom_text(aes(x=stand.and.integrated.ratings.downtown.long$"Casino Type", y=Percentage, ymax=Percentage, label=sprintf("%.01f%%",Percentage*100), hjust=.75),position = position_dodge(width=1),size=4) + scale_fill_few(palette="light") + theme_wsj()

# Reshaping the exhibition place ratings for graphing
stand.and.integrated.ratings.exhibition = cbind(prop.table(as.matrix(table(casino.orig$Q7_B_StandAlone)[2:6])),
                                              prop.table(as.matrix(table(casino.orig$Q7_B_Integrated)[2:6])))

colnames(stand.and.integrated.ratings.exhibition) = c("Standalone Casino","Integrated Entertainment Complex")

stand.and.integrated.ratings.exhibition.long = melt(stand.and.integrated.ratings.exhibition, varnames=c("Rating","Casino Type"), value.name="Percentage")

# This didn't work originally.  Needed to change [1:5] to [2:6]
# Reordering the rating text labels for the graphing.
stand.and.integrated.ratings.exhibition.long$Rating = factor(stand.and.integrated.ratings.exhibition.long$Rating, levels=levels(casino.orig$Q7_A_StandAlone)[2:6])

# Graphing ratings of casino suitability for the exhibition place location
ggplot(stand.and.integrated.ratings.exhibition.long, aes(x=stand.and.integrated.ratings.exhibition.long$"Casino Type", fill=Rating, y=Percentage,label=sprintf("%.02f %%", Percentage*100))) + geom_bar(position="dodge",stat="identity") + coord_flip() + ggtitle("Ratings of Casino Suitability \nat Exhibition Place by Casino Type") + scale_x_discrete(name="") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent) + geom_text(aes(x=stand.and.integrated.ratings.exhibition.long$"Casino Type", y=Percentage, ymax=Percentage, label=sprintf("%.01f%%",Percentage*100), hjust=.75), position = position_dodge(width=1),size=4) + scale_fill_few(palette="light") + theme_wsj()

# Reshaping the Port Lands ratings for graphing
stand.and.integrated.ratings.portlands = cbind(prop.table(as.matrix(table(casino.orig$Q7_C_StandAlone)[2:6])),
                                                prop.table(as.matrix(table(casino.orig$Q7_C_Integrated)[2:6])))

colnames(stand.and.integrated.ratings.portlands) = c("Standalone Casino", "Integrated Entertainment Complex")

stand.and.integrated.ratings.portlands.long = melt(stand.and.integrated.ratings.portlands, varnames=c("Rating","Casino Type"), value.name="Percentage")

# Reording the rating text labels for the graping.
stand.and.integrated.ratings.portlands.long$Rating = factor(stand.and.integrated.ratings.portlands.long$Rating, levels=levels(casino.orig$Q7_A_StandAlone)[2:6])

# Graphing ratings of casino suitability for the port lands location
ggplot(stand.and.integrated.ratings.portlands.long, aes(x=stand.and.integrated.ratings.portlands.long$"Casino Type", fill=Rating, y=Percentage,label=sprintf("%.02f %%", Percentage*100))) + geom_bar(position="dodge",stat="identity") + coord_flip() + ggtitle("Ratings of Casino Suitability \nat Port Lands by Casino Type") + scale_x_discrete(name="") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent) + geom_text(aes(x=stand.and.integrated.ratings.portlands.long$"Casino Type", y=Percentage, ymax=Percentage, label=sprintf("%.01f%%",Percentage*100), hjust=.75), position = position_dodge(width=1),size=4) + scale_fill_few(palette="light") + theme_wsj()

# Then the author decided to integrate postal codes and coordinates:

# Convert all post codes to uppercase
casino.orig$PostalCode = toupper(casino.orig$PostalCode)

# Here's the dataset of canadian postal codes and latitude/longitude coordinates
# zip codes from http://geocoder.ca/?freedata=1
pcodes = read.csv.ffdf(file="Canada.csv", first.rows=50000, next.rows=50000, colClasses=NA, header=FALSE)
names(pcodes) = c("Postal","Lat","Long","City","Prov")


# The code below didn't work as originally supplied by the author, for this reason:
# 'character' doesn't work because ff does not support character vectors. Character vector need to be stored as factors. The disadvantage of that is that the levels are stored in memory, so if the number of levels is very large (e.g. with unique strings) you might still run into memory problems.

# 'integer' doesn't work because read.csv.ffdf passes the colClasses on to read.table, which then tries to converts your second column to integer which it can't.

# FSA is a concept in Canada known as the Forward Sortation Area (FSA) and is a 6-character string that forms part of the postal address, e.g. K1A0B1.

# Extract the first three strings, for the merging operation to follow
# If you're unsure why, type head(casino.orig["PostalCode"]) at the command prompt
# and you should see 3-letter strings

pcodes$FSA = as.ff(as.factor(toupper(substr(pcodes[,"Postal"], 1,3))))

# Convert to upper case & treat as a factor (again to match casino.orig[,"PostalCode"])
casino.orig$PostalCode = as.ff(as.factor(toupper(casino.orig[,"PostalCode"])))

# Convert casino.orig to an ff data frame for merging, then merge!
# Note that all.x will append rows to casino.orig (in the 'x' position of the call to merge)
# when merge encounters a value in pcodes, not already in casino.orig.
casino.orig = as.ffdf(casino.orig)
casino.orig = merge(casino.orig, pcodes, by.x="PostalCode", by.y="FSA", all.x=TRUE)

# Now can generate a map plotting the respondents' postal codes.
casino.gc = casino.orig[which(!is.na(casino.orig[,"Lat"])),]  # making sure only records with coordinates are included...

# Get a map from the Google server
mymap = MapBackground(lat=casino.gc$Lat, lon=casino.gc$Long)

# Now plot respondents' postal codes
PlotOnStaticMap(mymap, casino.gc$Lat, casino.gc$Long, cex=1.5, pch=21, bg="limegreen")

# In the next piece of code, he's zooming in on the map (South Ontario) :)

cities = data.frame(table(casino.orig[,"City"]))
cities = cities[cities$Freq > 0,]
cities = cities[order(cities$Freq, decreasing=TRUE),]

# Remove cities with no name
cities = cities[cities$Var1 != '',]  

cities.filter = cities[1:28,] # Here's my top cities variable (i set an arbitrary dividing line...)
names(cities.filter) = c("City","# Responses")

# This code originally didn't work (he was pointing to the incorrect variable.  
# By changing cities.filter$Var1 to cities.filter$City, the code will work :)
# I've made this change already.
casino.top.so = casino.orig[which(casino.orig[,"City"] %in% cities.filter$City),]

# here's a transparency function he used for the southern Ontario map

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# Finally here's the southern ontario map code
mymap = MapBackground(lat=casino.top.so$Lat, lon=casino.top.so$Long)
PlotOnStaticMap(mymap, casino.top.so$Lat, casino.top.so$Long, cex=1.5, pch=21, bg=addTrans("limegreen",10))

# He then summarised the data to questions focusing on issues of importance regarding the new casino (question 3)

q3.summary = matrix(NA, 16,1,dimnames=list(c("Design of the facility",
            "Employment opportunities","Entertainment and cultural activities",
            "Expanded convention facilities", "Integration with surrounding areas",
          "New hotel accommodations","Problem gambling & health concerns",
          "Public safety and social concerns","Public space",
          "Restaurants","Retail","Revenue for the City","Support for local businesses",
          "Tourist attraction","Traffic concerns","Training and career development"),c("% Very Important")))

for (i in 8:23) {
  q3.summary[i-7] = mean(casino[,i] == 3, na.rm=TRUE)}

q3.summary = as.data.frame(q3.summary[order(q3.summary[,1], decreasing = FALSE),])
names(q3.summary)[1] = "% Very Important"
q3.summary$Concern = rownames(q3.summary)
q3.summary = q3.summary[order(q3.summary$"% Very Important", decreasing=FALSE),]
q3.summary$Concern = factor(q3.summary$Concern, levels=q3.summary$Concern)


ggplot(q3.summary, aes(x=Concern, y=q3.summary$"% Very Important")) + geom_point(size=5, colour="forest green") + coord_flip() + ggtitle("Issues of Importance Surrounding\nthe New Casino") + scale_x_discrete(name="Issues of Importance") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent) + theme_wsj()
  
# This chunk of code deals with summarizing and plotting the questions surrounding 
# what features people might want if a new Integrated Entertainment Complex is built

q7a.summary = matrix(NA, 9,1, dimnames=list(c("No Casino","Casino Only", "Convention Centre Space", "Cultural and Arts Facilities",
              "Hotel","Nightclubs","Restaurants","Retail","Theatre"),c("% Include")))

for (i in 36:44) {
  q7a.summary[i-35] = mean(casino[,i], na.rm=TRUE)
}

q7a.summary = as.data.frame(q7a.summary[order(q7a.summary[,1], decreasing = FALSE),])
names(q7a.summary)[1] = "% Include"
q7a.summary$feature = rownames(q7a.summary)
q7a.summary$feature = factor(q7a.summary$feature, levels=q7a.summary$feature)

ggplot(q7a.summary, aes(x=feature, y=q7a.summary$"% Include")) + geom_point(size=5, colour="forest green") + coord_flip() +   ggtitle("What People Would Want in an Integrated\nEntertainment Complex in Downtown Toronto") + scale_x_discrete(name="Features") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent,name="% Wanting the Feature") + theme_wsj()

q7b.summary = matrix(NA, 9,1, dimnames=list(c("No Casino","Casino Only", "Convention Centre Space", "Cultural and Arts Facilities",
                                              "Hotel","Nightclubs","Restaurants","Retail","Theatre"),c("% Include")))

for (i in 52:60) {
  q7b.summary[i-51] = mean(casino[,i], na.rm=TRUE)
}

q7b.summary = as.data.frame(q7b.summary[order(q7b.summary[,1], decreasing = FALSE),])
names(q7b.summary)[1] = "% Include"
q7b.summary$feature = rownames(q7b.summary)
q7b.summary$feature = factor(q7b.summary$feature, levels=q7b.summary$feature)

ggplot(q7b.summary, aes(x=feature, y=q7b.summary$"% Include")) + geom_point(size=5, colour="forest green") + coord_flip() + ggtitle("What People Would Want in an Integrated\nEntertainment Complex at the Exhbition Place") + scale_x_discrete(name="Features") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent,name="% Wanting the Feature") + theme_wsj()

q7c.summary = matrix(NA, 9,1, dimnames=list(c("No Casino","Casino Only", "Convention Centre Space", "Cultural and Arts Facilities",
                                              "Hotel","Nightclubs","Restaurants","Retail","Theatre"),c("% Include")))

for (i in 68:76) {
  q7c.summary[i-67] = mean(casino[,i], na.rm=TRUE)
}

q7c.summary = as.data.frame(q7c.summary[order(q7c.summary[,1], decreasing = FALSE),])
names(q7c.summary)[1] = "% Include"
q7c.summary$feature = rownames(q7c.summary)
q7c.summary$feature = factor(q7c.summary$feature, levels=q7c.summary$feature)

ggplot(q7c.summary, aes(x=feature, y=q7b.summary$"% Include")) + geom_point(size=5, colour="forest green") + coord_flip() + ggtitle("What People Would Want in an Integrated\nEntertainment Complex in Port Lands") + scale_x_discrete(name="Features") + theme(title=element_text(size=22),plot.title=element_text(hjust=.8)) + scale_y_continuous(labels=percent,name="% Wanting the Feature") + theme_wsj()