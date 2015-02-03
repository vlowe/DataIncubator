################################################################################
# V. Lowe's preliminary analysis for 2015 Data Incubator project 
################################################################################
# Problem: How does the location and cuisine affect the food inspection result
# of a restaurant? Do chains have better food inspection result than family-
# owned stores? How do food inspection results correlate with review websites 
# (e.g. Yelp, Urbanspoon, TripAdvisor). 
################################################################################
# Preliminary questions: 1. Where are the restaurants located? 2. How do food
# inspection results vary across the three largest US cities? i.e. New York 
# City, Los Angeles, and Chicago. 
################################################################################
# Caveat: Programming during my PhD was conducted mainly in IDL. It has a nifty
# astronomy library. By the time I found out that python also had an astronomy
# library, I had invested so much time with IDL. Since waiting for the outcome
# of my PhD thesis, I've been learning python and R. I recently  (on Feb 2) the
# January 2015 edition of "r programming", an online course delivered on 
# Coursera (https://www.coursera.org/course/rprog) and thought "I'm going to use
#  R!". Here's some code that I hacked together. Enjoy... 
################################################################################

# Read data
# Data from https://data.cityofchicago.org
# Exact link: http://goo.gl/MSos9T
data.Chicago <- read.csv("Chicago.csv", 
                         colClasses = c(rep("NULL", 2), "character", 
                                        rep("NULL", 9), "character",
                                        rep("NULL", 3)))

# Data from https://data.cityofnewyork.us
# Exact link: http://goo.gl/qNtfSS
data.NYC <- read.csv("NYC.csv", 
                     colClasses = c("integer", "NULL", "character", 
                                    rep("NULL", 4), rep("character", 2), 
                                    rep("NULL", 4), "integer", "character", 
                                    rep("NULL", 3)))

# Data from https://ehservices.publichealth.lacounty.gov
# Exact link: http://goo.gl/wvvhfj
# (but choose Inspections -> Food Facility (Restaurants/Markets))
# The LA data had some stray commas which were massaged out with my text editor
# of choice (Kate). I should have used grep/awk but I was watching the
# Australian Open men's final and mindless text edits seemed easier
data.LA <- read.csv("LA.csv", row.names = NULL, 
                     colClasses = c("character", "numeric", 
                                    rep("NULL", 4), "character", "NULL"))
colnames(data.LA) <- c("Facility", "Grade", "City")
data.LA$Grade <- as.numeric(data.LA$Grade)

# NYC includes multiple exceptions so extract the latest inspection
# for each restaurant
data.NYC <- data.NYC[!duplicated(data.NYC$CAMIS),]

# Convert LA scores into grades
for (i in 1:length(data.LA$Grade)) {
  if ((is.na(data.LA$Grade[i])) | (data.LA$Grade[i] == 0)) {
    data.LA$Grade[i] <- NA
  }
  else if ((data.LA$Grade[i] >= 90) | (data.LA$Grade[i] == 100)) {
    data.LA$Grade[i] <- "A"
  } else if (data.LA$Grade[i] >= 80) {
    data.LA$Grade[i] <- "B"
  } else if (data.LA$Grade[i] >= 70) {
    data.LA$Grade[i] <- "C"
  }
}

# Now let's make some plots
postscript("NYC_boroughs.eps")
counts <- table(data.NYC$BORO)
barplot(counts[-4], main="Where are the restaurants in NYC?", horiz=TRUE,
        names.arg=c("Bronx", "Brooklyn", "Manhattan", "Queens",
                    "Staten Island"), xlab = "Counts", col = "darkblue")
dev.off()

# Set up environoment for multiplot image
postscript("ALL_grades.eps")
par(mfrow = c(1,3))

# NYC barplot
counts <- table(data.NYC$GRADE)
barplot(counts[2:4], main="Grades in NYC", horiz=TRUE,
        names.arg=c("A (0-13)", "B (14-27)", "C (28+)"), xlab = "Counts",
        col = "red")

# LA barplot
counts <- table(data.LA$Grade)
barplot(counts[2:4], main="Grades in LA", horiz=TRUE,
        names.arg=c("A (90-100", "B (80-89)", "C (70-79)"),
        xlab = "Counts", col = "green")

# Chicago barplot
counts <- table(data.Chicago$Results)
barplot(counts[cbind(5, 6, 2)], main="Results in Chicago", horiz=TRUE,
        names.arg=c("Pass", "Pass w/ conditions", "Fail"), xlab = "Counts",
        col = "blue")

dev.off()
par(mfrow = c(1,1))

# To convert images from EPS to PNG
# /usr/bin/convert -quality 100 restaurant_grades.png restaurant_grades.eps
# /usr/bin/convert -quality 100 ALL_grades.png ALL_grades.png

# Until next time... -VL