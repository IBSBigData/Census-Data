# dillardsvis -- demo script for data preparation and visualization
# using part of the dillards data
#
# remember to set your working directory!
# Be sure to have the two necessary csv files in your Data folder
#
# the two files are called
#    OwnerOcc2000.csv -- number of owner occupied housing units in each MSA
#    dillards_store_msa.csv  -- selected columns from Dillards database
#
#  packages to use
install.packages("ggmap")  # another way to make maps
library(ggmap)
library(dplyr)  #  for data manipulation
library(ggplot2)
# Read the data into two dataframes
oo <- read.csv("Data/OwnerOcc2000.csv")
dill <- read.csv("Data/dillards_store_msa.csv")
str(oo)
str(dill)
head(oo)
head(dill)
#
# TASK 1:  Histogram of Income in Dillard's MSAs
#
# easy -- everything we need is in table dill.
# Use ggplot2 to make a pretty graph, well-labeled
p1 <- ggplot(dill, aes(x=MSA_INCOME)) +
     geom_histogram(color="blue", fill="blue") + 
     ggtitle("Income in MSAs with Dillards Stores")
p1

# TASK 2:  Scatterplot with MSA_BACH on x and MSA_INCOME on Y
# overlay a regression fit
#  both required variables are also in the dill df.
#
p2 <- ggplot(dill, aes(x=MSA_BACH, y=MSA_INCOME)) +
     geom_point(shape=21) + 
     stat_smooth(method=lm) +
     ggtitle("Income vs Bachelors Degree")
     
p2

# TASK 3 -- Create chloropleth map colored by MSA_HIGH
# note that longitude variable in dillards needs to 
# be negated. This has already been done, but could be
# carried out at follows
dill$LONG <- (dill$LONGITUDE * -1)

task3 <- select(dill, LONG, LATITUDE, MSA_HIGH)

m1 <- qmplot(LONG, LATITUDE, data=task3, color = I('green'),
     size=I(3), darken = .3)

m1

# TASK 4:  Combine the data from Dillards and the OwnerOccupied tables
# compute a new variable = owner-occ housing/population
# Make a map with dots on store locations, dot color=MSA_INCOME, 
# dot size by new owner-occ rate

# We'll want to join these two tables using the MSA name
# but one uses upper-case and the other uses lower-case
dill$msa <- tolower(dill$MSA_NAME)
oo$msa <- tolower(oo$MSA.Name)
# now we have 2 tables with shared key column name
combdf <- inner_join(dill, oo)
# create a new variable for the rate of owner-occupied housing per population
combdf <- mutate(combdf,oorate = OwnerOcc.HH/MSA_POP)

# subset the big joined table for the map
# we need long, lat, oorate, MSA_INCOME
task4 <- select(combdf, LONG, LATITUDE, oorate, MSA_INCOME)
m2 <- qmplot(LONG, LATITUDE, data=task4, color = MSA_INCOME, size=oorate, darken = .3)

m2

