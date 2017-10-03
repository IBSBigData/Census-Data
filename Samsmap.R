# SamsMap
# script for Sams Club data and individual explorations

library(plotly)
library(ggplot2)
library(readr)

# Set Working Directory as my home directory
setwd("~/")

# create all dataframes from available data
df1 <- read_csv("~/data/Sam_Sales_ by_city.csv")
states <- read_csv("~/data/Stateabbrevs.csv")
cities <- read_csv("~/data/US_cities.csv")

# Join the Sams Data to the city population data
# NOTE:  In Sam's data, city names are in ALL CAPITAL LETTERS, but in US_Cities dataframe
# they are not. Use the r funciton `toupper` to make every name all upper case.
cities$Municipality <- toupper(cities$Municipality)

Samcities <- inner_join(df1, cities, by=c("City" = "Municipality"))

# Population histogram
g1 <- ggplot(data=Samcities, aes(Samcities$Pop2000)) +
     geom_histogram(binwidth = 100000) +
     ggtitle("Population of Cities with Sams Club Stores")
g1

# Scatter plot of sales vs. population
g2 <- ggplot(data=Samcities, aes(x=Pop2000, y=Total_Sales)) +
     geom_point() +
     ggtitle("Sales vs. Population") +
     geom_smooth(method=lm)
g2


#  For mapping, aggregate total sales by STATE and join to state abbreviations
dfss <- df1 %>%
     group_by(State) %>%
     summarize(Tot_Sales = sum(Total_Sales))

dfss <-  left_join(dfss, states, by = c("State" = "State Abbreviation"))


dfss$hover <- with(dfss, paste("State",  State, '<br>', "Sales", Tot_Sales))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g4 <- list(
     scope = 'usa',
     projection = list(type = 'albers usa'),
     showlakes = TRUE,
     lakecolor = toRGB('white')
)

p <- plot_geo(dfss, locationmode = 'USA-states') %>%
     add_trace(
          z = ~Tot_Sales, text = ~hover, locations = ~State,
          color = ~Tot_Sales, colors = 'Purples'
     ) %>%
     colorbar(title = "Total Sales in USD") %>%
     layout(
          title = '2000 Sams Club Sales by State<br>(Hover for details)',
          geo = g4
     )

p

