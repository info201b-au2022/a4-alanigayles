library(tidyverse)

# The functions might be useful for A4
source("a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# What is the percentage of the total population who are black vs white 
# in prison?
percentage_white_people <- ((data$total_pop) / (data$white_prison_pop)) * 100
percentage_black_people <- ((data$total_pop) / (data$black_prison_pop)) * 100

# How many white women and black women were in prison each year? 
black_vs_white_women <- data %>% 
  arrange(year) %>% 
  na.omit(black_vs_white_women) %>% 
  select(year, black_female_prison_pop, white_female_prison_pop) %>% 
  group_by(year) %>% 
  summarize(
    black = sum(black_female_prison_pop),
    white = sum(white_female_prison_pop)
  )

  
# What is the proportion of black and white men in prison vs 
# black and white women were admitted? 
white_women <- data$white_female_prison_adm
black_women <- data$black_female_prison_adm
white_black_female <- white_women / black_women

black_men <- data$black_male_prison_adm
white_men <- data$white_male_prison_adm
black_white_men <- white_men / black_men

white_black_female / black_white_men
 

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function will create a data frame of the jail population total from 
# each year starting at 1970 and going to 2018 
get_year_jail_pop <- function() {
  year <- data$year
  total_jail_pop <- data$total_pop
  df <-  data.frame(year,total_jail_pop)
  df <- aggregate(df["total_jail_pop"], by = df["year"], sum)
return(df)   
}

# This function will graph the population of jails in increasing order from 1970
# to 2018 
library("ggplot2")
plot_jail_pop_for_us <- function()  {
 chart<-  ggplot(data = get_year_jail_pop(), 
   aes(x = year, y = total_jail_pop)) + 
   geom_bar(stat = "identity") +
 labs(
   title = "Increase of Jail Population (1970-2018)",
   x = "Year",
   y = "Total Jail Population"
 )
  return(chart)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# This function will create a new data frame 
get_jail_pop_by_states <- function(states) {
  states <- data$state 
  year <- data$year
  jail_pop <- data$total_jail_pop
  df <- data.frame(jail_pop, year, states)
  df[is.na(df)]=0
return(df)
}

plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(data = get_jail_pop_by_states(states)) 
  aes(x = year, y = jail_pop, color = states) +
    geom_line()
  return(chart)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


