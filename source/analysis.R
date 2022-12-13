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

# How many white women and black PEOPLE were in prison in 2020? 
black_vs_white_women <- data %>% 
  arrange(year) %>% 
  na.omit(black_vs_white_women) %>% 
  select(year, black_female_prison_pop, white_female_prison_pop) %>% 
  group_by(year) %>% 
  summarize(
    black = sum(black_female_prison_pop),
    white = sum(white_female_prison_pop)
  )

black_POP <- data %>% 
  group_by(black_pop_15to64) %>% 
  filter (year == '2020') %>% 
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = TRUE)) %>% 
  pull(black_pop_15to64)

# What is the proportion of black and white men in prison vs 
# black and white women were admitted? 
white_women <- data$white_female_prison_adm
black_women <- data$black_female_prison_adm
white_black_female <- sum(white_women, na.rm = TRUE) + sum(black_women, na.rm = TRUE)

black_men <- data$black_male_prison_adm
white_men <- data$white_male_prison_adm
black_white_men <- sum(white_men, na.rm = TRUE) + sum(black_men, na.rm = TRUE)

proportion <- white_black_female / black_white_men
 

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
   y = "Total Jail Population",
   caption = "Growth of the U.S. prison population from 1970 to 2018."
 )
 
  return(chart)   
} 
library(dplyr)
library(ggplot2)
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# This function will group the "data" data set by year and state to separate 
# and summaries the total jail population in that state by each year. 

get_jail_pop_by_states <- function(states) {
  data %>% 
    group_by(state, year) %>% 
    summarise(total = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(state %in% states)
}

plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(data = get_jail_pop_by_states(states)) +
  aes(x = year, y = total, group = state, color = state) +
    geom_line()+
    labs(
      title = "Growth of Prison Population by State",
      x = "Year",
      y = "Total Jail Population",
      caption = "Montana, Idaho, Wyoming, Alaska, Oregon, Washington. Trends of the PNW"
    )
  return(chart)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
female_vs_male_total <- function(){
  data %>% 
    filter(state == "CA") %>% 
    group_by(county_name, total_jail_pop) %>% 
    summarise(total = c(sum(female_pop_15to64, na.rm = TRUE), 
              sum(male_pop_15to64, na.rm = TRUE)),
              gender = c("Female", "Male"))
}

plot_male_vs_female <- function() {
  chart <- ggplot(data = female_vs_male_total()) +
    aes(y = county_name, x = total, group = gender, color = gender) + 
  geom_col()
  return(chart)
}
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
black_women_data <- function() {
  data %>% 
    group_by(state, county_name) %>% 
    summarise(unique(county_name), total = sum(black_female_prison_pop, na.rm = TRUE)) %>% 
    filter(state == "CA") 
}
 install.packages("usmap")
library(usmap)
library(ggplot2)

plot_usmap(include = "CA")

# grey map of California                
ca_base + theme_nothing()
## Load data frame ---- 


