#Library
library("ggplot2")
library("tidyverse")
library("dplyr")
library("maps")
library("mapproj")
library("patchwork")
#Load data set
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)

# Introduction & Variable----------------------------------------------------
#Introduction
#In this report, the problem domain mainly focuses on the black population age from 15 to 64, and the number of the black population in jail, comparing by using data and variables about the white. 
#In the variables part, I limit the time in year 2018 and calculate the number of the highest population of the white and the black, the state has the highest population of the black and the white, the state has the highest black and white population, the number of population in jail in 2018.
#I want to find the trends of population number changing over time in some specific locations, and the relationship between the population number and the population in jail.
#I narrow the data into the counties and compare the data from counties of different states to get usable conclusions.
#I use the map to compare the black population in jail through the whole country graphically.


#Variables
#1.What is the highest number of black population age from 15 to 64 among all counties in 2018?
#black_pop_15to64, number
highest_black_pop <- incarceration_trends %>%
  filter(year == 2018) %>%
  summarise(max = max(black_pop_15to64, na.rm = T)) %>%
  pull()
highest_black_pop#1289546

#2.What is the highest number of white population age from 15 to 64 among all counties in 2018?
#white_pop_15to64, number
highest_white_pop <- incarceration_trends %>%
  filter(year == 2018) %>%
  summarise(max = max(white_pop_15to64, na.rm = T)) %>%
  pull()
highest_white_pop#1842942

#3.Which state has the highest number of black population age from 15 to 64 in 2018?
#black_pop_15to64, state
state_most_black_15to64 <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(black_pop_15to64 == max(na.omit(black_pop_15to64))) %>%
  pull(state)
state_most_black_15to64#NY

#4.Which state has the highest number of white population age from 15 to 64 in 2018?
#white_pop_15to64, state
state_most_white_15to64 <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(white_pop_15to64 == max(na.omit(white_pop_15to64))) %>%
  pull(state)
state_most_white_15to64#CA

#5.Which state has the highest average number of black population in jail per day in 2018?
#black_jail_pop, state
state_most_black_jail <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(black_jail_pop == max(na.omit(black_jail_pop))) %>%
  pull(state)
state_most_black_jail#CA

#6.Which state has the highest average number of white population in jail per day in 2018?
#white_jail_pop, state
state_most_white_jail <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(white_jail_pop == max(na.omit(white_jail_pop))) %>%
  pull(state)
state_most_white_jail#AZ

#7.What is the highest average number of black population in jail per day in CA in 2018?
#black_jail_pop, state
highest_black_jail_pop <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(state == "CA") %>%
  summarize(max = max(black_jail_pop, na.rm = T)) %>%
  pull()
highest_black_jail_pop#5024

#8.What is the highest average number of white population in jail per day in AZ in 2018?
#white_jail_pop, state
highest_white_jail_pop <- incarceration_trends %>%
  filter(year ==2018) %>%
  filter(state == "AZ") %>%
  summarize(max = max(white_jail_pop, na.rm = T)) %>%
  pull()
highest_white_jail_pop#4577

#Analysis variables: 
#The white population among all counties in 2018 is larger than the Black population among all counties in 2018.
#CA has the highest white population in 2018.
#However, CA has the highest average number of the black population in jail per day in 2018.
#NY is the state that has the highest black population in 2018. The highest number of the average black population in jail daily in CA which state has the highest average number of the black population in jail are larger than the highest average number of the white population in jail in AZ which state has the highest number of the white population in jail, in 2018.
#Thus, we can conclude that there is disproportionate incarceration of people between races.



# Trend over time chart ----------------------------------------------------

#Create the data set1
incarceration_trends1 <- incarceration_trends %>%
  select(year, fips, county_name, state, black_pop_15to64, black_jail_pop) %>%
  filter(fips == 6001) %>%
  unite("location", county_name:state, sep = ", ")
View(incarceration_trends1)

incarceration_trends2 <- incarceration_trends %>%
  select(year, fips, county_name, state, black_pop_15to64, black_jail_pop) %>%
  filter(fips == 36001) %>%
  unite("location", county_name:state, sep=", ")
View(incarceration_trends2)

incarceration_trends3 <- full_join(incarceration_trends1, incarceration_trends2, by = c("year", "fips", "location", "black_pop_15to64", "black_jail_pop"))
View(incarceration_trends3)

#Line Chart
time_trend <- ggplot(data = incarceration_trends3, mapping = aes(x = year, y = black_pop_15to64, group = location))+
  geom_line(aes(linetype = location, color = location))+
  theme(legend.position = "right")+
  labs(title="Black Population in Alameda County, CA and Albany County, NY", x = "year", y = "Black Population age from 15 to 64")
time_trend

#Chart 1 Analysis:
#The reason why I make this chart is that I want to know the population changing trend in one county of CA and NY.
#The reason why I make this chart is that I want to know the population changing trend in one county of CA and NY.
#I choose the location CA and NY based on the variables and choose one county from each state randomly to get a general conclusion.
#From the chart, we can see the population changing trends over time in Alameda County, CA and Albany County, NY, and I find that the black population in one county of CA decreased dramatically, and the black population in one county of NY increased steadily.
#From the variable "state_most_black_15to64" we know that In 2018, the state NY has the highest number of black population aged from 15 to 64.
#One county of NY shows an increasing number of black population with the time flow, and one county of CA shows the decreasing number of black population with the time flow.
#This explains that why NY has the highest number of black people: the black population in counties keeps increasing steadily.
#From the variable "state_most_white_15to64", one county in the state CA which has the highest number of white population aged from 15 to 64 shows the decreasing number of the black population in 2018.
#However, the state CA had the highest average number of the black population in jail per day in 2018, which was calculated from the variable "state_most_black_jail".
#I try to find some relationships in the second chart.



# Variable comparison chart ----------------------------------------------------

#Create data set 2
incarceration_trends4 <- incarceration_trends3 %>%
  filter(fips == 36001)
View(incarceration_trends4)

#Scatter Chart
var_compare <-ggplot(incarceration_trends4, aes(x=black_pop_15to64, y=black_jail_pop))+ 
  geom_point()+
  geom_smooth(method = "loess", formula = y ~ x, se=FALSE, fullrange=TRUE)+
  labs(title="Relationship between Black Population (age from 15 to 64) & Daily Black Population in jail, in Albany County, NY",  x="Black Population (age from 15 to 64)", y = "Black Population in Jail / Day (average)")
var_compare
#Chart 2 Analysis:
#In this chart, I compare the black population (age from 15 to 64) and the average number of the black population in jail per day in Albany County, NY.
#The reason why I make this chart is that I want to know is there any relationship between the black population (age from 15 to 64) and the average number of the black population in jail per day in Albany County, NY.
#Based on this chart, as the black population increase, the black population in jail decreases slightly.
#However, the relationship between the black population (age from 15 to 64) and the average number of the black population in jail per day is not exactly negative or positive.
#There is a trend that as the black population is at the lowest and the highest number, the average number of the black population in jail is relatively low.
#So, this chart shows that increasing the number of the black population does not mean the increasing average number of the black population in jail per day. 



# Map ----------------------------------------------------

#Create data set 3
incarceration_trends4 <- incarceration_trends %>%
  filter(year == 2018) %>%
  select(year, fips, black_jail_pop)
View(incarceration_trends4)

county.fips1 <- county.fips %>%
  separate(polyname, c("polyname", "sub"))
View(county.fips1)

state_shape <- map_data("county") %>%
  rename(polyname = region) %>%
  full_join(county.fips1, by = "polyname")
View(state_shape)

incarceration_trends_map <- full_join(state_shape, incarceration_trends4, by = "fips")
View(incarceration_trends_map)

#Minimalist theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

#Map of the U.S.  
US_map <- ggplot(incarceration_trends_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop), color = "white", size = 0.3) +
  coord_map()+
  scale_colour_gradient2(low = "white", mid = "orange", high = "brown", guide = "colorbar", aesthetics = "fill") + 
  blank_theme +
  ggtitle("The Average Daily Number of Black Population in Jail in the United State in 2018")
US_map
#Map analysis
#I make this chart because I want to see the distribution of the average black population in the jail throughout the whole country per day in 2018.
#At the same time, through the color of the map, people can see which state had the highest average number of the black population in jail per day in 2018.
#Also, based on the color and numbers on this map, I can check with the correctness of the variable: CA has the highest number of the black population in jail.
#In addition, the average black population in jail in east America is less than the average black population in jail in west America per day in 2018.


