# Exam 2

#clear environment
rm(list = ls(all.names = TRUE))


#load college_scorecard
library(rio)
college_scorecard = import("2021_exam2_data.xlsx", which = 4)

#summary statistics
library(stargazer)
sumstats = stargazer(college_scorecard, type = "text")


#create smaller dataset 
library(tidyverse)
small_scorecard = subset(college_scorecard, college_scorecard$state_abbr == c("TX","LA"))
                        

small_scorecard = subset(small_scorecard, year == c("2014", "2015"))

small_scorecard = subset(small_scorecard, pred_degree_awarded_ipeds == "3")


#collapse it 

even_smaller_scorecard = small_scorecard %>%
  group_by(small_scorecard$state_abbr, small_scorecard$count_working) %>%
  summarize(across(where(is.numeric), mean))
 #fail



#avocados

avocados = import("2021_exam2_data.xlsx", which = 2)                   

#create year variable
library(lubridate)
avocados = avocados %>% mutate(year = lubridate::year(avocados$date))

#deflate price
library(WDI)
deflator_data =WDI(country = "USA", indicator =c("NY.GDP.DEFL.ZS"),
                   start = "1960",
                   end = "2018",
                   extra = FALSE, cache = NULL)

deflator_data$country <- NULL
deflator_data$iso2c <- NULL
                            
avocados = left_join(avocados, deflator_data ,by=c("year"))  

avocados$deflated_amount = avocados$average_price/(avocados$NY.GDP.DEFL.ZS/100)

#collapse it 

collapsed_avocados = avocados %>%
  group_by(year) %>%
  summarize(across(where(is.numeric), mean)) %>%
  select(-c(average_price, total_volume, NY.GDP.DEFL.ZS))

#show it 
head(collapsed_avocados)

#label it
library(labelled)
var_label(collapsed_avocados) <- list(collapsed_avocados$year = "year avocados sold", collapsed_avocados$deflated_amount = "dollar value accounting for inflation")
         


#training dataset
training = import("2021_exam2_data.xlsx", which = 3)    

#pivot it long 
pivot_longer(cols = starts_with("year"),

             
#time = over
#class = failure :/
