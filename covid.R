library(tidyverse)
library(zoo)
library(ggthemr)
ggthemr('fresh', spacing=2 ,layout="scientific")
thm = theme(panel.border = element_rect(colour = "#222222", fill = NA, size=1), text=element_text(family="Helvetica Neue",size=14))
setwd("covid-19-data")

# =================================================
# 
#   county level demographic data
#
# =================================================
append_leading_zeros = function(orig, n_digits=3){
    num_zeros_to_add = n_digits - nchar(as.character(orig))
    if (num_zeros_to_add > 0){
        lead = paste(rep("0", num_zeros_to_add), collapse="", sep="")
        new_string = paste(lead, as.character(orig), sep="", collapse="")
        return(new_string)
    }
    else{
        return(orig)
    }
}

fipsify = function(state, county){
    state_string = append_leading_zeros(state, n_digits=1)
    county_string = append_leading_zeros(county, n_digits=3)
    fips = paste(state_string, county_string, sep="")
    return(fips)
}

county_demographics = read.csv('county_demographics.csv') %>% rowwise() %>% mutate(fips=fipsify(STATE,COUNTY)) %>% glimpse()

# =================================================
# 
#   covid data via nyt
#
# =================================================
county_level_covid_data = read.csv('covid_data.csv') 
county_level_covid_data$date = as.Date(county_level_covid_data$date)
county_level_covid_data$fips = as.character(county_level_covid_data$fips)

colorado_covid_data = county_level_covid_data %>% filter(state=='Colorado')

colorado_full = colorado_covid_data %>% inner_join(county_demographics, by="fips")
colorado_full = colorado_full %>% 
        group_by(fips) %>% 
        mutate(number_new_cases= cases - lag(cases)) %>% 
        group_by(fips, date) %>% 
        mutate(new_cases_per_capita=number_new_cases/POPESTIMATE2019) 
colorado_full = colorado_full %>% group_by(fips) %>% mutate(new_cases_moving_average=c(0,0,0,0,rollsum(new_cases_per_capita,5))) %>% glimpse()


colorado_full %>% 
    filter(state=='Colorado', county %in% c("Boulder", "Denver", "Arapahoe", "Douglas")) %>% 
    ggplot(aes(date, new_cases_moving_average, color=county, group=county)) + geom_line(size=2) + 
    labs(x='', y='New Cases per Capita', title='New Cases per Capita, 5day ma') + scale_x_date() + thm

