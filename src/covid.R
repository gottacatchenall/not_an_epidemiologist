library(tidyverse)
library(zoo)
library(ggthemr)
ggthemr('fresh', spacing=2 ,layout="scientific")
thm = theme(panel.border = element_rect(colour = "#222222", fill = NA, size=1), text=element_text(family="Helvetica Neue",size=14))
setwd("not_an_epidemiologist")

# =================================================
# 
#   county level demographic data
# 
# =============================================mv ====
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
county_demographics$fips = (county_demographics$fips )

# =================================================
# 
#   covid data via nyt
#
# =================================================
county_level_covid_data = read.csv('us-counties.csv') 
county_level_covid_data$date = as.Date(county_level_covid_data$date)
county_level_covid_data$fips = as.character(county_level_covid_data$fips)

county_level_covid_data %>% filter(state=='Colorado')

colorado_full = county_level_covid_data %>% filter(state=='Colorado') %>% inner_join(county_demographics, by="fips")
colorado_full = colorado_full %>% 
    group_by(fips) %>%
    arrange(date) %>% 
    mutate(number_new_cases = cases - lag(cases)) %>%
    ungroup() %>%
    group_by(fips) %>% 
    mutate(new_cases_per_capita=number_new_cases/POPESTIMATE2019) %>% 
    mutate(new_cases_moving_average=c(0,0,0,0,rollsum(new_cases_per_capita,5))) %>% 
    glimpse()

plt = colorado_full %>% 
    filter(state=='Colorado', county %in% c("Boulder", "Denver", "Arapahoe", "Douglas", "El Paso")) %>% 
    ggplot(aes(date, new_cases_moving_average, color=county, group=county)) + geom_point(size=2.5, alpha=0.7) + geom_line(size=1.5,alpha=0.7) + 
    labs(x='', y='New Cases per capita', title='New Cases per capita, 5-day moving avg, 6/21/20') + scale_x_date(date_breaks = "3 weeks") + thm
plt



ggsave("covid_jun20.png", device=png(), width=10, height=7)
