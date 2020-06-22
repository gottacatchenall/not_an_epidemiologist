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

load_data = function(data_dir_path="./data", covid_dir_name="covid-19-data"){
    print(getwd())
    county_level_demographics_path = paste(data_dir_path, "county_demographics.csv", sep="/")
    county_level_historical_covid_data_path = paste(data_dir_path, covid_dir_name, "us-counties.csv", sep="/")
    county_level_live_covid_data_path = paste(data_dir_path, covid_dir_name, "live/us-counties.csv", sep="/")
    
    
    county_demographics = read.csv(county_level_demographics_path) %>% rowwise() %>% mutate(fips=fipsify(STATE,COUNTY)) 
    
    
    
    county_level_covid_data = read.csv(county_level_historical_covid_data_path) #%>% inner_join(read.csv(county_level_live_covid_data_path), by="fips")
    county_level_covid_data$date = as.Date(county_level_covid_data$date)
    county_level_covid_data$fips = as.character(county_level_covid_data$fips)
    
    
    
    county_level_data = county_level_covid_data %>% inner_join(county_demographics, by="fips")
    
    county_level_data = county_level_data %>% filter(state=="Colorado") %>% 
      group_by(fips) %>%
      arrange(date) %>% 
      mutate(number_new_cases = cases - lag(cases)) %>%
      ungroup() %>%
      group_by(fips) %>% 
      mutate(new_cases_per_capita=number_new_cases/POPESTIMATE2019) %>% 
      mutate(new_cases_moving_average=c(0,0,0,0,rollsum(new_cases_per_capita,5)))
    
    
    return(county_level_data)
}

create_plot = function(input, county_level_data){
    colorado_full = county_level_data %>% filter(state=='Colorado')
    
    colorado_full %>% 
        filter(state=='Colorado', county %in% c("Boulder", "Denver", "Arapahoe", "Douglas", "El Paso")) %>% 
        ggplot(aes(date, new_cases_moving_average, color=county, group=county)) + 
            geom_point(size=2.5, alpha=0.7) + 
            geom_line(size=1.5,alpha=0.7) + 
            labs(x='', y='New Cases per capita', title='New Cases per capita, 5-day moving avg') + 
            scale_x_date(date_breaks = "3 weeks") + 
            theme(panel.border = element_rect(colour = "#222222", fill = NA, size=1), text=element_text(family="Helvetica Neue",size=14))
}

server = function(input, output) {

  county_level_data = load_data()
  
  
  output$distPlot <- renderPlot({
    
    create_plot(input, county_level_data)
  })
  
}
