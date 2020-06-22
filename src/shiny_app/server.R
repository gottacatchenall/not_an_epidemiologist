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
  fips = as.numeric(fips)
  return(fips)
}

get_moving_avg = function(list, num_days=5){
  time_series_length = length(list)
  
  moving_avg = rollsum(list, num_days)
  
  append_ct = time_series_length - length(moving_avg)
  return_vec = c(rep(0, append_ct), moving_avg)
  return(return_vec)
}

load_data = function(data_dir_path="./data", covid_dir_name="covid-19-data"){

  county_level_demographics_path = paste(data_dir_path, "county_demographics.csv", sep="/")
  county_level_historical_covid_data_path = paste(data_dir_path, covid_dir_name, "us-counties.csv", sep="/")
  county_level_live_covid_data_path = paste(data_dir_path, covid_dir_name, "live/us-counties.csv", sep="/")
  
  
  county_demographics = read.csv(county_level_demographics_path) %>% rowwise() %>% mutate(fips=fipsify(STATE,COUNTY)) 
  county_demographics$fips = as.numeric(county_demographics$fips)
  
  
  county_level_covid_data = read.csv(county_level_historical_covid_data_path) #%>% inner_join(read.csv(county_level_live_covid_data_path), by="fips")
  county_level_covid_data$date = as.Date(county_level_covid_data$date)
  county_level_covid_data$fips = as.numeric(county_level_covid_data$fips)
  
  
  
  county_level_data = county_level_covid_data %>% inner_join(county_demographics, by="fips")
  
  county_level_data = county_level_data %>% 
    group_by(fips) %>%
    arrange(date) %>% 
    mutate(number_new_cases = cases - lag(cases)) %>%
    mutate(new_cases_per_capita=number_new_cases/POPESTIMATE2019) %>% 
    mutate(new_cases_moving_average=get_moving_avg(new_cases_per_capita))
  
  return(county_level_data)
}

create_plot = function(input, county_level_data){
  
  most_recent = max(county_level_data$date)
  
  this_data = county_level_data %>% 
    filter(state==input$select_state, county %in% input$select_county) 
  
 this_data %>% 
   ggplot(aes(date, new_cases_moving_average, color=county, group=county)) + 
    geom_line(size=2,alpha=0.7) + 
    labs(x='', y='New Cases per capita', title=paste('New Cases per capita, 5-day moving avg, as of: ', most_recent)) + 
    scale_x_date(date_breaks = "3 weeks") + 
    theme(panel.border = element_rect(colour = "#222222", fill = NA, size=1), text=element_text(family="Helvetica Neue",size=14))
}

server = function(input, output, session) {
  
  county_level_data = load_data()
  state_list = sort.default(  unique(county_level_data$state) )
  
  output$select_state <- renderUI({ 
    selectInput("select_state", "Select state", state_list)
  })
  
  output$select_county = renderUI({

    this_states_data = county_level_data %>% filter(state==input$select_state)
    county_list = sort.default(unique(this_states_data$county))
    selectInput("select_county", "Select counties", county_list, multiple=T)
  })
 # updateSelectInput("select_state", choices = state_list)
  
        #output$state = updateSelectInput("state", choices = state_list)
        output$distPlot = renderPlot({
           if (F == is.na(input$select_state)){
              create_plot(input, county_level_data)
           } 
        })
  
}
