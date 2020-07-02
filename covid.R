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

compute_ma = function(vector, window=5) {
  output_length = length(vector)
  
  ma = rollsum(vector, window)
  leading_zeros = rep(0, output_length - length(ma))
  
  return_vec = c(leading_zeros, ma)
  return(return_vec)
}


make_vector_equiv_length = function(input, reference){
  target_length = length(reference)
  length_to_add = target_length - length(input) 
  
  if (length_to_add <= 0){
    return(input)
  }
  
  return(c(rep(NA, length_to_add), input))
}

compute_r0 = function(timeseries,date_vec){
  timeseries[is.na(timeseries)] = 0
  timeseries[timeseries < 0] = 0
  
  if (length(timeseries) > 0){
    
    #### THESE VALUES ARE FROM blah blah blah paper
    r0_est = estimate_R(timeseries, method="parametric_si", config=make_config(mean_si=5.4, std_si=3.5))
    
    mean_r0 = make_vector_equiv_length(r0_est$R[["Mean(R)"]], timeseries)
    lower_r0 = make_vector_equiv_length(r0_est$R[["Quantile.0.05(R)"]], timeseries)
    upper_r0 = make_vector_equiv_length(r0_est$R[["Quantile.0.95(R)"]], timeseries)
    
    return(data.frame(mean_r0=mean_r0, lower_r0=lower_r0, upper_r0=upper_r0, date=date_vec))
  }
}

read_data = function(){
  # Read demographic data
  county_demographics = read.csv('./data/county_demographics.csv') %>% rowwise() %>% mutate(fips=fipsify(STATE,COUNTY)) %>% glimpse()
  
  # Read covid data
  county_level_covid_data = read.csv('./data/covid-19-data/us-counties.csv') 
  county_level_covid_data$date = as.Date(county_level_covid_data$date)
  county_level_covid_data$fips = as.character(county_level_covid_data$fips)
  

  # merge dfs
  county_level_data = county_level_covid_data %>% inner_join(county_demographics, by="fips")
  return(county_level_data)
}

filtering_vals = function(df) { df %>% 
    filter(state=="Colorado" | state=="California", county %in% c("Denver", "Boulder", "Los Angeles")) 
}

process_data = function(df) {
  # data processing pipelinie
  processed_data = df %>% 
    filtering_vals() %>% 
    group_by(fips) %>%
    arrange(date) %>% 
    mutate(number_new_cases=(cases - lag(cases))) %>%
    ungroup() %>%
    group_by(fips) %>% 
    mutate(new_cases_per_capita=number_new_cases/POPESTIMATE2019) %>% 
    mutate(new_cases_moving_average=compute_ma(new_cases_per_capita)) %>%
    arrange(date) %>% 
    group_by(fips) %>%
    do(merge(.,compute_r0(.$number_new_cases, .$date), by="date"))
  
  return(processed_data)
}


make_plot = function(processed_data){
  thm = theme(panel.border = element_rect(colour = "#222222", fill = NA, size=1), text=element_text(family="",size=12))
  
  title_str = paste('New Cases per capita, 5-day moving avg, ', max(processed_data$date))
  timeseries = processed_data %>% 
    ggplot(aes(date, new_cases_moving_average, color=county, group=county)) + geom_line(size=1.5,alpha=0.7)  +
    labs(x='', y='New Cases per capita', title=title_str) + scale_x_date(date_breaks = "3 weeks")  + thm + geom_vline(xintercept=as.Date("2020-06-01"), color="green") 
  r0_plot = processed_data %>% 
    ggplot(aes(date, mean_r0, color=county, group=county)) + geom_ribbon(aes(ymin=lower_r0, ymax=upper_r0), alpha=0.2)+ geom_line(size=1) + 
    labs(x='', y=TeX("$R_0$"), title=TeX("Estimated $R_0$")) + 
    scale_x_date(date_breaks="2 week", limits=as.Date(c("2020-04-01", "2020-07-01"))) + 
    ylim(0.4, 3) +  geom_hline(yintercept=1, color="black") + 
    facet_wrap(~county, ncol=1) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    thm + geom_vline(xintercept=as.Date("2020-06-01"), color="green") 
  
  plt = grid.arrange(timeseries, r0_plot, ncol=2)
  filename = paste(max(processed_data$date), ".png", sep="")
  ggsave(filename, plt, device=png(), height=10, width=20)
}

prep = function(wd="~/not_an_epidemiologist"){
  reqs = c("tidyverse", "zoo", "gridExtra", "ggthemr", "EpiEstim", "latex2exp")
  lapply(reqs, require, character.only=T)
  
  ggthemr('fresh', spacing=2 ,layout="scientific")
  setwd(wd)
}

prep()
make_plot(process_data(read_data()))

