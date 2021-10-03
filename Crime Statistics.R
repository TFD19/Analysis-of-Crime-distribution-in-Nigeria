# Set working directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets")

# Check the files in the working directory
dir()

# Load in the libraries required for manipulation
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)
library(readxl)
library(stringr)
library(plyr)
library(caTools)
library(png)
library(lubridate)
library(zoo)
library(xts)
library(Rcpp)
library(ggplot2)
library(plotly)
library(gganimate)
library(htmlwidgets)

# Import the datafile
  # Inspect how many sheets are contained in the excel file
  excel_sheets("Crime Statistics (2).xlsx")
  # Import the excel sheet
  crime_data <- read_excel("Crime Statistics (2).xlsx", col_names = TRUE, skip = 6)

# Remove the units column
  crime_data$Units <- NULL
  
# Slice the dataset into different parts for Nigeria and individual states
  nigeria_stat <- crime_data[1:135, ]
  state_stat <- crime_data[136:389, ]  

# Tidy up the nigeria_stat
  # Remove the columns without values
  nigeria_stat$`2012` <- NULL
  nigeria_stat$`2013` <- NULL  

  # Replace all NAs with zero
  na_to_0 <- function(x) {
    x[is.na(x)] <- 0
    return(x)
  }
  nigeria_stat <- na_to_0(nigeria_stat)
  
  # Gather the year columns into a single column
  nigeria_stat <- gather(nigeria_stat, key = "Year", value = "Number", 3:7)

# Tidy up the state_stat
  # Delete the columns without any values
  state_stat$`2007` <- NULL
  state_stat$`2008` <- NULL  
  state_stat$`2009` <- NULL  

  # Replace all NAs with zero
  state_stat <- na_to_0(state_stat)

  # Gather the year into a single column
  state_stat <- gather(state_stat, key = "Year", value = "Number", 3:6)
  
# Change the class of Year to date
  state_stat$Year <- as.numeric(state_stat$Year)
  state_stat$Year <- strptime(as.character(state_stat$Year), "%Y")  
  state_stat$Year <- year(state_stat$Year)
  state_stat$Crime <- state_stat$Items
  state_stat$Items <- NULL
  state_stat <- select(state_stat, States, Crime, Year, Number)
  
  
  # Convert the integers to date with 2010 as the origin
  state_stat$Year <- as.Date(state_stat$Year, origin = "2010")
  state_stat$Year %>% lubridate::ymd() %>% format("Y") %>% is.Date()
  
# Separating the Item rows with numbers
  s <- str_split_fixed(nigeria_stat$Items, "2", 2) %>% str_split_fixed("3", 2) %>% 
    str_split_fixed("4", 2) %>% str_split_fixed("5", 2) %>% 
    str_split_fixed("6", 2) %>% str_split_fixed("7", 2) %>% 
    str_split_fixed("8", 2) %>% str_split_fixed("9", 2) %>% 
    str_split_fixed("10", 2)
  nigeria_stat <- cbind.data.frame(s, nigeria_stat)
  nigeria_stat$`2` <- NULL
  nigeria_stat$States <- NULL
  nigeria_stat$Items <- NULL
  nigeria_stat$Crime <- nigeria_stat$`1`
  nigeria_stat$`1` <- NULL
  nigeria_stat <- select(nigeria_stat, Crime, Year, Number)  
  nigeria_stat$Crime <- trimws(nigeria_stat$Crime)
  nigeria_stat$Year <- strptime(nigeria_stat$Year, "%Y") %>% year()
  nigeria_stat <- nigeria_stat %>% group_by(Crime, Year) %>% summarise(Number = sum(Number, na.rm = FALSE))
  

# Investigate relationships and outcomes with visuals
  # How does the number of convicted criminals for major offences vary annually in Nigeria
  crime_rate <- nigeria_stat %>% filter(Crime %in% c("Abduction", "Arson", "Armed robbery", "Assault", "Criminal lundary", "Cultism / ritual", "Currency offence", "Debt", "Economic sabotage", "Forgery", "Human trafficking", "Immigration", "Indian hemp", "Murder", "Other offences", "Robbery", "sedition", "Sex offence", "Smuggling", "Stealing", "Traffic offence", "Treason", "Unlawful possession of arms", "Unlawful poss of property")) %>% 
    ggplot(aes(x = Crime, y = Number, fill = factor(Year))) + 
    geom_col() + scale_y_continuous(n.breaks = 20) + 
    labs(y = "Total number of Convicted Cases", fill = "Year", title = "Number of People Convicted for each Crime Annually") + 
    coord_flip() + transition_time(Year)
  
  p <- animate(crime_rate, width = 1200, height = 400, fps = 25, duration = 20, rewind = TRUE, renderer = av_renderer())
  anim_save("output3.gif", p)
  
# Assessing the total number in prisons in each state against the state's maximum capacity
  state_stat %>% filter(Crime %in% c("Total", "Maximum capacity") & Year %in% c(2010, 2011)) %>% group_by(Crime) %>% 
    ggplot(aes(x = States, y = Number, fill = Crime)) + geom_col(position = "dodge") + 
    facet_wrap(vars(Year), scales = "free") + labs(title = "Maximum Prison Capacity of each State versus Total Number of Prisoners") + 
    theme(legend.title = element_blank()) + coord_flip()
    
# Investigate the states with the highest number of prisoners between 2010 and 2011
  num_prs <- state_stat %>% filter(Crime == "Total" & Year %in% c(2010, 2011)) %>% ggplot(aes(x = States, y = Number, fill = Year)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 1.0)) + 
    labs(y = "Total Number of Prisoners", title = "Total Number of Prisoners in each State between 2010 and 2011") + 
    transition_time(Year)
  p <- animate(num_prs, width = 1200, height = 400, fps = 20, duration = 30, rewind = TRUE, renderer = av_renderer())
  anim_save("output.mp4", p)
  
# Investigate the number of people in Nigerian prisons according to their length of jail term
  nigeria_stat %>% filter(Crime %in% c("Remand/awaiting trial", "Short term", "Long term", "Condemned", "Detainees", "Others")) %>% 
    mutate(Crime = as.factor(Crime), Year = as.factor(Year)) %>% 
    plot_ly(x = ~Crime, y = ~Number, color = ~Year) %>% add_bars() %>% 
    layout(title = "Number of Prisoners in Nigeria Categorized by the Nature of their Incarceration", xaxis = list(title= "Category of Incarceration"), yaxis = list(title = "Number of Prisoners", barmode = "group"))
    
  
# Investigating how petitions are being treated in Nigeria
  nigeria_stat %>% filter(Crime %in% c("No. of Petitions Fully Investigated", "No. of Petitions Under Investigated", "No. of Petitions Rejected for Lack of Jurisdiction", "Total No. of petitions Referred")) %>% 
    mutate(Crime = as.factor(Crime), Year = as.factor(Year)) %>% plot_ly(x = ~Crime, y = ~Number, color = ~Year) %>% 
    add_bars() %>% layout(title = "Description of How Petitions are Treated in Nigeria", xaxis = list(title = "Category of Petitions"), yaxis = list(title = "Number of Petitions"))
  