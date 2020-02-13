###################################################
## CODE IT ## REACTIVE DATA VIS ## * ## SOLUTION ##
###################################################

#  Author:  Dan Turner (dturner@u.northwestern.edu)
#  Updated: 2-5-19

####################
## DATA WRANGLING ##
####################

# Import the CSV files with the raw data

US_births_2000_2014_SSA <- read_csv("data/US_births_2000-2014_SSA.csv")
US_births_1994_2003_CDC_NCHS <- read_csv("data/US_births_1994-2003_CDC_NCHS.csv")

# Set up our data frame with the combined data, without duplicate years
births <- US_births_1994_2003_CDC_NCHS %>%   # Start with the older data
  filter( year < 2000 ) %>%                  # Subtract duplicate years from smaller set
  bind_rows( US_births_2000_2014_SSA ) %>%   # Append the later data
  mutate( weekday = weekdays( as.Date( paste0( year, "-", month, "-", date_of_month ) ) ) )  %>% # Weekday name (i.e. Sunday)
  group_by(year, weekday, date_of_month) %>% # Pool across the months, days, and years
  summarize(sum = sum(births))               # Sum each day of each month



###############
## SHINY APP ##
###############

# USER SIDE
ui <- fluidPage(
  
  ## INTPUT PART
  
  # Take the input, which is a day of the month and weekday name
  
  # For day of the month, let's have the user type in the day number
  sliderInput(inputId = "monthday",
               label = "What range of days in the month do you want to check (exclusive)?",
               min = 1,         # Min days that can occur in a month (the first of April is still April)
               max = 31,        # Max days in a month
               value = c(1,7)), # Two values gives the option to select a range
  
  # For weekday name, there are only 7 options, so let's use a dropdown
  selectInput(inputId = "year",
              label = "What year do you want to check?",
              choices = unique(births$year)),   # Autopopulate the choices

  ## OUTPUT PART
  
  # Display a plot of the data with years across the x axis
  
  # Our title
  h2("Births by weekday"),
  
  # plotOutput displays a ggplot to the user, in this case the one saved to output$the.plot
  plotOutput("the.plot"),
  
  # Update the page (e.g. refresh the server with new input) when the user presses "Go"
  actionButton("update", "Go")
  
) #/ui


# SERVER SIDE
server <- function(input, output) {
  
  # Filter the data according to the user input
  selectedData <- eventReactive(input$update,{ 
    births %>% 
      filter(date_of_month > input$monthday[1],   # Match the user input for min day
             date_of_month < input$monthday[2],   # Match the user input for max day
             year == input$year)                  # Match the user input for year
  })

  # Make and render the ggplot as the.plot
  output$the.plot <- renderPlot({
    
    ggplot(selectedData(), aes(x = weekday, y = sum) ) +                 # our plot setup
    geom_bar(stat = "identity") +                                        # this will be a histogram
    ggtitle(label = "# babies by weekday for this year and date range")  # a title for the plot
    
  })

} #/server

shinyApp(ui = ui, server = server)     # Put it all together

# Done
