#########################################################
## CODE IT ## ONLINE CLOTHING REVIEWS ## * ## SOLUTION ##
#########################################################

#  Author:  Dan Turner (dturner@u.northwestern.edu)
#  Updated: 2-7-19

####################
## DATA WRANGLING ##
####################

# Import the data
reviews <- as.data.frame( read_csv("data/Womens Clothing E-Commerce Reviews.csv") )

# Add a column for the word count, just for fun
reviews$Review.Length <- sapply(strsplit(reviews$`Review Text`, " "), length) # this works by splitting the text at spaces and counting the parts

# Data wrangling
reviews.grouped <- reviews %>%
  rename_all(list(~make.names(.))) %>%  # replace tidy-unfriendly spaces with periods
  mutate(ageCat=cut_number(Age, n = 3, labels = c("younger", "middle", "older"))) %>% # bin ages in three equal categories 
  group_by(Department.Name, Class.Name, ageCat) %>%  # group by these factors
  summarize(meanRating = mean(Rating),            # Summarize the rating...
            meanReccom = mean(Recommended.IND),   #...and the recommendation boolean...
            meanLength = mean(Review.Length)      #...and the word count of the Review.Text
  ) %>% #/summarize
  na.omit() # strike NA's

## The Plan
# This task is about two kinds of preference, a boolean recommendation and a scalar recommendation.
#    -Since our two measures of interest are continuous, let's use a scatterplot of meanRecomm and meanRating.
#    -Since we always have threee age groups, we can facet by age, so there will be 3 plots per view.
#    -To let the user look for trends, we will let them group by Department.Name, and Class.Name.

# UI
# SERVER
# Call shinyApp()

###############
## SHINY APP ##
###############

# UI
ui <- fluidPage(
  
  shinytheme(theme = "flatly"), # add a theme
  
  titlePanel("ONLINE CLOTHING REVIEWS SOLUTION"), # add a title
  
  sidebarLayout(  # sidebar layout with left sidebar
    sidebarPanel(
      # Dropdown for the department, class, and age
      # This allows users to pick and choose what data gets shown based on lots of grouping options
      checkboxGroupInput(inputId = "department", label = "What department",
                         choices = unique(reviews.grouped$Department.Name),
                         selected = unique(reviews.grouped$Department.Name)),   # Autopopulate the choices
      
      checkboxGroupInput(inputId = "class", label = "What class?",
                         choices = unique(reviews.grouped$Class.Name),
                         selected = unique(reviews.grouped$Class.Name)),   # Autopopulate the choices
    
      # Update the page
      actionButton("update", "Update")
    ),#/sidebarPanel
    
    mainPanel(
      plotOutput("the.plot"), #  plotOutput displays a ggplot to the user
      plotOutput("the.plot.byage"), #  same as above, but faceted by age group
    )#/mainPanel
    
  )#/sidebarLayout
) #/ui

# SERVER
server <- function(input, output) {
  
  # Filter the data according to the user input
  selectedData <- eventReactive(input$update,{ 
    reviews.grouped %>% 
      filter(Department.Name %in% input$department,   # filter rows with a Department.Name not in the list of checked boxes
             Class.Name %in% input$class             # same for class
             )#/filter               
  })#/selectData
  
  # Make and render the ggplot as the.plot
  output$the.plot <- renderPlot({
    ggplot(selectedData(), aes(x = meanReccom, y = meanRating, color = ageCat) ) +        # our plot setup
      geom_point(size = 5) +                                        # this will be a histogram
      ggtitle(label = "Continuous vs Categorical Recommendation")  # a title for the plot
  })#/renderPlot

  # Make and render the ggplot as the.plot
  output$the.plot.byage <- renderPlot({
    ggplot(selectedData(), aes(x = meanReccom,       # mean logical rating
                               y = meanRating,       # mean scalar rating
                               color = ageCat) ) +   # color by age group
      geom_point(size = 5) +                         # larger points  
      facet_wrap(~ageCat) +                          # faceted by age group
      ggtitle(label = "Faceted by age group")        # a title for the plot
  })#/renderPlot
  
} #/server

# Call shinyApp()
shinyApp(ui = ui, server = server)

# Done
