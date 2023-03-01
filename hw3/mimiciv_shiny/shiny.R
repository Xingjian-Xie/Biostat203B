

library(shiny)
library(ggplot2)
#runExample("01_hello")
data = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ICU_Cohort"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
    
      selectInput(inputId = "variable",
                  label = "Choose a variable:",
                  choices = c('Ethnicity', 'Thirdy_Day_Mortality_Rate', 
                  'Language', 'Insurance', 'Marital_status', 'Gender'))
                  
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "BarPlot")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  var <- reactive({
    switch(input$variable,
           'Ethnicity' = 'ethnicity',
           'Thirdy_Day_Mortality_Rate' = 'thirty_day_mort',
           'Language' = 'language',
           'Insurance' = 'insurance', 
           'Marital_status' = 'marital_status', 
           'Gender' = 'gender'
    )
  })
  
  
  output$BarPlot <- renderPlot({
    
    ggplot(data, aes_string(x = var())) + geom_bar()

  })
  
}



shinyApp(ui, server)


#ggplot(data, aes_string(x = 'ethnicity')) + geom_bar()








