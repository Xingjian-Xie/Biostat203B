

library(shiny)
library(ggplot2)
data = readRDS("hw3/mimiciv_shiny/icu_cohort.rds")
flag_list = c('ethnicity', 'language', 'insurance',
              'marital_status', 'gender')
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
    
      selectInput(inputId = 'variable',
                  label = 'Choose a variable:',
                  choices = c('Ethnicity', 'Language', 'Insurance', 
                  'Marital_status', 'Gender', 'Age_at_Hospital_Admission',
                  'Bicarbonate', 'Chloride', 'Creatinine', 'Glucose',
                  'Potassium', 'Sodium', 'Hematocrit', 'White_Blood_Cells',
                  'Heart_Rate', 'Blood_Pressure_Systolic',
                  'Blood_Pressure_Mean','Respiratory_Rate',
                  'Temperature_Fahrenheit'))
                  
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput('Plots'),
      verbatimTextOutput('Summary')
      
    )
  )
)


server <- function(input, output) {
  
  var <- reactive({
    switch(input$variable,
           'Ethnicity' = 'ethnicity',
           'Language' = 'language',
           'Insurance' = 'insurance', 
           'Marital_status' = 'marital_status', 
           'Gender' = 'gender',
           'Age_at_Hospital_Admission' = 'age_adm',
           'Bicarbonate' = 'Bicarbonate', 
           'Chloride' = 'Chloride', 
           'Creatinine' = 'Creatinine', 
           'Glucose' = 'Glucose',
           'Potassium' = 'Potassium',
           'Sodium' = 'Sodium', 
           'Hematocrit' = 'Hematocrit',
           'White_Blood_Cells' = 'White_Blood_Cells',
           'Heart_Rate' = 'Heart_Rate',
           'Blood_Pressure_Systolic' = 
             'Non_Invasive_Blood_Pressure_systolic',
           'Blood_Pressure_Mean' = 
             'Non_Invasive_Blood_Pressure_mean',
           'Respiratory_Rate' = 'Respiratory_Rate',
           'Temperature_Fahrenheit' = 'Temperature_Fahrenheit'
    )
  })


observe(

  output$Plots <- renderPlot({
  if (var() %in% flag_list){
    
    ggplot(data, aes_string(x = var())) + geom_bar() +
      scale_x_discrete(guide = guide_axis(n.dodge=2))

  } else {
  
    ggplot(data, aes_string(y = var())) + geom_boxplot() +
      coord_cartesian(ylim =
      quantile(data[var()], c(0.05, 0.9), na.rm = T))

  }
})

)

output$Summary <- renderPrint(
  if (var() %in% flag_list){
    
    table(data[var()])
  
  } else {
    
    summary(data[var()])
    
  }  
)

}

shinyApp(ui, server)










