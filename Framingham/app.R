#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Framingham Risk Score"),
   
   h4('Please enter the following information:'),
   br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons('SexButton', 'Gender:', 
                      choices = list(
                        'Male',
                        'Female'
                      ), selected = 'Female'),
         sliderInput("AgeSlider", "Age:",
                     min = 20, max = 80,
                     value = 50),
         radioButtons('SmokingButton', 'Smoking status:',
                      choices = list('Smoker', 'Non-smoker'), selected = 'Non-smoker', T),
         strong('Systolic blood pressure:'),
         radioButtons('SBPButton', 'Treated:',
                      choices = list('Yes', 'No'), selected = 'Yes', T),
         sliderInput("SBPSlider", "Value, mm Hg:",
                     min = 120, max = 160,
                     value = 140),
         sliderInput("ChoSlider", "Total cholesterol, mg/dL:",
                     min = 160, max = 280,
                     value = 220),
         sliderInput("HDLSlider", "HDL cholesterol, mg/dL",
                     min = 40, max = 60,
                     value = 50)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h4('Risk points:'),
         textOutput('PointsOutput'),
         br(),
         h4('10-year risk in %:'),
         textOutput('RiskOutput')
      )
   )
)

# Define server logic required to draw a histogram

## calculate points in man
men_points <- function(age, total_chol, smoker, HDL_chol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age < 35) points = points - 9
  else if(age < 40) points = points - 4
  else if(age < 45) points = points
  else if(age < 50) points = point + 3
  else if(age < 55) points = points + 6
  else if(age < 60) points = points + 8
  else if(age < 65) points = points + 10
  else if(age < 70) points = points + 11
  else if(age < 75) points = points + 12
  else if(age < 80) points = points + 13
  
  # Total cholesterol points
  if(age > 19 & age < 40)
  {
    # Age 20–39 years
    if(total_chol < 160) points = points
    else if (total_chol < 200 ) points = points + 4
    else if(total_chol < 240) points = points + 7
    else if(total_chol < 280) points = points + 9
    else points = points + 11
  } else if(age > 39 & age < 50)
  {
    # Age 40–49 years
    if(total_chol < 160) points = points
    else if(total_chol < 200) points = points + 3
    else if(total_chol < 240) points = points + 5
    else if(total_chol < 280) points = points + 6
    else points = points + 8
  } else if(age > 49 & age < 60)
  {
    # Age 50–59 years
    if(total_chol < 160) points = points
    else if(total_chol < 200) points = points + 2
    else if(total_chol < 240) points = points + 3
    else if(total_chol < 280) points = points + 4
    else points = points + 5
  } else if(age > 59 & age < 70)
  {
    # Age 60–69 years
    if(total_chol < 160) points = points
    else if (total_chol < 200)points = points + 1
    else if(total_chol < 240) points = points + 1
    else if(total_chol < 280) points = points + 2
    else points = points + 3
  } else 
  {
    # Age 70–79 years
    if(total_chol > 240) points = points + 1
  } 
  
  # Cigarette smoker points
  if(smoker == "Smoker")
  {
    if(age > 19 & age < 40) points = points + 8
    else if(age < 50) points = points + 5
    else if(age < 60) points = points + 3
    else if(age < 70) points = points + 1
    else points = points + 1
  }
  
  # HDL cholesterol points
  if(HDL_chol > 59) points = points - 1
  else if(HDL_chol > 49) points = points
  else if(HDL_chol > 39) points = points + 1
  else points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP < 120) points = points
    else if(SBP < 130) points = points + 1
    else if(SBP < 140) points = points + 2
    else if(SBP < 160) points = points + 2
    else points = points + 3
  } else if(treated == 'No')
  {
    if(SBP < 130) points = points
    else if(SBP < 140) points = points + 1
    else if(SBP < 160) points = points + 1
    else points = points + 2
  }
  
  return(points)
}

## calculate points in women
women_points <- function(age, total_chol, smoker, HDL_chol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age < 35) points = points - 7
  else if(age < 40) points = points - 3
  else if(age < 45) points = points
  else if(age < 50) points = point + 3
  else if(age < 55) points = points + 6
  else if(age < 60) points = points + 8
  else if(age < 65) points = points + 10
  else if(age < 70) points = points + 12
  else if(age < 75) points = points + 14
  else if(age < 80) points = points + 16
  
  # Total cholesterol points
  if(age > 19 & age < 40)
  {
    # Age 20–39 years
    if(total_chol < 160) points = points
    else if (total_chol < 200 ) points = points + 4
    else if(total_chol < 240) points = points + 8
    else if(total_chol < 280) points = points + 11
    else points = points + 13
  } else if(age > 39 & age < 50)
  {
    # Age 40–49 years
    if(total_chol < 160) points = points
    else if(total_chol < 200) points = points + 3
    else if(total_chol < 240) points = points + 6
    else if(total_chol < 280) points = points + 8
    else points = points + 10
  } else if(age > 49 & age < 60)
  {
    # Age 50–59 years
    if(total_chol < 160) points = points
    else if(total_chol < 200) points = points + 2
    else if(total_chol < 240) points = points + 4
    else if(total_chol < 280) points = points + 5
    else points = points + 7
  } else if(age > 59 & age < 70)
  {
    # Age 60–69 years
    if(total_chol < 160) points = points
    else if (total_chol < 200)points = points + 1
    else if(total_chol < 240) points = points + 2
    else if(total_chol < 280) points = points + 3
    else points = points + 4
  } else 
  {
    # Age 70–79 years
    if(total_chol > 239) points = points + 2
    else if(total_chol > 159) points = points + 1
  } 
  
  # Cigarette smoker points
  if(smoker == "Smoker")
  {
    if(age > 19 & age < 40) points = points + 9
    else if(age < 50) points = points + 7
    else if(age < 60) points = points + 4
    else if(age < 70) points = points + 2
    else points = points + 1
  }
  
  # HDL cholesterol points
  if(HDL_chol > 59) points = points - 1
  else if(HDL_chol > 49) points = points
  else if(HDL_chol > 39) points = points + 1
  else points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP < 120) points = points
    else if(SBP < 130) points = points + 3
    else if(SBP < 140) points = points + 4
    else if(SBP < 160) points = points + 5
    else points = points + 6
  } else if(treated == 'No')
  {
    if(SBP < 120) points = points
    else if(SBP < 130) points = points + 1
    else if(SBP < 140) points = points + 2
    else if(SBP < 160) points = points + 3
    else points = points + 4
  }
  
  return(points)
}

## calculate risk
cal_risk <- function(points, gender){

 if(gender == "Male") 
 {
  if(points == 0) risk = '< 1%'
  else if(points >= 1 & points <= 4) risk = '1%'
  else if(points >= 5 & points <= 6) risk = '2%'
  else if(points == 7) risk = '3%'
  else if(points == 8) risk = '4%'
  else if(points == 9) risk = '5%'
  else if(points == 10) risk ='6%'
  else if(points == 11) risk = '8%'
  else if(points == 12) risk = '10%'
  else if(points == 13) risk = '12%'
  else if(points == 14) risk = '16%'
  else if(points == 15) risk = '20%'
  else if(points == 16) risk = '25%'
  else if(points >= 17) risk = 'Over 30%'
 }else if (gender == "Female")
 {
     if(points < 10) risk = '< 1%'
     else if(points < 13) risk = '1%'
    
     else if(points < 15) risk = '2%'
     else if(points == 15) risk = '3%'
     else if (points == 16) risk = '4%'
     else if(points == 17) risk = '5%'
     else if(points == 18) risk ='6%'
     else if(points == 19) risk = '8%'
     else if(points == 20) risk = '11%'
     else if(points == 21) risk = '14%'
     else if(points == 22) risk = '17%'
     else if(points == 23) risk = '22%'
     else if(points == 24) risk = '27%'
     else if(points >24) risk = 'Over 30%'
     
 }
  return(risk)
}

server <- function(input, output) {
   
   age <- reactive({input$AgeSlider})
   total_chol <- reactive({input$ChoSlider})
   smoker <- reactive({as.character(input$SmokingButton)})
   treated <- reactive({as.character(input$SBPButton)})
   SBP <- reactive({as.numeric(input$SBPSlider)})
   HDLChol <- reactive({as.numeric(input$HDLSlider)})
   sex <- reactive({as.character(input$SexButton)})
   
   output$PointsOutput <- renderPrint({
    
       if(sex() == 'Male')
       {
         men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       }
       else if(sex() == 'Female')
       {
         women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       }
   })
   
   output$RiskOutput <- renderPrint({
     
     if(sex() == 'Male')
     {
       points = men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       print(cal_risk(points, "Male"), quote = F)
     }
     else if(sex() == 'Female')
     {
       points = women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       print(cal_risk(points, "Female"), quote = F)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

