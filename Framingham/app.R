## this shiny app is to calculate the Framingham risk score.

library(shiny)
library(ggradar)
library(tidyverse)
library(plotly)

#######################################################
#    Define UI
#######################################################

ui <- fluidPage(
   
   # Application title
   titlePanel("Framingham Risk Score"),
   
   h4('Please enter the following information:'),

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
                      choices = list('Smoker', 'Non-smoker'), selected = 'Non-smoker'),
         strong('Systolic blood pressure:'),
         radioButtons('SBPButton', 'Treated:',
                      choices = list('Yes', 'No'), selected = 'Yes'),
         sliderInput("SBPSlider", "Value, mm Hg:",
                     min = 115, max = 160,
                     value = 140),
         sliderInput("ChoSlider", "Total cholesterol, mg/dL:",
                     min = 155, max = 280,
                     value = 220),
         sliderInput("HDLSlider", "HDL cholesterol, mg/dL",
                     min = 35, max = 60,
                     value = 50)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Result",
                  br(),
                  h3('Risk scale in each term'),
                  plotlyOutput("RiskRadarPlot"),
                  h3('Risk points:'),
                  textOutput('PointsOutput'),
                  br(),
                  h3('10-year risk:'),
                  textOutput('RiskOutput'),
                  br()
          ),
          tabPanel("Summary",
                   br(),
                   tableOutput("view")
          ),
          tabPanel("Relevant information", 
                   br(),
                   verbatimTextOutput("Intro"),
                   br(),
                   uiOutput("tab"))
        )
      )
   )
)


######################################################################
# Define server logic
######################################################################


######################################################################
# function1: convert single points into results
######################################################################

points_to_results = function(gender, hdl_points, chol_points, 
                             smoke_points, age_points, sbp_points){
  if (gender == "Male"){
    min_vec = c(-1, 0, 0, -9, 0)
    max_vec = c(2, 11, 8, 13, 3)
  }else{
    min_vec = c(-1, 0, 0, -7, 0)
    max_vec = c(2, 13, 9, 16, 6)
  }
  points_df = tibble(hdl_points, chol_points, 
                     smoke_points, age_points, sbp_points) %>% 
    gather(key = term, value = points, hdl_points:sbp_points) %>% 
    mutate(min = min_vec,
           max = max_vec,
           range = max - min, 
           scale = round(100*(points-min)/range, 0))
  
  total_points = sum(hdl_points, age_points, chol_points, 
                     smoke_points, age_points, sbp_points)
  return(list(points_df = points_df,
              total_points = total_points))
}

######################################################################
# function2: calculate points results in man
######################################################################

men_points <- function(age, total_chol, smoker, HDL_chol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age < 35) age_points = points - 9
  else if(age < 40) age_points = points - 4
  else if(age < 45) age_points = points
  else if(age < 50) age_points = points + 3
  else if(age < 55) age_points = points + 6
  else if(age < 60) age_points = points + 8
  else if(age < 65) age_points = points + 10
  else if(age < 70) age_points = points + 11
  else if(age < 75) age_points = points + 12
  else if(age < 80) age_points = points + 13
  
  # Total cholesterol points
  if(age > 19 & age < 40)
  {
    # Age 20–39 years
    if(total_chol < 160) chol_points = points
    else if (total_chol < 200 ) chol_points = points + 4
    else if(total_chol < 240) chol_points = points + 7
    else if(total_chol < 280) chol_points = points + 9
    else chol_points = points + 11
  } else if(age > 39 & age < 50)
  {
    # Age 40–49 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 3
    else if(total_chol < 240) chol_points = points + 5
    else if(total_chol < 280) chol_points = points + 6
    else chol_points = points + 8
  } else if(age > 49 & age < 60)
  {
    # Age 50–59 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 2
    else if(total_chol < 240) chol_points = points + 3
    else if(total_chol < 280) chol_points = points + 4
    else chol_points = points + 5
  } else if(age > 59 & age < 70)
  {
    # Age 60–69 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 1
    else if(total_chol < 240) chol_points = points + 1
    else if(total_chol < 280) chol_points = points + 2
    else chol_points = points + 3
  } else 
  {
    # Age 70–79 years
    if(total_chol > 240) chol_points = points + 1
  } 
  
  # Cigarette smoker points
  if(smoker == "Smoker")
  {
    if(age > 19 & age < 40) smoke_points = points + 8
    else if(age < 50) smoke_points = points + 5
    else if(age < 60) smoke_points = points + 3
    else if(age < 70) smoke_points = points + 1
    else smoke_points = points + 1
  }else smoke_points = 0
  
  # HDL cholesterol points
  if(HDL_chol > 59) hdl_points = points - 1
  else if(HDL_chol > 49) hdl_points = points
  else if(HDL_chol > 39) hdl_points = points + 1
  else hdl_points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP < 120) sbp_points = points
    else if(SBP < 130) sbp_points = points + 1
    else if(SBP < 140) sbp_points = points + 2
    else if(SBP < 160) sbp_points = points + 2
    else sbp_points = points + 3
  } else if(treated == 'No')
  {
    if(SBP < 130) sbp_points = points
    else if(SBP < 140) sbp_points = points + 1
    else if(SBP < 160) sbp_points = points + 1
    else sbp_points = points + 2
  }
 
  
  points_to_results("Male", hdl_points, chol_points, 
                    smoke_points, age_points, sbp_points)
                
}


######################################################################
# function3: calculate points results in woman
######################################################################

women_points <- function(age, total_chol, smoker, HDL_chol, treated, SBP)
{
  points = 0
  
  # Age points
  if(age < 35) age_points = points - 7
  else if(age < 40) age_points = points - 3
  else if(age < 45) age_points = points
  else if(age < 50) age_points = points + 3
  else if(age < 55) age_points = points + 6
  else if(age < 60) age_points = points + 8
  else if(age < 65) age_points = points + 10
  else if(age < 70) age_points = points + 12
  else if(age < 75) age_points = points + 14
  else if(age < 80) age_points = points + 16
  
  # Total cholesterol points
  if(age > 19 & age < 40)
  {
    # Age 20–39 years
    if(total_chol < 160) chol_points = points
    else if (total_chol < 200 ) chol_points = points + 4
    else if(total_chol < 240) chol_points = points + 8
    else if(total_chol < 280) chol_points = points + 11
    else chol_points = points + 13
  } else if(age > 39 & age < 50)
  {
    # Age 40–49 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 3
    else if(total_chol < 240) chol_points = points + 6
    else if(total_chol < 280) chol_points = points + 8
    else chol_points = points + 10
  } else if(age > 49 & age < 60)
  {
    # Age 50–59 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 2
    else if(total_chol < 240) chol_points = points + 4
    else if(total_chol < 280) chol_points = points + 5
    else chol_points = points + 7
  } else if(age > 59 & age < 70)
  {
    # Age 60–69 years
    if(total_chol < 160) chol_points = points
    else if(total_chol < 200) chol_points = points + 1
    else if(total_chol < 240) chol_points = points + 2
    else if(total_chol < 280) chol_points = points + 3
    else chol_points = points + 4
  } else 
  {
    # Age 70–79 years
    if(total_chol > 239) chol_points = points + 2
    else if(total_chol > 159) chol_points = points + 1
  } 
  
  # Cigarette smoker points
  if(smoker == "Smoker")
  {
    if(age > 19 & age < 40) smoke_points = points + 9
    else if(age < 50) smoke_points = points + 7
    else if(age < 60) smoke_points = points + 4
    else if(age < 70) smoke_points = points + 2
    else smoke_points = points + 1
  }else smoke_points = 0
  
  # HDL cholesterol points
  if(HDL_chol > 59) hdl_points = points - 1
  else if(HDL_chol > 49) hdl_points = points
  else if(HDL_chol > 39) hdl_points = points + 1
  else hdl_points = points + 2
  
  # Systolic blood pressure points
  if(treated == 'Yes')
  {
    if(SBP < 120) sbp_points = points
    else if(SBP < 130) sbp_points = points + 3
    else if(SBP < 140) sbp_points = points + 4
    else if(SBP < 160) sbp_points = points + 5
    else sbp_points = points + 6
  } else if(treated == 'No')
  {
    if(SBP < 120) sbp_points = points
    else if(SBP < 130) sbp_points = points + 1
    else if(SBP < 140) sbp_points = points + 2
    else if(SBP < 160) sbp_points = points + 3
    else sbp_points = points + 4
  }
  
  points_to_results("Female", hdl_points, chol_points, 
                    smoke_points, age_points, sbp_points)
  
}


##########################################################################
# function4: radar plot
##########################################################################

radar_plotly = function(points_df){
  scale = append(points_df$scale, points_df$scale[1])
  plot_ly(
    type = 'scatterpolar',
    r = scale,
    theta = c('HDL cholesterol','Total cholesterol','Smoke', 'Age', 'SBP', 'HDL cholesterol'),
    fill = 'toself'
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,100)
        )
      ),
      showlegend = F
    )
}

#######################################################################
# function 5: calculate risk
#######################################################################

cal_risk <- function(points, gender){

 if(gender == "Male") 
 {
  if(points <= 0) risk = 'Less than 1%'
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
     if(points < 10) risk = 'Less than 1%'
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
     else if(points > 24) risk = 'Over 30%'
     
 }
  print(risk, quote = F)
  if (risk == 'Less than 1%') print("low risk", quote = F)
  else if (risk == 'Over 30%') print("high risk", quote = F)
  else {
    value = as.numeric(sub("%","",risk))
    if(value < 10) print("low risk", quote = F)
    else if(value < 20) print("intermediate risk", quote = F)
    else print("high risk", quote = F)
  }
  
}
####################################################################
# define server
####################################################################
server <- function(input, output) {
   
   age <- reactive({input$AgeSlider})
   total_chol <- reactive({input$ChoSlider})
   smoker <- reactive({as.character(input$SmokingButton)})
   treated <- reactive({as.character(input$SBPButton)})
   SBP <- reactive({as.numeric(input$SBPSlider)})
   HDLChol <- reactive({as.numeric(input$HDLSlider)})
   sex <- reactive({as.character(input$SexButton)})
   
   output$Intro <- renderText({"The Framingham Risk Score is a gender-specific algorithm used to estimate the 10-year cardiovascular risk of an individual.
It is one of a number of scoring systems used to determine an individual's chances of developing cardiovascular disease."})
   
   output$RiskRadarPlot <- renderPlotly({
     if(sex() == 'Male')
     {
       men_results = men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       radar_plotly(men_results$points_df)
     }
     else if(sex() == 'Female')
     {
       women_results = women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       radar_plotly(women_results$points_df)
     }

   
})

   output$PointsOutput <- renderPrint({
    
       if(sex() == 'Male')
       {
         men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())$total_points
       }
       else if(sex() == 'Female')
       {
         women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())$total_points
       }
   })
   
   output$RiskOutput <- renderPrint({
     
     if(sex() == 'Male')
     {
       points = men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())$total_points
       cal_risk(points, "Male")
     }
     else if(sex() == 'Female')
     {
       points = women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())$total_points
       cal_risk(points, "Female")
     }
   })
   
   url <- a("Framingham Risk Score", href="https://en.wikipedia.org/wiki/Framingham_Risk_Score")
   url2 <- a("what is cardiovascular disease", href="http://www.heart.org/en/health-topics/consumer-healthcare/what-is-cardiovascular-disease")
   output$tab <- renderUI({
     tagList(url, br(), url2)
   })
   
   output$view <- renderTable({
     if(sex() == 'Male')
     {
       result = men_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       result$points_df %>% 
         mutate(term = c('HDL cholesterol','Total cholesterol','Smoking','Age','SBP'),
                input = c(HDLChol(), total_chol(), smoker(), age(), SBP()),
                scale = paste(as.character(scale), '%', '')) %>% 
         select(term, risk_points = points, risk_scale = scale) 
         
     }
     else if(sex() == 'Female')
     {
       result = women_points(age(), total_chol(), smoker(), HDLChol(), treated(), SBP())
       result$points_df %>% 
         mutate(term = c('HDL cholesterol','Total cholesterol','Smoking','Age','SBP'),
                input = c(HDLChol(), total_chol(), smoker(), age(), SBP()),
                scale = paste(as.character(scale), '%', ''),
                risk_points = as.integer(points)) %>% 
         select(term, input, risk_points, risk_scale = scale) 
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

