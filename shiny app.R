#SHINY APP CODE####

##Reader instructions####
#To run this Shiny app simply click "Run app". The interactive app will open in a new window.
#Shiny,dplyr, ggplot and readxl packages are needed to run this script.
#The script will gather the data by itself via github.

#install.packages("shiny")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

##Fetching Data####
download.file(url = "https://github.com/albertobling/BLM---Logit/raw/fb017652e26ceb86b692e24ef6cab062741dac85/case_data.xlsx",
              destfile = "case_data2.xlsx")

case.data<-read_excel("case_data2.xlsx")
df<-as.data.frame(case.data)

#Recoding docket_type
df$docket_type1 = case_when(
  df$docket_type == "mandatory" ~ 0,
  df$docket_type == "mandatory principled" ~ 1,
  df$docket_type == "docket control" ~ 2)

#Reversing government win 
df$litigant_win <- as.numeric(!df$government_win)
df$litigant_win

##Running the model####
mod1 <- glm(litigant_win ~ as.factor(docket_type1) +
              individuals * as.factor(docket_type1) +
              individuals +
              government_appellant,
            data = df,
            family = binomial(link = "logit"))

##Creating the app####
#Defining UI
ui <- fluidPage(
  titlePanel("Litigant Win Probability"),
  sidebarLayout(
    sidebarPanel(
      selectInput("docket_type", "Docket Type", 
                  choices = c("Mandatory" = "0", "Mandatory Principled" = "1", "Discretionary" = "2"),
                  selected = "0")
    ),
    mainPanel(
      plotOutput("probability_plot")
    )
  )
)

#Setting server
server <- function(input, output) {
  predicted_probability <- reactive({
    individuals <- 1
    corporation <- 1
    
    new_data <- data.frame(
      docket_type1 = as.factor(input$docket_type),
      individuals = individuals,
      corporation = corporation,
      government_appellant = 0
    )
    
    #PRobabilities
    prob_individuals <- predict(mod1, newdata = new_data, type = "response")
    
    new_data$individuals <- 0
    new_data$corporation <- 1
    
    #Probabilities
    prob_corporation <- predict(mod1, newdata = new_data, type = "response")
    
    #Gathering in data-fram
    result <- data.frame(
      Litigant_Type = c("Individuals", "Corporation/association"),
      Probability = c(prob_individuals, prob_corporation)
    )
    
    return(result)
  })
  
  #Predicted prob. plot
  output$probability_plot <- renderPlot({
    result <- predicted_probability()
    
    #Plotting
    plot <- ggplot(result, aes(x = Litigant_Type, y = Probability, fill = Litigant_Type)) +
      geom_col(position = "dodge", fill = c("grey", "skyblue")) +
      labs(x = "Litigant Type", y = "Predicted Probability", fill = "Litigant Type") +
      theme_minimal()+
      geom_text(aes(label = round(Probability, 3)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5)
  
    
    #Showing the plot
    print(plot)
  })
}

#Running the app
shinyApp(ui = ui, server = server)
