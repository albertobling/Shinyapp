#SHINY APP CODE####
library(shiny)
library(dplyr)

##Data####
download.file(url = "https://github.com/albertobling/BLM---Logit/raw/fb017652e26ceb86b692e24ef6cab062741dac85/case_data.xlsx",
              destfile = "case_data2.xlsx")

case.data<-read_excel("case_data2.xlsx")
df<-as.data.frame(case.data)

#Recoding docket_type
df$docket_type1 = case_when(
  df$docket_type == "mandatory" ~ 0,
  df$docket_type == "mandatory principled" ~ 1,
  df$docket_type == "docket control" ~ 2)

#We choose to use litigant win as the DV, thus reversing government win 
df$litigant_win <- as.numeric(!df$government_win)
df$litigant_win

##Define your logistic regression model####
mod1 <- glm(litigant_win ~ as.factor(docket_type1) +
              individuals * as.factor(docket_type1) +
              individuals +
              government_appellant,
            data = df,
            family = binomial(link = "logit"))

##Creating the app####
# Define the UI
ui <- fluidPage(
  titlePanel("Litigant Win Probability"),
  sidebarLayout(
    sidebarPanel(
      # Add input element for selecting docket type
      selectInput("docket_type", "Docket Type", 
                  choices = c("Mandatory" = "0", "Mandatory Principled" = "1", "Discretionary" = "2"),
                  selected = "0")
    ),
    mainPanel(
      # Display the predicted probability graph
      plotOutput("probability_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Create a reactive function for prediction
  predicted_probability <- reactive({
    individuals <- 1
    corporation <- 1
    
    new_data <- data.frame(
      docket_type1 = as.factor(input$docket_type),
      individuals = individuals,
      corporation = corporation,
      government_appellant = 0  # Set to 0 for illustration purposes
    )
    
    # Predict the probability using the logistic regression model for individuals
    prob_individuals <- predict(mod1, newdata = new_data, type = "response")
    
    # Set corporation to 1 and individuals to 0 for prediction
    new_data$individuals <- 0
    new_data$corporation <- 1
    
    # Predict the probability using the logistic regression model for corporations
    prob_corporation <- predict(mod1, newdata = new_data, type = "response")
    
    # Create a data frame with the predicted probabilities
    result <- data.frame(
      Litigant_Type = c("Individuals", "Corporation/association"),
      Probability = c(prob_individuals, prob_corporation)
    )
    
    return(result)
  })
  
  # Render the predicted probability graph
  output$probability_plot <- renderPlot({
    result <- predicted_probability()
    
    # Create the plot
    plot <- ggplot(result, aes(x = Litigant_Type, y = Probability, fill = Litigant_Type)) +
      geom_col(position = "dodge", fill = c("grey", "skyblue")) +
      labs(x = "Litigant Type", y = "Predicted Probability", fill = "Litigant Type") +
      theme_minimal()+
      geom_text(aes(label = round(Probability, 3)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5)
    
    
    # Display the plot
    print(plot)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
