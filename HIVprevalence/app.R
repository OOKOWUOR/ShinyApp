library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Read the CSV data
data <- read.csv("C:/Users/USER/Desktop/HIV/HIVdata.csv")

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "HIV Prevalence Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1", tabName = "tab1"),
      menuItem("Tab 2", tabName = "tab2"),
      selectInput("residenceFilter", "Filter by Residence",
                  choices = c("All", unique(data$residence))),
      selectInput("wealthFilter", "Filter by Wealth Index",
                  choices = c("All", unique(data$wealthind))),
      actionButton("refreshData", "Refresh Data")
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1
      tabItem(tabName = "tab1",
              fluidRow(
                valueBoxOutput("hivPrevalence"),
                plotOutput("hivBySexPie")
              )
      ),
      # Tab 2
      tabItem(tabName = "tab2",
              fluidRow(
                plotOutput("hivByMaritalStatus"),
                plotOutput("hivByAgeAtFirstSex")
              )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  data_filtered <- reactive({
    filtered_data <- data
    if (input$residenceFilter != "All") {
      filtered_data <- filtered_data[filtered_data$residence == input$residenceFilter, ]
    }
    if (input$wealthFilter != "All") {
      filtered_data <- filtered_data[filtered_data$wealthind == input$wealthFilter, ]
    }
    return(filtered_data)
  })
  
  # Tab 1 - Value Box for HIV Prevalence
  output$hivPrevalence <- renderValueBox({
    prevalence <- mean(data_filtered()$hivstatusfinal == "Positive")
    valueBox(
      value = sprintf("%.2f%%", prevalence * 100),
      subtitle = "HIV Prevalence",
      icon = icon("heartbeat")
    )
  })
  
  # Tab 1 - Pie Chart for HIV Status by Sex
  output$hivBySexPie <- renderPlot({
    hiv_by_sex <- data_filtered() %>%
      group_by(sex, hivstatusfinal) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ungroup()
    
    ggplot(hiv_by_sex, aes(x = "", y = count, fill = hivstatusfinal)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = "HIV Status by Sex")
  })
  
  # Tab 2 - Bar Graph for HIV Status by Marital Status
  output$hivByMaritalStatus <- renderPlot({
    hiv_by_marital_status <- data_filtered() %>%
      group_by(mstatus, hivstatusfinal) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ungroup()
    
    ggplot(hiv_by_marital_status, aes(x = mstatus, y = count, fill = hivstatusfinal)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "HIV Status by Marital Status")
  })
  
  # Tab 2 - Line Graph for Age at First Sex
  output$hivByAgeAtFirstSex <- renderPlot({
    data_filtered <- na.omit(data_filtered())  # Remove rows with NA values
    
    ggplot(data_filtered, aes(x = agefirstsex, group = hivstatusfinal, color = hivstatusfinal)) +
      geom_density() +
      labs(title = "HIV Prevalence by Age at First Sex")
  })
}

# Run the Shiny app
shinyApp(ui, server)
