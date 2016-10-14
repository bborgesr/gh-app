
data$GitHubUsername <- as.factor(data$GitHubUsername)
data$Color <-  rep("#9d2", nrow(data))

# Define UI
ui <- fluidPage(
  selectInput("flt", "Filter by title:", 
              choices = unique(rstudio$Title), 
              selected = c("Software Engineer", "Founder", "CTO", 
                           "Master Instructor", "Chief Scientist"),
              multiple = TRUE),
  selectInput("yaxis", "Select the y-axis", 
              choices = c("Repos", "Stars", "Followers", "Following", "Contributions"),
              selected = "Contributions"),
  plotOutput("plt", hover = hoverOpts(id = "plt_hover"))
)

# Server logic
server <- function(input, output, session) {
  
  selectedData <- reactive({
    data <- data %>% filter(Title %in% input$flt)
    yaxis <- 
      switch(isolate(input$yaxis),
             Repos = data$Repos,
             Stars = data$Stars,
             Followers = data$Followers,
             Following = data$Following,
             Contributions = data$Contributions)
    if (!is.null(input$plt_hover$x)) {
      selected <- round(input$plt_hover$x)
      data[selected, Color] <- "#ddd"
    }
    data %>% arrange(yaxis)
  })
  
  yaxis <- reactive({
    switch(input$yaxis,
           Repos = selectedData()$Repos,
           Stars = selectedData()$Stars,
           Followers = selectedData()$Followers,
           Following = selectedData()$Following,
           Contributions = selectedData()$Contributions)
  })
  
  output$plt <- renderPlot({
    ggplot(selectedData(), aes(x = GitHubUsername, y = yaxis(), fill = Color)) + 
      geom_bar(stat="identity")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
