
data$GitHubUsername <- as.factor(data$GitHubUsername)

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
  actionButton("sort", "Sort"),
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
    data %>% arrange(yaxis)
  })
  
  yaxis <- reactive({
    switch(isolate(input$yaxis),
           Repos = selectedData()$Repos,
           Stars = selectedData()$Stars,
           Followers = selectedData()$Followers,
           Following = selectedData()$Following,
           Contributions = selectedData()$Contributions)
  })
  
  xaxis <- reactive({
    input$sort
    factor(selectedData()$GitHubUsername, 
           levels = selectedData()$GitHubUsername[order(isolate(yaxis()))])
  })
  
  color <- reactive({
    rep("#9d2", nrow(selectedData()))
  })
  
  output$plt <- renderPlot({
    colors <- color()
    if (!is.null(input$plt_hover$x)) {
      selected <- round(input$plt_hover$x)
      print(as.numeric(xaxis()))
      print((xaxis()))
      print((xaxis()[selected]))
      colors[xaxis()[selected]] <- "#ddd"
    }
    print(colors)
    ggplot(selectedData(), aes(x = xaxis(), y = yaxis(), fill = colors)) + 
      geom_bar(stat="identity")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
