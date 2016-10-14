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

server <- function(input, output, session) {
  
  
  oldSort <- isolate(input$sort)
  
  selectedData <- reactive({
    data <- data %>% filter(Title %in% input$flt)
    data$yaxis <- 
      switch(input$yaxis,
             Repos = data$Repos,
             Stars = data$Stars,
             Followers = data$Followers,
             Following = data$Following,
             Contributions = data$Contributions)
    #data <- data %>% arrange(yaxis)
    data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
    # if (!is.null(input$plt_hover$x)) {
    #   selected <- round(input$plt_hover$x)
    #   data[selected, "Color"] <- "#ddd"
    # }
    data
  })
  
  sortedData <- reactive({
    data <- selectedData()
    invalidateLater(100, session)
    
    if (isolate(input$sort) != oldSort) {
      oldSort <<- isolate(input$sort)
      data <- data %>% arrange(yaxis)
      data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
      if (!is.null(input$plt_hover$x)) {
        selected <- round(input$plt_hover$x)
        data[selected, "Color"] <- "#ddd"
      }
    } else {
      if (!is.null(input$plt_hover$x)) {
        selected <- round(input$plt_hover$x)
        data[selected, "Color"] <- "#ddd"
      }
    }
    # data <- isolate(selectedData())
    # data <- data %>% arrange(yaxis)
    # data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
    # if (!is.null(input$plt_hover$x)) {
    #   selected <- round(input$plt_hover$x)
    #   data[selected, "Color"] <- "#ddd"
    # }
    data
  })
  
  # hover <- {
  #   data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
  #   if (!is.null(input$plt_hover$x)) {
  #     selected <- round(input$plt_hover$x)
  #     data[selected, "Color"] <- "#ddd"
  #   }
  # }
  # 
  # observeEvent(input$sort, {
  #   data <- data %>% arrange(selectedData()$yaxis)
  #   data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
  # })
  
  output$plt <- renderPlot({
    # print(selectedData()$GitHubUsername)
    # print(as.numeric(selectedData()$GitHubUsername))
    ggplot(sortedData(), aes(x = GitHubUsername, y = yaxis, fill = Color)) + 
      geom_bar(stat="identity")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
