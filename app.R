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
  actionButton("unslct", "Unselect"),
  plotOutput("plt", hover = hoverOpts(id = "plt_hover", delay = 150)),
  htmlOutput("info")
)

server <- function(input, output, session) {
  
  oldSort <- new.env()
  oldSort$n <- 0
  
  rv <- reactiveValues(
    data = data,
    selected = NULL
  )
  
  observeEvent(input$flt, {
    rv$data <- rv$data %>% filter(Title %in% input$flt)
    rv$selected <- NULL
  })
  
  observe({
    rv$data$yaxis <- switch(input$yaxis,
                            Repos = rv$data$Repos,
                            Stars = rv$data$Stars,
                            Followers = rv$data$Followers,
                            Following = rv$data$Following,
                            Contributions = rv$data$Contributions)
    if (input$sort != oldSort$n) {
      username <- rv$data$GitHubUsername[rv$selected]
      rv$data <- rv$data %>% arrange(yaxis)
      rv$selected <- which(rv$data$GitHubUsername == username)
      rv$data$GitHubUsername <- factor(rv$data$GitHubUsername, 
                                       levels = rv$data$GitHubUsername)
      oldSort$n <- input$sort
    }
    if (!is.null(input$plt_hover$x)) {
      rv$selected <- round(input$plt_hover$x)
    }
  })
  
  observeEvent(input$unslct, {
    rv$selected <- NULL
  })
  
  output$plt <- renderPlot({
    data <- rv$data
    data[rv$selected, "Color"] <- "#ddd"
    ggplot(data, aes(x = GitHubUsername, y = yaxis, fill = Color)) + 
      geom_bar(stat="identity")
  })
  
  output$info <- renderText({
    req(rv$selected)
    HTML(paste0(
      '<h1 class="page-header">',
        rv$data[rv$selected, "FirstName"], " ", rv$data[rv$selected, "LastName"], " ",
         '<small>', rv$data[rv$selected, "Title"], '</small>',
      '</h1>',
      '<div class="row">',
        '<div class="col-xs-3">',
        '<img class="img-responsive" 
              src="/photos/', rv$data[rv$selected, "Photo"], '.jpg" 
              alt="" height="200" width="200" style="inline">',
        '</div>',
        '<div class="col-xs-9">',
          '<div><strong>Github Info:</strong></div>',
            '<div>Number of Repos: ', rv$data[rv$selected, "Repos"],'</div>',
            '<div>Number of Stars: ', rv$data[rv$selected, "Stars"],'</div>',
            '<div>Number of Followers: ', rv$data[rv$selected, "Followers"],'</div>',
            '<div>Number of Followees: ', rv$data[rv$selected, "Following"],'</div>',
            '<div>Number of Contributions: ', rv$data[rv$selected, "Contributions"],'</div>',
          '</div>',
        '</div>'
    ))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
