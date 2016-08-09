# Addin for bi-variate analysis

bivariate <- function(){
  
  require("shiny")
  require("miniUI")
  
  ui <- miniPage(
    gadgetTitleBar("Bi-variate analysis"),
    miniContentPanel(
      fluidRow(
        fluidRow(align = "center",
          selectInput("dat", "Select data frame", 
                      choices = ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) class(get(x))) == 'data.frame'], selected = NULL)
        ),
        fillRow(align = "center",
          column(6, selectInput("var1", "Select variable 1", choices = NULL, width = "50%")),
          column(6, selectInput("var2", "Select variable 2", choices = NULL, width = "50%"))
        ),
        uiOutput("analysis")
      )
    )
  )
  
  server <- function(input, output, session){
    
    values <- reactiveValues()
    values$df <- data.frame()
    
    observeEvent(input$dat, {
      values$df <- get(input$dat, envir = .GlobalEnv)
      updateSelectInput(session, "var1", "Select variable 1", choices = names(values$df))
      updateSelectInput(session, "var2", "Select variable 2", choices = names(values$df))
    })
    
    output$analysis <- renderUI({
      
      dat <- values$df
      var1 <- input$var1
      var2 <- input$var2
      type1 <- class(dat[,var1])
      type2 <- class(dat[,var2])
      
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  
runGadget(ui, server, viewer = dialogViewer("Bi-variate analysis"), stopOnCancel = TRUE)
  
}