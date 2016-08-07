
launch_km <- function(){
  
  require("shiny")
  require("miniUI")
  require("cluster")
  require("clValid")
  require("plotly")
  
  ui <- miniPage(
    gadgetTitleBar("K-means clustering model and diagnostics"),
    miniTabstripPanel(selected = "Model building",
                      miniTabPanel("Model building",
                                   miniContentPanel(scrollable = TRUE,
                                                    fluidRow(align = "center",
                                                      HTML("<br>If the k-means model object is already avaialble in the global environment,
                                                           <br>head over to the next panel for model diagnostics<br>
                                                           <em>Load the input data and select the variables for model diagnostics as well</em><br><br>"),
                                                      fluidRow(column(2),
                                                        column(4,
                                                               selectizeInput("data","Select the input data frame or matrix",
                                                                              choices= c(unlist(ls(envir = .GlobalEnv))), selected = NULL)
                                                               ),
                                                        column(4,
                                                               selectizeInput("vars","Select the variables", choices = NULL, multiple = TRUE)
                                                               ),column(2)
                                                      ),br(),
                                                      uiOutput("options")
                                                    )
                                                    )
                                   ),
                      miniTabPanel("Model diagnostics",
                                   miniContentPanel(scrollable = TRUE,
                                                    fluidRow(
                                                      fluidRow(align = "center",
                                                               fluidRow(
                                                                 column(4),
                                                                 column(4,selectInput("model", "Select model object", choices = c("",unlist(ls(envir = .GlobalEnv))), selected = NULL, width = "50%")),
                                                                column(4)
                                                               ),br(),
                                                               actionButton("load", "Load model object")
                                                      ),
                                                      fluidRow( uiOutput("performance"))
                                                    )
                                                    ))
                      )
  )
  
  server <- function(input, output, session){
    
    values <- reactiveValues()
    values$df <- data.frame()
    values$label <- ""
    values$fit_obj <- list()
    values$dunn <- c()
    
    observeEvent(input$data,{
      dat <- get(input$data, envir = .GlobalEnv)
      updateSelectizeInput(session,"vars","Select the variables", choices = names(dat))
      values$df <- dat
      values$label <- ""
    })
    
    observeEvent(input$load, {
      values$fit_obj <- get(input$model, envir = .GlobalEnv)
      values$dunn <- dunn(clusters = values$fit_obj[["cluster"]], Data = values$df)
    })
    
    output$options <- renderUI({
      fluidRow(
        column(1),
        column(10,
               tags$fieldset(tags$legend("Options"),
                             fluidRow(
                               column(1),
                               column(10,
                                      fluidRow(
                                        column(6,fluidRow(align = "right", HTML('Number of clusters, <em>k</em>'))),
                                        column(6,fluidRow(align = "left", textInput("centers", "", value = 3,placeholder = "3")))
                                      ),
                                      fluidRow(
                                        column(6,fluidRow(align = "right", HTML('The maximum number of iterations allowed'))),
                                        column(6,fluidRow(align = "left", numericInput("itermax","", value = 10, min = 1)))
                                      ),
                                      fluidRow(
                                        column(6,fluidRow(align = "right", HTML('if <em>centers</em> is a number, how many random sets should be chosen?'))),
                                        column(6,fluidRow(align = "left", numericInput("nstart", "", value = 1, min = 1)))
                                      ),
                                      fluidRow(
                                        column(6,fluidRow(align = "right", HTML("Give a name for the model object"))),
                                        column(6,fluidRow(align = "left", textInput("name", "", value = "model_km")))
                                      ),br(),
                                      fluidRow(align = "center",actionButton("build", "Build Model")),br(),
                                      HTML(values$label)
                                      ),
                               column(1)
                             ))
               ),
        column(1)
      )
    })
    
    output$performance <- renderUI({
      if (inherits(values$fit_obj, "kmeans")){
        uiOutput("kmModel")
      } else {
        fluidRow(align = "center", HTML("<br><br>Select and load a k-means clustering model object into Workspace.
                                        "))
      }
    })
    
    observeEvent(input$build,{
      dat <- values$df
      vars <- as.character(input$vars)
      centers <- as.numeric(strsplit(input$centers, " ")[[1]])
      itermax <- as.numeric(input$itermax)
      nstart <- as.numeric(input$nstart)
      name <- c(as.character(input$name))
      model <- kmeans(dat[,vars], centers = centers, iter.max = itermax, nstart = nstart)
      assign(name, model, envir = .GlobalEnv)
      values$label <- HTML(paste0('k-means model ',name,' built!'))
      updateSelectInput(session, "model", "Select model object", choices = c(name,unlist(ls(envir = .GlobalEnv))), selected = name)
      
    })
    
    output$kmModel <- renderUI({
      fluidRow(
        br(),br(),
        fluidRow(align = "center", HTML("<h3>Model Summary:</h3>")),
        br(),
        fluidRow(align="center",
                 column(1),
                 column(5,verbatimTextOutput("km_summary")),
                 column(5, uiOutput("components")),
                 column(1)),
        br(),
        fluidRow(align = "center",br(),
                 HTML("<h3>Clustering performance indices and plots:</h3>"),br(),
                 fluidRow(align = "center",
                          column(1),
                          column(5,uiOutput("km_plot")),
                          column(5,uiOutput("silhouette")),
                          column(1)
                 ),
                 br()
        )
      )
    })
    
    output$components <- renderUI({
      input$load
      fti <- values$fit_obj
      fluidRow(
        br(),br(),br(),br(),
        HTML("<h4> Total sum of squares: <b>",round(values$fit_obj[["totss"]], digits=3),"</b></h4>"), br(),br(),
        HTML("<h4> Total within-cluster sum of squares: <b>",round(values$fit_obj[["tot.withinss"]], digits = 3),"</b></h4>"), br(),br(),
        HTML("<h4> Between cluster sum of squares: <b>",round(values$fit_obj[["betweenss"]], digits = 3),"</b></h4>"), br(),br(),
        HTML("<h4> Number of iterations: <b>",values$fit_obj[["iter"]],"</b></h4>"), br(),br()
      )
    })
    
    output$km_summary <- renderPrint({
      fit <- values$fit_obj
      print(fit)
    })
    
    output$km_plot <- renderUI({
      fluidRow(
        HTML("<b><u>Visualize the clusters: two variable plot</u></b><br>"),br(),
        fluidRow(
          column(6, selectInput("x", "Select X:", choices = names(values$df), selected = NULL)),
          column(6, selectInput("y", "Select Y:", choices = names(values$df), selected = NULL))
        ),
        plotlyOutput("varplot", width = "90%")
      )
    })
    
    output$varplot <- renderPlotly({
      dat <- values$df
      fit <- values$fit_obj
      plt <- plot_ly(x = dat[,input$x], y = dat[,input$y], mode = "markers", color = factor(fit$cluster))
      plt %>% layout(xaxis = list(title = input$x), yaxis = list(title = input$y))
    })
    
    output$silhouette <- renderUI({
      fluidRow(
        fluidRow(align = "center",
                 HTML("<br><b><u>Silhouette Plot</u></b>")
        ), 
        fluidRow(align = "center",
                 plotOutput("silPlot", width = "90%")
        ),
        HTML(paste('<br><br><div align = "center"><b><u> Dunn Index: </u></b>', '<h4>', round(values$dunn, digits = 3), '</h4></div>'))
      )
    })
    
    output$silPlot <- renderPlot({
      fit <- values$fit_obj
      dat <- values$df
      len <- length(fit$size)
      dissE <- daisy(dat)
      plot(silhouette(fit$cluster, dissE), col = 1:len, main = "")
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = paneViewer(), stopOnCancel = TRUE)
  #dialogViewer("K-means")
}