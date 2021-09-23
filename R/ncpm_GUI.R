#' @import shiny dplyr ggplot2
NULL


#' NCPMGUI()
#'
#' @return GUI
#' @export
#' @description All GUI code
#'
#' @examples
#' NCPMGUI()
NCPMGUI <- function () {

  ##### UI part
  ui <- fluidPage(

    # CSS
    tags$head(
      tags$style(HTML("
                  .btn {
                    display: inline-block;
                    height: 50px;
                    width: 200px;
                    border: 1px solid green;
                    background-color: cyan;
                  }

                    "))
    ),
    titlePanel("Novice Cognitive Performance Model"),
    navlistPanel(id = "tabs",
                 tabPanel("Home",
                          fluidPage(
                            fluidRow(
                              column(12,
                                     fluidRow(
                                       column(6,
                                              # CHANGE
                                              img(src="new driver.PNG", height = 208, width = 312)),
                                       column(6)
                                     )
                              )
                            ),
                            fluidRow(
                              column(6,"Model Overview:
    The purpose of this human performance model is to
    estimate the closest task time and
    workload to a naturalistic setting possible for
    novices. Click on the help tab or upload a file to get started."),
                   #Junho - 0827 - goal
                   column(6,fileInput("dataset2", "Choose CSV File",
                   # column(6,fileInput("dataset", "Choose CSV File", # This is for testing data. We can switch this to the build a scenario part at the end.
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                   )))
               )
      ),
      tabPanel("Develop a Scenario", # needs more cleaning up

               # Junho - 0827 - goal
               radioButtons('goal','Goal statement', choices = c('Goal'), selected = character(0)),

               radioButtons('cognitive','Cognitive Operators', choices = c('Look','Read','Search', 'Saccade','Hear'), selected = character(0)),
               radioButtons('perceptual','Perceptual Operators', choices = c('Attend','Initiate','Ignore','Mental', 'Recall','Store','Think','Verify'), selected = character(0)),
               radioButtons('motor','Motor Operators', choices = c('Drag','Grasp','Hands','Keystroke','Point','Swipe','Tap','Touch','Turn','Type','Write','Say','Wait','Reach','Flick','Zoomin','Zoomout'), selected = character(0)),
               radioButtons('chunk', 'Chunks', choices = c('Plate Number','Street name','Road Name','Call sign', 'custom'), selected = character(0)),
               textOutput("result"),
               textOutput("Description"),
               textOutput("chunkresult"),
               # the next part will only show up if custom chunk is chosen
               uiOutput("customtext"),
               uiOutput("custombutton"),
               # end
               textInput("desc","Describe the use of the operator"),
               checkboxInput("parallel","Parallel?", FALSE),
               actionButton("add","Add new line to Code"),
               actionButton("same","Add to current line"),
               actionButton("reset","Remove Selections"),
               actionButton("undo","Remove last line of code"), #make more robust
               tableOutput("Code"), # should put this to the right of inputs deal with aesthetic last

               downloadButton("downloadData", "Download"),
               actionButton("dataset","Work Done")
               # actionButton("complete_code","Work Done")
      ),
      tabPanel("See Novice vs. Expert",
               "The novice performance can be compared to",
               "the expert performance here. Click a tab to",
               "view a graph.",
               tabsetPanel(
                 tabPanel("Task completion time", tableOutput("Table_tct")),
                 tabPanel("Memory chunks", tableOutput("Table_mem")),
                 tabPanel("Number of operators", tableOutput("Table_oper"))
               )

      ),
      tabPanel("Results Summary", # this is the "all" button
               sidebarLayout(
                 sidebarPanel(
                   "Total Completion Time:",
                   textOutput("Time"),
                   "Memory Load:",
                   textOutput("Load"),
                   "Perceptual Operators:",
                   textOutput("Perc"),
                   "Cognitive Operators:",
                   textOutput("Cog"),
                   "Motor Operators:",
                   textOutput("Motor"),

                 ),
                 mainPanel(h2("Chunk Results Summary"),
                           tableOutput("contents"))
               )


      ),
      tabPanel("Help", "Click on a tab to get some help with that tab.",
               tabsetPanel(
                 tabPanel("Develop a Scenario", "The help page for developing scenarios goes here."),
                 tabPanel("Novice vs. Expert", "The help page for Novice vs. Expert goes here."),
                 tabPanel("Results Summary", "The help page for Results summary goes here.")
               ))


    )
  )

  ###################################
  # SERVER

  #' Shiny Server
  #'
  #' @param input input
  #' @param output output
  #' @param session session
  #'
  #' @return contents for ui
  #' @export
  #'
  #' @examples
  #' no example
  server = shinyServer(function(input, output,session) {

    #####################
    ### RESULTS SUMMARY - Novice

    # Task completion time
    timeInput <- reactive({
      #Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data <- NCPMcalc(data)
      data[[2]]
    })
    output$Time <- renderText({
      timeInput()
    })

    # Memory chunks
    loadInput <- reactive({
      #Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data <- NCPMcalc(data)
      data[[4]]

    })
    output$Load <- renderText({
      loadInput()
    })

    # Number of operators
    opInput <- reactive({
      #Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data <- NCPMcalc(data)
      data[[5]]
    })
    output$Perc <- renderText({
      opInput()[[1]]
    })
    output$Cog <- renderText({
      opInput()[[2]]
    })
    output$Motor <- renderText({
      opInput()[[3]]
    })

    # Chunk structure
    datasetInput <- reactive({
      #Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data <- NCPMcalc(data)
      data <- data[[1]]
      data <- dplyr::select(data, -stack_depth, -activation )
      data
    })
    output$contents <- renderTable({
      datasetInput()
    })


    ### NOT USED ###################### RESULTS SUMMARY - Expert <NOT USED>

    # Task completion time
    timeInput_exp <- reactive({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      data <- NCPMcalc_exp(data)
      data[[2]]
    })
    output$Time_exp <- renderText({
      timeInput_exp()
    })

    # Memory chunks
    loadInput_exp <- reactive({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      data <- NCPMcalc_exp(data)
      data[[4]]

    })
    output$Load_exp <- renderText({
      loadInput_exp()
    })

    # Number of operators
    opInput_exp <- reactive({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      data <- NCPMcalc_exp(data)
      data[[5]]
    })
    output$Perc_exp <- renderText({
      opInput_exp()[[1]]
    })
    output$Cog_exp <- renderText({
      opInput_exp()[[2]]
    })
    output$Motor_exp <- renderText({
      opInput_exp()[[3]]
    })


    # Chunk structure
    datasetInput_exp <- reactive({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      data <- NCPMcalc_exp(data)
      data <- data[[1]]
      data <- dplyr::select(data, -stack_depth, -activation )
      data
    })
    output$contents_exp <- renderTable({
      datasetInput_exp()
    })


    ############################
    # Nov vs. Exp - TCT - DATA TABLE
    output$Table_tct <- renderTable({
      # Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #  return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)
      # print(data)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data_nov <- NCPMcalc(data)
      data_nov[[2]]
      data_exp <- NCPMcalc_exp(data)
      data_exp[[2]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[2]]
      tableTCT[1,2] <- data_exp[[2]]
      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Task completion time (seconds)")

      tableTCT
    }, rownames = TRUE)

    # Nov vs. Exp - MEM - DATA TABLE
    output$Table_mem <- renderTable({
      # Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)
      # print(data)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data_nov <- NCPMcalc(data)
      data_nov[[4]]
      data_exp <- NCPMcalc_exp(data)
      data_exp[[4]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[4]]
      tableTCT[1,2] <- data_exp[[4]]
      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Memory chunks (count)")

      tableTCT
    }, rownames = TRUE)

    # Nov vs. Exp - Oper - DATA TABLE
    output$Table_oper <- renderTable({
      # Junho - 0827
      # inFile <- input$dataset
      #
      # if(is.null(inFile))
      #   return(NULL)
      #
      # data <- read.csv(inFile$datapath, header = TRUE)

      # data<-as.data.frame(v$code, header=FALSE)
      # print(data)

      inFile <- input$dataset2

      if(is.null(inFile))
        data<-as.data.frame(v$code, header=FALSE)
      else
        data <- read.csv(inFile$datapath, header = TRUE)

      data_nov <- NCPMcalc(data)
      data_nov[[5]]
      data_exp <- NCPMcalc_exp(data)
      data_exp[[5]]

      a<-c()
      b<-c()

      tableTCT <- data.frame(a, b)

      tableTCT[1,1] <- data_nov[[5]][1]
      tableTCT[1,2] <- data_exp[[5]][1]
      tableTCT[2,1] <- data_nov[[5]][2]
      tableTCT[2,2] <- data_exp[[5]][2]
      tableTCT[3,1] <- data_nov[[5]][3]
      tableTCT[3,2] <- data_exp[[5]][3]

      colnames(tableTCT)[1]<-"Novice"
      colnames(tableTCT)[2]<-"Expert"
      rownames(tableTCT) <- c("Perceptual (count)", "Cognitive (count)", "Motor (count)")
      tableTCT

    }, rownames=TRUE)


    ### <NOT USED> ########################################################
    # Plotting Nov vs. Exp <NOT USED>

    output$Plot_tct <- renderPlot ({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      # data <- read.csv(file="./data/task_1.csv", header=FALSE)
      data_nov <- NCPMcalc(data)
      data_nov[[2]]
      data_exp <- NCPMcalc_exp(data)
      data_exp[[2]]

      # Create the data for bar chart
      TCT <- c(data_nov[[2]], data_exp[[2]])
      xnames <- c("Novice", "Expert")

      barplot(TCT, names.arg=xnames, xlab="Skill level", ylab="Time (seconds)"
              , col="blue", main="Task completion time", border="black"
              , ylim = c(0, 50))

    })

    # Plotting Nov vs. Exp - Memory chunks
    output$Plot_mem <- renderPlot ({
      inFile <- input$dataset

      if(is.null(inFile))
        return(NULL)

      data <- read.csv(inFile$datapath, header = TRUE)
      # data <- read.csv(file="./data/task_1.csv", header=FALSE)
      data_nov <- NCPMcalc(data)
      data_nov[[4]]
      data_exp <- NCPMcalc_exp(data)
      data_exp[[4]]

      # Create the data for bar chart
      mem <- c(data_nov[[4]], data_exp[[4]])
      xnames <- c("Novice", "Expert")

      barplot(mem, names.arg=xnames, xlab="Skill level", ylab="Numbers (counts)"
              , col="Red", main="Memory chunks", border="black"
              , ylim = c(0, 7))

    })

    ############################
    # DEVELOP A SCENARIO
    v <- reactiveValues(current_selection = "", parallel = "", chunk = "", code = "")

    observeEvent(input$parallel,{
      if(input$parallel)
      {
        v$parallel <- "Also: "
      }
      else
      {
        v$parallel <- ""
      }
    })

    observeEvent(input$chunk,{
      v$chunk <- paste("<",input$chunk,">", sep = "")
      output$chunkresult <- renderText({
        paste("Your chunk is ", input$chunk)
      })
      if (input$chunk == 'custom')
      {
        output$customtext <- renderUI({ textInput("customtext", "Input the chunk:")})
        output$custombutton <- renderUI ({
          actionButton("confirm", label = "Confirm Custom Chunk Input")
        })
      }
      else
      {
        output$customtext <- renderUI({NULL})
        output$custombutton <- renderUI({NULL})
      }

    })

    # Junho-0827 - goal
    observeEvent(input$goal,{
      v$current_selection <- input$goal
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
    })

    observeEvent(input$cognitive,{
      v$current_selection <- input$cognitive
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
    })
    observeEvent(input$perceptual,{
      v$current_selection <- input$perceptual
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
    })
    observeEvent(input$motor,{
      v$current_selection <- input$motor
      output$result <- renderText({
        paste("You chose ", v$current_selection)
      })
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
    })

    observeEvent(input$confirm,{
      v$chunk <- paste("<",input$customtext,">", sep = "")
      output$chunkresult <- renderText({
        paste("You chose ", input$customtext)
      })

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("code.csv", sep = "")
      },
      content = function(file) {
        write.csv(codelines$df, file, row.names = FALSE)
      }
    )
    observeEvent(input$custom,{
      output$Description <- renderText({ paste("Descriptions go here")})
      #need to insert a very long if statement here, get it all from Junho.
    }
    )

    # syntax depends on Junho.
    codelines <- reactiveValues()
    codelines$df <- data.frame(Code = numeric(0))
    newEntry <- observe({
      if(input$add > 0) {
        # Junho - 0827 - goal
        updateRadioButtons(session,
                           'goal',
                           selected = character(0))
        updateRadioButtons(session,
                           'perceptual',
                           selected = character(0))
        updateRadioButtons(session,
                           'cognitive',
                           selected = character(0))
        updateRadioButtons(session,
                           'motor',
                           selected = character(0))
        updateRadioButtons(session,
                           'chunk',
                           selected = character(0))
        newLine <- isolate(c(input$desc))
        isolate(codelines$df[nrow(codelines$df)+1,] <- c(paste(v$parallel,v$current_selection,v$chunk,input$desc))) # this is where you put the code in
        v$current_selection <- c("")
        v$chunk <- c("")
      }
    })

    # Junho - 0827
    observeEvent(input$dataset,{
      v$code <- codelines$df
    })

    # This just blanks the line. Much simpler than removing line outright.
    # There's a couple ways to make this better. For now it's just the blanking.
    observeEvent(input$undo,{
      newLine <- isolate(c(input$desc))
      isolate(codelines$df[nrow(codelines$df),] <- c(""))
    })

    observeEvent(input$same,{
      #Junho - 0827 - goal
      updateRadioButtons(session,
                         'goal',
                         selected = character(0))
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'chunk',
                         selected = character(0))
      newLine <- isolate(c(input$desc))
      isolate(codelines$df[nrow(codelines$df),] <- c(paste(codelines$df[nrow(codelines$df),],v$parallel,v$current_selection,v$chunk,input$desc)))
      v$current_selection <- c("")
      v$chunk <- c("")
    })


    observeEvent(input$reset,{
      #Junho - 0827 - goal
      updateRadioButtons(session,
                         'goal',
                         selected = character(0))
      updateRadioButtons(session,
                         'perceptual',
                         selected = character(0))
      updateRadioButtons(session,
                         'cognitive',
                         selected = character(0))
      updateRadioButtons(session,
                         'motor',
                         selected = character(0))
      updateRadioButtons(session,
                         'chunk',
                         selected = character(0))
      v$current_selection <- c("")
      v$chunk <- c("")

    })

    output$Code <- renderTable({codelines$df})

    ## this is the variable Junho wanted
    v$code <- function(file) {
      write.csv(codelines$df, file, row.names = FALSE)
    }

    ###########################
    # NOVICE VS EXPERT

    output$time <- renderPlot({ # In novice vs expert
      cars2 <- cars + rnorm(nrow(cars))
      plot(cars2)
    })
    output$bar <- renderPlot({ # in novice vs expert
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = 5)
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })


    output$stats = DT::renderDataTable({ #Doesn't work when output$memt is up for some reason
      mtcars
    })


  })

  shinyApp(ui, server)
}


#' NCPMcalc
#'
#' @param scenario scenario file
#'
#' @description Runs N-CPM model
#' @return all outcome
#' @export
#'
#' @examples
#' NCPMcalc(sce, "Novice")
NCPMcalc <- function(scenario){

  ans = RunMain(scenario, "Novice")

  return(ans)
}

#' NCPMcalc_expert
#'
#' @param scenario scenario file
#'
#' @description Runs N-CPM model in an expert's performance
#' @return all outcome - expert
#' @export
#'
#' @examples
#' NCPMcalc_exp(sce, "Expert")
NCPMcalc_exp <- function(scenario){

  ans = RunMain(scenario, "Expert")

  return(ans)
}
