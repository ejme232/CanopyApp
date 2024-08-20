#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("app_functions.R")

options(readr.show_col_types = FALSE)

treelist=c("NL","NM","RD","SL","SM")

StationList=c("UC","MC","GR")

variables=c("Solar","Wetness.Level.1","Wetness.Level.2","Air.TempC","VPD","Atmos.Pressure","Vapor.Pressure")

format="%m/%d/%Y %H:%M" #FIX THIS

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Robinson data portal"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("daterange", 
                      label = h4("Select date range"), 
                      min = ymd("2024-03-30"), 
                      max = as_date(Sys.time()), 
                      value = c(ymd("2024-03-30"), Sys.time())),
          fluidRow(radioButtons("fixed.y", inline = T,
                                label = h4("Fix y-axis?"),
                                choices = c("No", "Coarse"),
                                selected = "No")),
          fluidRow(sliderInput("y.coarse",
                               label = h4("Select coarse y-axis range"),
                               min = -100,
                               max = 1500,
                               value = c(0, 1500))),
        
          column(3,
            fluidRow(
              radioButtons("Tree",
                                 label = h4("Select Tree"),
                                 choices = treelist,
                                 selected = 'SM')
            ),
            fluidRow(
              checkboxGroupInput("Station",
                                 label = h4("Select station"),
                                 choices = StationList,
                                 selected = 'UC')
            )
          ),
          
          column(3,offset=1,
            fluidRow(
              radioButtons("Variable",
                           label = h4("Select variable"),
                           choices = variables,
                           selected = "Air.TempC")
            )
          ),
          
          column(3, offset = 1,
                 radioButtons("Time.res",
                              label = h4("Select time resolution"),
                              choices = c("15 Min", "Hourly", "Daily", "Weekly"),
                              selected = "15 Min")
                 ),
          titlePanel("Download options"),
          fluidRow(
            column(5,
                   radioButtons("Time.format",
                                label = h4("Select time format"),
                                choices = list("ISO", "Excel_ready"),
                                selected = "ISO")),
            column(5,
                   radioButtons("All.variables",
                                label = h4("Download all variables?"),
                                choices = list("All", "Only_selected"),
                                selected = "Only_selected"))
          ),
          downloadButton("downloadData", "Download")
        ), #end sidebar
        
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Plot",
                     "Graph shows data until:",
                     verbatimTextOutput("max.date.out"),
                     plotOutput("plot",
                                click = "plot_click",
                                brush = "plot_brush"),
                     verbatimTextOutput("plot_clickinfo"),
                     plotOutput("plot_brushedpoints")),
            tabPanel("Data", tableOutput("data1"), tableOutput("data2")))
          )
    )
)

## Define server logic -------------

server <- function(input, output) {

  dataInput <- reactive({
    read_csv(file.path("Microclimate_data_L2", str_c(input$Tree, "_L2.csv"))) %>% 
      filter(Timestamp >= as.POSIXct(input$daterange[1], tz = "UTC", format)  &
               Timestamp <= as.POSIXct(input$daterange[2], tz = "UTC", format))
  })
  
  dataInput2 <- reactive({
    dataInput() %>% 
      filter(Station %in% input$Station) %>%
      pivot_longer(4:ncol(dataInput()), 
                   names_to = "Variable", values_to = "Measure") %>% 
      filter(Variable == input$Variable)
  })
  
  dataInput3 <- reactive({
    if(input$Time.res == "15 Min"){
      dataInput2()
    } else if(input$Time.res == "Hourly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "hour"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()
    } else if(input$Time.res == "Daily"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "day"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()
    } else if(input$Time.res == "Weekly"){
      dataInput2() %>% 
        group_by(Tree, Station, Timestamp = floor_date(Timestamp, "week"), Variable) %>% 
        summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>% 
        ungroup()}
  })

## Plots -------------------------

  output$plot <- renderPlot({
    p <- ggplot(dataInput3()) +
      geom_line(aes(x = Timestamp, y = Measure, color = Station)) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) +
      ggtitle(str_c(input$Tree, "_", input$Variable))
    if(input$fixed.y == "Coarse"){
      p <- p +
        ylim(input$y.coarse[1], input$y.coarse[2])
    }
    if(input$fixed.y == "Fine"){
      p <- p +
        ylim(input$y.fine.min, input$y.fine.min + input$y.fine.range)
    }
    p
  }) 
  
  output$plot_clickinfo <- renderPrint({
    val <- nearPoints(dataInput3(), input$plot_click, maxpoints = 1)
    unique(val$Timestamp)
  })
  
  output$plot_brushedpoints <- renderPlot({
    dat <- brushedPoints(dataInput3(), input$plot_brush)
    if (nrow(dat) == 0)
      return()
    ggplot(dat) +
      geom_line(aes(x = Timestamp, y = Measure)) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 24),
            axis.text.y = element_text(size = 20),
            plot.title = element_text(size = 24)) 
    ggtitle(str_c(input$Tree, "_", input$Variable))
    if(input$fixed.y == "Coarse"){
      p <- p +
        ylim(input$y.coarse[1], input$y.coarse[2])
    }
    p
  })

## Data and tables -------------------------------------------------------
  
  data.for.summaries <- reactive({
    dataInput3() %>% 
      mutate(Timestamp = as.character(Timestamp)) 
  })
  
  output$data1 <- renderTable({
    head(data.for.summaries())
  })
  
  output$data2 <- renderTable({
    tail(data.for.summaries())
  })
  
  output$summary <- renderTable({
    summarise_MC(data.for.summaries())
  })
  
## Download options --------------------------------------------------------

  data.for.download <- reactive({
    if(input$All.variables == "All"){
      dataInput()
    } else {
      dataInput3()
    } 
  })
  
  data.for.download2 <- reactive({
    if(input$Time.format == "Excel_ready"){
      data.for.download() %>% 
        mutate(Timestamp = as.character(Timestamp))
    } else {
      data.for.download()
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      str_c(str_c(input$Tree,
                  str_sub(as.character(input$daterange[1]), start = 1, end = 10), 
                  str_sub(as.character(input$daterange[2]), start = 1, end = 10), 
                  sep = "_"),
            ".csv")
    },
    content <-  function(file) {
      write_csv(data.for.download2(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

# Test server -------------------------------------------------------------

testServer(server, {
  session$setInputs(Tree = "SM")
  session$setInputs(Station =c("MC"))
  session$setInputs(Variable = "Solar")
  session$setInputs(daterange = c(min = ymd("2024-04-01"),
                              max = ymd("2024-04-02")))
  session$setInputs(Time.res = "15 min")
  test <<- print(dataInput3())
})
