library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(benford.analysis)

state_ts <- read_csv("State_clean.csv")

appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  title = "Zillow Housing Price Trend",
  
  div(id = "header",
      h1("Zillow Housing Price Trend"),
      h4("This app is a supplement to my",
         a(href = "https://github.com/cqyin/MA615_FinalProj",
           "Final Project of MA615")
      ),
      strong( 
        span("Created by "),
        a("Chaoqun Yin", href = "https://github.com/cqyin"),
        HTML("&bull;"),
        span("Code"),
        a("on GitHub", href = "https://github.com/cqyin/MA615_FinalProj/blob/master/app.R"),
        HTML("&bull;"))
  ),
  
  sidebarLayout(
    sidebarPanel("An interactive app to help visualize the Zillow housing prices in America.",
                 br(),
                 
                 sliderInput("yearInput", "Years",
                             min = 2000,
                             max = 2017,
                             value = c(2000,2017),
                             sep = ""),
                 
                 sliderInput("priceInput", "Price range",
                             min = 30000,
                             max = 1500000, 
                             value = c(50000,1500000 ),
                             pre = "$",
                             sep = ",",
                             step = 20000),
                 
                 uiOutput("stateOutput"),
                 
                 checkboxInput("allInput", "Include all states", value = FALSE),
                 
                 checkboxGroupInput("typeInput", "Home types",
                                    choices = c("1 bedroom" = "1bedroom", 
                                                "2 bedroom" = "2bedroom", 
                                                "3 bedroom" = "3bedroom", 
                                                "4 bedroom" = "4bedroom", 
                                                "5 bedroom or more" = "5BedroomOrMore", 
                                                "Condo and co-operatives" = "CondoCoop", 
                                                "House" = "SingleFamilyResidence"),
                                    selected = c("1bedroom", "2bedroom",  "CondoCoop", "SingleFamilyResidence")
                 ),         
                 
                 
                 checkboxGroupInput("tierInput", "Tier types",
                                    choices = c("Top Tier" = "TopTier", "Middle Tier" = "MiddleTier", "Bottom Tier" = "BottomTier"),
                                    selected = c("TopTier", "MiddleTier")
                 )
                 
                 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", 
                 br(),
                 
                 downloadButton("graphhome", "Download the plot (Home types)"),
                 plotOutput("homePlot"),
                 br(),
                 downloadButton("graphtier", "Download the plot (Tier types)"),
                 br(),
                 plotOutput("tierPlot")
                 
        ),
        
        tabPanel("Data Table",
                 br(),
                 downloadButton("downloadData", "Download the data"),
                 br(),
                 DT::dataTableOutput("data")
                 ),
        
        tabPanel("Benford's Law",
                 plotOutput("bfdPlot")
                 )
        
      )
    )
  )
)

# Sever
server <- function(input, output, session) {
  observeEvent(
    input$allInput,
    updateSelectInput(session,
                      inputId = "stateInput",
                      'State',
                      choices = sort(unique(state_ts$region)),
                      selected = c(unique(state_ts$region))),
    ignoreInit = T
  )
  
  output$stateOutput <- renderUI({
    selectInput("stateInput", "State",
                sort(unique(state_ts$region)),
                selected = c("NewYork", "Michigan", "California", "Ohio", "Texas", "Washington") ,
                multiple = FALSE)
  })  
  
  filtered <- reactive({
    if (input$allInput){
      state_ts %>% 
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               price >= input$priceInput[1],
               price <= input$priceInput[2])
    } else {
      state_ts %>% 
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               price >= input$priceInput[1],
               price <= input$priceInput[2],
               region %in% input$stateInput)
    }
  })
  
  filteredtype <- reactive({
    filtered() %>% filter(type %in% input$typeInput)
    
    
  })
  
  filteredtier <- reactive({
    filtered() %>% filter(type %in% input$tierInput)
    
  })
  
  
  
  plottierInput <- reactive({
    p <-  
      ggplot(filteredtier(), aes( x = year, y = price, fill = type)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format()) +
      xlab("Year") +
      ylab("Price - median estimated home value") +
      ggtitle("Zillow's median estimated home value across different year", 
              subtitle = "By Tier types") +
      scale_fill_discrete("") +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.text.y = element_text(size = 12, angle = 45 ),
            axis.text.x = element_text(size = 12, angle = 90 ),
            axis.title = element_text(size = 14),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            legend.text = element_text(size = 10, angle = 30),
            strip.text = element_text(size = 13))
    
  })
  
  plottypeInput <- reactive({
    p <- 
      ggplot(filteredtype(),  aes( x = year, y = price, fill = type)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format()) +
      xlab("Year") +
      ylab("Price - median estimated home value") +
      ggtitle("Zillow's median estimated home value across different year",
              subtitle = "By Home types") +
      scale_fill_discrete("") +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.text.y = element_text(size = 12, angle = 45 ),
            axis.text.x = element_text(size = 12, angle = 90 ),
            axis.title = element_text(size = 14),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            legend.text = element_text(size = 10, angle = 30),
            strip.text = element_text(size = 13))
 
  })
  
  
  
  output$tierPlot <- renderPlot({
    print(plottierInput())
  })
  
  output$homePlot <- renderPlot({
    print(plottypeInput())
  })
  
  output$data <- DT::renderDataTable({
    filtered()
  })
  
  output$bfdPlot <- renderPlot({
    bfd <- benford(state_ts$price)
    plot(bfd)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = "house.csv"
    ,
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
  output$graphtier <- downloadHandler(
    filename = "graph.png"
    ,
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
      ggsave(filename = file, plot = plottierInput(), device = device)
    }
  )
  
  output$graphhome <- downloadHandler(
    filename = "graph.png"
    ,
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 150, units = "in")
      ggsave(filename = file, plot = plottypeInput(), device = device)
    }
  )
  
  
  
}

# Run
shinyApp(ui = ui, server = server)
