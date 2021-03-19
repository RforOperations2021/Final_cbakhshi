#importing required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(leaflet.extras)
library(tidyr)

#importing required data--------------------------------------------------------
earthquake.data = read.csv("query_2.csv")

#splitting time column into date and time columns-------------------------------
earthquake.data = earthquake.data%>%
    separate(time, c('date','time'), "T")

#splitting date column into year, month, and date columns-----------------------
earthquake.data = earthquake.data%>%
    separate(date, c('year','month', 'day'), "-")

# To avoid plotly issues -------------------------------------------------------
pdf(NULL)

# Application title ------------------------------------------------------------
header <- dashboardHeader(title = "Earthquakes (2020)", 
                          titleWidth = 250)

# Dashboard sidebar
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                                id = "tabs",
                                
                                # Menu Items -----------------------------------
                                menuItem(text ="Map", icon = icon("map")
                                         , tabName = "map-tab"),
                                
                                menuItem(text ="Plots", icon = icon("bar-chart")
                                         , tabName = "plots-tab")
                            )
)

body <- dashboardBody(
    tabItems(
        # Map page -------------------------------------------------------------
        tabItem("map-tab",
                
                h1("Earthquakes in the U.S. (2020)"),
                
                br(),
                
                h4("The magnitude and the color of the circles varies by magnitude of the earthquakes.",
                   "Hover your cursor over the circles to see the magnitude, depth, and the month of the earthquakes.",
                   "You can also select the 'Show Heatmap' option to add the heatmap layer to the map."),
                
                br(),
                
                # Map -----------------------------------------------------------
                fluidRow(
                    box(
                        # Select if heat map needed-----------------------------
                        checkboxInput("heatmap", "Show Heatmap", FALSE),
                        width = 200) 
                ),
                
                fluidRow(    
                    # Map Output----------------------------------------------
                    leafletOutput("mymap")
                    
                    
                )
                
        ),
        
        # Map page ----------------------------------------------
        tabItem("plots-tab",
                
                #Value Boxes ---------------------------------------------------
                fluidRow(valueBoxOutput("count"),
                         valueBoxOutput("avg.mag")),
                
                #plots tab box--------------------------------------------------
                fluidRow(
                    tabBox(title = "Plots",
                           width = 10,
                           tabPanel(strong("Total Earthquakes by Month"), plotlyOutput("plot1")),
                           tabPanel(strong("Magnitude of Earthquakes"), plotlyOutput("plot2"))),
                    
                    box(
                        # Select maximum mag for plots-----------------------
                        sliderInput(inputId = "max.mag",
                                    label = "Select Maximum Magnitude:",
                                    min = 3, max = 6.5, step = 0.5,
                                    value = 6.5),
                        br(),
                        
                        # Show data table -----------------------------------
                        checkboxInput(inputId = "show_data1",
                                      label = "Show data table",
                                      value = TRUE),
                        
                        width = 2)),
                
                fluidRow(
                    #Download button -----------------------------------------------
                    downloadButton(
                        outputId = "downloadbutton",
                        label = "Download",
                        class = NULL,
                        icon = shiny::icon("download")),
                    
                    # Box with data table 
                    box(DT::dataTableOutput(outputId = "datatable1"), width = 12)
                )
                
        )
        
    ))


# Define UI for application 
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #define the color palate for the magnitude of the earthquakes
    mag.pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = earthquake.data$mag)
    
    #creating the map
    output$mymap <- renderLeaflet({
        leaflet(earthquake.data) %>% 
            setView(lng = -95, lat = 37, zoom =3.5)  %>% 
            addTiles() %>% 
            addCircles(data = earthquake.data, 
                       lat = ~ latitude, 
                       lng = ~ longitude, 
                       weight = 1, 
                       #radius = ~sqrt(mag)*15000, 
                       radius = ~mag*10000,
                       popup = ~as.character(mag), 
                       label = ~as.character(paste("Magnitude: ", mag, ',',
                                                   "Depth: ", depth, ',',
                                                   "Month: ", month)), 
                       color = ~mag.pal(mag), 
                       fillOpacity = 0.5)
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = earthquake.data)
        proxy %>% clearMarkers()
        if (input$heatmap) {
            proxy %>%  addHeatmap(lng=~longitude, 
                                  lat=~latitude, 
                                  intensity = ~mag, 
                                  blur = 10, 
                                  radius = 10) 
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
    
    # Making valuebox for Total earthquakes in 2020 ---------------------------
    output$count = renderValueBox({
        count = prettyNum(nrow(earthquake.data), 
                          big.mark = ",")
        
        valueBox(subtitle = "Total Number of Earthquakes in 2020", 
                 value = count, icon = icon("calculator"), color = "blue")
    })
    
    # Making valuebox for Average magnitude of earthquakes -------------------
    output$avg.mag <- renderValueBox({
        avg = prettyNum(round(mean(earthquake.data$mag)), 
                        big.mark = ",")
        
        valueBox(subtitle = "Average Magnitude of the Earthquakes", 
                 value = avg, icon = icon("balance-scale-right"), color = "blue")
    })
    
    # Create a subset of data for getting histogram upto max magnitude ------
    max_mag <- reactive({
        req(input$max.mag) # ensure availablity of value before proceeding
        earthquake.data[earthquake.data$mag<= input$max.mag,]
        
    })
    
    # Create barplot object the plotOutput function is expecting --
    output$plot1 <- renderPlotly({
        ggplot(data = max_mag(), aes_string(x = "month"))+
            geom_bar(fill= 'steelblue')+ theme_bw()+
            labs(title = "Number of Earthquakes in the U.S. by month", 
                 subtitle = "Choose the number max Magnitude",
                 y = "Number of Earthquakes", 
                 x = 'Months')
    }
    )
    
    # Create barplot object the plotOutput function is expecting --
    output$plot2 <- renderPlotly({
        ggplot(data = max_mag(), aes_string(x= "month", y ="mag"))+
            geom_point(fill= 'steelblue')+ theme_bw()+
            labs(title = "Magnitude of Earthquakes in the U.S. by month", 
                 subtitle = "Choose the number max Magnitude",
                 y = "Magnitude of Earthquakes", 
                 x = 'Months')
    }
    )
    
    # Print data table if checked -------------------------------------
    output$datatable1 <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = max_mag()[, c("month", "day", "depth", 
                                               "mag", "latitude",
                                               "longitude","place")], 
                          options = list(pageLength = 5), 
                          rownames = FALSE)
        }
    )
    
    #Download data if checked 
    output$downloadbutton <- downloadHandler(
        filename = function(){
            paste('file1.csv')
        },
        content = function(file) {
            write.csv(max_mag(), file)
        }
    )
    
}

shinyApp(ui, server)