## YLL - Causas de muerte en Ecuador

# Librerias
rm(list=ls())
# install.packages("Rtools")
# install.packages("tidyverse")
# install.packages("treemap")
# install.packages("devtools")
# devtools::install_github("gluc/data.tree")
# devtools::install_github("timelyportfolio/d3treeR")
# install.packages("shinythemes")
library("tidyverse")
library("treemap")
library("d3treeR")
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("DT")
library("leaflet")
library("sf")
library("sp")
library("htmltools")
library("geojsonio")

#Directorios
dir <- "C:/Users/esteban.lombeida.MSPCENTRAL/Downloads/MSP/YLL/YLL/"
dir_bdd <- paste0(dir,"BDD/")

# base inicial
EDG_bdd <- read.csv(paste0(dir_bdd,"BDD_YLL/YYL_BDD_2020.csv"), header = TRUE, fileEncoding="UTF-8") %>% mutate(ylldis=round(ylldis,1))
names(EDG_bdd)

# Cargar funciones de ayuda (helpers)
source(paste0(dir,"Sintaxis/Helpers.R"))

# Definir app
# UI - Crea interfaz -----------------------------------------------------------
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # title
                headerPanel("Causas de muerte en Ecuador"),
                
                #input
                sidebarPanel(
                  selectInput("year",  "Año", choices= c(1997:2020), multiple = F),
                  pickerInput("prov", "Provincia", choices= unique(EDG_bdd$name_prov),  options = list(`actions-box` = TRUE), multiple = T),
                  selectInput("sex",  "Sexo", choices= unique(EDG_bdd$sexo), multiple = T),
                  # actionButton("go", "Go")
                  actionButton("go", "Go", icon("paper-plane"), 
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  
                ),	
                
                # output				
                mainPanel(
                  tabsetPanel(tabPanel("Tremap", selectInput("YLL",  "Tipo de Análisis", choices= c("yll","ylldis"), multiple = F),
                                       d3tree2Output("treemap_causas")),
                              tabPanel("Gráficos",  uiOutput("patologia"),
                                       actionButton("run", "Map", icon("paper-plane"), 
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       leafletOutput("Mapa"),
                                       textOutput("detalle")),
                              tabPanel("Datos",  dataTableOutput("Datos")),
                              tabPanel("Descarga", downloadButton('downloadData', 'Download data'))
                  )
                )
)

# Server - Crea lógica de funcionamiento----------------------------------------
server <- function(input, output, session) {
  
  ## Datos Reactivos ante cambios en input - GENERAL
  data <- eventReactive(input$go,{
    read.csv(paste0(dir_bdd,"BDD_YLL/YYL_BDD_",input$year,".csv"), header = TRUE)  %>% rename(prov = name_prov)
  })
  ## Data treemap-----------------------------------
  data_treemap <- eventReactive(input$go,{
    data() %>%  filter(., prov %in% input$prov,
                       sexo  %in% input$sex)
  })
  # Treemap plot
  output$treemap_causas <- renderD3tree2({
    tm <-  treemap(data_treemap(),
                   index=c("name_cau_67", "Desc"),
                   vSize=input$YLL,
                   vColor="yll",
                   type="dens")
    d3tree3( tm, rootname = "YLL por causa de muerte", width="96%")
  })
  
  ## Data Mapa-----------------------------------
  output$patologia <- renderUI({
    pickerInput("patologia", "Patología", choices= unique(data()$name_cau_67),  options = list(`actions-box` = TRUE), multiple = F)
  })

  
  data_map <- eventReactive(input$run,{
    merge_geo(data=data(), filtro = as.character(input$patologia)) 
    
  })
  # Render Mapa
  output$Mapa <- renderLeaflet({
    bins   <-  round(quantile(data_map()$yll, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm=T),1)
    pal    <- colorBin("YlOrRd", domain = data_map()$yll, bins = bins, na.color="transparent")
    labels <- paste("Prov: ", data_map()$dpa_despro,"<br/>", 
                    "Años perdidos: ", round(data_map()$yll,1), sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(data_map()) %>%
      setView(lng = -78.183406 ,lat = -1.831239 , zoom=6.4) %>%
      addTiles() %>%  addPolygons(
        fillColor = ~pal(yll),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addLegend( pal=pal, values=~yll, opacity=0.8, title = "Años Perdidos", position = "bottomright" )
  })
  output$detalle <- renderText("Nota: Se presenta Años perdidos descontados por edad (ylldis)")
  
  # Data tabla -----------------------------------
  output$Datos <- renderDataTable(tabla(data()), options = list(pageLength = 25))
  
  # Descargar data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("YLL_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabla(data()), file)
    })
  
}

# Run the application 
# shiny::runApp(display.mode="showcase")
shinyApp(ui = ui, server = server)
