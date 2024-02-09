#load packages-------------------------------------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinybusy)
library(shinyalert)
library(shinyWidgets)
library(reactable)
library(plotly)
library(tidyverse)
library(zoo)
library(lubridate)
library(janitor)

#load partitions-----------------------------------
#ui
source("./partitions/ui_partitions/ui_about.R")
source("./partitions/ui_partitions/ui_down_inmet.R")
source("./partitions/ui_partitions/ui_load_file.R")
source("./partitions/ui_partitions/ui_proc.R")
source("./partitions/ui_partitions/ui_eto.R")

#server
source("./partitions/server_partitions/serverDownInmet.R")
source("./partitions/server_partitions/serverUpload.R")
source("./partitions/server_partitions/serverProcess.R")
source("./partitions/server_partitions/serverCharts.R")
source("./partitions/server_partitions/serverUpETo.R")
source("./partitions/server_partitions/serverEvapo.R")

#load functions-----------------------------------
source("./functions/JS.R")


#interface-----------------------------------------
ui <-
dashboardPage(
  dashboardHeader(
    title = "DataMetProcess"
  ),#Header
  dashboardSidebar(
    useShinyjs(),
    shinyjs::extendShinyjs(text = jscode,functions = c('disableTab','disableTabMenu','disableSubMenu','enableTab','enableTabMenu','enableSubMenu')),
    sidebarMenu(
      menuItem(
        text = "About",
        tabName = "about-menu",
        icon = icon("circle-info")
      ),#menu
      
      menuItem(
        text = "Download Inmet",
        tabName = "Down-Inmet",
        icon = icon("download")
      ),#menu
      
      menuItem(
        text = "Data Processing",
        tabName = "Data-Process",
        icon = icon("gears"),
        menuSubItem("Load File",
                    tabName = "load-file",
                    icon = icon("upload")),
        menuSubItem("Process",
                    tabName = "process",
                    icon = icon("gear"))
      ),#menu
      
      menuItem(
        text = "Evapotranspiration",
        tabName = "evapo-pm",
        icon = icon("water"),
        menuSubItem("Load File",
                    tabName = "load-evapo",
                    icon = icon("upload")),
        menuSubItem("Evapotranspiration calculation",
                    tabName = "proc_evapo",
                    icon = icon("droplet")
      ))#menu
    )#Sidemenu
  ),#Sidebar
  dashboardBody(
    tags$head(includeCSS("./materials/styles.css")),
    tabItems(
      tabItem(
        tabName = "about-menu",
        ui_about()
      ),#tabItem
      
      tabItem(
        tabName = "Down-Inmet",
        ui_down_inmet("downinmet")
      ),#tabItem
      
      tabItem(
        tabName = "load-file",
        ui_load_file("tableupload")
      ),#tabItem
      
      tabItem(
        tabName = "process",
        ui_proc()
      ),#tabItem
      tabItem(
        tabName = 'load-evapo',
        ui_eto_load("upload_evapo")
      ),#tabItem
      tabItem(
        tabName = 'proc_evapo',
        ui_eto_calc("etocalc")
      )
    )#tabItems
  )#Body
)#Page


#server--------------------------------
server <-
function(input, output,session){
  
  js$disableTabMenu('Plots')
  js$disableSubMenu('process')
  js$disableSubMenu('proc_evapo')
  
  #images
  output$logoabout <- renderImage({
    list(src = "./materials/Logo.png")
  },deleteFile = FALSE)
  
  output$ufrpe <- renderImage({
    list(src = "./materials/UFRPE2.png",
         height = "70px",width= "43px")
  },deleteFile = FALSE)
  
  output$uast <- renderImage({
    list(src = "./materials/UAST2.png",
         height = "70px",width= "70px")
  },deleteFile = FALSE)
  
  output$pgea <- renderImage({
    list(src = "./materials/PGEA.png",
         height = "70px",width= "90px")
  },deleteFile = FALSE)
  
  output$capes <- renderImage({
    list(src = "./materials/CAPES.png",
         height = "70px",width= "76px")
  },deleteFile = FALSE)
  
  output$cnpq <- renderImage({
    list(src = "./materials/CNPQ.png",
         height = "70px",width= "227px")
  },deleteFile = FALSE)
  
  output$facepe <- renderImage({
    list(src = "./materials/FACEPE.png",
         height = "70px",width= "196px")
  },deleteFile = FALSE)
  

  
  serverDownInmet("downinmet")
  
  
  
  #process
  TableInput_ex <- serverUpload("tableupload")
  TablesInputs <- serverProcess("processtabs",TableInput_ex)
  serverCharts("ui_grafic",TablesInputs[[1]],TablesInputs[[2]],TablesInputs[[3]])
  
  observeEvent(TablesInputs[[3]](),{
    if(nrow(TablesInputs[[3]]())>0){
      js$enableTabMenu("Plots")
    }
  })
  
  #evapo
  Table_ETO <- serverUpETo("upload_evapo")
  serverEvapo("etocalc",Table_ETO)
  
}


#run app-------------------------------------------
shinyApp(ui = ui, server = server)