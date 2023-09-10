library(shiny)
library(bs4Dash)
library(shinyWidgets)

library(DT)
library(data.table)
library(keys)
library(shinyalert)
library(shinyBS)
library(toastui)

library(dplyr)
library(lubridate)

library(shinymanager)

options(shiny.maxRequestSize=100*1024^2)

tabs <- list(
  list("Sales", 'sale', 'chart-line'),
  list("Installations", 'inst', 'toolbox'),
  list("Marketing", 'mark', 'comments-dollar'),
  list("Management", 'mng', 'chart-pie')
)

AppTitle = "BULLOCH GROUP"
logoName = "logo.png"

hotkeys <- c(
  "q", 
  "a",
  "z"
)


source("scripts/subFuncs.R")
source("scripts/bs4elem.R",local = T)





if(!file.exists("data/creds.sqlite")){
  source("scripts/log_db_create.R")
}


ui <- bs4DashPage(header = nav,sidebar = side,body = body,controlbar = cb,title = AppTitle,fullscreen = T,dark = T)

ui <- secure_app(
  ui = ui,
  tags_top = 
    tags$div(
      tags$head(
        tags$link(rel = "stylesheet", href ="folder.css"),
        tags$title(AppTitle)
      ),
      tags$img(
        src = "logo-no-background.png", width = 200
      )
    ),
  background  = "url('bg1.jpg')  no-repeat center fixed;",
  theme = shinythemes::shinytheme(theme ="paper"),
  enable_admin = TRUE)


server <- function(input, output, session) {
  
  # Authenticating user
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(
  #     "data/creds.sqlite"
  #   )
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  # -- Authentication end
  
  source("scripts/dataW.R")
  
  modAct<-reactive({input$sideB})
  
  lapply(tabs,function(i){
    moduleN = paste0("scripts/modules/",i[[2]], "-", "mod.R")
    mods<-i[[2]]
    if(file.exists(moduleN)){
      callModule(module = get(mods),id = "lv1",cust=cust,cont=cont,cont_it=cont_it,modAct=modAct,inst_cal=inst_cal,costTable_it=costTable_it)
    }
  })
  
  observe({
    print(input$sideB)
  })

}

shinyApp(ui, server,options = list(port=8282))