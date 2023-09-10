source("scripts/modules/cust_form_mod.R")
source("scripts/modules/cont_mod.R")
source("scripts/modules/cont_mod_view.R")

sale_UI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("salesTabs"),
    tabPanel(
      title = "New Contracts",
      value = "cont",
      icon = icon("money-check-alt"),
      br(),
      contract_UI(id = ns('lv2'))
    ),
    tabPanel(
      title = "View Contracts",
      value = "contV",
      icon = icon("file-invoice-dollar"),
      br(),
      contview_UI(id = ns('lv2'))
    ),
    tabPanel(
      title = "Customers",
      value = 'cust',
      icon = icon("users"),
      br(),
      fluidRow(
        column(4, cust_form_UI(id = ns("lv2")), hr(), cust_info_UI(id = ns("lv2"))),
        column(
          8,
          style = "box-sizing:border-box; padding:0 0 0 40px;",
          div(style = "background-color:#fff; border-radius:10px; padding:10px;",
              cust_dt_UI(id = ns("lv2")))
        )
      )
    ),
    tabPanel(
      title = "Performance",
      value = 'perf',
      icon = icon("chart-line")
    )
  ))
}



sale <- function(input, output, session, cust, cont, cont_it,modAct,inst_cal,costTable_it) {
  callModule(
    cust_form,
    id = 'lv2',
    cust = cust,
    active = reactive(input$salesTabs == 'cust' & modAct()=="sale")
  )
  
  callModule(
    contract,
    id = 'lv2',
    cust = cust,
    cont = cont,
    active = reactive(input$salesTabs == 'cont' & modAct()=="sale"),
    cont_it = cont_it
  )
  
  callModule(
    contview,
    id = 'lv2',
    cust = cust,
    cont = cont,
    active = reactive(input$salesTabs == 'contV' & modAct()=="sale"),
    cont_it = cont_it
  )
  
}