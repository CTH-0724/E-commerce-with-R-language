source("scripts/modules/inst-mod_pend.R")
source("scripts/modules/inst-mod_planed.R")
source("scripts/modules/inst-mod_history.R")

inst_UI <- function(id) {
  ns <- NS(id)
  tagList(tabsetPanel(selected = "pl_inst",
    id = ns("instTabs"),
    tabPanel(
      title = "Pending Installations",
      value = "p_inst",
      icon = icon("money-check-alt"),
      br(),
      pendingInst_UI(id=ns('lv2'))
    ),
    tabPanel(
      title = "Planned Installations",
      value = "pl_inst",
      icon = icon("file-invoice-dollar"),
      br(),
      ins_pend_UI(id=ns("lv2"))
    ),
    tabPanel(
      title = "Installations History",
      value = "hi_inst",
      icon = icon("file-invoice-dollar"),
      br(),
      inst_history_UI(id=ns('lv2'))
    )
  ))
}

inst <- function(input, output, session, cust, cont, cont_it,modAct,inst_cal,costTable_it) {
  callModule(pendingInst,id="lv2",
             cont = cont,
             active = reactive(input$instTabs == 'p_inst' & modAct() == "inst"),
             cont_it = cont_it,
             inst_cal = inst_cal,
             costTable_it=costTable_it
             )
  
  callModule(ins_pend,id="lv2",
             cont = cont,
             active = reactive(input$instTabs == 'pl_inst' & modAct() == "inst"),
             cont_it = cont_it,
             inst_cal = inst_cal,
             costTable_it=costTable_it
  )
  
  
  callModule(inst_history,id="lv2",
             cont = cont,
             active = reactive(input$instTabs == 'hi_inst' & modAct() == "inst"),
             cont_it = cont_it,
             inst_cal = inst_cal,
             costTable_it=costTable_it
  )
  
}