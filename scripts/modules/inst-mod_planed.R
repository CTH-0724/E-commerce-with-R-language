source("scripts/modules/calender_mod.R")

ins_pend_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             cal_UI(id = ns("lv3.1"))
      ),
      column(4,
             cal_edit_UI(id=ns("lv3.1"))
             )
    ),
    cal_info_UI(id = ns("lv3.1"))
    # DTOutput(ns("calDT"))
  )
}

ins_pend <- function(input, output, session,active,cont,
                     cont_it,inst_cal,costTable_it) {
  output$calDT<-renderDT({
    inst_cal()
  })
  
  
  callModule(cal,id = "lv3.1",calDT=inst_cal,cont=cont,cont_it=cont_it,costTable_it=costTable_it)
}