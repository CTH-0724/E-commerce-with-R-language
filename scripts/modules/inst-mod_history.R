inst_history_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9,
             div(style="background-color:#fff; padding:10px; border-radius:10px;",
             DTOutput(ns('costDt'))
             )
             )
    )
  )
}

inst_history <- function(input, output, session,
                         cont,
                         cont_it,
                         active,
                         inst_cal,
                         costTable_it
                         ) {
  
  
  costDT<-reactive({
    costTbl <- costTable_it()
    itTbl <- cont_it()
    
    left_join(costTbl,itTbl)
  })
  
  output$costDt<-renderDT({
    costDT()
  },options=list(scrollX=T))
}