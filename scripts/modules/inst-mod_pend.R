source("scripts/modules/calender_mod.R")

pendingInst_UI <- function(id) {
  ns <- NS(id)
  tagList(br(),
          fluidRow(column(
            7,
            div(style = "background-color:#fff; padding:10px; border-radius:10px;font-size:80%;",
                h3("Contracts", style = 'color:#333;'),
                hr(),
                DTOutput(ns("conts")))
          ),
          column(5,
                 div(style = "background-color:#fff; padding:10px; border-radius:10px; color:#333;font-size:80%;",
                     h3("Contract items", style = 'color:#333;'),
                     hr(),
                     uiOutput(ns("itemsUI"))
                     )
                 )
          ),
          
          div(style="background-color:#007bff; margin-top:20px; border-radius:10px; padding:10px",
              tags$i(tags$b(style="color:#fff;",
                           p("This tab will show the list of pending contracts to be installed.
                           Objective of this panel is to plan the installations for the pending contracts"),
                           p("Select a contract from left and the relevant items of the contract will be displayed on the right and
                           then select one or more items from the right table and press 'q' to plan."),
                           p("Planned items for installation can be view in 'Planned Installations' tab."),
                           p("Press 'c' to view the installation calender plan")
              ))
          )#,
          # cal_UI(ns("lv3"))
          
          )
}

pendingInst <-
  function(input,
           output,
           session,
           active,
           cont,
           cont_it,
           inst_cal,
           costTable_it) {
    
    ns<-session$ns
    
    r<-reactiveValues(contID=NULL,toPlan=NULL,contLoc=NULL,contCont=NULL,contEst=NULL)
    
    
    output$conts <- renderDT({
      cont() %>%
        select(-customerID, -price)
    },rownames= FALSE,selection="single",options=list(scrollX=T))
    
    
    observeEvent(input$conts_rows_selected,{
      r$contID<-cont()[input$conts_rows_selected,"contractID"]
      r$contLoc<-cont()[input$conts_rows_selected,"contract.address"]
      r$contCont<-cont()[input$conts_rows_selected,"contract.contact"]
      r$contEst<-cont()[input$conts_rows_selected,"estimated.instalation.date"]
      
    })
    
    output$itemsUI<-renderUI({
      if(!is.null(input$conts_rows_selected)){
        DTOutput(ns("contItems"))
      }else{
        p(tags$i(icon("info-circle"),"Select a Contract to see items"))
        }
    })
    
    
    itx<-reactive({
      cont_it()%>%
        filter(contractID==r$contID)
      })
    
    cont_its<-reactive({
      itx()
    })
    
    output$contItems<-renderDT({
      cont_its()%>%
        select(-contractID)
      
    },rownames= FALSE,options=list(scrollX=T))
    
    observeEvent(input$keys,{
      
      if (active()) {
        switch(input$keys,
        "q" = {
          if (!is.null(input$contItems_rows_selected) & !is.null(input$conts_rows_selected)) {
            r$toPlan <- itx()[input$contItems_rows_selected, ]
          } else if (!is.null(input$conts_rows_selected)) {
            r$toPlan <- itx()
          }else{
            r$toPlan <- NULL
          }
          
          showModal(modalDialog(
            size = 'l',
            title = "You're about to plan below items",
            uiOutput(ns("toPlanUI")),
            hr(),
            p("Note that sales team has provided",
              tags$span(r$contEst,style="font-size:110%; color:red;"), 
              "as an estimated installation finalization date to customer.",style="font-weight:900; "),
            hr(),
            if(!is.null(r$toPlan)){
              div(style="display:flex;align-items:center;",
                  div(style="flex:1",
                      awesomeCheckbox(inputId = ns("isSin"),"One day job",value = F,width = "100%")
                      ),
                  conditionalPanel(condition = "!input.isSin",ns = ns,style="flex:2; display:flex; margin:0 20px;",
                                   dateRangeInput(inputId = ns("i_period"),"Instalation period",width = '100%',start = Sys.Date()+7,end = Sys.Date()+14)
                  ),
                  conditionalPanel(condition = "input.isSin",ns = ns,style="flex:2; display:flex; margin:0 20px;",
                                   dateInput(inputId = ns("i_date"),label = "Installation date",value = Sys.Date()+14,width = "100%")
                  ),
                  div(style="flex:1",
                      actionBttn(inputId = ns("addP"),"Plan",icon = icon("project-diagram"),style = 'stretch',color = 'primary',block = T,size = 'sm')
                  )
                  )
              }
          ))
          
        },
        "c" = {
          print("c")
          showModal(ui = modalDialog(size = "l",
            cal_UI(ns("lv3"))
          ),session = session)
        }
        )
        
        
        
        
        
      }
    })
    
    
    output$toPlanUI<-renderUI({
      div(style="background-color:#fff; padding:10px; border-radius:10px; color:#333;",
          if(!is.null(r$toPlan)){
            renderDT({
              r$toPlan
            },options=list(scrollX=T)) 
          }else{
            h4("You've not selected any item or contract to plan.")
          }
      )
    })
    
    
    observeEvent(input$addP,{
      items<-r$toPlan$itemID
      r$contID
      
      its<-cont_it()
      its[its$itemID%in%items,"status"]<-50
      saveRDS(its,"data/contractItems.Rds")
      
      contx<-cont()
      
      contx[contx$contractID==r$contID,"status"]<-30
      
      if(all(its[its$contractID==r$contID,"status"]==50)){
        contx[contx$contractID==r$contID,"status"]<-50
      }
      saveRDS(contx,"data/contracts.Rds")
      
      ### Calender data set
      
      descs<-paste("ItemID:",r$toPlan$itemID,"|","@",r$toPlan$location,"-",r$toPlan$width,"x",r$toPlan$height)
      
      if(input$isSin){
        d<-c(input$i_date,input$i_date)
      }else{
        d<-input$i_period
      }
      
      calRow<-c(
        1,
        paste("Contract:",r$contID,"| Items:",paste(r$toPlan$itemID,collapse = ",")),
        paste(descs,collapse = ", "),
        NA,
        as.character(d[1]),
        as.character(d[2]),
        "allday",
        paste(r$contLoc,":",r$contCont),
        "#21d150",
        "#fff",
        "#2185d1"
      )
      
      if(nrow(inst_cal())>=1){
      inst_calx<-rbind(inst_cal(),calRow)
      }else{
        inst_calx<-inst_cal()
        inst_calx[1,]<-calRow
      }
      
      saveRDS(inst_calx,"data/cal.Rds")
      
      shinyalert(title = "Done!",text = "Items successfully added to the plan",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
      removeModal(session = session)
    })
    
    callModule(cal,id = "lv3",calDT=inst_cal,cont=cont,cont_it=cont_it,costTable_it=costTable_it)
  }