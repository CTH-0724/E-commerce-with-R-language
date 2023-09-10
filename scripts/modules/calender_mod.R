cal_UI <- function(id) {
  ns <- NS(id)
  tagList(
    calendarOutput(outputId = ns("instCal")),
    # fluidRow(
    #   column(6,
    DTOutput(ns("contCal")),
    #        ),
    # column(6,
    DTOutput(ns("itemCal"))
    # )
    # )
  )
}

cal_info_UI<- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("calInfo"))
  )
}

cal_edit_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    bs4TabCard(id = ns('cal_ed'),title = NULL,width = 12,side = 'left',status = 'primary',collapsible = F,closable = F,maximizable = F,solidHeader = F,type = 'tabs',selected = "comp",
               tabPanel(title = "Replan",value = 'edit',icon = icon("edit"),
                        conditionalPanel(condition = "input.instCal_click",ns = ns,
                                         uiOutput(ns('replanH')),
                                         dateRangeInput(ns('up_date_r'),"New Planed Date"),
                                         hr(),
                                         fluidRow(
                                           column(5,
                                                  actionBttn(inputId = ns('del_p'),label = "Remove",icon = icon("trash-alt"),block = T,style = 'stretch',color = 'danger')
                                           ),
                                           column(7,
                                                  actionBttn(inputId = ns('up_p'),label = "Update the Plan",icon = icon("retweet"),block = T,style = 'stretch',color = 'primary')
                                           )
                                         )
                                         
                        ),
                        conditionalPanel(condition = "!input.instCal_click",ns = ns,
                                         hr(),
                                         p(icon('info-circle'),"Select a planed installation from the calender to enable options.")
                        )
               ),
               tabPanel(title = "Complete",value = 'comp',icon = icon("check-circle"),
                        conditionalPanel(condition = "input.instCal_click",ns = ns,
                            uiOutput(ns("cost_pan"))
                        ),
                        conditionalPanel(condition = "!input.instCal_click",ns = ns,
                                         hr(),
                                         p(icon('info-circle'),"Select a planed installation from the calender to enable options.")
                        )
                        
               )
    )
  )
}


cal <- function(input, output, session,calDT,cont,cont_it,costTable_it) {
  
  ns<-session$ns
  
  r<-reactiveValues(contP=NULL,itemP=NULL,itemC=NULL,costT=NULL)
  
  output$instCal<-renderCalendar({
    calendar(calDT(), useNavigation = TRUE,navigation = T) %>%
      cal_month_options(
        startDayOfWeek  = 1, 
        narrowWeekend = TRUE
      )
  })
  
  
  output$calInfo<-renderUI({
    tagList(
      renderPrint(
        input$instCal_click
      )
    )
  })
  
  observeEvent(input$instCal_click,{
    k<-input$instCal_click$title
    r$contP<-trimws(strsplit(strsplit(k,"[|]")[[1]][[1]],":")[[1]][[2]])
    r$itemP<-trimws(strsplit(strsplit(strsplit(k,"[|]")[[1]][[2]],":")[[1]][[2]],",")[[1]])
    
    r$costT<-costTable_it()
    
    updateDateRangeInput(session = session,inputId = "up_date_r",start = as_date(input$instCal_click$start$`_date`)+1,end = input$instCal_click$end$`_date`)
  })
  
  output$contCal<-renderDT({
    cont()%>%
      filter(contractID%in%r$contP)
  })
  
  output$itemCal<-renderDT({
    cont_it()%>%
      filter(itemID%in%r$itemP)
  })
  
  
  output$cost_pan <- renderUI({
    tagList(h4("Contract: ", r$contP),
            p(icon("map-marker-alt"),input$instCal_click$location),
            lapply(r$itemP, function(item) {
              row<-cont_it()[cont_it()$itemID==item,]
              
              div(class = 'well dark-mode', style = "border-radius:5px;margin-bottom:10px;",
                  h4("Item:", item),
                  bs4Badge(color = "primary","Location:",row[["location"]]),
                  bs4Badge(color = "info","Width:",row[["width"]]),
                  bs4Badge(color = "success","Height",row[["height"]]),
                  tags$b(textOutput(ns(paste(item,"costText")))),
                  hr(),
                  actionBttn(inputId = ns(paste0(item,"addCost")),label = 'Add Cost',icon = icon("money-bill-alt"),size = "sm",style = 'stretch',block = T)
              )
            }),
            div(class = 'well dark-mode', style = "border-radius:5px;margin-bottom:10px;",
                uiOutput(ns("saveCbtnUI"))
            ),
            div(class = 'well dark-mode', style = "border-radius:5px;margin-bottom:10px;",
                actionBttn(inputId = ns("markCompBtn"),label = "Mark installation complete",icon = icon("check"),style = "stretch",size = "md",block = T,color = 'success')
            )
            
    )
    
    
  })
  
  output$replanH<-renderUI({
    tagList(
    h4("Contract: ", r$contP," | Item:",paste(r$itemP,collapse = ", ")),
    p(icon("map-marker-alt"),input$instCal_click$location)
    )
  })
  
  observe({
    lapply(r$itemP,function(item){
      
      if(!is.null(r$costT)) {
        l <- r$costT[r$costT$itemID == item, ]
        
        
        output[[paste(item,"costText")]] <- renderText({
          if (nrow(l) > 0) {
            paste("Cost: ",
                  sum(as.numeric(l$cost), na.rm = T)
            )
          }else{
            "Cost: Not loged"
          }
        })
        
      }
      
      observeEvent(input[[paste0(item,"addCost")]],{
        r$itemC<-item
        
        showModal(modalDialog(title = paste("Log cost for",r$contP,":","Item",item),size = "m",easyClose = F,
                              uiOutput(ns("addCModal"))
        ))
      })
    })
  })
  
  
  output$addCModal<-renderUI({
    tagList(
      
      div(style='background-color:#fff; border-radius:10px; padding:10px;',
          DTOutput(ns('itemCs'))
      ),
      hr(),
      dateInput(inputId = ns("addCDate"),label = "Date of installation",value = Sys.Date(),width = "100%"),
      fluidRow(
        column(8,
               selectizeInput(inputId = ns("addCelem"),"Cost element",
                              choices = c(
                                "Product cost (1)",
                                "Product cost (2)",
                                "Fit Cost",
                                "Admin Cost",
                                "Fixed Costs",
                                "Set Costs"
                              ),width = "100%",options = list(create=T)
               )
        ),
        column(4,
               numericInput(ns("addCcost"),"Cost",value = NULL,min = 0)
        )
        
      ),
      actionBttn(inputId = ns("addCadd"),"Log cost",style = "stretch",icon = icon('plus'),size = 'sm',block = T,color = 'primary')
    )
  })
  
  observeEvent(input$addCadd,{
    row<-c(
      r$itemC,
      as.character(input$addCDate),
      input$addCelem,
      input$addCcost
    )
    
    if(nrow(r$costT)>0){
      r$costT<-rbind(r$costT,row)
    }else{
      r$costT[1,]<-row
    }
    updateNumericInput(session = session,inputId = ns("addCcost"),value = NULL)
  })
  
  
  output$itemCs<-renderDT({
    r$costT%>%
      filter(itemID==r$itemC)%>%
      select(-itemID)
  },rownames = FALSE)
  
  
  
  
  observeEvent(input$addCbtn,{
    nwClr<-"#007bff"
    
    k<-calDT()
    
    k[k$title==input$instCal_click$title,"bgColor"]<-nwClr
    saveRDS(k,"data/cal.Rds")
    
    saveRDS(r$costT,"data/itemCost.Rds")
    shinyalert(title = "Done",text = "Cost has been saved",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 2000)
  })
  
  
  
  observeEvent(input$markCompBtn,{
    k<-calDT()
    k<-k[k$title!=input$instCal_click$title,]
    saveRDS(k,"data/cal.Rds")
    
    cont_itx<-cont_it()
    
    cont_itx[cont_itx$itemID%in%r$itemP,"status"]<-100
    
    saveRDS(cont_itx,"data/contractItems.Rds")
    
    cIt<-cont_itx%>%
      filter(contractID==r$contP)%>%
      select(status)
    
    if(all(cIt==100)){
      contx<-cont()
      contx[contx$contractID==r$contP,"status"]<-100
      saveRDS(contx,"data/contracts.Rds")
    }else if(any(cIt==100)){
      contx<-cont()
      contx[contx$contractID==r$contP,"status"]<-80
      saveRDS(contx,"data/contracts.Rds")
    }
    
    shinyalert(title = "Done",text = "Schedule marked complete!",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 2000)
    
    
  })
  
  
  output$saveCbtnUI<-renderUI({
    actionBttn(inputId = ns("addCbtn"),label = paste("Save/","Total Cost:",sum(as.numeric(r$costT[r$costT$itemID%in%r$itemP,'cost']),na.rm = T))
               ,icon = icon("plus"),style = "stretch",size = "md",block = T,color = 'default')
  })
  
  observeEvent(input$up_p,{
    calc<-calDT()
    calc[calc$title==input$instCal_click$title,c('start','end')]<-as.character(input$up_date_r)
    saveRDS(calc,"data/cal.Rds")
  })
  
  observeEvent(input$del_p,{
    tit<-input$instCal_click$title
    r$contP
    r$itemP
    
    calc<-calDT()
    calc<-calc[calc$title!=tit,]
    saveRDS(calc,"data/cal.Rds")
    
    cont_itx<-cont_it()
    cont_itx[cont_itx$itemID%in%r$itemP,'status']<-1
    
    saveRDS(cont_itx,"data/contractItems.Rds")
    
    if(
      all(cont_itx[cont_itx$contractID==r$contP,'status']==1)
    ){
      contx<-cont()
      contx[contx$contractID==r$contP,"status"]<-1
      saveRDS(contx,"data/contracts.Rds")
    }else if(
      any(cont_itx[cont_itx$contractID==r$contP,'status']==1)
    ){
      contx<-cont()
      contx[contx$contractID==r$contP,"status"]<-30
      saveRDS(contx,"data/contracts.Rds")
    }
    
    costT<-costTable_it()%>%
      filter(!itemID%in%r$itemP)
    
    saveRDS(costT,"data/itemCost.Rds")
    
  })
  
}