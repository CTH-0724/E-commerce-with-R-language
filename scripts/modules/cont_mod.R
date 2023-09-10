contract_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      column(6,
             fluidRow(
               column(
                 6,
                 pickerInput(
                   inputId = ns("cust"),
                   label = "Customer",
                   choices = NULL,
                   width = "100%"
                 ),
                 textInput(
                   inputId = ns("address"),
                   "Contract address",
                   width = '100%'
                 ),
                 textInput(
                   inputId = ns("contact"),
                   "Contact number for Contract",
                   width = '100%'
                 ),
                 dateInput(
                   inputId = ns("date"),
                   "Date of contract",
                   value = Sys.Date(),
                   width = '100%'
                 ),
                 dateInput(
                   inputId = ns("e.date"),
                   "Estimated installation date",
                   value = Sys.Date() + 21,
                   width = '100%'
                 ),
               ),
               column(6,
                      uiOutput(ns("item_UI")))
             ),
             div(style="background-color:#007bff; margin-top:20px; border-radius:10px; padding:10px",
                 tags$i(tags$b(style="color:#fff;",
                               p("First select the customer. if customer is new, create a new customer. 'Contract address' and 'Contact number for contract' will be auto updated as per customer database. If there's any difference edit the relevant fields."),
                               p("Add items to the contract with the second panel. All added items will be displayed in third panel"),
                               p("To delete an item from the contract, select the line item from the table and press 'd'"),
                               p("Once items are added, a button will appear below the table to create the contract")
                 ))
             )
             ),
      column(
        6,
        div(
          style = "box-sizing:border-box; padding:10px; border-radius:10px;background-color:#fff;font-size:80%;",
          h3("Contract items", style = 'color:#333;'),
          hr(),
          DTOutput(ns("cont_items"))
        ),
        div(
          style = "box-sizing:border-box; padding:10px; border-radius:10px;background-color:#fff;margin-top:10px;",
        uiOutput(ns("cContBtn"))
        )
      )
    ),
    br(),
    hr(),hr(),
    DTOutput(ns("allCont"))
  )
}

contract <- function(input, output, session,cust,cont,active,cont_it) {
  ns<-session$ns
  
  r<-reactiveValues(itemL=NULL)
  
  output$allCont<-renderDT(
    # cont(),options=list(scrollX=TRUE)
    cont_it()
  )
  
  output$item_UI<-renderUI({
    k<-input$add_i
    tagList(
    textInput(ns('i_loc'),"Location",placeholder = "Bed 3",width = "100%"),
    numericInput(ns('i_width'),'Width',value = NULL,width = '100%',min = 0),
    numericInput(ns('i_height'),'Height',value = NULL,width = '100%',min = 0),
    textAreaInput(ns('i_desc'),"Description",placeholder = "Whitewood PUC, Brass handles",width = "100%",resize = 'vertical'),
    numericInput(ns('i_price'),'Price',value = NULL,width = '100%',min = 0),
    actionBttn(inputId = ns("add_i"),"Add item to contract",icon = icon("plus"),style = "stretch",color = "primary",block = T,size = 'md')
    )
  })
  
  
  observe({
    updatePickerInput(session = session,inputId = "cust",choices = paste(cust()$customer.fname,cust()$customer.lname))
  })
  observe({
    updateTextInput(session = session,inputId = "address",
                    value = cust()[paste(cust()$customer.fname,cust()$customer.lname)==input$cust,"customer.address"]
    )
  })
  observe({
    updateTextInput(session = session,inputId = "contact",
                    value = cust()[paste(cust()$customer.fname,cust()$customer.lname)==input$cust,"customer.mobile"]
    )
  })
  
  
  observeEvent(input$cust,{
    r$itemL<-data.frame(location=character(0),width=numeric(0),height=numeric(0),description=character(0),price=numeric(0))
  })
  
  output$cont_items<-renderDT({
    r$itemL
  },options=list(scrollX=T))
  
  
  observeEvent(input$add_i,{
    row<-c(
      input$i_loc,
      input$i_width,
      input$i_height,
      input$i_desc,
      input$i_price
    )
    
    if(all(lapply(row,isTruthy))){
    if(nrow(r$itemL)==0){
      r$itemL[1,]<-row
    }else{
      r$itemL<-rbind(r$itemL,row)
    }
    }else{
        shinyalert(title = "Check fields",text = "One or more fields are empty.",type = 'error',closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
      }
  })
  
  observeEvent(input$keys,{
    if(active()){
    if(!is.null(input$cont_items_rows_selected)){
      switch(input$keys,
             "d" = {
               shinyalert(
                 title = "You're about to delete a item from the contract",
                 text = "Type 'lmnop' below to confirm",
                 inputType = "text",
                 type = 'input',
                 closeOnEsc = T,
                 closeOnClickOutside = T,
                 callbackR = function(x) {
                   if (x == "lmnop") {
                     
                     r$itemL<-r$itemL[-input$cont_items_rows_selected,]
                     
                     shinyalert(title = "Deleted!",text = "Item has been removed from the contract",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                   } else{
                     shinyalert(title = "Not deleted!",text = "Passphrase typing error",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                   }
                 }
               )
             }
      )
    }else{
      if(input$keys=='d'){
      shinyalert(title = "No item is selected",text = sprintf("Select a item from the table and press '%s' again", input$keys),type = 'error',closeOnEsc = T,closeOnClickOutside = T)
        }
    }
      }
  })
  
  
  output$cContBtn<-renderUI({
    if(nrow(r$itemL)>0){
      total<-sum(as.numeric(r$itemL$price),na.rm = T)
      
      print(total)
      actionBttn(ns("addCont"),label = paste("Create contract | Value:",total),icon = icon("plus"),style = 'stretch',color = 'primary',block = T,size = 'md')
    }else{
      p(style='text-align:center;margin-bottom:0;',
      tags$b(style="color:#333;",
        "Add items to the contract"
      )
      )
    }
  })
  
  observeEvent(input$addCont,{
    contractID<-max(as.numeric(cont()$contractID),na.rm = T)+1
    
    itemID<-max(as.numeric(cont_it()$itemID),na.rm = T)+1
    
    total<-sum(as.numeric(r$itemL$price),na.rm = T)
    custID<-cust()[paste(cust()$customer.fname,cust()$customer.lname)==input$cust,"customerID"]
    
    row_cont<-c(
      contractID,
      custID,
      input$address,
      input$contact,
      as.character(input$date),
      as.character(input$e.date),
      total,
      1
    )
    
    
    row_items<-cbind(itemID=(itemID:(itemID+nrow(r$itemL)-1)),r$itemL,status=1)
    
    k<-rbind(cont(),row_cont)
    saveRDS(k,"data/contracts.Rds")
    
    
    k1<-rbind(cont_it(),cbind(contractID=contractID,row_items))
    saveRDS(k1,"data/contractItems.Rds")
    
    r$itemL<-data.frame(location=character(0),width=numeric(0),height=numeric(0),description=character(0),price=numeric(0))
    
    shinyalert(title = "Contract Saved",text = "The contract has been successfully saved to the database.",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
      
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}