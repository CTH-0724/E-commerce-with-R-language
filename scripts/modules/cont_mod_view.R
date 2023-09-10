contview_UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style="background-color:#fff;padding:10px;border-radius:10px;margin-bottom:10px;font-size:80%;",
        h3(textOutput(ns('contractsText')),style='color:#333;'),hr(),
    DTOutput(ns("contractDT"))
    ),
    fluidRow(
      column(6,
             div(style="background-color:#fff;padding:10px;border-radius:10px;font-size:80%",
                 h3("Customer Info",style='color:#333;'),hr(),
             DTOutput(ns("cust_DT"))
             )
             ),
      column(6,
             div(style="background-color:#fff;padding:10px;border-radius:10px;",
                 h3("Contract items",style='color:#333;'),hr(),
             DTOutput(ns('cont_it_DT'))
             )
             )
    ),
    div(style="background-color:#007bff; margin-top:20px; border-radius:10px; padding:10px;font-size:80%;",
        tags$i(tags$b(style="color:#fff;",
                      p("Select a contract from the 'Contracts' table."),
                      p("Relevent customer info and Item-wise info will be displayed on 'Customer info' table and 'Contract items' table"),
                      p("To add an missing item to the selected contract, press 'a'"),
                      p("To delete selected contract, press 'd"),
                      p("If any row in 'Contract items' are selected, it'll be removed from the contract. If not selected the whole contract will be deleted by pressing 'd'")
        ))
    )
  )
}

contview <- function(input, output, session,active,cust,cont,cont_it) {
  ns<-session$ns
  
  r<-reactiveValues(contID=NULL,custID=NULL)
  
  output$contractDT<-renderDT({
    cont()
  },selection = 'single',rownames= FALSE,options = list(scrollX = TRUE))
  
  output$contractsText<-renderText({
    k<-"Contracts"
    if(!is.null(r$contID)){
      k<-paste(k,"| Selected: Contract",r$contID)
    }
    k
  })
  
  output$cont_it_DT<-renderDT({
    if(!is.null(r$contID)){
    cont_it()%>%
      filter(contractID==r$contID)%>%
      select(-2)
      }
  },rownames= FALSE,options = list(scrollX = TRUE))
  
  output$cust_DT<-renderDT({
    if(!is.null(r$custID)){
    cust()%>%
      filter(customerID==r$custID)%>%
      select(-1)
      }
  },rownames= FALSE,options = list(scrollX = TRUE))
  
  
  observeEvent(input$contractDT_rows_selected,{
    r$contID<-cont()[input$contractDT_rows_selected,"contractID"]
    r$custID<-cont()[input$contractDT_rows_selected,"customerID"]
  })
  
  
  observeEvent(input$keys,{
    if(active()){
      switch (input$keys,
        'd' = {
          if(any(!is.null(input$cont_it_DT_rows_selected),!is.null(input$contractDT_rows_selected))){
          shinyalert(
            title = if(is.null(input$cont_it_DT_rows_selected)){"You're about to delete a contract from the database."}else{"You're about to delete an item from the contract"},
            text = "Type 'lmnop' below to confirm",
            inputType = "text",
            type = 'input',
            closeOnEsc = T,
            closeOnClickOutside = T,
            callbackR = function(x) {
              if (x == "lmnop") {
                if(!is.null(input$cont_it_DT_rows_selected)){
                  cont_itx<-cont_it()%>%
                    filter(contractID!=r$contID)
                  
                  cont_ity<-cont_it()%>%
                    filter(contractID==r$contID)
                  
                  cont_ity<-cont_ity[-input$cont_it_DT_rows_selected,]
                  
                  cont_itz<-rbind(cont_itx,cont_ity)
                  
                  saveRDS(cont_itz,"data/contractItems.Rds")
                  
                  contx<-cont()
                  
                  contx[contx$contractID==r$contID,'price']<-sum(as.numeric(cont_itz[cont_itz$contractID==r$contID,'price']),na.rm = T)
                  saveRDS(contx,"data/contracts.Rds")
                  
                  shinyalert(title = "Deleted!",text = "Item has been removed from the contract.",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                }else{
                  contx <- cont()[-input$contractDT_rows_selected, ]
                  saveRDS(contx, "data/contracts.Rds")
                  
                  cont_itx<-cont_it()%>%
                    filter(contractID!=r$contID)
                  saveRDS(cont_itx,"data/contractItems.Rds")
                  
                  shinyalert(title = "Deleted!",text = "Contract has been removed from the database",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                }
              } else{
                shinyalert(title = "Not deleted!",text = "Passphrase typing error",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
              }
            }
          )
          }else{
            shinyalert(title = "No items are selected to perform the action",text = paste("Select an line item from the table and press '",input$keys,"' again"),type = "error",closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
          }
        },
        "a" = {
          if(any(!is.null(input$cont_it_DT_rows_selected),!is.null(input$contractDT_rows_selected))){
          showModal(modalDialog(
            title = paste("Add item to contract",r$contID),footer = NULL,
            size = 'm',easyClose = T,
            textInput(ns('i_locx'),"Location",placeholder = "Bed 3",width = "100%"),
            numericInput(ns('i_widthx'),'Width',value = NULL,width = '100%',min = 0),
            numericInput(ns('i_heightx'),'Height',value = NULL,width = '100%',min = 0),
            textAreaInput(ns('i_descx'),"Description",placeholder = "Whitewood PUC, Brass handles",width = "100%",resize = 'vertical'),
            numericInput(ns('i_pricex'),'Price',value = NULL,width = '100%',min = 0),
            actionBttn(inputId = ns("add_ix"),"Add item to contract",icon = icon("plus"),style = "stretch",color = "primary",block = T,size = 'md')
                                ))
          }else{
            shinyalert(title = "No items are selected to perform the action",text = paste("Select an line item from the table and press '",input$keys,"' again"),type = "error",closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
          }
        }
      )
    }
  })
  
  observeEvent(input$add_ix,{
    cont_itx<-cont_it()
    itemID<-max(as.numeric(cont_itx$itemID),na.rm = T)+1
    row<-c(
      itemID,
      r$contID,
      input$i_locx,
      input$i_widthx,
      input$i_heightx,
      input$i_descx,
      input$i_pricex,
      1
    )
    
    cont_itx<-rbind(cont_itx,row)
    
    contx<-cont()
    
    contx[contx$contractID==r$contID,'price']<-sum(as.numeric(cont_itx[cont_itx$contractID==r$contID,'price']),na.rm = T)
    
    saveRDS(contx,"data/contracts.Rds")
    saveRDS(cont_itx,"data/contractItems.Rds")
    
    shinyalert(title = "Done",text = "Item added to the contract",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
  })
  
  # proxy_cont<-dataTableProxy(outputId = "contractDT",session = session)
  # 
  # observeEvent(input$cont_it_DT_rows_selected,{
  #   selectRows(proxy = proxy_cont,selected = NULL)
  # })
  
}














