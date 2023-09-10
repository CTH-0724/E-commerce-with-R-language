hotkeysCust <- c(
  "d","x","a","q","c"
)


cust_form_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("mainUI"))
  )
}


cust_dt_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("cust")),
    useKeys(),
    keysInput(ns("keys"), hotkeysCust)
  )
}

cust_info_UI <-function(id){
  div(style="background-color:#007bff; margin-top:20px; border-radius:10px; padding:10px",
  tags$i(tags$b(style="color:#fff;",
                p("Double click on a cell to edit and press tab to save edits."),
                p("To delete a customer select a customer from the table and press 'd'")
  ))
  )
}



cust_form <- function(input, output, session, cust,active) {
  
  ns<-session$ns
  
  output$mainUI<-renderUI({
    k<-input$addCust
    tagList(
      textInput(inputId = ns('fname'),label = "First Name",placeholder = "Ex: John",width = "100%"),
      textInput(inputId = ns('lname'),label = "Last Name",placeholder = "Ex: Doe",width = "100%"),
      textInput(inputId = ns('cont'),label = "Contact Number",placeholder = "Ex: +94767329138",width = "100%"),
      textInput(inputId = ns('mail'),label = "e-mail",placeholder = "Ex: john.doe@gmail.com",width = "100%"),
      textInput(inputId = ns('address'),label = "Address",placeholder = "Ex: 21/B, Baker Street, London.",width = "100%"),
      actionBttn(inputId = ns("addCust"),label = "Add Customer",icon = icon("plus"),style = 'stretch',color = "primary",block = T,size = "md")
    )
  })
  
  output$cust<-renderDT({
    cust()
  },editable=T,rownames= FALSE)
  
  observeEvent(input$addCust,{
    row<-c(
      max(as.numeric(cust()$customerID),na.rm = T)+1,
      input$fname,
      input$lname,
      input$cont,
      input$mail,
      input$address
    )

    
    if(all(lapply(row[-1],isTruthy))){
      cust<-rbind(cust(),row)%>%
        arrange(desc(customerID))
      rownames(cust)<-NULL
      
      saveRDS(cust,"data/customers.Rds")
      shinyalert(title = "Customer created",text = "Customer successfully added to the database",type = 'success',closeOnEsc = T,closeOnClickOutside = T,timer = 2000)
    }else{
      shinyalert(title = "Please check the input fields",text = "One or more input fields are not provided",type = 'error',closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
    }
    })
  
  
  
  observeEvent(input$keys,{
    
    if(active()){
    if(!is.null(input$cust_rows_selected)){
      
      switch(input$keys,
             "d" = {
               shinyalert(
                 title = "You're about to delete a customer from the database.",
                 text = "Type 'lmnop' below to confirm",
                 inputType = "text",
                 type = 'input',
                 closeOnEsc = T,
                 closeOnClickOutside = T,
                 callbackR = function(x) {
                   if (x == "lmnop") {
                     cust <- cust()[-input$cust_rows_selected, ]
                     saveRDS(cust, "data/customers.Rds")
                     shinyalert(title = "Deleted!",text = "Customer has been removed from the database",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                   } else{
                     shinyalert(title = "Not deleted!",text = "Passphrase typing error",type = 'error',timer = 3000,closeOnEsc = T,closeOnClickOutside = T)
                   }
                 }
               )
             }
      )
      
    }else{
      shinyalert(title = "No customer is selected",text = sprintf("Select a customer from the customer table and press '%s' again", input$keys),type = 'error',closeOnEsc = T,closeOnClickOutside = T)
    }
      }
  })
  
  
  
  observeEvent(input$cust_cell_edit,{
    i<-input$cust_cell_edit
    str(i)
    
    cust<-cust()
    cust[i$row , i$col]<-i$value
    
    saveRDS(cust,"data/customers.Rds")
    shinyalert(title = "Success",text = "Customer info successfully edited",type = "success",closeOnEsc = T,closeOnClickOutside = T,timer = 3000)
  })
  
  
  
}