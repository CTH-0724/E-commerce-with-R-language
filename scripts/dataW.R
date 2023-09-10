

cust <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/customers.Rds",readFunc = readRDS)

cont <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/contracts.Rds",readFunc = readRDS)

cont_it <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/contractItems.Rds",readFunc = readRDS)

comp_contracts <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/competed_contracts.Rds",readFunc = readRDS)

inst_cal <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/cal.Rds",readFunc = readRDS)

costTable_it <- reactiveFileReader(intervalMillis = 1000,session = NULL,filePath = "data/itemCost.Rds",readFunc = readRDS)