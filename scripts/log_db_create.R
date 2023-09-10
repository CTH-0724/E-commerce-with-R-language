# Below commented section create initial sqlite database for shiny app >> shinymanager logins. 
library(shinymanager)

# Creating users credentials data Frame
credentials <- data.frame(
  user = c("ozi543", "rumil",'nonadmin'),
  password = c("Signmein1#", "Signmein1#","signmein"),
  admin = c(TRUE, TRUE,FALSE),
  type=c("manager","sales","install"),
  stringsAsFactors = FALSE
)

create_db(
  credentials_data = credentials,
  sqlite_path = "data/creds.sqlite"
)
