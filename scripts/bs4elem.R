## This File contains bs4dash elements to render in the app

# Defining sidebar
side <-
  bs4DashSidebar(
    skin = 'light',
    status = 'primary',
    collapsed = T,
    expandOnHover = T,
    bs4SidebarMenu(id = "sideB", .list = lapply(tabs, function(i) {
      bs4SidebarMenuItem(text = i[[1]],selected = if(i[[2]]=="inst"){T}else{F},
                         tabName = i[[2]],
                         icon = icon(i[[3]]))
    })),
    customArea = uiOutput('sbRest')
  )

# Defining Header
nav <-
  bs4DashNavbar(
    title = bs4DashBrand(
      title = tags$b(AppTitle),
      color = 'primary',
      image = logoName,
      href = "#"
    ),
    leftUi = uiOutput("navLeft"),
    rightUi = uiOutput('navRight'),
    skin = 'light',
    status = 'primary',
    uiOutput("navMiddle")
  )

# Defining right side bar | controlbar
cb <-
  bs4DashControlbar(
    id = 'cbar',
    collapsed = T,
    overlay = T,
    pinned = F,
    skin = 'light'
  )



# Calling for modules for each tab
for (i in tabs) {
  moduleN = paste0("scripts/modules/",i[[2]], "-", "mod.R")
  
  if (file.exists(moduleN)) {
    print(moduleN)
    source(moduleN)
  }
  
}

# i<-tabs[[1]]
# 
# do.call(what = paste0(i[[2]], "_UI"), args = list(id = "lv1"))



# Defining body

k <- lapply(tabs, function(i) {
  
  moduleN = paste0("scripts/modules/",i[[2]], "-", "mod.R")
  
  bs4TabItem(tabName = i[[2]],
             if (file.exists(moduleN)) {
               do.call(what = paste0(i[[2]], "_UI"), args = list(id = "lv1"))
             } else{
               uiOutput(paste(i[[2]], "ui", sep = "-"))
             })
  
})

body <- bs4DashBody(
  useCss(file = 'style.css'),
  useShinyalert(force = TRUE),
  useKeys(),
  keysInput("keys", hotkeys),
  do.call(what = bs4TabItems, args = k)
  )





