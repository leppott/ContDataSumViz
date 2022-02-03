#Sidebar----
#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    #Steps, do *not* need to be done sequentially----
    , sidebarMenu(id = id
      , menuItem(text = "About"
               , tabName = "tab_about"
               , icon = icon("home")
               )## menuItem ~ About ~ END
      , menuItem(text = "Import Files"
                 , tabName = "tab_import"
                 , icon = icon("file-upload")
                 , startExpanded = TRUE)
      , menuItem(text = "Temperature"
                 , icon = icon("thermometer-full") #
                 , menuSubItem("Thermal Stats"
                               , tabName = "tab_about"
                               , icon = icon("check-square"))
                 , menuSubItem("Growing Degree Days"
                               , tabName = "seedling"
                               , icon = icon("microscope"))
                 , menuSubItem("Thermal Classification"
                               , tabName = "tab_about"
                               , icon = icon("clone"))
                 )## menuItem ~ Data Preparation ~ END
      , menuItem(text = "Hydrology"
                 , icon = icon("water") #
                 , menuSubItem("IHA"
                               , tabName = "tab_about"
                               , icon = icon("map"))
                 , menuSubItem("Flashiness"
                               , tabName = "tab_about"
                               , icon = icon("calculator"))
                 )## menuItem ~ Analysis ~ END
      , menuItem(text = "Reports"
                 , icon = icon("clipboard-check")
                 , menuSubItem("Single Site"
                               , tabName = "tab_X"
                              # , icon = "pencil-alt"
                               )
                 , menuSubItem("Muliple Sites"
                               , tabName = "tab_Y"
                              # , icon = "pencil-ruler"
                               )
                 , menuSubItem("Continuous Sensor Metadata"
                               , tabName = "tools")
                 )## menuItem ~ Summary ~ END
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END
