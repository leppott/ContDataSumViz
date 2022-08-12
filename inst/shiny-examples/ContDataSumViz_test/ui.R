#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dashboardPage(
  header = dashboardHeader(title = "ContDataSumViz")
  #, sidebar = dashboardSidebar(sb_main("leftsidebarmenu"))
  , sidebar = dashboardSidebar(sb_main("leftsidebarmenu"))
  , body = dashboardBody(db_main("dbBody"))
) ## dashboardPage ~ END

# https://rstudio.github.io/shinydashboard/get_started.html

