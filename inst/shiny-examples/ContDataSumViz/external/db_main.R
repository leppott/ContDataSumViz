# Main

function(id) {
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_about", tab_code_about())
      , tabItem(tabName = "tab_import", tab_code_import())
      , tabItem(tabName = "tab_allparam", tab_code_about())
      , tabItem(tabName = "tab_temp", tab_code_about())
      , tabItem(tabName = "tab_hydro", tab_code_about())
      , tabItem(tabName = "tab_reports", tab_code_about())
    )## tabItems
  )## dashboardBody ~ END
}## FUNCTION ~ END


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "tab_about", h2("About"))
#     , tabItem(tabName = "tab_import", h2("Import"))
#   )## tabItems
# )## dashboardBody ~ END
