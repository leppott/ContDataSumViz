#
# This is the user-interface definition of ContDataSumViz Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(DT)
library(plotly)
#library(shinycustomloader)
library(shinycssloaders)

# Define UI for application
options(spinner.color.background="#ffffff",spinner.size=1)
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # main panel for variable selection
  mainPanel(width = 12,
            
            css_tabPanels <- '.nav-tabs>li>a{
                                   color: cornflowerblue;
                                   font-size: 18px;
                                   font-weight: bold;
                                   }',
            tags$head(tags$style(HTML(css_tabPanels))),
            
            tags$style("#big-heading {font-size:15px;color:black;font-style:bold;display:block; }"),
            
            tags$style(HTML(".shiny-notification{
                            position:fixed;
                            top:calc(50%);
                            left:calc(50%);}")),
            
            # spacing
            fluidRow(p()),
            
            # top controls  
            fluidRow(
              
              column(width = 12,
                     actionButton('reset all', 
                                  label = img(src = "ContDataSumViz_banner_no_EPA.png", 
                                              width = '100%'),
                                  width = '100%')
              )
              ),
              
            
            fluidRow(p(),
                     
                     tabsetPanel(
                       
                       # Upload Data----
                       tabPanel("Upload Data", 
                                
                                fluidPage(
                                  
                                  fluidRow(
                                    
                                    column(width = 12,
                                           
                                           sidebarLayout(
                                             sidebarPanel(width=3,
                                                          fileInput("uploaded_data_file",
                                                                    label=h4(id="big-heading",
                                                                             "Upload your data"),
                                                                    multiple=FALSE,
                                                                    accept = c("text/csv",
                                                                               "text/comma-separated-values,
                                                                               text/plain",
                                                                               ".csv")),
                                                          hr(),
                                                          actionButton(inputId="uploadId", 
                                                                       label= "Use this file",
                                                                       style="color:cornflowerblue;background-color:black;font-weight:bold"),
                                                          hr(),
                                                          uiOutput("siteType"),
                                                          hr(),
                                                          uiOutput("manage"),
                                                          hr(),
                                                          uiOutput("select"), 
                                                          hr(),
                                                          uiOutput("display_button")
                                                          
                                                          
                                                          
                                                          
                                             ),
                                             mainPanel(width=9,
                                                       uiOutput("display_raw_ts")
                                               ) # mainPanel end
                                             
                                           ) # sidebarLayout end
                                           
                                    ), #column close
                                    
                                    column(width = 12,
                                             tableOutput("contents")
                                           
                                    ), #column close
                                    
                                    
                                  ) #fluidRow close
                                )  #fluidPage close
                       ),  # tabPanel end 
                       
                       # Data Exploration ----
                       tabPanel("Data Exploration", 
                                
                                fluidPage(
                                  
                                  fluidRow(
                                    
                                    tabsetPanel( id="tabset",
                                                 tags$head(tags$style(HTML(".radio-inline {margin-right: 40px;}"))),
                                                 ## DE, All Parameters ----
                                                 tabPanel("All parameters", value="all_parameters_tab",br(),
                                                          tabsetPanel(id="all_parameters_subtabs",
                                                          ### DE, All, Summary Tables ----
                                                          tabPanel("Summary tables", 
                                                                   value="tab_summary_tables",
                                                                   br(),                     
                                                          br(),
                                                          column(width = 12,
                                                                 sidebarLayout(
                                                                   sidebarPanel(width=3,
                                                                                # EWL, Start
                                                                                p("Must calulcate daily statistics before can proceed."),
                                                                                p("'Upload Data' - 'Use this file' - 'Run meta survey' - 'Calculate daily statistics'"),
                                                                                # EWL, End
                                                                                hr(),
                                                                                uiOutput("summary_table_input_1"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_2"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_3"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_4"),
                                                                                
                                                                   ),
                                                                   mainPanel(width=9,
                                                                             column(width=9,uiOutput("display_summary_table_1")),
                                                                             column(width=9,uiOutput("display_summary_table_2"))
                                                                   ) # mainPanel end
                                                                   
                                                                 ) # sidebarLayout end
                                                                 
                                                          ), #column close
                                                          br(),
                                                                      ), # tabPanel 1 end
                                                          
                                                          ### DE, All, TS Plots ----
                                                          tabPanel("Time series plots", value="tab_time_series",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         uiOutput("time_series_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_input_2"),
                                                                                         div(  
                                                                                           id = "cp_shaded_region",  
                                                                                           conditionalPanel(
                                                                                             condition = "input$dailyStats_ts_metrics == 'mean'|input$dailyStats_ts_metrics == 'median'",   
                                                                                             hr(),
                                                                                             uiOutput("time_series_input_3"),
                                                                                           ), #conditionalPanel end
                                                                                         ), # div end
                                                                                         shinyjs::hidden(
                                                                                           div(  
                                                                                             id = "cp_new_data",  
                                                                                             conditionalPanel(
                                                                                               condition = "input$dailyStats_shading == 'newData' " ,   
                                                                                               hr(),
                                                                                               fileInput("uploaded_newData_file",label="Upload your new data",multiple=FALSE,
                                                                                                         accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                                                                               hr(),
                                                                                               selectizeInput("newData_lower_col",label ="Select column to be used as lower bound",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               selectizeInput("newData_upper_col",label ="Select column to be used as upper bound",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               selectizeInput("newData_date_col",label ="Select date column",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               textInput("newData_name",label="New data name",value="USGS")
                                                                                               
                                                                                             ), #conditionalPanel end
                                                                                           ) # div end
                                                                                         ), #shinyjs:: hidden end
                                                                                         
                                                                                         hr(),
                                                                                         uiOutput("time_series_input_4"),
                                                                                         hr(),
                                                                                         fluidRow(column(width=6,
                                                                                                         uiOutput("time_series_input_5")
                                                                                                        ),
                                                                                                  column(width=6,align="right",
                                                                                                         uiOutput("time_series_input_6")
                                                                                                        )
                                                                                                  ) # fluidRow close
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series"))),
                                                                                      br(),
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series_1"))),
                                                                                      br()
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   br(),   
                                                                   column(width =12,
                                                                          uiOutput("another_time_series_UI"))
                                                                   
                                                          ), #tabPanel 2 end
                                                          
                                                          ### DE, All, TS Annual----
                                                          tabPanel("Time series - Annual overlays", value="tab_time_series_overlay",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         uiOutput("time_series_overlay_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_4"),
                                                                                         shinyjs::hidden(
                                                                                           div(  
                                                                                             id = "cp_new_data_overlay",  
                                                                                             conditionalPanel(
                                                                                               condition = "input$overlay_shading == 'newData' " ,   
                                                                                               hr(),
                                                                                               fileInput("uploaded_overlay_newData_file",label="Upload your new data",multiple=FALSE,
                                                                                                         accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                                                                               hr(),
                                                                                               selectizeInput("overlay_newData_lower_col",label ="Select column to be used as lower bound",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               
                                                                                               hr(),
                                                                                               selectizeInput("overlay_newData_longterm_col",label ="Select column to be used as long-term reference line",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               selectizeInput("overlay_newData_upper_col",label ="Select column to be used as upper bound",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               selectizeInput("overlay_newData_date_col",label ="Select month-day column",
                                                                                                              choices=NULL,
                                                                                                              multiple = FALSE,
                                                                                                              selected=NULL,
                                                                                                              options = list(hideSelected = FALSE)),
                                                                                               hr(),
                                                                                               textInput("overlay_newData_name",label="New data name",
                                                                                                         value="USGS")
                                                                                               
                                                                                             ), #conditionalPanel end
                                                                                           ) # div end
                                                                                         ), #shinyjs:: hidden end
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_5"),
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series_overlay"))),
                                                                                      br(),
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series_overlay_1"))),
                                                                                      br()
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 3 end
                                                          
                                                          ### DE, All, Box Plots----
                                                          tabPanel("Box plots", value="tab_box",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("box_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_5")
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_box_plots"))),
                                                                                      br(),
                                                                                      br(),
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 4 end
                                                          
                                                          ### DE, All, CDFs ----
                                                          tabPanel("CDFs", value="tab_CDF",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_3"), 
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_5"),
                                                                                         hr(),
                                                                                         uiOutput("display_CDF_button")
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_plot_CDF"))),
                                                                                      br(),
                                                                                      br(),
                                                                                      
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   br(),
                                                                   
                                                          ), #tabPanel 5 end
                                                          
                                                          ### DE, All, Raster Graphs ----
                                                          tabPanel("Raster graphs", value="tab_raster",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("raster_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_5"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_6"),
                                                                                         
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      column(width=9,uiOutput("display_raster_graphs"))
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 6 end
                                                          
                                                          ### DE, All, Climate Spiral ----
                                                          tabPanel("Climate spiral", value="tab_climate",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("climate_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("climate_input_2"),
                                                                                         
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      column(width=9,uiOutput("display_climate_spiral"))
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 7 end
                                                          
                                                          ) #inner tabsetPanel end
                                                          
                                                          
                                                 ), #tabPanel end
                                                 
                                                
                                                 ## DE, Temperature ----
                                                 tabPanel("Temperature", value="temp_tab",
                                                          tabsetPanel(id="temp_subtabs",
                                                                      tags$head(tags$style("#help_text_air_water,#help_text_water_temp_class,#help_text_thermal_statistics,#help_text_growing_degree_days
                                                                                          {font-size:16px;color:black;font-style:bold;display:block; }")),
                                                                      ### DE, Temp, Thermal Stats----
                                                                      tabPanel("Thermal statistics",value = "sb1",br(),
                                                                               column(width = 12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_2"),
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_3"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_run_thermal_button"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_save_thermal_button"),
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_help_text_thermal_statistics")
                                                                                                         ,uiOutput("display_thermal_table_1")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_2")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_3")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_4")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_5"))
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                                      
                                                                               ), #column close
                                                                               
                                                                      ),
                                                                      ### DE, Temp, Air v Water ----
                                                                      tabPanel("Air vs Water",value = "sb2",br(),
                                                                               column(width = 12,
                                                                               sidebarLayout(
                                                                                 sidebarPanel(width=3,
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_1"),
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_2"),
                                                                                              hr(),
                                                                                              radioButtons("exclude_data_points", 
                                                                                                           "Limit the data points with air temperature", 
                                                                                                           choices = c("No"="No","Yes"="Yes"),
                                                                                                           selected = "No"),
                                                                                              shinyjs::hidden(
                                                                                              div(  
                                                                                                id = "cp_air_temp",  
                                                                                              conditionalPanel(
                                                                                              condition = "input$exclude_data_points == 'Yes' " ,   
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_4"),
                                                                                              ), #conditionalPanel end
                                                                                              ) # div end
                                                                                              ), #shinyjs:: hidden end
                                                                                              hr(),
                                                                                              uiOutput("display_thermal_sensitivity_button"),
                                                                                             
                                                                                 ),
                                                                                 mainPanel(width=9,
                                                                                           column(width=12
                                                                                                  ,uiOutput("display_help_text_air_water")
                                                                                                  ,uiOutput("display_thermal_sensitivity_plot_1")
                                                                                                  ,br()
                                                                                                  ,uiOutput("display_thermal_sensitivity_plot_2")
                                                                                                  )
                                                                                           
                                                                                 ) # mainPanel end
                                                                                 
                                                                               ) # sidebarLayout end
                                                                               ) # column close
                                                                               
                                                                      ),
                                                                      ### DE, Temp, GDD ----
                                                                      tabPanel("Growing degree days",value = "sb3",br(),
                                                                               br(),
                                                                               column(width=12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("growing_degree_days_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_growing_degree_days_button"),
                                                                                                     
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_help_text_growing_degree_days")
                                                                                                         ,div(  
                                                                                                           id = "hyper_link_panel",  
                                                                                                           conditionalPanel(
                                                                                                             condition = "0==0" ,   
                                                                                                             a("Download R script",href="GDD.R")
                                                                                                           ), #conditionalPanel end
                                                                                                         ) # div end      
                                                                                                         
                                                                                                  ),
                                                                                                  
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_growing_degree_days_table"))
                                                                                                  
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                                      )  # column close
                                                                      ),
                                                                      ### DE, Temp, Therm Class ----
                                                                      tabPanel("Thermal classification",value = "sb4",br(),
                                                                               br(),
                                                                               column(width=12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("water_temp_class_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_water_temp_class_button"),
                                                                                                     
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_help_text_water_temp_class")
                                                                                                         ,uiOutput("display_water_temp_class_table")
                                                                                                         
                                                                                                  )
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                                      ) # column close
                                                                      ))), #outer tabPanel end
                                                 
                                                 ## DE, Hydrology ----
                                                 tabPanel("Hydrology", value="hydro_tab",
                                                          tabsetPanel(id="hydro_subtabs",
                                                                      tags$head(tags$style("#help_text_IHA,#help_text_flashiness
                                                                                          {font-size:16px;color:black;font-style:bold;display:block; }")),
                                                                      ### DE, Hydro, IHA----
                                                                      tabPanel("IHA",value = "IHA_tab",br(),
                                                                               column(width = 12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_2"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_3"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_4"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_IHA_button"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_save_IHA_button")
                                                                                                     
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_help_text_IHA")
                                                                                                         ,uiOutput("display_IHA_table_1")
                                                                                                         ,uiOutput("display_IHA_plot_button_1")
                                                                                                         ,shinyjs::hidden(
                                                                                                           div(  
                                                                                                             id = "IHA_plot_1_panel",  
                                                                                                             conditionalPanel(
                                                                                                               condition = "input$display_IHA_plot_1 %%2 !=0" ,   
                                                                                                               hr(),
                                                                                                               uiOutput("IHA_plot_1"),
                                                                                                             ), #conditionalPanel end
                                                                                                           ) # div end
                                                                                                         ) #shinyjs:: hidden end
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_IHA_table_2")
                                                                                                         ,uiOutput("display_IHA_plot_button_2")
                                                                                                         ,shinyjs::hidden(
                                                                                                           div(  
                                                                                                             id = "IHA_plot_2_panel",  
                                                                                                             conditionalPanel(
                                                                                                               condition = "input$display_IHA_plot_2 %%2 !=0" ,   
                                                                                                               hr(),
                                                                                                               uiOutput("IHA_plot_2a"),
                                                                                                               br(),
                                                                                                               uiOutput("IHA_plot_2b")
                                                                                                             ), #conditionalPanel end
                                                                                                           ) # div end
                                                                                                         ) #shinyjs:: hidden end
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_IHA_table_3")
                                                                                                         ,uiOutput("display_IHA_plot_button_3")
                                                                                                         ,shinyjs::hidden(
                                                                                                           div(  
                                                                                                             id = "IHA_plot_3_panel",  
                                                                                                             conditionalPanel(
                                                                                                               condition = "input$display_IHA_plot_3 %%2 !=0" ,   
                                                                                                               hr(),
                                                                                                               uiOutput("IHA_plot_3")
                                                                                                             ), #conditionalPanel end
                                                                                                           ) # div end
                                                                                                         ) #shinyjs:: hidden end
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_IHA_table_4")
                                                                                                         ,uiOutput("display_IHA_plot_button_4")
                                                                                                         ,shinyjs::hidden(
                                                                                                           div(  
                                                                                                             id = "IHA_plot_4_panel",  
                                                                                                             conditionalPanel(
                                                                                                               condition = "input$display_IHA_plot_4 %%2 !=0" ,   
                                                                                                               hr(),
                                                                                                               uiOutput("IHA_plot_4a"),
                                                                                                               br(),
                                                                                                               uiOutput("IHA_plot_4b")
                                                                                                             ), #conditionalPanel end
                                                                                                           ) # div end
                                                                                                         ) #shinyjs:: hidden end
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_IHA_table_5")
                                                                                                         ,uiOutput("display_IHA_plot_button_5")
                                                                                                         ,shinyjs::hidden(
                                                                                                           div(  
                                                                                                             id = "IHA_plot_5_panel",  
                                                                                                             conditionalPanel(
                                                                                                               condition = "input$display_IHA_plot_5 %%2 !=0" ,   
                                                                                                               hr(),
                                                                                                               uiOutput("IHA_plot_5"),
                                                                                                               br()
                                                                                                             ), #conditionalPanel end
                                                                                                           ) # div end
                                                                                                         ) #shinyjs:: hidden end
                                                                                                  )
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                               ) # column close
                                                                      ),
                                                                     
                                                                      ### DE, Hydro, Flashiness ----
                                                                      tabPanel("Flashiness",value = "Flashiness_tab",br(),
                                                                               br(),
                                                                               column(width=12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("flashiness_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_flashiness_button"),
                                                                                                     
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_help_text_flashiness")
                                                                                                         ,uiOutput("display_flashiness_table")
                                                                                                         
                                                                                                  )
                                                                                        ) # mainPanel end
                                                                                      ) # sidebarLayout end
                                                                                      ) # column close
                                                                      )))  
                                                 
                                                 
                                  ) #tabsetPanel end              
                                  ) #fluidRow close
                                )  #fluidPage close
                       ),# tabPanel end 
               
               # Create Report----        
               tabPanel("Create Report",
                        fluidPage(
                                fluidRow(
                                  tabsetPanel(id="report_subtabs",
                                              ### CR, Single ----
                                              tabPanel("SingleSite",value = "SingleSite_tab",br(),
                                                       column(width = 12,
                                                              sidebarLayout(
                                                                sidebarPanel(width=3,
                                                                             hr(),
                                                                             radioButtons("report_format", 
                                                                                          "Select report format", 
                                                                                          choices = c("pdf"="pdf","html"="html","word"="docx"),
                                                                                          selected = "html"),
                                                                             hr(),
                                                                             textInput(inputId="report_name",
                                                                                       label="Report file name",
                                                                                       value="myReport"),
                                                                             hr(),
                                                                             actionButton('createReport',"Create and save Report")
                                                                             
                                                                ),
                                                                mainPanel(width=9,
                                                                          column(width=12
                                                                                 ,uiOutput("display_report_content_1")
                                                                                 ,br()
                                                                                 ,uiOutput("display_report_content_2")
                                                                          )
                                                                          
                                                                ) # mainPanel end
                                                                
                                                              ) # sidebarLayout end
                                                       ), # column close
                                                       br(),
                                                       column(width=12,uiOutput("display_table_single_site"))
                                              ), #tabPanel close
                                              
                                              ### CR, Multi----
                                              tabPanel("MultiSites",value = "MultiSites_tab",br(),
                                                       br(),
                                                       fluidPage(h4(id="big-heading","Coming later")),
                                                       column(width=12,uiOutput("display_table_multiple_sites"))
                                              ) #tabPanel close 
                                             ) #tabsetPanel end              
                                    ) #fluidRow end
                        ) #fluidPage end
               ) # tabPanel end 
                       
            ) #tabsetPanel close
            
            )
                     
  )
))
