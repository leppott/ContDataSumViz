#
# This is the server logic of ContDataSumViz Shiny web application. You can run
# the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Notes:
# 1) If this is the first time to run this app, you need to install the required
#    R packages first
#    you can do that by typing this in your Console:
#    source("_moved/install_packages_for_app.R")
# 2) This App use several revised R functions from "ContDataQC" R package, but
#    the changes/edits may not be committed yet when you run the testing
#    versions, please remember to include the directory /update_ContDataQC/
#    under the same folder where you unzip this App package.
#    We don't need to source these R functions after we finalize them
#

##if(!require(ContDataQC)){source("_moved/install_packages_for_app.R")}  #install if not yet

# library("readxl")        # to read excel files
# library("writexl")
# library("data.table")
# library("DT")
# library("tidyverse")
# library("tibbletime")
# library("shiny")
# library("shinydashboard")
# library("shinyjs")
# library("shinyBS")
# library("shinythemes")
# library("shinyalert")
# library("conflicted")
# library("dataRetrieval")
# library("doBy")
# library("knitr")
# library("htmltools")
# library("rmarkdown")
# library("highr")
# library("survival")
# library("shinyFiles")
# library("plotly")
# library("zip")
# library("reshape2")
# library("ContDataQC")
# library("ContDataSumViz")
# library("StreamThermal")
# library("IHA")
# library("XLConnect")

## Moved to Global ***

# source("_moved/import_raw_data.R")
# source("update_ContDataQC/config.R")
# source("update_ContDataQC/CompSiteCDF.updated.R")
# source("update_ContDataQC/SumStats.updated.R")
# source("update_ContDataQC/ReportMetaData.R")
#
#
# options(shiny.maxRequestSize = 100*1024^2)

function(input, output, session) {

  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  loaded_data <- reactiveValues()
  raw_data_columns<-reactiveValues()
  selected_to_plot <- reactiveValues(all_selected=data.frame())
  processed <- reactiveValues(processed_dailyStats=list(),
                              ST.freq=data.frame(),
                              ST.mag=data.frame(),
                              ST.roc=data.frame(),
                              ST.tim=data.frame(),
                              ST.var=data.frame())
  saveToReport <- reactiveValues(metadataTable=data.frame())

  #  Upload Data##############################

  #EWL
  if (file.exists("_moved/File_Format.rds")) file.remove("_moved/File_Format.rds")
  do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))

  uploaded_data<-eventReactive(c(input$uploaded_data_file),{
                                loaded_data$name <- input$uploaded_data_file$name
                               if(grepl("csv$",input$uploaded_data_file$datapath)){
                                 my_data<-import_raw_data(input$uploaded_data_file$datapath,"csv",has_header=TRUE)
                               }else if(grepl("xlsx$",input$uploaded_data_file$datapath)){
                                 my_data<-import_raw_data(input$uploaded_data_file$datapath,"xlsx",has_header=TRUE)
                               }else{
                                 shinyalert("Warning","not valid data format",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                                            confirmButtonText="OK",inputId = "alert_data_not_valid")
                               }
                                 cols_avail <- colnames(my_data)
                                 print(cols_avail)
                                 return(my_data)
                               })



  observeEvent(input$alert_data_not_valid,{
    shinyjs::runjs("swal.close();")
  })

  ## Copy uploaded files to local folder
  observeEvent(input$uploadId,{

    ## this part is for copying multiple files and saving them in a reactive datasetlist()
    # if (!file.exists("Selected_Files")) dir.create(file.path("","Selected_Files"),showWarnings = FALSE, recursive = TRUE)
    # #print(is.null(input$uploaded_data_file))
    # if (is.null(input$uploaded_data_file) ) {    return(NULL)  }
    # file.copy(from = input$uploaded_data_file$datapath, to =  paste0('Selected_Files/',input$uploaded_data_file$name )  )
    # df <- list(file = input$uploaded_data_file$name , header= TRUE,
    #            sep = ",",dec = input$dec,
    #            quote = '',
    #            index = input$uploadId)
    # if(input$uploadId > 1){
    #   old_df <- readRDS("File_Format.rds")
    #   df <- sapply(names(old_df),function(n){c(old_df[[n]],df[[n]])},simplify=FALSE)
    # }
    # saveRDS2 <- function(object,file){str(object);saveRDS(object,file)}
    # saveRDS2(df, "File_Format.rds")
    ## update the line choices in the raw time series plot
    my_data <- uploaded_data()

    output$display_raw_ts <- renderUI({

      if (length(my_data) > 0 ) {
        ## this part is to find any column name related to Date or Time
        all_date_related_keys <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay","MONTHDAY","Time","TIME","Month","Day","RAW.Date.Time","mm.dd.yyyy.HH.MM.SS")
        date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay","mm.dd.yyyy.HH.MM.SS")
        my_colnames <- colnames(my_data)

        ## get possible date columns in order according to "date_keys_in_favor_order"
        possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% my_colnames]
        all_date_columns <- all_date_related_keys[all_date_related_keys %in% my_colnames]

        print(possible_date_columns)

        ## send out alert message if the date column cannot be identified
        alert_message_no_date_column = paste0("We assume the dataset you uploaded contains at lease one date time column, but no date time column is identified, please check.")
        if (identical(length(possible_date_columns),integer(0))){
          print("inside shinyalert loop now...")
          shinyalert("Warning",alert_message_no_date_column,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                     confirmButtonText="OK",inputId = "alert_no_date")
        }else{
          raw_data_columns$date_column_name <- possible_date_columns[1]
        }
        ## this part is to find any column name related to "ID" or "Flag"
        idx_no_ID_Flag <- !str_detect(my_colnames,"ID") & !str_detect(my_colnames,"SITE") & !str_detect(my_colnames,"Flag") & !str_detect(my_colnames,"Comment")
        not_ID_or_Flag_cols <- my_colnames[idx_no_ID_Flag]
        parameters_cols_best_guess <- not_ID_or_Flag_cols[!not_ID_or_Flag_cols %in% all_date_columns]
        print(parameters_cols_best_guess)

        sidebarLayout(
          sidebarPanel(h4(id="big-heading","Plot Raw Data"),width=2,
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line1_1sec",label="","choose a column")),
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line2_1sec",label="","choose a column")),
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line3_1sec",label="","choose a column")),
                       div(style="display: inline-block;vertical-align:top; width: 95%;",selectInput("line4_1sec",label="","choose a column")),
                       hr(),
                       radioButtons("raw_datetime_format", "Select datetime format", choices = c("%Y-%m-%d %H:%M:%S"="%Y-%m-%d %H:%M:%S",
                                                                                                 "%Y-%m-%d %H:%M"="%Y-%m-%d %H:%M",
                                                                                                 "%d-%m-%Y %H:%M:%S"="%d-%m-%Y %H:%M:%S",
                                                                                                 "%Y-%m-%d"="%Y-%m-%d",
                                                                                                 "%d-%m-%Y"="%d-%m-%Y",
                                                                                                 "%m%d"="%m%d"),
                                    selected = "%Y-%m-%d %H:%M:%S"),
                       hr(),
                       actionButton(inputId="showrawTS", label="Display time series",style="color:cornflowerblue;background-color:black;font-weight:bold"),
                       hr(),
                       #EWL, start
                       p("Must selet at least one parameter before 'run' (or crashes)"),
                       # EWL, end
                       selectizeInput("parameters_to_process",label ="Select parameters to process",
                                      choices=parameters_cols_best_guess,
                                      multiple = TRUE,
                                      options = list(hideSelected = FALSE,plugins=list('remove_button'))
                                      ), # selectizeInput close
                       br(),
                       actionButton(inputId="runQS", label="Run meta summary",style="color:cornflowerblue;background-color:black;font-weight:bold"),

          ), # sidebarPanel close
          mainPanel(width=10,
                    tags$head(tags$style(HTML(".radio-inline {margin-left: 10px;}"))),
                    tags$head(tags$style("#calculateDailyStatistics,#saveDailyStatistics{
                                                                                    font-size: 14px;
                                                                                    }
                                                                                    ")),
                    tags$head(tags$style(HTML("#quick_summary_table {
                                              text-align: center;
                                              font-size: 16px;
                                              font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}
                                              "))),
                    tags$head(tags$style("#quick_summary_table_footnote{
                                        text-align:center;
                                        color: black;
                                        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
                                        font-size: 16px;
                                        font-weight: bold;}
                                        ")),
                    fluidRow(column(width=12,uiOutput("display_all_raw_ts"),
                                    hr()
                                     ), # column close
                                     ), #fluidRow end
                    fluidRow(column(width=12,uiOutput("display_quick_summary_table"),
                                     uiOutput("display_footnote_text")), # column close
                                     ), #fluidRow end

                            fluidRow(column(width=8,style="padding:20px;",
                                                    uiOutput("display_checkBoxes_dailyStats_1"),
                                                    uiOutput("display_radioButtons_dailyStats_2")
                                           ), # column close
                                     column(width=4,style="padding:25px;",uiOutput("display_actionButton_calculateDailyStatistics")
                                           ), # column close
                                     column(width=4,div(style="margin-bottom:20px")),
                                     column(width=4,style="padding:30px;",uiOutput("display_actionButton_saveDailyStatistics")
                                     ), # column close
                                   ) #fluidRow end
                                   ) # mainPanel end
        ) # sidebarLayout end

      }

    })

    my_data_continuous <- my_data %>% select(where(is.numeric))
    all_continuous_col_names <- colnames(my_data_continuous)

    updateSelectInput(session,"line1_1sec",choices = c("",all_continuous_col_names),selected=NULL)
    updateSelectInput(session,"line2_1sec",choices = c("",all_continuous_col_names),selected=NULL)
    updateSelectInput(session,"line3_1sec",choices = c("",all_continuous_col_names),selected=NULL)
    updateSelectInput(session,"line4_1sec",choices = c("",all_continuous_col_names),selected=NULL)
    updateSelectInput(session,"line5_1sec",choices = c("",all_continuous_col_names),selected=NULL)
    updateSelectInput(session,"line6_1sec",choices = c("",all_continuous_col_names),selected=NULL)

  })  # observeEvent end

  observeEvent(input$alert_no_date,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })

  ## Load all the uploaded files to a list, this feature will be activated in the future
  # datasetlist <- eventReactive(input$uploadId,{
  #
  #   Selected_Files <- list.files("Selected_Files/")
  #   Sys.sleep(2)
  #   File_Format <- readRDS("File_Format.rds")
  #   datalist <- list()
  #   datalist <- lapply(1:length(File_Format[[1]]), function(d) read.csv(paste0("Selected_Files/",File_Format$file[d] ),
  #                                                                       header = File_Format$header[d],
  #                                                                       sep = File_Format$sep[d],
  #                                                                       dec = File_Format$dec[d],
  #                                                                       check.names = FALSE,
  #                                                                       quote = File_Format$quote[d]))
  #   names(datalist) <- paste(File_Format$index, File_Format$file,sep = ". ")
  #   return(datalist)
  #
  # })

  output$manage <- renderUI({
    data <- uploaded_data() ## datasetlist()
    print(length(data))
    selectInput("dataset", "Dataset", choices = loaded_data$name, selected = loaded_data$name)  ## names(data) if use datasetlist()

  })

  output$siteType <- renderUI({
    data <- uploaded_data() ## datasetlist()
    selectInput("siteType_input",label="Single site or multiple sites",
                choices = c("Single site","Multiple sites"),
                selected = "Single site")
  })

  output$select <- renderUI({
    data <- uploaded_data() ## datasetlist()
    radioButtons("disp", "Display", choices = c(Head = "head",Tail="tail",ColumnNames="Column names"),
                 selected = "head")
  })

  output$display_button <- renderUI({
    data <- uploaded_data() ## datasetlist()
    if (length(data) > 0 ) {
    actionButton(inputId = "displayid",label = "Display file contents",style="color:cornflowerblue;background-color:black;font-weight:bold")
    }

  })

  observeEvent(input$displayid, {

    output$contents <- renderTable({

     # data <- datasetlist()
     # sub_df <- data[[paste0(input$dataset)]]
      sub_df <- uploaded_data()
      if (isolate(input$disp == "head")) {
        return(head(sub_df))
      }
      else if (isolate(input$disp == "tail")) {
        return(tail(sub_df))
      } else {
        return(colnames(sub_df))
      }
    },type="html",bordered = TRUE,striped=TRUE,align="c")
  })

  myQuickSummary <- function(myDf){
    all.days <- seq.Date(min(myDf$Date),max(myDf$Date),by="day")
    N.missing.days <- (length(all.days)-sum(all.days %in% myDf$Date))+sum(myDf$sumNA>0)
    if (ncol(myDf)>2){
    N.days.flagged.fail <- sum(myDf$sumFail>0)
    N.days.flagged.suspect <- sum(myDf$sumSuspect>0)
    N.days.flagged.noFlagData <- sum(myDf$SumNoFlagData>0)
    }else{
      N.days.flagged.fail <- "No flag field found"
      N.days.flagged.suspect <- "No flag field found"
      N.days.flagged.noFlagData <- "No flag field found"
    }
    mySummary <- c(N.missing.days,N.days.flagged.fail,N.days.flagged.suspect,N.days.flagged.noFlagData)
    return(mySummary)
  }

  observeEvent(input$showrawTS,{

    output$display_all_raw_ts <- renderUI({
      withSpinner(plotlyOutput(outputId="all_raw_ts",width="90%",height="500px"))
    })

    isolate(input$raw_datetime_format)
    raw_data <- uploaded_data()
    ## gather all the columns were selected for time series
    my_raw_choices = c(input$line1_1sec,input$line2_1sec,input$line3_1sec,input$line4_1sec,input$line5_1sec,input$line6_1sec)
    ## remove those choices not initiated
    my_raw_choices = my_raw_choices[!grepl("choose a column",my_raw_choices)]
    print(my_raw_choices)
    print(is.null(my_raw_choices))
    if (!is.null(my_raw_choices)){
     all_raw_selected =data.frame(cbind(raw_data[,input$line1_1sec==colnames(raw_data)],raw_data[,input$line2_1sec==colnames(raw_data)],
                                     raw_data[,input$line3_1sec==colnames(raw_data)],raw_data[,input$line4_1sec==colnames(raw_data)]
                                     ))

      colnames(all_raw_selected) <- my_raw_choices
      raw_data_columns$to_plot_raw_ts <- my_raw_choices

      all_raw_selected$TimeStamp <- raw_data[,(names(raw_data) %in% raw_data_columns$date_column_name)]

     ## save(all_raw_selected,file="test_all_raw_selected.RData")

      all_raw_selected <- all_raw_selected[!duplicated(as.list(all_raw_selected))]


      if (input$raw_datetime_format=="%m%d" & is.null(input$get_the_year)){
        alert_message_to_get_year = "The date/time in this file only provides month and day, please provide the year"

        shinyalert("",alert_message_to_get_year,closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="Submit",inputId = "get_the_year",type="input")
      } # the first if end

    }

    output$all_raw_ts <- renderPlotly({

      if (ncol(all_raw_selected)>1) {
        x_date_label = "%Y-%m"
        print(input$raw_datetime_format)
        myBreaks = paste0(2," months")

        all_raw_selected_to_plot <- reshape2::melt(all_raw_selected,"TimeStamp")
        ##save(all_raw_selected_to_plot,file="test_selected_raw_data.RData")

          if (input$raw_datetime_format=="%m%d" & !is.null(input$get_the_year)){
            print(input$get_the_year)
            all_raw_selected_to_plot$TimeStamp = paste0(input$get_the_year,"-0",substr(all_raw_selected_to_plot$TimeStamp,1,1),"-",substr(all_raw_selected_to_plot$TimeStamp,2,3))
            my_datetime_format = "%Y-%m-%d"
            all_raw_ts_plot <- ggplot(data=all_raw_selected_to_plot)+
              geom_point(mapping = aes(x=as.POSIXct(TimeStamp,format=my_datetime_format),y=value,color=variable),size=0.5)+
              labs(x="Date",y=paste0(" "))+
              scale_x_datetime(date_labels=x_date_label,date_breaks="2 months")+
              theme(text=element_text(size=14,face = "bold", color="cornflowerblue"),
                    axis.text.x=element_text(angle=45, hjust=1), panel.grid.major.y=element_blank())
            all_raw_ts_plot <- ggplotly(all_raw_ts_plot,height=500,width=1200,dynamicTicks = TRUE)

          }else if(input$raw_datetime_format!="%m%d"){
            print("inside else loop now...")
            all_raw_ts_plot <- ggplot(data=all_raw_selected_to_plot)+
            geom_point(mapping = aes(x=as.POSIXct(TimeStamp,format=paste0(input$raw_datetime_format)),y=value,color=variable),size=0.5)+
            labs(x="Date",y=paste0(" "))+
            scale_x_datetime(date_labels=x_date_label,date_breaks="2 months")+
            theme(text=element_text(size=14,face = "bold", color="cornflowerblue"),
                  axis.text.x=element_text(angle=45, hjust=1), panel.grid.major.y=element_blank())
            all_raw_ts_plot <- ggplotly(all_raw_ts_plot,height=500,width=1200,dynamicTicks = TRUE)

          }  ## else end
    } ## outer if loop end
    })  ## renderPlot end
  })  ## observeEvent end

  observeEvent(input$runQS,{

    raw_data <- uploaded_data()
    ##save(raw_data,file="test_raw_data.RData")
    my_colnames <- names(raw_data)
    idx_ID_col <- str_detect(my_colnames,"SiteID") | str_detect(my_colnames,"SITEID")
    loaded_data$siteID <- unique(raw_data[idx_ID_col])
    if (length(unique(raw_data[idx_ID_col]))>1){
      shinyalert("Warning","More than one site ID is detected, please confirm if the loaded data is a single site file.",
                 closeOnClickOutside = TRUE,closeOnEsc = TRUE, confirmButtonText="OK",inputId = "alert_not_single_site")
    }else if (length(unique(raw_data[idx_ID_col]))==0){
      shinyalert("Warning","No site ID is detected, please confirm if the loaded data has the SiteID column.",
                 closeOnClickOutside = TRUE,closeOnEsc = TRUE, confirmButtonText="OK",inputId = "alert_no_siteID")
    }
    output$display_quick_summary_table <- renderUI({
      column(12,align="center",withSpinner(tableOutput("quick_summary_table")))
    })

    output$display_footnote_text <- renderUI({
     verbatimTextOutput("quick_summary_table_footnote")
    })

    ## create a quick metadata summary regarding the raw data file
    dailyCheck <- ReportMetaData(fun.myFile=NULL
                                 ,fun.myDir.import=NULL
                                 ,fun.myParam.Name=input$parameters_to_process
                                 ,fun.myDateTime.Name=raw_data_columns$date_column_name
                                 ,fun.myDateTime.Format=input$raw_datetime_format
                                 ,fun.myThreshold=20
                                 ,fun.myConfig=""
                                 ,df.input=raw_data
    )
    #save(dailyCheck, file="test_dailyCheck.RData")

    getQuickSummary <- lapply(dailyCheck,myQuickSummary)

    toReport <- as.data.frame(matrix(nrow=length(dailyCheck),ncol=5))
    colnames(toReport) <- c("Parameters","Number of days with missing data","Number of days with data flagged as fail",
                            "Number of days with data flagged as suspect","Number of days with data flagged not known")
    toReport$Parameters <- names(dailyCheck)
    for (n in 1:length(dailyCheck)){
      toReport[n,2:5] <-getQuickSummary[[n]]
    }


    output$quick_summary_table <- renderTable({
      toReport

    },align="c") # #renderTable end

    saveToReport$metadataTable <- toReport

    date_column <- raw_data[,raw_data_columns$date_column_name]
    max_date <- max(as.POSIXct(date_column,format=input$raw_datetime_format),na.rm = TRUE)
    min_date <- min(as.POSIXct(date_column,format=input$raw_datetime_format),na.rm = TRUE)
    total_N_days <- as.integer(difftime(max_date,min_date,units="days"))


    if (is.na(total_N_days)){
      shinyalert("Warning","the selected datetime format does not match what's in the data file, please check.",
                 closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "alert_datatime_format")
    }

    output$quick_summary_table_footnote <- renderText({
      Note_text_line1= paste0("Period of record: ",min_date," to ", max_date)
      Note_text_line2= paste0("Total number of days in this period: ",total_N_days," days")
      paste(Note_text_line1,Note_text_line2,sep="\n")
    })

    check_no_flags <- all(toReport[,3]=="No flag field found") && all(toReport[,4]=="No flag field found")
    print(paste0("check flags is:",check_no_flags))

    if (!check_no_flags){
    output$display_checkBoxes_dailyStats_1 <- renderUI({
      checkboxGroupInput("exclude_flagged"
                         ,"Select data points to be excluded"
                         ,choices = c("fail"="fail","suspect"="suspect","flag not known"="flag not known")
                         ,selected = "fail"

                        )
    }) # renderUI close
    } # if loop close

    output$display_radioButtons_dailyStats_2 <- renderUI({
      radioButtons("how_to_save"
                   ,"How to save daily statistics"
                   ,choices = c("Per site Per parameter"="save1","Per site with all parameters"="save2","Multiple sites together"="save3")
                   ,selected = "save2"
                   ,inline=FALSE)
    })

    output$display_actionButton_calculateDailyStatistics <- renderUI({
      actionButton(inputId="calculateDailyStatistics"
                   ,label="Calculate daily statistics"
                   ,style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_actionButton_saveDailyStatistics <- renderUI({
    actionButton(inputId="saveDailyStatistics"
                   ,label="Save daily statistics"
                   ,style="color:cornflowerblue;background-color:black;font-weight:bold;padding-left:15px;padding-right:15px;")
    })

  })  ## observeEvent end

  ## close the warning messages inside the above oberveEvent

  observeEvent(input$alert_not_single_site,{
    shinyjs::runjs("swal.close();")
  })

  observeEvent(input$alert_no_siteID,{
    shinyjs::runjs("swal.close();")
  })

  observeEvent(input$alert_datatime_format,{
    shinyjs::runjs("swal.close();")
  })


  observeEvent(input$get_the_year,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })

  ### when user clicked actionButton "calculateDailyStatistics"

  observeEvent(input$calculateDailyStatistics,{

    raw_data <- uploaded_data()
    showModal(modalDialog("Calculating the daily statistics now...",footer=NULL))

    variables_to_calculate <- input$parameters_to_process

    ## how to handle "fail" or "suspect" measurements

    if (is.null(input$exclude_flagged)){
      ContData.env$myStats.Fails.Exclude = FALSE
      ContData.env$myStats.Suspects.Exclude = FALSE
      print(input$exclude_flagged)
    }else if(length(input$exclude_flagged)==1 & input$exclude_flagged[1] == 'fail'){
      print(paste0("check the exclude flagged choices are:",input$exclude_flagged))
      ContData.env$myStats.Fails.Exclude = TRUE
      ContData.env$myStats.Suspects.Exclude = FALSE
    }else if(length(input$exclude_flagged)==1 & input$exclude_flagged[1] == 'suspect'){
      print(paste0("check the exclude flagged choices are:",input$exclude_flagged))
      ContData.env$myStats.Fails.Exclude = FALSE
      ContData.env$myStats.Suspects.Exclude = TRUE
    }else if(length(input$exclude_flagged) == 2){
      ContData.env$myStats.Fails.Exclude = TRUE
      ContData.env$myStats.Suspects.Exclude = TRUE
      print(paste0("check the exclude flagged choices are:",input$exclude_flagged))
    }

    dailyStats <- SumStats.updated(fun.myFile=NULL
                                   ,fun.myDir.import=NULL
                                   ,fun.myParam.Name=variables_to_calculate
                                   ,fun.myDateTime.Name=raw_data_columns$date_column_name
                                   ,fun.myDateTime.Format=input$raw_datetime_format
                                   ,fun.myThreshold=20
                                   ,fun.myConfig=""
                                   ,df.input=raw_data
                                   )

    #save(dailyStats, file="test_dailyStats.RData")
    processed$processed_dailyStats <- dailyStats

    removeModal()

  })


  ### when user clicked actionButton "saveDailyStatistics"
  observeEvent(input$saveDailyStatistics,{

    if (!file.exists("Output/saved_dailyStats")) dir.create(file.path("Output/saved_dailyStats"),showWarnings = FALSE, recursive = TRUE)
    name_in_file <- loaded_data$name
    if (endsWith(loaded_data$name,".csv")) name_in_file <- sub(".csv$","",loaded_data$name)
    if (endsWith(loaded_data$name,".xlsx")) name_in_file <- sub(".xlsx$","",loaded_data$name)

    if (input$how_to_save == "save2"){
       filename = paste0("Output/saved_dailyStats/saved_dailyStats_",name_in_file,"_dailyStats.csv")
       combined_data <- Reduce(full_join,processed$processed_dailyStats)
       write.csv(combined_data,file=filename,row.names=FALSE)
    }else if(input$how_to_save == "save1"){
      for (i in 1:length(processed$processed_dailyStats)){
        name_i <- names(processed$processed_dailyStats)[i]
        filename = paste0("Output/saved_dailyStats/saved_dailyStats_",name_in_file,"_",name_i,"_dailyStats.csv")
        write.csv(processed$processed_dailyStats[[i]],file=filename,row.names=FALSE)
      }
    }

  })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data Exploration####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  observeEvent(input[["tabset"]], {


    ### DE, ALL, summary table ####

    output$summary_table_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("summarise_variable_name",
                     label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$summary_table_input_2 <- renderUI({

      radioButtons("summarise_by", "Summarise by", choices = c("year/month"="year/month"
                                                               ,"year"="year"
                                                               ,"year/season"="year/season"
                                                               ,"season"="season"),
                   selected = "year/month")
    })

    output$summary_table_input_3 <- renderUI({

      selectizeInput("summarise_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$summary_table_input_4 <- renderUI({
      actionButton(inputId="display_table", label="Summarise",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })


    ## DE, ALL, time series plot" << All parameters ####

    output$time_series_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$time_series_input_2 <- renderUI({

      selectizeInput("dailyStats_ts_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$time_series_input_3 <- renderUI({
      div(
          radioButtons("dailyStats_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                           "minimum & maximum"="minMax",
                                                                           "newData"="newData"),
                       selected = "quantiles"))

    })

    output$time_series_input_4 <- renderUI({
      textInput(inputId="dailyStats_ts_title", label="Plot title",value="")
    })

    output$time_series_input_5 <- renderUI({
      actionButton(inputId="display_ts", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$time_series_input_6 <- renderUI({
      actionButton(inputId="add_more_ts", label="Add more...")
    })

    ## DE, ALL, time series - annual overlays" << All parameters ############

    output$time_series_overlay_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_ts_overlay_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$time_series_overlay_input_2 <- renderUI({

      selectizeInput("dailyStats_ts_overlay_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$time_series_overlay_input_3 <- renderUI({
      textInput(inputId="dailyStats_ts_overlay_title", label="Plot title",value="")
    })

    output$time_series_overlay_input_4 <- renderUI({
          radioButtons("overlay_shading", "Add shading with", choices = c("none"="none"
                                                                          ,"overall minimum and maximum(all years)"="overall"
                                                                          ,"newData"="newData"),
                       selected = "none")

    })

    output$time_series_overlay_input_5 <- renderUI({
      actionButton(inputId="display_ts_overlay", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    ############ DE, ALL, box plots" << All parameters ############

    output$box_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("boxplot_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$box_input_2 <- renderUI({

      selectizeInput("boxplot_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$box_input_3 <- renderUI({
      div(br(),
          radioButtons("box_group", "Group by", choices = c("month"="month"
                                                            ,"month(years side by side)"="month2"
                                                            ,"year"="year"
                                                            ,"season"="season"
                                                            ,"season(years side by side)"="season2"),
                       selected = "month"))

    })

    output$box_input_4 <- renderUI({
      textInput(inputId="box_title", label="Plot title",value="")
    })


    output$box_input_5 <- renderUI({
      actionButton(inputId="display_box", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })


    ############ DE, ALL, CompSiteCDF" << All parameters ############

    output$CDF_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("CDF_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$CDF_input_2 <- renderUI({
      div(br(),
          radioButtons("CDF_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                    "minimum & maximum"="minMax",
                                                                    "newData"="newData"),
                       selected = "minMax"))

    })

    output$CDF_input_3 <- renderUI({
          myList <- processed$processed_dailyStats
          variable_to_plot <- input$CDF_variable_name
          myData.all <- myList[[which(names(myList)==variable_to_plot)]]
          myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")

          selectizeInput("CDF_select_year",label ="Select year",
                         choices=c("All", unique(myData.all$year)),
                         multiple = FALSE,
                         selected = "All",
                         options = list(hideSelected = FALSE))
    })

    output$CDF_input_4 <- renderUI({

      selectizeInput("CDF_select_season",label ="Select season",
                     choices=c("All","Fall", "Winter", "Spring","Summer" ),
                     multiple = FALSE,
                     selected = "All",
                     options = list(hideSelected = FALSE))
    })

    output$CDF_input_5 <- renderUI({

      textInput(inputId="CDF_title", label="Plot title",value="")

    })

    output$display_CDF_button <- renderUI({
      actionButton(inputId="run_CDF", label="Run and display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    ############  DE, ALL, Raster graphs" << All parameters ############

    output$raster_input_1 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      selectizeInput("dailyStats_raster_variable_name",label ="Select variable name",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=variables_avail[1],
                     options = list(hideSelected = FALSE))
    })

    output$raster_input_2 <- renderUI({

      selectizeInput("dailyStats_raster_metrics",label ="Select daily statistics metrics",
                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                     multiple = FALSE,
                     selected="mean",
                     options = list(hideSelected = FALSE))
    })

    output$raster_input_3 <- renderUI({
      textInput(inputId="dailyStats_raster_title", label="Plot title",value="")

    })

    output$raster_input_4 <- renderUI({
      numericInput(inputId="raster_plot_aspect_ratio", label="Adjust plot aspect ratio",0.5,min=0,max=10,step=0.1)

    })

    output$raster_input_5 <- renderUI({
      radioButtons(inputId="raster_plot_color",label ="Color palette options",choices=c("hcl","rainbow","heat","terrain","topo"),selected="hcl",inline=FALSE)
    })

    output$raster_input_6 <- renderUI({
      actionButton(inputId="run_raster", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })



    ############  DE, ALL, Thermal Statistics" << Temperature ############
    output$thermal_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      site_keys_in_favor_order <- c("Site","SITE","SiteID","SITEID")
      possible_site_columns <- site_keys_in_favor_order[site_keys_in_favor_order %in% variables_avail]
      if (length(possible_site_columns)==0){
        site_to_select <- variables_avail[grep('site',variables_avail,ignore.case=TRUE)][1]
      }else{
        site_to_select <- possible_site_columns[1]
      }
      selectizeInput("thermal_SiteID_name",label ="Select SiteID Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=site_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$thermal_input_2 <- renderUI({
      variables_avail <- names(uploaded_data())
      date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
      possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
      selectizeInput("thermal_Date_name",label ="Select Date Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=possible_date_columns[1],
                     options = list(hideSelected = FALSE))
    })

    output$thermal_input_3 <- renderUI({
      variables_avail <- names(uploaded_data())
      temp_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C",
                                    "WATER_TEMP_C","Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_temp_columns <- temp_keys_in_favor_order[temp_keys_in_favor_order %in% variables_avail]
      if (length(possible_temp_columns)==0){
        temp_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
      }else{
        temp_to_select <- possible_temp_columns[1]
      }
      selectizeInput("thermal_Temp_name",label ="Select Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected= temp_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_run_thermal_button <- renderUI({
      actionButton(inputId="display_thermal", label="Display streamThermal",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_save_thermal_button <- renderUI({
      actionButton(inputId="save_thermal", label="Save thermal statistics to excel",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_help_text_thermal_statistics <- renderUI({
      verbatimTextOutput("help_text_thermal_statistics")
    })

    output$help_text_thermal_statistics <- renderText({
      filePath <- "help_text_files/Temperature_ThermalStatistics.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############ DE, TEMP, Air vs water" << Temperature ############

    output$air_vs_water_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      air_keys_in_favor_order <- c("Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
      possible_air_columns <- air_keys_in_favor_order[air_keys_in_favor_order %in% variables_avail]
      if (length(possible_air_columns)==0){
        air_to_select <- variables_avail[grep('air',variables_avail,ignore.case=TRUE)][1]
      }else{
        air_to_select <- possible_air_columns[1]
      }
      selectizeInput("air_temp_name",label ="Select Air Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=air_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$air_vs_water_input_2 <- renderUI({
      variables_avail <- names(uploaded_data())
      water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
      possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
      if (length(possible_water_columns)==0){
        water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
      }else{
        water_to_select <- possible_water_columns[1]
      }
      selectizeInput("water_temp_name",label ="Select Water Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=water_to_select,
                     options = list(hideSelected = FALSE))
    })

    air_limit_temp_tooltip_text = paste0("limit the data points with air temperature")
    output$air_vs_water_input_4 <- renderUI({
      tipify(numericInput("air_limit_temp",label ="air temperature less than this value will be excluded",0,min=-10,max=100,step=1.0),air_limit_temp_tooltip_text,placement="right",trigger="hover")
    })

    output$display_thermal_sensitivity_button <- renderUI({
      actionButton(inputId="display_thermal_sensitivity", label="Display thermal sensitivity",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_help_text_air_water <- renderUI({
      verbatimTextOutput("help_text_air_water")
    })

    output$help_text_air_water <- renderText({
      filePath <- "help_text_files/Temperature_AirWater.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, TEMP, "growing degree days" << Temperature ###############

    output$growing_degree_days_input_1 <- renderUI({
      verbatimTextOutput("come_later_text")
    })

    # output$come_later_text <- renderText({
    #   myText <- "Coming later"
    # })

    output$display_help_text_growing_degree_days <- renderUI({
      verbatimTextOutput("help_text_growing_degree_days")
    })

    output$help_text_growing_degree_days <- renderText({
      filePath <- "help_text_files/Temperature_GrowingDegreeDays.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })



    ############ DE, TEMP, thermal classification" << Temperature ############

    output$water_temp_class_input_1 <- renderUI({
      variables_avail <- names(uploaded_data())
      water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
      possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
      if (length(possible_water_columns)==0){
        water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
      }else{
        water_to_select <- possible_water_columns[1]
      }
      selectizeInput("water_temp_name_in_class",label ="Select Water Temperature Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=water_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_water_temp_class_button <- renderUI({
      actionButton(inputId="display_water_class", label="Display water temperature class",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_help_text_water_temp_class <- renderUI({
      verbatimTextOutput("help_text_water_temp_class")
    })

    output$help_text_water_temp_class <- renderText({
      filePath <- "help_text_files/Temperature_Classification.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, HYDRO, IHA" << Hydrology ############

    output$IHA_input_1 <- renderUI({
      if (length(processed$processed_dailyStats)==0){
      variables_avail <- names(uploaded_data())
      date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
      possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
      }else{
        possible_date_columns <- "Date"
        variables_avail <- possible_date_columns
      }
      selectizeInput("IHA_Date_name",label ="Select Date Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected=possible_date_columns[1],
                     options = list(hideSelected = FALSE))
    })

    output$IHA_input_2 <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)
      parameter_to_select <- variables_avail[grep('Discharge',variables_avail,ignore.case=TRUE)][1]
      selectizeInput("parameter_name",label ="Select Parameter Column",
                     choices=variables_avail,
                     multiple = FALSE,
                     selected= parameter_to_select,
                     options = list(hideSelected = FALSE))
    })

    output$display_IHA_button <- renderUI({
      actionButton(inputId="display_IHA", label="Display IHA tables",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })


    output$display_save_IHA_button <- renderUI({
      actionButton(inputId="save_IHA", label="Save IHA results to excel",style="color:cornflowerblue;background-color:black;font-weight:bold")
    })

    output$display_help_text_IHA <- renderUI({
      verbatimTextOutput("help_text_IHA")
    })

    output$help_text_IHA <- renderText({
      filePath <- "help_text_files/Hydrology_IHA.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

    ############  DE, HYDRO, Flashiness" << Hydrology ############

    output$flashiness_input_1 <- renderUI({
      verbatimTextOutput("come_later_text_1")
    })

    output$come_later_text_1 <- renderText({
      myText <- "Coming later"
    })

    output$display_help_text_flashiness <- renderUI({
      verbatimTextOutput("help_text_flashiness")
    })

    output$help_text_flashiness <- renderText({
      filePath <- "help_text_files/Hydrology_RBI.txt"
      fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
      fileText
    })

  }) #observe Event end



  #################  1:Summary table << All parameters #################
  reorderSeason <- function(seasonCol=mySeasonCol){
    seasonNames <- unique(seasonCol)
    if (length(seasonNames)==4){
      seasonCol <- factor(seasonCol,levels=c("Spring","Summer","Fall","Winter"))
    }else if(length(seasonNames)==3 & !('Winter' %in% seasonNames)){
      seasonCol <- factor(seasonCol,levels=c("Spring","Summer","Fall"))
    }else if(length(seasonNames)==3 & !('Spring' %in% seasonNames)){
      seasonCol <- factor(seasonCol,levels=c("Summer","Fall","Winter"))
    }else if(length(seasonNames)==2 & !('Winter' %in% seasonNames)&!('Spring' %in% seasonNames)){
      seasonCol <- factor(seasonCol,levels=c("Summer","Fall"))
    }
    return(seasonCol)
  }

  addSeason <- function(df=myDf){
    df[,"year"] <- format(df[,"Date"],"%Y")
    df[,"monthday"] <- format(df[,"Date"],"%m%d")
    df[,"season"] <- NA
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric("0101") & as.numeric(df[
      ,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Spring.Start)] <- "Winter"
    df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Spring.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Summer.Start)] <- "Spring"
    df[,"season"][as.numeric(df[,"monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Summer.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Fall.Start)] <- "Summer"
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Fall.Start) &
                    as.numeric(df[,"monthday"])< as.numeric(ContData.env$myTimeFrame.Season.Winter.Start)] <- "Fall"
    df[,"season"][as.numeric(df[, "monthday"]) >= as.numeric(ContData.env$myTimeFrame.Season.Winter.Start) &
                    as.numeric(df[,"monthday"])<= as.numeric("1231")] <- "Winter"
    df$season <- reorderSeason(seasonCol=df$season)
    df[,"yearseason"] <- paste(df[,"year"],df[,"season"], sep="")
    return(df)
  }

  mySummarisemore <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
       variable_col_name <- paste0(variable,".",metrics)
       names(df)[match(variable_col_name,names(df))] <- "x"
    if (timeframe == "year/month"){
      ## summarise by each year&month first
      df[,"yearmonth"] <- format(df[,"Date"],"%Y%m")
      df.summary <- doBy::summaryBy(x~yearmonth,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearmonth"],1,4)
      df.summary[,"month"] <- substr(df.summary[,"yearmonth"],5,6)
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      ## summarise by each month regardless of the year to get overall mean for each month
      df[,"month"] <- format(df[,"Date"],"%m")
      df.summary.overall <- doBy::summaryBy(x~month,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="month")
      return(df.summary.all)
    }else if(timeframe =="year"){
      df[,"year"] <- format(df[,"Date"],"%Y")
      df.summary <- doBy::summaryBy(x~year,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else if(timeframe == "year/season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~yearseason,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"year"] <- substr(df.summary[,"yearseason"],1,4)
      df.summary[,"season"] <- substr(df.summary[,"yearseason"],5,nchar(df.summary[,"yearseason"]))
      df.summary <- df.summary[2:4]
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
      df.summary.overall <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
      names(df.summary.overall)[2] <- "Overall"
      df.summary.all <- merge(df.summary.wide,df.summary.overall,by="season")
      return(df.summary.all)

    }else if(timeframe =="season"){
      df <- addSeason(df)
      df.summary <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
      df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
      df.summary.wide <- pivot_wider(df.summary,names_from=season,values_from=x.mean)
      df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
      return(df.summary.wide)
    }else{
      stop("please specify one of the following summarise timeframes:
         'year/month','year','year/season','season'")
    }
  }


  observeEvent(input$display_table, {
    output$display_summary_table_1 <- renderUI({
      withSpinner(dataTableOutput("summary_table_1"))
    })
    myList <- processed$processed_dailyStats
    variable_to_summarise <- input$summarise_variable_name
    myData <- myList[[which(names(myList)==variable_to_summarise)]]
    summary_df <- mySummarisemore(df=myData,variable=input$summarise_variable_name,metric=input$summarise_metrics,timeframe=input$summarise_by)
    table_title <- paste0(input$summarise_variable_name," ",input$summarise_metrics)
    output$summary_table_1 <- DT::renderDataTable({
      print("inside renderDT now...")
      myTable <- DT::datatable(
                 summary_df,
                 caption = htmltools::tags$caption(table_title,style="color:black;font-size:16px;font-weight:bold;text-align:center;"),
                 extensions ="Buttons",
                 rownames = FALSE,
          options = list(
          scrollX = TRUE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 15,
          dom = 'Bt',
          buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
          columnDefs = list(list(className="dt-center",targets="_all"))
          )
      ) # dataTable end
      saveToReport$summaryTable <- myTable

      print(myTable)
    })  # renderDT end

  }) # observeEvent end

  ################# 2:Time series plot << All parameters #################
  uploaded_newData<-eventReactive(c(input$uploaded_newData_file),{

    if(grepl("csv$",input$uploaded_newData_file$datapath)){
      my_data<-import_raw_data(input$uploaded_newData_file$datapath,"csv",has_header=TRUE)
    }else if(grepl("xlsx$",input$uploaded_newData_file$datapath)){
      my_data<-import_raw_data(input$uploaded_newData_file$datapath,"xlsx",has_header=TRUE)
    }else{
      shinyalert("Warning","not valid data format",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_data_not_valid")
    }

    return(my_data)
  })

  observeEvent(input$dailyStats_ts_metrics,{
    if (input$dailyStats_ts_metrics == 'mean' |input$dailyStats_ts_metrics == 'median'){
      shinyjs::show("cp_shaded_region")
    }else{
      shinyjs::hide("cp_shaded_region")
    }
  })

  observeEvent(input$dailyStats_shading,{
    if (input$dailyStats_shading == 'newData'){
      shinyjs::show("cp_new_data")
    }else{
      shinyjs::hide("cp_new_data")
    }
  })

  observeEvent(input$uploaded_newData_file,{
    cols_avail <- colnames(uploaded_newData())
    #print(cols_avail)
    updateSelectizeInput(session,"newData_lower_col",label ="Select column to be used as lower bound",
                         choices=cols_avail,
                         selected=NULL
    )

    updateSelectizeInput(session,"newData_upper_col",label ="Select column to be used as upper bound",
                         choices=cols_avail,
                         selected=NULL,
                         options = list(hideSelected = FALSE))

    updateSelectizeInput(session,"newData_date_col",label ="Select date column",
                         choices=cols_avail,
                         selected=NULL,
                         options = list(hideSelected = FALSE))
  })

  observeEvent(input$display_ts, {
    output$display_time_series <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_ts",height="550px",width="1200px"),type=2)
    })
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_ts_variable_name,".",input$dailyStats_ts_metrics)
    ## dynamically change the "date_breaks" based on the width of the time window

    time_range <- difftime(max(as.POSIXct(myData$Date,format="%Y-%m-%d")),min(as.POSIXct(myData$Date,format="%Y-%m-%d")),units="days")
    if (as.numeric(time_range)<365*2){
      myBreaks = paste0(1," months")
      x_date_label = "%Y-%m-%d"
    }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
      myBreaks = paste0(2," months")
      x_date_label = "%Y-%m-%d"
    }else{
      myBreaks = paste0(6," months")
      x_date_label = "%Y-%m"
    }

    if (input$dailyStats_shading=="quantiles"){
      upper_col <- paste0(input$dailyStats_ts_variable_name,".q.75%")
      lower_col <- paste0(input$dailyStats_ts_variable_name,".q.25%")
      shading_text <- paste0(input$dailyStats_ts_variable_name, " between daily 25th percentiles and 75th percentiles")
    }else if (input$dailyStats_shading=="minMax"){
      upper_col <- paste0(input$dailyStats_ts_variable_name,".min")
      lower_col <- paste0(input$dailyStats_ts_variable_name,".max")
      shading_text <- paste0(input$dailyStats_ts_variable_name, " between daily minimum and maximum values")
    }else if (input$dailyStats_shading=="newData"){

    }

    if ((input$dailyStats_ts_metrics=="mean"|input$dailyStats_ts_metrics=="median")&input$dailyStats_shading!="newData"){
       cols_selected = c("Date",mean_col,lower_col,upper_col)
       data_to_plot <- myData[cols_selected]
       if (!all(is.na(data_to_plot[,mean_col]))){

         output$plot_dailyStats_ts <- renderPlot({
         p1 <- ggplot(data_to_plot)+
         geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
         geom_ribbon(aes(ymin=!!sym(lower_col),ymax=!!sym(upper_col),x=as.POSIXct(Date,format="%Y-%m-%d"),fill=shading_text),alpha=0.5)+
         scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
         labs(title=isolate(input$dailyStats_ts_title),x = "Date",y = mean_col)+
         theme_minimal()+
         scale_colour_manual("", values = "blue")+
         scale_fill_manual("", values = "grey12")+
         theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
               ,plot.title = element_text(hjust=0.5)
               ,plot.background = element_rect(color="grey20",size=2)
               ,legend.position = "bottom"
               ,axis.text.x=element_text(angle=45, hjust=1))
        #ggplotly(p1)
         print(p1)
         })  # renderPlot close
       }else{
              shinyalert("Warning","No data available to plot for the selected variable!"
                         ,closeOnClickOutside = TRUE
                         ,closeOnEsc = TRUE
                         ,confirmButtonText="OK"
                         ,inputId = "alert_data_not_avail_for_ts")
       }##inner if else loop close

    }else if ((input$dailyStats_ts_metrics=="mean"|input$dailyStats_ts_metrics=="median")&input$dailyStats_shading=="newData"){
      shading_data <- uploaded_newData()
      shading_cols_selected <- c(input$newData_date_col,input$newData_lower_col,input$newData_upper_col)
      data_to_add_as_shading <- shading_data[shading_cols_selected]

      data_to_plot <- myData[c("Date",mean_col)]
      output$plot_dailyStats_ts <- renderPlot({
        p1 <- ggplot(data_to_plot)+
          geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
          geom_ribbon(data=data_to_add_as_shading,aes(x=as.POSIXct(!!sym(input$newData_date_col),format="%Y-%m-%d"),
                                                      ymin=isolate(!!sym(input$newData_lower_col)),ymax=isolate(!!sym(input$newData_upper_col)),fill=isolate(input$newData_name)),alpha=0.5)+
          scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
          scale_fill_manual("",labels=isolate(input$newData_name),values=c("grey80"="grey80"))+
          labs(title=isolate(input$dailyStats_ts_title), x = "Date",y = mean_col)+
          theme_minimal()+
          scale_colour_manual("", values = "blue")+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "bottom"
                ,axis.text.x=element_text(angle=45, hjust=1))
        #ggplotly(p1)
        print(p1)
      })  # renderPlot close

    }else{
      cols_selected = c("Date",mean_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){

        output$plot_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$dailyStats_ts_title), x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    }

  })  # observeEvent end

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_ts,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  observeEvent(input$add_more_ts, {

    output$another_time_series_UI <- renderUI({
      variables_avail <- names(processed$processed_dailyStats)

      sidebarLayout(
        sidebarPanel(width=3,id="ts_sidePanel",
                     h2(strong("UI for another time series plot"),style = "font-size:150%;font-weight:bold;color:#404040;"),
                     hr(),
                     selectizeInput("another_dailyStats_ts_variable_name",label ="Select variable name",
                                    choices=variables_avail,
                                    multiple = FALSE,
                                    selected=variables_avail[length(variables_avail)],
                                    options = list(hideSelected = FALSE)),
                     hr(),
                     selectizeInput("another_dailyStats_ts_metrics",label ="Select daily statistics metrics",
                                    choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                    multiple = FALSE,
                                    selected="mean",
                                    options = list(hideSelected = FALSE)),
                     div(
                       id = "another_cp_shaded_region",
                       conditionalPanel(
                         condition = "input$another_dailyStats_ts_metrics == 'mean'|input$another_dailyStats_ts_metrics == 'median'",
                         hr(),
                         radioButtons("another_dailyStats_shading", "Add shading with", choices = c("25th & 75th percentiles"="quantiles",
                                                                                                    "minimum & maximum"="minMax",
                                                                                                    "newData"="newData"),
                                      selected = "quantiles"),
                       ), #conditionalPanel end
                     ), # div end
                     hr(),
                     textInput(inputId="another_dailyStats_ts_title", label="Plot title",value=""),
                     hr(),
                     fluidRow(column(width=6,
                                     actionButton(inputId="display_another_ts", label="Display",style="color:cornflowerblue;background-color:black;font-weight:bold")
                                     ), # column close
                              column(width=6,align="right",
                                     actionButton("toggleLayout",label="Hide")
                                     ) # column close
                     )
        ),
        mainPanel(width=9,id="ts_mainPanel",
                  fluidRow(column(width=9,uiOutput("display_another_time_series"))),
                  br(),
                  br()

        ) # mainPanel end

      ) # sidebarLayout end

    })  # renderUI close

  })  # observeEvent end

  observeEvent(input$toggleLayout,{
    shinyjs::toggle(id="ts_sidePanel")
    shinyjs::toggle(id="ts_mainPanel")
  })

  observeEvent(input$another_dailyStats_ts_metrics,{
    if (input$another_dailyStats_ts_metrics == 'mean' |input$another_dailyStats_ts_metrics == 'median'){
      shinyjs::show("another_cp_shaded_region")
    }else{
      shinyjs::hide("another_cp_shaded_region")
    }
  })


  observeEvent(input$display_another_ts, {
    output$display_another_time_series <- renderUI({
      withSpinner(plotOutput("plot_another_dailyStats_ts",height="550px",width="1200px"),type=2)
    })
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$another_dailyStats_ts_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$another_dailyStats_ts_variable_name,".",input$another_dailyStats_ts_metrics)
    if (input$another_dailyStats_shading=="quantiles"){
      upper_col <- paste0(input$another_dailyStats_ts_variable_name,".q.75%")
      lower_col <- paste0(input$another_dailyStats_ts_variable_name,".q.25%")
      shading_text <- paste0(input$another_dailyStats_ts_variable_name, " between daily 25th percentiles and 75th percentiles")
    }else if (input$another_dailyStats_shading=="minMax"){
      upper_col <- paste0(input$another_dailyStats_ts_variable_name,".min")
      lower_col <- paste0(input$another_dailyStats_ts_variable_name,".max")
      shading_text <- paste0(input$another_dailyStats_ts_variable_name, " between daily minimum and maximum values")
    }

    ## dynamically change the "date_breaks" based on the width of the time window

    time_range <- difftime(max(as.POSIXct(myData$Date,format="%Y-%m-%d")),min(as.POSIXct(myData$Date,format="%Y-%m-%d")),units="days")
    if (as.numeric(time_range)<365*2){
      myBreaks = paste0(1," months")
      x_date_label = "%Y-%m-%d"
    }else if(as.numeric(time_range)>=365*2&as.numeric(time_range)<365*5){
      myBreaks = paste0(2," months")
      x_date_label = "%Y-%m-%d"
    }else{
      myBreaks = paste0(6," months")
      x_date_label = "%Y-%m"
    }

    if (input$another_dailyStats_ts_metrics=="mean"){
      cols_selected = c("Date",mean_col,lower_col,upper_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){

        output$plot_another_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            geom_ribbon(aes(ymin=!!sym(lower_col),ymax=!!sym(upper_col),x=as.POSIXct(Date,format="%Y-%m-%d"),fill=shading_text),alpha=0.5)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$another_dailyStats_ts_title),x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            scale_fill_manual("", values = "grey12")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close

    }else{
      cols_selected = c("Date",mean_col)
      data_to_plot <- myData[cols_selected]
      if (!all(is.na(data_to_plot[,mean_col]))){

        output$plot_another_dailyStats_ts <- renderPlot({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(y=!!sym(mean_col),x=as.POSIXct(Date,format="%Y-%m-%d"),colour=mean_col),size=0.8)+
            scale_x_datetime(date_labels=x_date_label,date_breaks=myBreaks)+
            labs(title=isolate(input$another_dailyStats_ts_title), x = "Date",y = mean_col)+
            theme_minimal()+
            scale_colour_manual("", values = "blue")+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "bottom"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1)
          print(p1)
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    }

  })  # observeEvent end


  #################  3:Time series - Annual overlays << All parameters #################
  uploaded_overlay_newData <- eventReactive(c(input$uploaded_overlay_newData_file),{

    if(grepl("csv$",input$uploaded_overlay_newData_file$datapath)){
      my_data<-import_raw_data(input$uploaded_overlay_newData_file$datapath,"csv",has_header=TRUE)
    }else if(grepl("xlsx$",input$uploaded_overlay_newData_file$datapath)){
      my_data<-import_raw_data(input$uploaded_overlay_newData_file$datapath,"xlsx",has_header=TRUE)
    }else{
      shinyalert("Warning","not valid data format",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_data_not_valid")
    }

    return(my_data)
  })

  observeEvent(input$overlay_shading,{
    if (input$overlay_shading == 'newData'){
      shinyjs::show("cp_new_data_overlay")
    }else{
      shinyjs::hide("cp_new_data_overlay")
    }
  })

  observeEvent(input$uploaded_overlay_newData_file,{
    cols_avail <- colnames(uploaded_overlay_newData())
    #print(cols_avail)
    updateSelectizeInput(session,"overlay_newData_lower_col",label ="Select column to be used as lower bound",
                         choices=cols_avail,
                         selected=''
    )

    updateSelectizeInput(session,"overlay_newData_longterm_col",label ="Select column to be used as long-term reference line",
                         choices=cols_avail,
                         selected=''
    )

    updateSelectizeInput(session,"overlay_newData_upper_col",label ="Select column to be used as upper bound",
                         choices=cols_avail,
                         selected='',
                         options = list(hideSelected = FALSE))

    updateSelectizeInput(session,"overlay_newData_date_col",label ="Select month-day column",
                         choices=cols_avail,
                         selected='',
                         options = list(hideSelected = FALSE))
  })




  observeEvent(input$display_ts_overlay, {

    output$display_time_series_overlay <- renderUI({
      withSpinner(plotlyOutput("plot_dailyStats_ts_overlay",height="550px",width="1200px"),type=2)
    })

    output$display_time_series_overlay_1 <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_ts_overlay_1",height="550px",width="1200px"),type=2)
    })

    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_ts_overlay_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_ts_overlay_variable_name,".",input$dailyStats_ts_overlay_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
    ## dynamically change the "date_breaks" based on the width of the time window

    if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="overall"){
    ## calculate overall(include all years) monthly minimum and maximum values
      min_col <- paste0(input$dailyStats_ts_overlay_variable_name,".min")
      max_col <- paste0(input$dailyStats_ts_overlay_variable_name,".max")
      data_for_overlay <- myData[c("Date",min_col,max_col)]
      data_for_overlay[,"MonthDay"] <- format(data_for_overlay[,"Date"],"%m-%d")
      monthDay_min <- aggregate(data_for_overlay[,2],list(data_for_overlay$MonthDay),FUN=mean)
      monthDay_max <- aggregate(data_for_overlay[,3],list(data_for_overlay$MonthDay),FUN=mean)
      merged_overlay <- merge(monthDay_min,monthDay_max,by="Group.1")
      colnames(merged_overlay) <- c("MonthDay","min","max")
      #save(merged_overlay,file="test_overall_min_max_overlay.RData")
      output$plot_dailyStats_ts_overlay <- renderPlotly({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
            geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                        ymin=min,ymax=max,
                                                fill="overall minimum and maximum" ),alpha=0.5)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          ggplotly(p1,dynamicTicks = FALSE)
          print(p1)
        })
      }) # renderPlot close

      output$plot_dailyStats_ts_overlay_1 <- renderPlot({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=isolate(!!sym(mean_col)),colour=year),size=0.8)+
            geom_ribbon(data=merged_overlay,aes(x=as.Date(yday(paste0("2000","-",MonthDay)),"2000-01-01"),
                                                ymin=min,ymax=max,
                                                fill="overall minimum and maximum" ),alpha=0.5)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            scale_fill_manual(" ",labels="overall minimum and maximum",values=c("grey80"="grey80"))+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #ggplotly(p1,dynamicTicks = FALSE)
          print(p1)
        })
      }) # renderPlot close

    }else if ((input$dailyStats_ts_overlay_metrics=="mean"|input$dailyStats_ts_overlay_metrics=="median")&input$overlay_shading=="newData") {
      overlay_data <- uploaded_overlay_newData()


     # save(data_to_add_as_overlay,file="test_data_to_add_as_overlay.RData")

      output$plot_dailyStats_ts_overlay <- renderPlotly({
        isolate({

          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)

          if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
            overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
            data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
             p1<- p1 +
             geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
                                                      ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
                                                      fill=input$overlay_newData_name),alpha=0.5)+
             scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
          }

          if(input$overlay_newData_longterm_col!=''){
          data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
          print(input$overlay_newData_longterm_col)
          p1 <- p1 +
            geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
          }

         p1 <- p1+
          scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
          labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
          theme_classic()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                ,plot.title = element_text(hjust=0.5)
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
                ,axis.text.x=element_text(angle=45, hjust=1))
         p1<- plotly::ggplotly(p1)
         print(p1)

      })
      }) # renderPlot close

      output$plot_dailyStats_ts_overlay_1 <- renderPlot({
        isolate({
          p1 <- ggplot(data_to_plot)+
            geom_line(aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col),colour=year),size=0.8)
            if((input$overlay_newData_lower_col!='')&(input$overlay_newData_upper_col!='')){
              overlay_cols_selected <- c(input$overlay_newData_date_col,input$overlay_newData_lower_col,input$overlay_newData_upper_col)
              data_to_add_as_overlay <- overlay_data[overlay_cols_selected]
              p1 <- p1+
              geom_ribbon(data=data_to_add_as_overlay,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),
                                                          ymin=!!sym(input$overlay_newData_lower_col),ymax=!!sym(input$overlay_newData_upper_col),
                                                          fill=input$overlay_newData_name),alpha=0.5)+
              scale_fill_manual(" ",labels=input$overlay_newData_name,values=c("grey80"="grey80"))
            }

            if(input$overlay_newData_longterm_col!=''){
            data_to_add_as_longterm <- overlay_data[c(input$overlay_newData_date_col,input$overlay_newData_longterm_col)]
            p1 <- p1 + geom_line(data=data_to_add_as_longterm,aes(x=as.Date(yday(paste0("2000","-",month_day)),"2000-01-01"),y=!!sym(input$overlay_newData_longterm_col),color="USGS long-term"),size=0.8,color="black")
            }

          p1 <- p1+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
          #p1<- plotly::ggplotly(p1)
          print(p1)

        })
      }) # renderPlot close

    }else{
      if (!all(is.na(data_to_plot[,mean_col]))){

        output$plot_dailyStats_ts_overlay <- renderPlotly({
          p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=!!sym(mean_col)))+
            geom_line(aes(colour=year),size=0.8)+
            scale_x_date(date_breaks="1 month",limits=c(as.Date("2000-01-01"),as.Date("2000-12-31")),date_labels = "%m%d")+
            labs(title=isolate(input$dailyStats_ts_overlay_title),x = "MonthDay",y =mean_col)+
            theme_classic()+
            theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                  ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                  ,plot.title = element_text(hjust=0.5)
                  ,plot.background = element_rect(color="grey20",size=2)
                  ,legend.position = "right"
                  ,axis.text.x=element_text(angle=45, hjust=1))
            ggplotly(p1,dynamicTicks = FALSE) %>% plotly::layout()
        })  # renderPlot close
      }else{
        shinyalert("Warning","No data available to plot for the selected variable!"
                   ,closeOnClickOutside = TRUE
                   ,closeOnEsc = TRUE
                   ,confirmButtonText="OK"
                   ,inputId = "alert_data_not_avail_for_ts")
      }##inner if else loop close
    } ## outer if else loop close

  }) ##observeEvent end

  #################  4:Boxplots << All parameters #################

  observeEvent(input$display_box, {

    output$display_box_plots <- renderUI({
      withSpinner(plotlyOutput("plot_dailyStats_box",height="600px",width="1200px"),type=2)
    })

    myList <- processed$processed_dailyStats
    variable_to_plot <- input$boxplot_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$boxplot_variable_name,".",input$boxplot_metrics)
    if(input$box_group=="year"){
    myData[,input$box_group] <- format(myData[,"Date"],"%Y")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month"){
    myData[,input$box_group] <- format(myData[,"Date"],"%m")
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="season"){
    myData <- addSeason(myData)
    cols_selected = c("Date",input$box_group,mean_col)
    }else if(input$box_group=="month2"){
    myData[,"year"] <- format(myData[,"Date"],"%Y")
    myData[,"month"] <- format(myData[,"Date"],"%m")
    cols_selected = c("Date","year","month",mean_col)
    }else if(input$box_group=="season2"){
    myData <- addSeason(myData)
    cols_selected = c("Date","year","season",mean_col)
    }

    data_to_plot <- myData[cols_selected]
    if (!all(is.na(data_to_plot[,mean_col]))&input$box_group!="month2"&input$box_group!="season2"){
    output$plot_dailyStats_box <- renderPlotly({

      p2 <- ggplot(data=data_to_plot,aes(x=!!sym(isolate(input$box_group)),y=!!sym(isolate(mean_col)))) +
        geom_boxplot()+
        labs(title=isolate(input$box_title),x = isolate(input$box_group),y = isolate(input$boxplot_variable_name))+
        theme_bw()+
        theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
              ,plot.title = element_text(hjust=0.5)
              ,axis.text.x = element_text(angle=0, hjust=1))
      p2 <- ggplotly(p2)
      print(p2)
    })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="month2"){
      output$plot_dailyStats_box <- renderPlotly({

        p2 <- ggplot(data=data_to_plot,aes(x=month,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot(position=position_dodge(width=0.1))+
          labs(title=isolate(input$box_title),x = "month",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        ggplotly(p2) %>% plotly::layout(boxmode="group")
      })
    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="season2"){
      output$plot_dailyStats_box <- renderPlotly({

        data_to_plot$season = reorderSeason(data_to_plot$season)
        p2 <- ggplot(data=data_to_plot,aes(x=season,y=!!sym(isolate(mean_col)),fill=year)) +
          geom_boxplot(position=position_dodge(width=0.1))+
          labs(title=isolate(input$box_title),x = "season",y = isolate(input$boxplot_variable_name))+
          theme_bw()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,axis.text.x = element_text(angle=0, hjust=1))
        ggplotly(p2) %>% plotly::layout(boxmode="group")
      })
    }else{
      shinyalert("Warning","No data available to plot for the selected variable!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                 confirmButtonText="OK",inputId = "alert_data_not_avail_for_box")
    }

  })  #observeEvent end

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_box,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  #################  5:CDF << All parameters  #################

  observeEvent(input$run_CDF, {

    output$display_plot_CDF <- renderUI({
      withSpinner(plotOutput("plot_CDF",height="600px",width="1200px"),type=2)
    })


    myList <- processed$processed_dailyStats
    variable_to_plot <- input$CDF_variable_name
    myData.all <- myList[[which(names(myList)==variable_to_plot)]]

    if (input$CDF_select_year=="All"){
      myData <- myData.all
    }else{
      myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
      myData <- myData.all[myData.all$year==input$CDF_select_year,]
    }
    mean_col <- paste0(input$CDF_variable_name,".mean")
    if (input$CDF_shading=="quantiles"){
      upper_col <- paste0(input$CDF_variable_name,".q.75%")
      lower_col <- paste0(input$CDF_variable_name,".q.25%")
    }else if (input$CDF_shading=="minMax"){
      lower_col <- paste0(input$CDF_variable_name,".min")
      upper_col <- paste0(input$CDF_variable_name,".max")
    }
    cols_selected = c("Date",mean_col,lower_col,upper_col)
    data.plot <- myData[cols_selected]

    if (input$CDF_select_season=="All"){
      season.choice = NULL
    }else{
      season.choice = input$CDF_select_season
    }

    output$plot_CDF <- renderPlot({

      # g <- ggplot(data=data.plot,aes(x=!!sym(mean_col)))+
      #      geom_step(stat="ecdf")
      # inside <- ggplot_build(g)
      # matched <- merge(inside$data[[1]],data.frame(x=data.plot[,names(data.plot)==mean_col]
      #                                              ,data.plot[,names(data.plot)==lower_col]
      #                                              ,data.plot[,names(data.plot)==upper_col]),by=("x"))
      # names(matched)[ncol(matched)] <- "data.plot.max"
      # names(matched)[ncol(matched)-1] <-"data.plot.min"
      # CDF_plot <- g+geom_ribbon(data=matched,aes(x=x,ymin=ecdf(data.plot.min)(x),ymax=ecdf(data.plot.max)(x)),alpha=0.5,fill="green")

      CDF_plot <- CompSiteCDF.updated(file.input = NULL
                                     , dir.input = getwd()
                                     , dir.output = getwd()
                                     , Param.Name = mean_col
                                     , Shaded.Names = c(lower_col,upper_col)
                                     , Plot.title = isolate(input$CDF_title)
                                     , Plot.season = isolate(season.choice)
                                     , hist.columnName = NULL
                                     , df.input = data.plot)
      if (is.null(CDF_plot)){
        shinyalert("Warning","No data available to plot for the selected variable/year/season!",closeOnClickOutside = TRUE,closeOnEsc = TRUE,
                   confirmButtonText="OK",inputId = "alert_data_not_avail_for_CDF")
      }
      print(CDF_plot)
    })


  }) # observeEvent close

  ## close the alert messages
  observeEvent(input$alert_data_not_avail_for_CDF,{
    #print(input$alert_no_date)
    shinyjs::runjs("swal.close();")
  })


  #################  6:Raster graphs << All parameters #################
  observeEvent(input$run_raster, {
    output$display_raster_graphs <- renderUI({
      withSpinner(plotOutput("plot_dailyStats_raster",height="550px",width="1200px"),type=2)
    })

    myMonth <- seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="1 month")
    month_numeric <- lubridate::yday(myMonth)/365*52+1
    month_label <- lubridate::month(myMonth,label=TRUE)
    myList <- processed$processed_dailyStats
    variable_to_plot <- input$dailyStats_raster_variable_name
    myData <- myList[[which(names(myList)==variable_to_plot)]]
    mean_col <- paste0(input$dailyStats_raster_variable_name,".",input$dailyStats_raster_metrics)
    cols_selected <- c("Date",mean_col)
    data_to_plot <- myData[cols_selected]
    data_to_plot[,"year"] <- format(data_to_plot[,"Date"],"%Y")
    if (input$raster_plot_color=="hcl"){
      colorV <- hcl.colors(12)
    }else if(input$raster_plot_color=="rainbow"){
      colorV <- rainbow(12)
    }else if(input$raster_plot_color=="terrain"){
      colorV <- terrain.colors(12)
    }else if (input$raster_plot_color=="heat"){
      colorV <- heat.colors(12)
    }else if (input$raster_plot_color=="topo"){
      colorV <- topo.colors(12)
    }
    #data_to_plot[,"yday"] <- lubridate::yday(as.Date(data_to_plot[,"Date"],format="%Y-%m-%d"))
    if (!all(is.na(data_to_plot[,mean_col]))){
    output$plot_dailyStats_raster <- renderPlot({
         p1 <- ggplot(data_to_plot,aes(x=as.Date(yday(Date),"2000-01-01"),y=year))+
           geom_raster(aes(fill=!!sym(mean_col)))+
           coord_equal()+
           scale_fill_gradientn(name=mean_col,na.value="white",colours=colorV)+
           scale_x_date(date_breaks="1 month",date_labels = "%b")+
           scale_colour_manual(values=NA)+
           labs(title=isolate(input$dailyStats_raster_title), x = "month",y = "year")+
           guides(color=guide_legend("No data",override.aes = list(fill="white")))+
           theme_classic()+
           theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                 ,panel.border = element_rect(colour="black",fill=NA, size=0.5)
                 ,plot.title = element_text(hjust=0.5)
                 ,aspect.ratio = isolate(input$raster_plot_aspect_ratio)
                 ,plot.background = element_rect(color="grey20",size=2)
                 ,legend.position = "right"
                 )
         #ggplotly(p1)
         print(p1)
       })  # renderPlot close

  }else{
    shinyalert("Warning","No data available to plot for the selected variable!"
               ,closeOnClickOutside = TRUE
               ,closeOnEsc = TRUE
               ,confirmButtonText="OK"
               ,inputId = "alert_data_not_avail_for_ts")
  }##inner if else loop close

  }) ##observeEvent end

  #################  1: Thermal Statistics << Temperature  #################

  observeEvent(input$display_thermal, {

    hide("help_text_thermal_statistics")

  output$display_thermal_table_1 <- renderUI({
    withSpinner(dataTableOutput("thermal_statistics_table_1"))
  })

  output$display_thermal_table_2 <- renderUI({
    dataTableOutput("thermal_statistics_table_2")
  })

  output$display_thermal_table_3 <- renderUI({
    dataTableOutput("thermal_statistics_table_3")
  })

  output$display_thermal_table_4 <- renderUI({
    dataTableOutput("thermal_statistics_table_4")
  })

  output$display_thermal_table_5 <- renderUI({
    withSpinner(dataTableOutput("thermal_statistics_table_5"))
  })

  myData <- uploaded_data()

  streamThermal_exported <- Export.StreamThermal(myData
                                                 ,fun.col.SiteID = input$thermal_SiteID_name
                                                 ,fun.col.Date = input$thermal_Date_name
                                                 ,fun.col.Temp = input$thermal_Temp_name
                                                 )

  ##save(streamThermal_exported, file="test_streamThermal_exported.RData")
  ST.freq <- T_frequency(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.mag  <- T_magnitude(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.roc  <- T_rateofchange(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.tim  <- T_timing(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
  ST.var  <- T_variability(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)

  processed$ST.freq <- ST.freq
  processed$ST.mag <- ST.mag
  processed$ST.roc <- ST.roc
  processed$ST.tim <- ST.tim
  processed$ST.var <- ST.var

  thermal.statistics.table.options <- list(
    scrollX = TRUE, #allow user to scroll wide tables horizontally
    stateSave = FALSE,
    pageLength = 15,
    dom = 'Bt',
    buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
    columnDefs = list(list(className="dt-center",targets="_all"))
  )

  output$thermal_statistics_table_1 <- DT::renderDataTable({
   table.title.1 <- "Frequency"
    myTable <- DT::datatable(
      ST.freq,
      caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end


  output$thermal_statistics_table_2 <- DT::renderDataTable({
    table.title.2 <- "Magnitude"
    myTable <- DT::datatable(
      ST.mag,
      caption = htmltools::tags$caption(table.title.2,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end

  output$thermal_statistics_table_3 <- DT::renderDataTable({
    table.title.3 <- "Rate of Change"
    myTable <- DT::datatable(
      ST.roc,
      caption = htmltools::tags$caption(table.title.3,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end

  output$thermal_statistics_table_4 <- DT::renderDataTable({
    table.title.4 <- "Timing"
    myTable <- DT::datatable(
      ST.tim,
      caption = htmltools::tags$caption(table.title.4,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end

  output$thermal_statistics_table_5 <- DT::renderDataTable({
    table.title.5 <- "Variability"
    myTable <- DT::datatable(
      ST.var,
      caption = htmltools::tags$caption(table.title.5,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
      extensions ="Buttons",
      rownames = FALSE,
      options = thermal.statistics.table.options
    ) # dataTable end
    print(myTable)
  })  # renderDT end

  }) #observeEvent end

  observeEvent(input$save_thermal, {
    require(XLConnect)

    # Descriptions
    #
    Desc.freq <- "Frequency metrics indicate numbers of days in months or seasons
that key events exceed user-defined temperatures. "
    #
    Desc.mag <- "Magnitude metrics characterize monthly and seasonal averages and
the maximum and minimum from daily temperatures as well as 3-, 7-, 14-, 21-,
and 30-day moving averages for mean and maximum daily temperatures."
    #
    Desc.roc <- "Rate of change metrics include monthly and seasonal rate of
change, which indicates the difference in magnitude of maximum and minimum
temperatures divided by number of days between these events."
    #
    Desc.tim <- "Timing metrics indicate Julian days of key events including
mean, maximum, and minimum temperatures; they also indicate Julian days of
mean, maximum, and minimum values over moving windows of specified size."
    #
    Desc.var <- "Variability metrics summarize monthly and seasonal range in
daily mean temperatures as well as monthly coefficient of variation of daily
mean, maximum, and minimum temperatures. Variability metrics also include
moving averages for daily ranges and moving variability in extreme
temperatures, calculated from differences in average high and low
temperatures over various time periods"
    #
    Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
    df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var")
                                     ,Group.Desc))
    SiteID <- processed$ST.freq[1,1]
    myDate <- format(Sys.Date(),"%Y%m%d")
    myTime <- format(Sys.time(),"%H%M%S")
    Notes.User <- Sys.getenv("USERNAME")

    Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
                     , "Analysis.Time (HHMMSS)", "Analysis.User")
    Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
    df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
    ## New File Name
    if (!file.exists("Output/saved_streamThermal/")) dir.create(file.path("Output/saved_streamThermal"),showWarnings = FALSE, recursive = TRUE)
    name_in_file <- loaded_data$name
    myFile.XLSX <- paste("Output/saved_streamThermal/StreamThermal"
                         , name_in_file
                         , SiteID
                         , myDate
                         , myTime
                         , "xlsx"
                         , sep=".")
    ## Copy over template with Metric Definitions
    file.copy(file.path(path.package("ContDataQC")
                        ,"extdata"
                        ,"StreamThermal_MetricList.xlsx")
              , myFile.XLSX)
    ## load workbook, create if not existing
    wb <- loadWorkbook(myFile.XLSX, create = TRUE)
    # create sheets
    createSheet(wb, name = "NOTES")
    createSheet(wb, name = "freq")
    createSheet(wb, name = "mag")
    createSheet(wb, name = "roc")
    createSheet(wb, name = "tim")
    createSheet(wb, name = "var")
    # write to worksheet
    writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
    writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=10)
    writeWorksheet(wb, processed$ST.freq, sheet = "freq")
    writeWorksheet(wb, processed$ST.mag, sheet = "mag")
    writeWorksheet(wb, processed$ST.roc, sheet = "roc")
    writeWorksheet(wb, processed$ST.tim, sheet = "tim")
    writeWorksheet(wb, processed$ST.var, sheet = "var")
    # save workbook
    saveWorkbook(wb, myFile.XLSX)

  }) # observeEvent close


  #################  2: Thermal Sensitivity << Temperature  #################
  observeEvent(input$exclude_data_points,{
    if (input$exclude_data_points == 'Yes'){
      shinyjs::show("cp_air_temp")
    }else{
      shinyjs::hide("cp_air_temp")
    }
  })


  observeEvent(input$display_thermal_sensitivity, {

    hide("help_text_air_water")

    output$display_thermal_sensitivity_plot_1 <- renderUI({
      withSpinner(plotOutput("thermal_sensitivity_plot_1"))
    })

    myList <- processed$processed_dailyStats
    ## check if both of "Air.Temp.C" and "Water.Temp.C" are available
    if(all(names(myList) %in% c(input$air_temp_name,input$water_temp_name))){
      myData.Air <- myList[[which(names(myList)==input$air_temp_name)]]
      myData.Water <- myList[[which(names(myList)==input$water_temp_name)]]
      mean_col_air <- paste0(input$air_temp_name,".mean")
      mean_col_water <- paste0(input$water_temp_name,".mean")
      data_air_to_plot <- myData.Air[c("Date",mean_col_air)]
      data_water_to_plot <- myData.Water[c("Date",mean_col_water)]
      data_to_plot <- merge(data_air_to_plot,data_water_to_plot,by="Date")
      if (input$exclude_data_points=="Yes"){
        data_to_plot <- data_to_plot[data_to_plot$Air.Temp.C.mean>input$air_limit_temp,]
      }
      data_to_model <- data_to_plot
      names(data_to_model)[match(mean_col_water,names(data_to_model))] <- "y"
      names(data_to_model)[match(mean_col_air,names(data_to_model))] <- "x"
      myModel <- lm(y ~ x,data_to_model,na.action=na.exclude)
      myEquation <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                               list(a = format(unname(coef(myModel)[1]), digits = 2),
                                    b = format(unname(coef(myModel)[2]), digits = 2),
                                    r2 = format(summary(myModel)$r.squared, digits = 3)))

      output$thermal_sensitivity_plot_1 <- renderPlot({
        p1 <- ggplot(data_to_plot,aes(x=!!sym(mean_col_air),y=!!sym(mean_col_water)))+
          geom_point(alpha=0.5,size=1.5)+
          geom_smooth(method="loess",se=FALSE,color="black")+
          geom_smooth(method="lm",se=FALSE,color="cornflowerblue",linetype="dashed",size=2)+
          geom_text(x=(min(data_to_plot[,mean_col_air],na.rm=TRUE)+5)
                    ,y=(max(data_to_plot[,mean_col_water],na.rm=TRUE)-1.5)
                    ,label=as.character(as.expression(myEquation))
                    ,color="cornflowerblue"
                    ,size=8
                    ,parse= TRUE)+
          labs(x = "Air Temperature",y = "Water Temperature")+
          theme_minimal()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue"),
                plot.background = element_rect(color="grey20",size=2),
                legend.position = "right",
          )
        #ggplotly(p1)
        print(p1)
      })  # renderPlot close

    }else{
      shinyalert("Warning","We need both of air temperature and water temperature data to run thermal sensitivity. Please check."
                 ,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "alert_data_not_val_for_thermal")
    } ## outer if else loop close

  }) ## observeEvent end

  observeEvent(input$alert_data_not_val_for_thermal,{
    shinyjs::runjs("swal.close();")
  })


  #################  4: Thermal classification << Temperature  #################
  observeEvent(input$display_water_class, {
    hide("help_text_water_temp_class")

    output$display_water_temp_class_table <- renderUI({
      withSpinner(dataTableOutput("water_temp_class_table"))
    })
    myList <- processed$processed_dailyStats
    myData.Water <- myList[[which(names(myList)==input$water_temp_name_in_class)]]
    mean_col_water <- paste0(input$water_temp_name_in_class,".mean")
    data_water_to_calculate <- myData.Water[c("Date",mean_col_water)]
    #save(data_water_to_calculate,file="test_data_water_class.RData")
    ## calculate the July/August mean for each year
    all.years <- unique(format(data_water_to_calculate$Date,format="%Y"))
    #print(all.years)
    calculated.mean <- data.frame(matrix(ncol=3,nrow=0))

    for (i in 1:length(all.years)){
      year.now = all.years[i]
      to.select <- data_water_to_calculate$Date >= as.Date(paste0(year.now,"-07-01")) & data_water_to_calculate$Date <= as.Date(paste0(year.now,"-08-31"))
      mean.this.year <- mean(data_water_to_calculate[to.select,2],na.rm=TRUE)
      if(is.nan(mean.this.year)){
        class.this.year <- "NaN"
      }else if (mean.this.year<10){
        class.this.year <- "Very cold"
      }else if(mean.this.year>=10&mean.this.year<15){
        class.this.year <- "Cold"
      }else if(mean.this.year>=15&mean.this.year<18){
        class.this.year <- "Cold-cool"
      }else if(mean.this.year>=18&mean.this.year<21){
          class.this.year <- "Cool"
      }else if(mean.this.year>=21&mean.this.year<=24){
        class.this.year <- "Cool-warm"
      }else if(mean.this.year>24){
        class.this.year <- "Warm"
      }
      calculated.mean[i,] <- c(year.now,round(mean.this.year,digits=1),class.this.year)
    } # for loop end
    second_col_name <- paste0("Mean July/Aug water temperature(C)")
    colnames(calculated.mean) <- c("Year",second_col_name,"Class")

    output$water_temp_class_table <- DT::renderDataTable({
      myTable <- DT::datatable(
        calculated.mean,
        extensions ="Buttons",
        rownames = FALSE,
        options = list(
          scrollX = FALSE, #allow user to scroll wide tables horizontally
          stateSave = FALSE,
          pageLength = 15,
          dom = 'Bt',
          buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
          columnDefs = list(list(className="dt-center",targets="_all"))
        )
      ) # dataTable end
      print(myTable)
    })  ## renderDataTable ebd

  })  ##observeEvent end


  #################  1: IHA << Hydrology  ####
  observeEvent(input$display_IHA, {

    hide("help_text_IHA")

    output$display_IHA_table_1 <- renderUI({
      withSpinner(dataTableOutput("IHA_table_1"))
    })

    output$display_IHA_plot_button_1 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_1", label="Show/hide plot",style="color:cornflowerblue;background-color:black;font-weight:bold")
      )) # column and fluidRow close
    })

    output$display_IHA_table_2 <- renderUI({
      dataTableOutput("IHA_table_2")
    })

    output$display_IHA_plot_button_2 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_2", label="Show/hide plot",style="color:cornflowerblue;background-color:black;font-weight:bold")
      )) # column and fluidRow close
    })

    output$display_IHA_table_3 <- renderUI({
      dataTableOutput("IHA_table_3")
    })

    output$display_IHA_plot_button_3 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_3", label="Show/hide plot",style="color:cornflowerblue;background-color:black;font-weight:bold")
      )) # column and fluidRow close
    })

    output$display_IHA_table_4 <- renderUI({
      dataTableOutput("IHA_table_4")
    })

    output$display_IHA_plot_button_4 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_4", label="Show/hide plot",style="color:cornflowerblue;background-color:black;font-weight:bold")
      )) # column and fluidRow close
    })

    output$display_IHA_table_5 <- renderUI({
      dataTableOutput("IHA_table_5")
    })

    output$display_IHA_plot_button_5 <-renderUI({
      fluidRow(column(width=12,align="right",
                      actionButton(inputId="display_IHA_plot_5", label="Show/hide plot",style="color:cornflowerblue;background-color:black;font-weight:bold")
      )) # column and fluidRow close
    })

    myList <- processed$processed_dailyStats

    if (length(myList)>0){
    variable_to_IHA <- input$parameter_name
    myData <- myList[[which(names(myList)==variable_to_IHA)]]
    mean_col <- paste0(input$parameter_name,".mean")
    myData <- myData[c('Date',mean_col)]
    myData.IHA<- read.zoo(myData,format="%Y-%m-%d")
    processed$myData.IHA <- myData.IHA
    }else{
    myData <- uploaded_data()
    print(paste0("the file name is:",loaded_data$name))
    }

    IHA.table.options <- list(
      scrollX = TRUE, #allow user to scroll wide tables horizontally
      stateSave = FALSE,
      pageLength = 15,
      dom = 'Bt',
      buttons = list('copy','print',list(extend = 'collection',buttons = c('csv','excel','pdf'),text='Download')),
      columnDefs = list(list(className="dt-center",targets="_all"))
    )

    myYr <- "calendar"
    ## IHA parameters group 1; Magnitude of monthly water conditions
    Analysis.Group.1 <- group1(myData.IHA, year=myYr)
    #save(Analysis.Group.1,file="IHA_group_1.RData")
    Analysis.Group.1 <- as.data.frame(Analysis.Group.1) %>% mutate_if(is.numeric,round,digits=2)
    processed$IHA.group.1 <- Analysis.Group.1

    output$IHA_table_1 <- DT::renderDataTable({
      table.title.1 <- "Group 1: Magnitude of monthly water conditions"
      myTable <- DT::datatable(
        Analysis.Group.1,
        caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end


   ## IHA parameters group 2: Magnitude of monthly water condition and include 12 parameters
    Analysis.Group.2 <- group2(myData.IHA, year=myYr)
    #save(Analysis.Group.2,file="IHA_group_2.RData")
    Analysis.Group.2 <- as.data.frame(Analysis.Group.2) %>% mutate_if(is.numeric,round,digits=2)
    processed$IHA.group.2 <- Analysis.Group.2
    output$IHA_table_2 <- DT::renderDataTable({
      table.title.1 <- "Group 2: Magnitude of monthly water condition and include 12 parameters"
      myTable <- DT::datatable(
        Analysis.Group.2,
        caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = FALSE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end

    ## IHA parameters group 3:Timing of annual extreme water conditions
    Analysis.Group.3 <- group3(myData.IHA, year=myYr)
    #save(Analysis.Group.3,file="IHA_group_3.RData")
    processed$IHA.group.3 <- Analysis.Group.3
    output$IHA_table_3 <- DT::renderDataTable({
      table.title.3 <- "Group 3: Timing of annual extreme water conditions"
      myTable <- DT::datatable(
        Analysis.Group.3,
        caption = htmltools::tags$caption(table.title.3,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end

    ## IHA parameters group 4; Frequency and duration of high and low pulses
    # defaults to 25th and 75th percentiles
    Analysis.Group.4 <- group4(myData.IHA, year=myYr)
    #save(Analysis.Group.4,file="IHA_group_4.RData")
    processed$IHA.group.4 <- Analysis.Group.4
    output$IHA_table_4 <- DT::renderDataTable({
      table.title.4 <- "Group 4: Frequency and duration of high and low pulses"
      myTable <- DT::datatable(
        Analysis.Group.4,
        caption = htmltools::tags$caption(table.title.4,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end


    ## IHA parameters group 5; Rate and frequency of water condition changes
    Analysis.Group.5 <- group5(myData.IHA, year=myYr)
    #save(Analysis.Group.5,file="IHA_group_5.RData")
    processed$IHA.group.5 <- Analysis.Group.5
    Analysis.Group.5 <- as.data.frame(Analysis.Group.5) %>% mutate_if(is.numeric,round,digits=2)
    output$IHA_table_5 <- DT::renderDataTable({
      table.title.5 <- "Group 5: Rate and frequency of water condition changes"
      myTable <- DT::datatable(
        Analysis.Group.5,
        caption = htmltools::tags$caption(table.title.5,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
        extensions ="Buttons",
        rownames = TRUE,
        options = IHA.table.options
      ) # dataTable end
      print(myTable)
    })  # renderDT end


  }) #observeEvent end

  observeEvent(input$save_IHA, {
    require(XLConnect)
    if (!file.exists("Output/saved_IHA/")) dir.create(file.path("Output/saved_IHA"),showWarnings = FALSE, recursive = TRUE)

    Group.Desc <- c("Magnitude of monthly water conditions"
                    ,"Magnitude of monthly water condition and include 12 parameters"
                    ,"Timing of annual extreme water conditions"
                    ,"Frequency and duration of high and low pulses"
                    ,"Rate and frequency of water condition changes")
    df.Groups <- as.data.frame(cbind(paste0("Group",1:5),Group.Desc))
    myDate <- format(Sys.Date(),"%Y%m%d")
    myTime <- format(Sys.time(),"%H%M%S")
    Notes.User <- Sys.getenv("USERNAME")
    myYr <- "calendar"
    Notes.Names <- c("Dataset (SiteID)","IHA.Year","Analysis.Date (YYYYMMDD)"
                     ,"Analysis.Time (HHMMSS)","Analysis.User")
    Notes.Data <- c(loaded_data$name, myYr, myDate, myTime, Notes.User)
    df.Notes <- as.data.frame(cbind(Notes.Names,Notes.Data))
    # Open/Create file
    myFile.XLSX <- paste("Output/saved_IHA/IHA",loaded_data$name, myYr, myDate, myTime, "xlsx", sep=".")
    Notes.Summary <- summary(processed$myData.IHA)
    wb <- loadWorkbook(myFile.XLSX, create = TRUE) # load workbook, create if not existing
    # create sheets
    createSheet(wb, name = "NOTES")
    createSheet(wb, name = "Group1")
    createSheet(wb, name = "Group2")
    createSheet(wb, name = "Group3")
    createSheet(wb, name = "Group4")
    createSheet(wb, name = "Group5")
    # write to worksheet
    writeWorksheet(wb, df.Notes, sheet = "NOTES", startRow=1)
    writeWorksheet(wb, Notes.Summary, sheet = "NOTES", startRow=10)
    writeWorksheet(wb, df.Groups, sheet="NOTES", startRow=25)
    writeWorksheet(wb, processed$IHA.group.1, sheet = "Group1",rownames=c("year",rownames(processed$IHA.group.1)))
    writeWorksheet(wb, processed$IHA.group.2, sheet = "Group2")
    writeWorksheet(wb, processed$IHA.group.3, sheet = "Group3",rownames=c("year",rownames(processed$IHA.group.3)))
    writeWorksheet(wb, processed$IHA.group.4, sheet = "Group4",rownames=c("year",rownames(processed$IHA.group.4)))
    writeWorksheet(wb, processed$IHA.group.5, sheet = "Group5",rownames=c("year",rownames(processed$IHA.group.5)))
    # save workbook
    saveWorkbook(wb, myFile.XLSX)
  })# observeEvent end

  observeEvent(input$display_IHA_plot_1, {
    output$IHA_plot_1 <- renderUI({
      plotOutput("IHA_plot_1_to_show")
    })

    if(input$display_IHA_plot_1 %% 2 !=0){

       shinyjs::show("IHA_plot_1_panel")
       data_to_plot <- cbind(rownames(processed$IHA.group.1),data.frame(processed$IHA.group.1,row.names = NULL))
       colnames(data_to_plot)[1]<-'Year'
       data_for_plot_1 <- data_to_plot %>% gather(key,value,-Year) ## convert into long format
       data_for_plot_1$Year <- as.numeric(as.character(data_for_plot_1$Year))
       data_for_plot_1$key <- factor(data_for_plot_1$key, levels = c("January","February","March","April","May",
                                                                   "June","July","August","September","October","November","December"))
       output$IHA_plot_1_to_show <- renderPlot({
       p1 <- ggplot(data_for_plot_1)+
             geom_line(aes(x=Year,y=value,colour=key),size=0.8,linetype="dashed")+
             labs(x = "Year",y = paste0("Magnitude of monthly water conditions"),color="Month")+
             theme_minimal()+
             scale_x_continuous(breaks=unique(data_for_plot_1$Year),labels = unique(data_for_plot_1$Year))+
             theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                   ,plot.background = element_rect(color="grey20",size=2)
                   ,legend.position = "right"
                   )
       print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_1_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 1 end

  observeEvent(input$display_IHA_plot_2, {
    output$IHA_plot_2a <- renderUI({
      plotOutput("IHA_plot_2a_to_show")
    })

    output$IHA_plot_2b <- renderUI({
      plotOutput("IHA_plot_2b_to_show")
    })

    if(input$display_IHA_plot_2 %% 2 !=0){

      shinyjs::show("IHA_plot_2_panel")
      data_to_plot <- processed$IHA.group.2[,1:11]
      column_names <- colnames(data_to_plot)
      min_cols_to_select <- c("year",column_names[str_detect(column_names,"Min")])
      max_cols_to_select <- c("year",column_names[str_detect(column_names,"Max")])
      data_to_plot_min <- data_to_plot[min_cols_to_select]
      data_to_plot_max <- data_to_plot[max_cols_to_select]
      data_for_plot_2a <- data_to_plot_min %>% gather(key,value,-year) ## convert into long format
      data_for_plot_2b <- data_to_plot_max %>% gather(key,value,-year) ## convert into long format
      output$IHA_plot_2a_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_2a,aes(x=year,y=value,fill=key))+
              geom_bar(stat="identity",position="dodge")+
              labs(x = "Year",y = paste0("Magnitude of water condition"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_2a$year),labels = unique(data_for_plot_2a$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end

      output$IHA_plot_2b_to_show <- renderPlot({
        p2 <- ggplot(data_for_plot_2b,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Magnitude of water condition"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_2b$year),labels = unique(data_for_plot_2b$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p2)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_2_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 2 end


  observeEvent(input$display_IHA_plot_3, {
    output$IHA_plot_3 <- renderUI({
      plotOutput("IHA_plot_3_to_show")
    })

    if(input$display_IHA_plot_3 %% 2 !=0){

      shinyjs::show("IHA_plot_3_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.3),data.frame(processed$IHA.group.3,row.names = NULL))
      colnames(data_to_plot)[1]<-'year'
      data_for_plot_3 <- data_to_plot %>% gather(key,value,-year) ## convert into long format
      data_for_plot_3$year <- as.numeric(as.character(data_for_plot_3$year))
      output$IHA_plot_3_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_3,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Julian days"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_3$year),labels = unique(data_for_plot_3$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_3_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 3 end


  observeEvent(input$display_IHA_plot_4, {
    output$IHA_plot_4a <- renderUI({
      plotOutput("IHA_plot_4a_to_show")
    })

    output$IHA_plot_4b <- renderUI({
      plotOutput("IHA_plot_4b_to_show")
    })

    if(input$display_IHA_plot_4 %% 2 !=0){

      shinyjs::show("IHA_plot_4_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.4),data.frame(processed$IHA.group.4,row.names = NULL))
      colnames(data_to_plot)[1]<-'year'
      column_names <- colnames(data_to_plot)
      number_cols_to_select <- c("year",column_names[str_detect(column_names,"number")])
      length_cols_to_select <- c("year",column_names[str_detect(column_names,"length")])
      data_to_plot_number <- data_to_plot[number_cols_to_select]
      data_to_plot_length <- data_to_plot[length_cols_to_select]
      data_for_plot_4a <- data_to_plot_number %>% gather(key,value,-year) ## convert into long format
      data_for_plot_4b <- data_to_plot_length %>% gather(key,value,-year) ## convert into long format
      data_for_plot_4a$year <- as.numeric(as.character(data_for_plot_4a$year))
      data_for_plot_4b$year <- as.numeric(as.character(data_for_plot_4b$year))

      output$IHA_plot_4a_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_4a,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Frequency"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_4a$year),labels = unique(data_for_plot_4a$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end

      output$IHA_plot_4b_to_show <- renderPlot({
        p2 <- ggplot(data_for_plot_4b,aes(x=year,y=value,fill=key))+
          geom_bar(stat="identity",position="dodge")+
          labs(x = "Year",y = paste0("Duration"),fill="parameters")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_4b$year),labels = unique(data_for_plot_4b$year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p2)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_4_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 4 end

  observeEvent(input$display_IHA_plot_5, {
    output$IHA_plot_5 <- renderUI({
      plotOutput("IHA_plot_5_to_show")
    })

    if(input$display_IHA_plot_5 %% 2 !=0){

      shinyjs::show("IHA_plot_5_panel")
      data_to_plot <- cbind(rownames(processed$IHA.group.5),data.frame(processed$IHA.group.5,row.names = NULL))
      colnames(data_to_plot)[1]<-'Year'
      data_to_plot <- data_to_plot[,1:3]
      data_for_plot_1 <- data_to_plot %>% gather(key,value,-Year) ## convert into long format
      data_for_plot_1$Year <- as.numeric(as.character(data_for_plot_1$Year))

      output$IHA_plot_5_to_show <- renderPlot({
        p1 <- ggplot(data_for_plot_1)+
          geom_line(aes(x=Year,y=value,colour=key),size=0.8,linetype="dashed")+
          labs(x = "Year",y = paste0("Rate"),color="Month")+
          theme_minimal()+
          scale_x_continuous(breaks=unique(data_for_plot_1$Year),labels = unique(data_for_plot_1$Year))+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.background = element_rect(color="grey20",size=2)
                ,legend.position = "right"
          )
        print(p1)

      }) # renderPlot end
    }else{
      shinyjs::hide("IHA_plot_5_panel")
    } # if else loop end
  }) #observeEvent to display IHA plot 5 end


  ################### "Create report"####

  observeEvent(input$createReport,{

    showModal(modalDialog("Saving the report now...",footer=NULL))

    if (!file.exists("Output/reports")) dir.create(file.path("Output/reports"),showWarnings = FALSE, recursive = TRUE)

    # out <- rmarkdown::render('SiteSummary_from_app.Rmd'
    #                          ,output_format=switch(input$report_format,
    #                                  pdf = pdf_document(),
    #                                  html= html_document(),
    #                                  word= word_document())
    #                          ,output_file = "Output/reports/SiteSummary_rendered"
    #                          ,quiet = TRUE
    #                          )

    build_summary(dir_data="Output/to_report/"
                  ,file_main="_Captions_SiteX.xlsx"
                  ,sheet_main="metadata"
                  ,file_prefix_sep="_"
                  ,rmd_template="SiteSummary.Rmd"
                  ,output_format=input$report_format
                  ,output_file=paste0("Output/reports/",input$report_name,".",input$report_format))

    removeModal()

  })




}
