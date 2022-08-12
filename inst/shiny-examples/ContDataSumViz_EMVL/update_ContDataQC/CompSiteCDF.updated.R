#' CompSiteCDF.updated, compare CDFs of sites
#' this R function is adopted from "CompSiteCDF" 
#' from https://github.com/leppott/ContDataQC/blob/main/R/CompSiteCDF.R
#' @examples
#' CompSiteCDF.updated(ParamName.xlab = myXlab, df.input=myDF)
#' 
#' @export
#' 


CompSiteCDF.updated <- function(file.input = NULL
                        , dir.input = getwd()
                        , dir.output = getwd()
                        , Param.Name = " "
                        , Shaded.Names = NULL
                        , Plot.title = " "
                        , Plot.season = "Summer"
                        , hist.columnName = NULL
                        , df.input = NULL){##FUNCTION.CompSiteCDF.START
  
  # load data (data.frame or from CSV)
  # if no data frame then import file.
  if (!is.null(df.input)) {##IF.START
    data.import <- df.input
  } else {
    data.import <- utils::read.csv(file.path(dir.input, file.input))
  }##IF.END
  #
  # Site Names (Columns) before adding seasonal columns including the "Date" column
  #Col.Sites <- names(data.import)[!(names(data.import) %in% ContData.env$myName.Date)]
  Col.Sites <- names(data.import)
  
  
  # Add columns for time periods
  # add Year, Month, Season, YearSeason (names are in config.R)
  # assume Date is POSIXct
  #
  # add time period fields
  data.import[,ContData.env$myName.Yr]   <- format(as.Date(data.import[
    , ContData.env$myName.Date]),format="%Y")
  data.import[,ContData.env$myName.Mo]   <- format(as.Date(data.import[
    , ContData.env$myName.Date]),format="%m")
  data.import[,ContData.env$myName.YrMo] <- format(as.Date(data.import[
    , ContData.env$myName.Date]),format="%Y%m")
  data.import[,ContData.env$myName.MoDa] <- format(as.Date(data.import[
    , ContData.env$myName.Date]),format="%m%d")
  # data.import[,ContData.env$myName.JuDa] <- as.POSIXlt(data.import[
  #,ContData.env$myName.Date], format=ContData.env$myFormat.Date)$yday +1
  # ## add Season fields
  data.import[,ContData.env$myName.Season] <- NA
  data.import[,ContData.env$myName.Season][as.numeric(data.import[
    , ContData.env$myName.MoDa]) >= as.numeric("0101") & as.numeric(data.import[
      , ContData.env$myName.MoDa])<
      as.numeric(ContData.env$myTimeFrame.Season.Spring.Start)] <- "Winter"
  data.import[,ContData.env$myName.Season][as.numeric(data.import[
    , ContData.env$myName.MoDa]) >=
      as.numeric(ContData.env$myTimeFrame.Season.Spring.Start) &
      as.numeric(data.import[,ContData.env$myName.MoDa])<
      as.numeric(ContData.env$myTimeFrame.Season.Summer.Start)] <- "Spring"
  data.import[,ContData.env$myName.Season][as.numeric(data.import[
    , ContData.env$myName.MoDa]) >=
      as.numeric(ContData.env$myTimeFrame.Season.Summer.Start) &
      as.numeric(data.import[,ContData.env$myName.MoDa])<
      as.numeric(ContData.env$myTimeFrame.Season.Fall.Start)] <- "Summer"
  data.import[,ContData.env$myName.Season][as.numeric(data.import[
    , ContData.env$myName.MoDa]) >=
      as.numeric(ContData.env$myTimeFrame.Season.Fall.Start) &
      as.numeric(data.import[,ContData.env$myName.MoDa])<
      as.numeric(ContData.env$myTimeFrame.Season.Winter.Start)] <- "Fall"
  data.import[,ContData.env$myName.Season][as.numeric(data.import[
    , ContData.env$myName.MoDa]) >=
      as.numeric(ContData.env$myTimeFrame.Season.Winter.Start) &
      as.numeric(data.import[,ContData.env$myName.MoDa])<=
      as.numeric("1231")] <- "Winter"
  data.import[,ContData.env$myName.YrSeason] <- paste(data.import[
    , ContData.env$myName.Yr],data.import[,ContData.env$myName.Season], sep="")
  # Remove bad date records
  data.import <- data.import[!is.na(data.import[,ContData.env$myName.Yr]),]
  # rectify December as part of winter of year + 1
  mySelection <- data.import[,ContData.env$myName.Mo]=="12"
  if(sum(mySelection) != 0){##IF.sum.START
    data.import[, ContData.env$myName.YrSeason][mySelection] <- paste(
      as.numeric(data.import[, ContData.env$myName.Yr]) + 1
      , data.import[, ContData.env$myName.Season],sep="")
  }##IF.sum.END
  
  # Season Names
  SeasonNames <- c("Fall", "Winter", "Spring","Summer")
  if (!is.null(Plot.season)){
    data.import <- data.import[data.import[,ContData.env$myName.Season]==Plot.season,]
  }
  
  if (is.null(Shaded.Names)){
  data.plot <- data.import[colnames(data.import) %in% Col.Sites]
  data.plot <- reshape2::melt(data.plot,"Date")
  print(colnames(data.plot))
  my_plot <- ggplot(data=data.plot,aes(x=value,colour=variable))+
    stat_ecdf(geom="step",alpha=0.6,pad=TRUE)+
    labs(x=ParamName.xlab,y="Proportion <= value")+
    theme(text=element_text(size=14,face = "bold", color="blue"),
          plot.title = element_text(hjust=0.5),
          legend.position = "right",
          legend.title=element_blank(),
          panel.grid.major.y=element_blank())+
    scale_color_viridis_d(option="D")+
    ggtitle(Plot.title)
  
  }else{
    if (nrow(data.import)>0){
    g <- ggplot(data=data.import,aes(x=!!sym(Param.Name),color=paste0(Param.Name," CDF")))+
         geom_step(stat="ecdf")
    inside <- ggplot_build(g)
    matched <- merge(inside$data[[1]],data.frame(x=data.import[,names(data.import)==Param.Name]
                                                 ,data.import[,names(data.import)==Shaded.Names[1]]
                                                 ,data.import[,names(data.import)==Shaded.Names[2]]),
                     by=("x"))
    names(matched)[ncol(matched)] <- "data.import.max"
    names(matched)[ncol(matched)-1] <-"data.import.min"
    if (grepl("25%",Shaded.Names[1])){
      shading_text <- paste0(Param.Name, " CDF between daily 25th percentiles and 75th percentiles")
    }else if (grepl("min",Shaded.Names[1])){
      shading_text <- paste0(Param.Name, " CDF between daily minimum and maximum values ")
    }else{
      shading_text <- "Shading"
    }
    
    my_plot <- g+
                geom_ribbon(data=matched,aes(x=x,ymin=ecdf(data.import.min)(x),ymax=ecdf(data.import.max)(x),fill=shading_text),color="transparent",alpha=0.5)+
                theme_minimal()+
                labs(title=Plot.title,y="Proportion <= value")+
                scale_colour_manual("", values = "black")+
                scale_fill_manual("", values = "green")+
                theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                      ,plot.background = element_rect(color="grey20",size=2)
                      ,plot.title = element_text(hjust=0.5)
                      ,legend.position = "bottom")
    }else{
      my_plot <- NULL
    }
                
  }
  
 return(my_plot);
  
}
  