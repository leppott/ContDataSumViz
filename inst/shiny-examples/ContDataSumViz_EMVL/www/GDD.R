
#Function for downloading airport weather data from Iowa State Mesonet
weatherdownload = function(iata,startdate,enddate,parameters=c("Air Temperature","Dew Point","Relative Humidity","Wind Direction","Wind Speed",
                                                                      "Altimeter","Precipitation","Gust Speed","Clouds")){
  if (length(iata)==1){
    #Setup first portion of URL
    wthrdata=paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=",iata)
    #Parameters
    #Air temperature
    if("Air Temperature" %in% parameters){
      atp="&data=tmpc"
    }else{
      atp=""
    }
    
    #Dew Point
    if("Dew Point" %in% parameters){
      dwp="&data=dwpc"
    }else{
      dwp=""
    }
    
    #Relative Humidity
    if("Relative Humidity" %in% parameters){
      rlh="&data=relh"
    }else{
      rlh=""
    }
    
    #Wind Direction
    if("Wind Direction" %in% parameters){
      wdr="&data=drct"
    }else{
      wdr=""
    }
    
    #Wind Speed
    if("Wind Speed" %in% parameters){
      wsd="&data=sknt"
    }else{
      wsd=""
    }
    
    #Altimeter
    if("Altimeter" %in% parameters){
      alt="&data=alti"
    }else{
      alt=""
    }
    
    #Sea level pressure
    slp=""
    # if(input$slp==TRUE){
    #   slp="&data=mslp"
    # }else{
    #   slp=""
    # }
    
    #Precipitation
    if("Precipitation" %in% parameters){
      pcp="&data=p01m"
    }else{
      pcp=""
    }
    
    #Visibility
    vsb=""
    # if(input$vis==TRUE){
    #   vsb="&data=vsby"
    # }else{
    #   vsb=""
    # }
    
    #Gust
    if("Gust Speed" %in% parameters){
      gsd="&data=gust"
    }else{
      gsd=""
    }
    
    #Cloud Coverage
    
    if("Clouds" %in% parameters){
      sky="&data=skyc1&data=skyc2&data=skyc3&data=skyl1&data=skyl2&data=skyl3"
    }else{
      sky=""
    }
    
    #Compile URL based upon the selected parameters
    wthrdata=paste0(wthrdata,atp,dwp,rlh,wdr,wsd,alt,slp,pcp,vsb,gsd,sky,
                    "&year1=",lubridate::year(startdate),
                    "&month1=",lubridate::month(startdate),
                    "&day1=",lubridate::day(startdate),
                    "&year2=",lubridate::year(enddate),
                    "&month2=",lubridate::month(enddate),
                    "&day2=",lubridate::day(enddate),
                    "&tz=Etc%2FUTC&format=onlycomma&latlon=no&direct=no&report_type=1&report_type=2")
    
    #Download data
    wthrdld=data.table::fread(wthrdata)
    wthrdld=as.data.frame(wthrdld)
    
  }else{
    wthrdld=NULL
    warning("No weather data for this lake.")
  }
  return(wthrdld)
}


#Download data and calculate annual GDD5
GDDFinal = NULL
for (j in seq(from = 1970,to = 2019,by = 1)){
  message(j)
  
  #Download Airport weather station data from Minneapolis-Saint Paul International Airport
  weather = weatherdownload(
    iata = "MSP",
    startdate = paste0(j,"-01-01"),
    enddate = paste0(j,"-12-31"),
    parameters = "Air Temperature"
  ) 
  
  #Ensure weather data frame is populated and contains enough values
  if(!is.null(weather)){
    if(nrow(weather)>1){
      #Convert date/time field to POSXIXct
      weather$valid = as.POSIXct(weather$valid,format = "%Y-%m-%d %H:%M",tz = "UTC")
      #Ensure 99% of the days of the year are present
      if (length(unique(as.Date(weather$valid)))>365*0.99){
        
        #Convert temperature field to numberic
        weather$tmpc = as.numeric(weather$tmpc)
        #Remove NAs
        weather=weather[which(!is.na(weather$tmpc)),]
        #Convert data to an xts time series
        GDDyear.xts = xts::xts(weather$tmpc,order.by = weather$valid)
        #Calculate daily means
        GDDyearmean.xts = xts::apply.daily(GDDyear.xts,"mean")
        #Convert time series to data frame
        GDDyearmean=data.frame("Date" = as.Date(zoo::index(GDDyearmean.xts)),"Temp_Mean" = zoo::coredata(GDDyearmean.xts))
        #Subtract 5 from the mean temp of each day to calculate GDD
        GDDyearmean$GDD_day=GDDyearmean$Temp_Mean - 5
        #Set all GDD less than 0 equal to 0
        GDDyearmean$GDD_day[GDDyearmean$GDD_day < 0] = 0
        #Create data frame, add up all of the daily GDD, and calculate yearly mean temp
        GDDFinalrow=data.frame("iata" = "MSP","Year" = j,"GDD" = sum(GDDyearmean$GDD_day),"Temp_Mean" = mean(GDDyearmean$Temp_Mean))
        GDDFinal=rbind(GDDFinal,GDDFinalrow)
        rm(weather,GDDyear.xts,GDDyearmean.xts,GDDyearmean,GDDFinalrow)
      }
    }
  }
  
}
