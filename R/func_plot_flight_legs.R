plot_flight_legs = function(flight_data,
                            flight_sum_path,
                            plot_range = T,
                            maptype = "toner",
                            output_file_path,
                            colour_by = c("nox_conc","o3_teco","alt_gin"),
                            flight_map,
                            missing_flag,
                            ...){

    #Read in flight summary and store flight number
    flight_sum = read.faam_flight_sum(flight_sum_path)
    flight_no = flight_sum$flight_no[1]
    
    #Tidy Missing data
    flight_data[flight_data == missing_flag] = NA
    
    #tidy and store lat longs
    flight_data$lat_gin[flight_data$lat_gin == 0] = NA
    
    flight_data$date = ymd_hms(flight_data$timestamp)
    
    max_lon = max(flight_data$lon_gin,na.rm = T)
    min_lon = min(flight_data$lon_gin,na.rm = T)
    
    max_lat = max(flight_data$lat_gin,na.rm = T)
    min_lat = min(flight_data$lat_gin,na.rm = T)
    
    #function to find the halfwqay point of the lat longs
    halfway_between = function(max,min){
      range = max-min
      halfway = min+(range/2)
      halfway
    }
    
    #download map if none supplied
    if(missing(flight_map)){
      flight_map = get_map(location = c(lon = halfway_between(max_lon,min_lon),
                                        lat = halfway_between(max_lat,min_lat)),
                           maptype = maptype,
                           zoom = 8)
      }
    
    #Plot the whole flight for each colour_by
    #full_flight_list = list()
    pdf(paste(output_file_path,flight_no,"_","full_flight_plots.pdf",sep = ""))
    for (i in 1:length(colour_by)){
      colour_col = colour_by[i]
      #full_flight_list[[i]] = 
      ggp = ggmap(flight_map)+
          geom_path(data = flight_data,aes_string(x = "lon_gin",y = "lat_gin",colour = colour_col),size = 3,inherit.aes = FALSE)+
          scale_color_gradientn(colours = rainbow(7))+
          ggtitle(paste(flight_no,"full_flight",colour_by[i],sep = " "))
        print(paste("Plotting ",trimws(colour_by[i],which = "right")," for all of ",flight_no,sep = ""))
        print(ggp)
    }
 #   pdf(paste(output_file_path,flight_no,"_","full_flight_plots.pdf",sep = ""))
  #  for (i in 1:length(colour_by)){
#     print(full_flight_list[[i]])
#     print(paste("Plotting ",trimws(colour_by[i],which = "right")," for all of ",flight_no,sep = ""))
#    }
      
    dev.off()
  
    #Plot flight segments
    if(plot_range == T){
      #select segment
      range_rows = which(flight_sum$range_event == 1)
 
      for (j in 1:length(range_rows)){
        range_flight_list = list()
        range_flight_data = flight_data[(flight_data$date > flight_sum$start_time[range_rows[j]]) &
                                            (flight_data$date < flight_sum$end_time[range_rows[j]]),]
        #plot
        for (i in 1:length(colour_by)){
          colour_col = colour_by[i]
          range_flight_list[[i]] = ggmap(flight_map)+
            geom_path(data = range_flight_data,aes_string(x = "lon_gin",y = "lat_gin",colour = colour_col),size = 3)+
            scale_color_gradientn(colours = rainbow(7))+
            ggtitle(paste(flight_no,trimws(flight_sum$event[range_rows][j],"right"),colour_by[i],sep = " "))
        }
        pdf(paste(output_file_path,flight_no,"_",trimws(flight_sum$event[range_rows][j],"right"),"_plots.pdf",sep = ""))
        for (i in 1:length(colour_by)){
          print(range_flight_list[[i]])
          print(paste("Plotting ",trimws(colour_by[i],which = "right")," for ",flight_sum$event[range_rows][j]," for ",flight_no,sep = ""))
        }
        dev.off()
      }
      
    }

}

  