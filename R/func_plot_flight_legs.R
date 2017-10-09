#' Plot flight legs
#' 
#' creates plots of the flight and its sections cloured by supplied parameters. date must be supllied as POSIXct, in column "date"
#' 
#' @param flight_data faam merge file
#' @param flight_sum_path file path of flight summary
#' @param plot_range T/F plot subsections of flight
#' @param maptype string for getmap, toner
#' @param output_file_path location to save plots
#' @param colour_by vector of cloums to colour by, defualt "nox_conc","o3_teco","alt_gin"
#' @param flight_map supply a gg_map file to override the automatic map generation
#' @param missing_flag data flag to convert to NA
#' @param zoom adjust map zoom level
#' @param logplot vector of equal length to colour_by in order should these be on log scales deafault (T,F,F)
#' @param colourpoints how many points in the colour bar
#' @param range_control default false, enable for colour bars to change between flight segments
#' @param cube_helix defualt true, disable to use plasma colourbar
#' 
#' @author W S Drysdale
#' 
#' @export


plot_flight_legs = function(flight_data,
                            flight_sum_path,
                            plot_range = T,
                            maptype = "toner",
                            output_file_path = "plots/",
                            colour_by = c("nox_conc","o3_teco","alt_gin"),
                            flight_map,
                            missing_flag = -9999,
                            zoom = 8,
                            logplot = c(T,F,F),
                            colourpoints = 10,
                            range_control = T,
                            cube_helix = T
                            ){
    if(length(logplot) != length(colour_by))
      stop("Logplot and colour_by lengths differ")
    if(cube_helix)
      cols = rje::cubeHelix(colourpoints,start = pi*2)
    else
      cols = viridis::plasma(colourpoints)
    #Read in flight summary and store flight number
    flight_sum = read.faam_flight_sum(flight_sum_path)
    flight_no = flight_sum$flight_no[1]
    
    #Tidy Missing data
    flight_data[flight_data == missing_flag] = NA
    
    #tidy and store lat longs
    flight_data$lat_gin[flight_data$lat_gin == 0] = NA
    
    max_lon = max(flight_data$lon_gin,na.rm = T)
    min_lon = min(flight_data$lon_gin,na.rm = T)
    
    max_lat = max(flight_data$lat_gin,na.rm = T)
    min_lat = min(flight_data$lat_gin,na.rm = T)
    
    #function to find the halfway point of the lat longs
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
                           zoom = zoom)
      }
    
    #Determine percentile_value for each colourby 
    for (i in 1:length(colour_by)){
     if(i == 1){
       minimum = min(flight_data[,colour_by[i]],na.rm = T)
       maximum = max(flight_data[,colour_by[i]],na.rm = T)
     }
     else{
       minimum = c(minimum,min(flight_data[,colour_by[i]],na.rm = T))
       maximum = c(maximum,max(flight_data[,colour_by[i]],na.rm = T))
     }
    }
    minimum[minimum <= 0] = 0.1
    #Plot the whole flight for each colour_by
    pdf(paste(output_file_path,flight_no,"_","full_flight_plots.pdf",sep = ""))
    for (i in 1:length(colour_by)){
      if(logplot[i])
        colour_bar_values = rescaler(exp(seq(log(minimum[i]), log(maximum[i]), length = colourpoints)),"range")
      else
        colour_bar_values = rescaler(seq(1,colourpoints,1),"range")
      colour_col = colour_by[i]
      ggp = ggmap(flight_map)+
          geom_path(data = flight_data,aes_string(x = "lon_gin",y = "lat_gin",colour = colour_col),size = 3,inherit.aes = FALSE)+
          scale_color_gradientn(colours = cols,limits = c(0,maximum[i]),values = colour_bar_values)+
          ggtitle(paste(flight_no,"full_flight",colour_by[i],sep = " "))
        print(paste("Plotting ",trimws(colour_by[i],which = "right")," for all of ",flight_no,sep = ""))
        print(ggp)
    }
      
    dev.off()
  
    #Plot flight segments
    if(plot_range){
      #select segment
      range_rows = which(flight_sum$range_event == 1)
 
      for (j in 1:length(range_rows)){
        range_flight_data = flight_data[(flight_data$date > flight_sum$start_time[range_rows[j]]) &
                                            (flight_data$date < flight_sum$end_time[range_rows[j]]),]
        #plot
        pdf(paste(output_file_path,flight_no,"_",trimws(flight_sum$event[range_rows][j],"right"),"_plots.pdf",sep = ""))
        for (i in 1:length(colour_by)){
          if(logplot[i]){
            colour_bar_values = rescaler(exp(seq(log(minimum[i]), log(maximum[i]), length = colourpoints)),"range")
          }else{
            colour_bar_values = rescaler(seq(1,colourpoints,1),"range")
          }
          if(!range_control)
            maximum = max(range_flight_data[,colour_by[i]],na.rm = T)
          
          colour_col = colour_by[i]
          ggp = ggmap(flight_map)+
            geom_path(data = range_flight_data,aes_string(x = "lon_gin",y = "lat_gin",colour = colour_col),size = 3)+
            scale_color_gradientn(colours = cols,limits = c(0,maximum[i]),values = colour_bar_values)+
            ggtitle(paste(flight_no,range_control,trimws(flight_sum$event[range_rows][j],"right"),colour_by[i],sep = " "))
        
          
          print(ggp)
          print(paste("Plotting ",trimws(colour_by[i],which = "right")," for ",
                      trimws(flight_sum$event[range_rows][j],which = "right")," for ",flight_no,sep = ""))
        }
        dev.off()
      }
      
    }

}

  