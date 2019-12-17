#' Plot Normal distribution
#' 
#' Fit and plot a normal distribution histogram alongside a timeseries of the data the distribution has been fitted from
#' 
#' @param df data.frame containing a date column as POSIXct and the columns to plot
#' @param columns column names expressed unquoted.
#' 
#' @export
#' 
#' @author W. S. Drysdale

plot_ndist = function(df,columns = c(CH1_Hz,CH2_Hz)){
  
  columns = substitute(columns)
  
  require(ggplot2)
  require(patchwork)
  require(dplyr)
  
  df = df %>% 
    dplyr::select(date,
                  !!columns) %>% 
    reshape::melt("date")
  
  dfList = split(df,df$variable)
  
  hist_plots = list()
  
  ts_plots = list()
  
  n = ifelse(length(dfList) >= 3, length(dfList),3)
  
  cols = RColorBrewer::brewer.pal(n,"Set1")
  for(i in 1:length(dfList)){
    plotDat = dfList[[i]] %>% 
      dplyr::mutate(date = lubridate::floor_date(date,unit = "1 sec"))
    
    hist_plots[[i]] = 
      ggplot(plotDat)+
      geom_histogram(aes(x = value, y = ..density..),binwidth = 20,fill = cols[i],col = "black")+
      stat_function(fun = dnorm,args = list(mean = mean(plotDat$value),sd = sd(plotDat$value)),size = 1.2)+
      xlab(ifelse(i == length(dfList),"Value",""))+
      ylab("")+
      wsdmiscr::gen_theme("white")+
      theme(plot.background = element_rect(colour = "white"))
    
    ts = data.frame(date = seq(min(plotDat$date),max(plotDat$date),1))
    plotDat = dplyr::left_join(ts,plotDat,by = "date")
    
    ts_plots[[i]] = 
      ggplot(plotDat)+
      geom_line(aes(date,value),colour = cols[i])+
      geom_hline(aes(yintercept = mean(value,na.rm = T)),size = 1.2)+
      geom_hline(aes(yintercept = mean(value,na.rm = T)+sd(value,na.rm = T)),size = 1.2,linetype = 2)+
      geom_hline(aes(yintercept = mean(value,na.rm = T)-sd(value,na.rm = T)),size = 1.2,linetype = 2)+
      xlab(ifelse(i == length(dfList),"Date",""))+
      ylab("Value")+
      wsdmiscr::gen_theme("white")+
      theme(plot.background = element_rect(colour = "white"))
    
  }
  
  hist_plots = do.call("/",hist_plots)
  
  ts_plots = do.call("/",ts_plots)
  
  
  #return
  ts_plots|hist_plots|plot_layout(ncol = 2,widths = c(3,1))
  
}
