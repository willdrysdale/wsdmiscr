#' Time Series Compare
#' 
#' Produces basic plots and stats summary comparing two time series
#' 
#' @param df dataframe containing the two time series to compare
#' @param date column name for timestamp
#' @param s1_col series 1 column name 
#' @param s2_col series 2 column name
#' @param s1_name series 1 name (method, instrument etc),used on plots
#' @param s2_name series 2 name (method, instrument etc),used on plots
#' @param comparison_name what about the time series is being comapred, used on plots
#' @param time_res time resolution of data in seconds
#' @param font select font for plotting
#' @param scale scales the axis tick and axis label text
#' @param den_range for density plotting, overwhat range of densities should the overlap integral be calculated c(min,max)
#' 
#' @return List object containing list of plots and dataframe of stats
#' 
#' @author W S Drysdale
#' 
#' @export



time_series_compare = function(df,
                               date = "date",
                               s1_col = "F_NO1_mass",
                               s2_col = "no_flux_mg_m2_h",
                               s1_name = "eddy4R",
                               s2_name = "EddyPro",
                               comparison_name = bquote("NO Flux / mg "~m^-2~h^-1),
                               time_res = 3600,
                               font = "Garamond",
                               scale = 5,
                               den_range = c(-5,30)){
  
  #Setup
  padding = seq(min(df[,date]),max(df[,date]),time_res) %>% as.data.frame
  names(padding) = "date"
  
  df_pad = left_join(padding,df,date)
  
  df_narm = df_pad
  df_narm = df_narm[!is.na(df_pad[,s1_col]),]
  df_narm$index = 1:nrow(df_narm)
  df_narm$s1_col_cs = cumsum(df_narm[,s1_col])
  df_narm$s2_col_cs = cumsum(df_narm[,s2_col])
  df_narm$cs_diff = df_narm$s2_col_cs - df_narm$s1_col_cs
  
  orth_reg = MethComp::Deming(df_pad[,s2_col],df_pad[,s1_col])
  reg = lm(df_pad[,s1_col] ~ df_pad[,s2_col])
  
  s1_den = approxfun(density(df_pad[,s1_col],na.rm = T,from = den_range[1],to = den_range[2]))
  s2_den = approxfun(density(df_pad[,s2_col],na.rm = T,from = den_range[1],to = den_range[2]))
  
  #Plotting
  time_series = ggplot(df_pad)+
    geom_line(aes_string(date,s2_col),col = "red")+
    geom_line(aes_string(date,s1_col))+
    xlab("Date")+
    ylab(comparison_name)+
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(family = font),
          axis.text = element_text(colour = "black",size = 6*scale,face = "bold"),
          axis.title = element_text(colour = "black",size = 7*scale),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black")
    )
  
  density_plot = ggplot(df_pad)+
    geom_density(aes_string(s2_col),fill = rgb(1,0,0),alpha = 0.6,size = 1)+
    geom_density(aes_string(s1_col),fill = rgb(0,0,0),alpha = 0.6,lty = 2,size = 1)+
    geom_point(aes(intersect,s1_den(intersect)),col = "red")+
    ylab("Density")+
    xlab(comparison_name)+
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(family = font),
          axis.text = element_text(colour = "black",size = 6*scale,face = "bold"),
          axis.title = element_text(colour = "black",size = 7*scale),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          legend.position = "none"
    )
  
  cumsum = ggplot(df_narm)+
    geom_line(aes(index,s1_col_cs),size = 2,lty = 2)+
    geom_line(aes(index,s2_col_cs),col = "red",size = 2)+
    geom_line(aes(index,cs_diff),col = "blue",size = 2, lty = 4)+
    ylab(comparison_name)+
    xlab("Index")+
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(family = font),
          axis.text = element_text(colour = "black",size = 6*scale,face = "bold"),
          axis.title = element_text(colour = "black",size = 7*scale),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          legend.position = "none"
    )
  
  regression = ggplot(df_pad)+
    geom_point(aes_string(s2_col,s1_col),size = 4,alpha = 0.5)+
    geom_abline(aes(intercept = orth_reg[[1]],slope = orth_reg[[2]]))+
    geom_abline(aes(intercept = reg[[1]][[1]],slope = reg[[1]][[2]]),lty = 2)+
    xlab(s2_name)+
    ylab(s1_name)+
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(family = font),
          axis.text = element_text(colour = "black",size = 6*scale,face = "bold"),
          axis.title = element_text(colour = "black",size = 7*scale),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black")
    )
  
  #Statistics
  
  means = c(mean(df[,s1_col],na.rm = T),
            mean(df[,s2_col],na.rm = T))
  
  medians = c(median(df[,s1_col],na.rm = T),
              median(df[,s2_col],na.rm = T))
  
  sds = c(sd(df[,s1_col],na.rm = T),
          sd(df[,s2_col],na.rm = T))
  
  cor_coeff = cor(df_narm[,c(s1_col,s2_col)])[2] %>% rep(2)
  
  s1_den = approxfun(density(df_pad[,s1_col],na.rm = T,from = den_range[1],to = den_range[2]))
  s2_den = approxfun(density(df_pad[,s2_col],na.rm = T,from = den_range[1],to = den_range[2]))
  
  den_diff = function(x){s2_den(x) - s1_den(x)}
  
  intersect = uniroot(den_diff,c(den_range[1],den_range[2]))$root
  
  
  overlap_int = integrate(s1_den,lower = 0,intersect)$value + 
    integrate(s2_den, intersect, upper = 30)$value %>% rep(2)

  diverge = max(df_narm$cs_diff,na.rm = T) %>% rep(2)
  
  orth_slope = orth_reg[[2]] %>% rep(2)
  
  lin_r2 = summary(reg)[[8]] %>% rep(2)
  
  #Binding
  
  plots = list(
    time_series = time_series,
    density_plot = density_plot,
    cumsum = cumsum,
    regression = regression
  )
  
  stats = matrix(nrow = 8,ncol = 2) %>% as.data.frame
  stats[1,] = means
  stats[2,] = medians
  stats[3,] = sds
  stats[4,] = cor_coeff
  stats[5,] = overlap_int
  stats[6,] = diverge
  stats[7,] = orth_slope
  stats[8,] = lin_r2
  
  base::row.names(stats) = c("mean","median","st_dev","cor","overlap_int","diverge","orth_slope","lin_r2")
  names(stats) = c(s1_name,s2_name)
  out = list(plots = plots,stats = stats)
  
  #return
  out
  
}
