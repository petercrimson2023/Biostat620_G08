# Functions used for plotting

time_to_angle <- function(time) {
  # Extract hours and minutes
  hrs <- as.numeric(format(time, "%H"))
  mins <- as.numeric(format(time, "%M"))
  
  # Calculate the angle (0 degrees at midnight, 360 degrees at next midnight)
  angle <- (hrs * 60 + mins) / (24 * 60) * 360
  return(angle)
}


circular_plot = function(data,user_name,bw_default=15)
{
  
  time = data$Pickup.1st %>% 
    paste("2000-01-01",.) %>% 
    as.POSIXct(.,format="%Y-%m-%d %H:%M")
  
  angle = sapply(time, time_to_angle)
  circular_data = circular(angle, units = "degrees",template='clock24')
  screendata.den = density(circular_data, bw = bw_default)
  
  circular(angle, units = "degrees", template = "clock24") %>%
    plot(., stack = TRUE, bins = 30, col = "red", main = paste("Histogram of First Pickup for ", user_name))
  
  plot(screendata.den, col = "blue", main = paste("Screen time density for ", user_name))
  
  #combined_plot <- grid.arrange(first_plot, second_plot, ncol = 2, )
  #title(paste("Screen time density for ",user_name))
  # grid.draw(combined_plot)
}

violin_plot = function(data, user_name,trim=FALSE)
{
  temp_gg = ggplot(data, aes(x = factor(Stay.late), y = Social.ST.min)) +
    geom_violin(trim=trim) +
    labs(
      title = paste("Social Screen Time distribution for ", user_name),
      x = "Stay up",
      y = "Social Screen Time (min)"
    ) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1 / 2)) +
    theme(axis.title.x = element_text(size = 12, face = "bold")) +
    theme(axis.title.y = element_text(size = 12, face = "bold")) +
    theme(plot.title = element_text(
      size = 14,
      hjust = 1 / 2,
      face = "bold"
    )) +
    theme(axis.text.x = element_text(size = 12, face = "bold")) +
    theme(axis.text.y = element_text(size = 12, face = "bold"))
  return(temp_gg)
  
}

plot_occupation_time_curve <-
  function(data,
           time_column,
           condition_column,
           condition_values,
           colors = c("blue", "red"),
           user) {
    if (length(condition_values) != 2 || length(colors) != 2) {
      stop("condition_values and colors must be vectors of length 2.")
    }
    
    condition_vec = c(condition_column, paste("Not ", condition_column))
    
    data_list <- lapply(condition_values, function(cond) {
      data_filtered <- data %>%
        filter(!!sym(condition_column) == cond)
      
      c_values = seq(0, max(data_filtered[[time_column]]) + 1, length.out = 101)
      st_values = sapply(c_values, function(x) {
        sum(data_filtered[[time_column]] >= x)
      }) / length(data_filtered[[time_column]])
      temp_data = data.frame(c = c_values,
                             st = st_values,
                             condition = as.character(cond))
      
      return(temp_data)
    })
    
    temp_combined <- do.call(rbind, data_list)
    
    temp_gg = ggplot(temp_combined, aes(x = c, y = st, color = condition)) +
      geom_point(size = 1) +
      geom_line() +
      labs(
        title = paste(user, time_column , "grpuped by ", condition_column),
        x = time_column,
        y = "Occupation Time Curve"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = setNames(colors, condition_values)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1 / 2)) +
      theme(axis.title.x = element_text(size = 12, face = "bold")) +
      theme(axis.title.y = element_text(size = 12, face = "bold")) +
      theme(plot.title = element_text(
        size = 14,
        hjust = 1 / 2,
        face = "bold"
      )) +
      theme(axis.text.x = element_text(size = 12, face = "bold")) +
      theme(axis.text.y = element_text(size = 12, face = "bold"))
    
    return(temp_gg)
  }


plot_pairwise = function(data,user_name)
{
  gg_temp = ggpairs(data %>% select(Social.ST.min,
                              Pickups,
                              Duration.per.use,
                              Temperature_F,
                              Steps),
          lower = list(continuous = wrap("points", alpha = 0.3, size = 2.5)), 
          axisLabels = "none")+
    theme(text = element_text(size = 10), 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))+
    labs(title = paste("Pairwise Scatterplot for ",user_name))+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(gg_temp)
}

plot_IMF = function(data,user_name)
{
  if(!require("EMD"))
  {
    install.packages("EMD")
    
  }
  
  library("EMD")
  
  IMF_series = emd(data$Social.ST.min,1:nrow(data))$imf
  
  n = dim(IMF_series)[2]
  
  res = emd(data$Social.ST.min,1:nrow(data))$residue
  
  IMF_series = cbind(IMF_series,res)
  
  IMF_series = as.data.frame(IMF_series)
  
  x = 1:nrow(data)
  
  par(mfrow=c(n+1,1))
  
  for(i in 1:n)
  {
    plot(x,IMF_series[,i], type = "l", col = "blue", xlab = "Time", ylab = "IMF", main = paste("IMF",i,"for Social Screen Time for ",user_name))
  }
  
  plot(x,IMF_series[,n+1], type = "l", col = "red", xlab = "Time", ylab = "Residue", main = paste("Residue for Social Screen Time for ",user_name))
  
}


plot_heatmap = function(data,user_name)
{
  cormat = cor(data_xin %>% select(Social.ST.min,
                                   Duration.per.use,
                                   Temperature_F,
                                   Steps,))
  melted_cormat = melt(cormat)
  gg_temp = ggplot(melted_cormat, aes(Var1, Var2, fill=value))+
    geom_tile(color="white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint=0, limit=c(-1,1), space="Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
    coord_fixed()+
    labs(title = paste0("Correlation Heatmap for ",user_name),
         x = "Variables",
         y = "Variables")
  
  return(gg_temp)
}



data_select = function(data,user_name)
{
  data_select = data %>% select(Social.ST.min,
                                Stay.late,
                                Duration.per.use,
                                Temperature_F,
                                Snow,
                                Weekday,
                                Semester,
                                Steps) %>% mutate(
                                  Steps = (Steps-mean(Steps))/100,
                                  Step_Week = Steps*Weekday,
                                  Temperature_F = (Temperature_F-mean(Temperature_F)),
                                  Temperature_F_Snow = Temperature_F*Snow,
                                  Intercept = rep(1,nrow(data)))
  
  
  y = data_select["Social.ST.min"] %>% as.matrix()
  x= data_select %>% select(-Social.ST.min) %>% as.matrix()
  x = x[,c("Intercept",
           "Stay.late",
           "Duration.per.use",
           "Temperature_F",
           "Snow","Weekday",
           "Semester",
           "Steps","Step_Week",
           "Temperature_F_Snow"
  )]
  return(list(x=x,y=y))
}


data_select_lag = function(data,user_name)
{
  data_select = data %>% select(Social.ST.min,
                                Stay.late,
                                Duration.per.use,
                                Temperature_F,
                                Snow,
                                Weekday,
                                Semester,
                                Steps) %>% mutate(
                                  Social.ST.min.lag1 = lag(Social.ST.min,1),
                                  Steps = (Steps-mean(Steps))/100,
                                  Step_Week = Steps*Weekday,
                                  Temperature_F = (Temperature_F-mean(Temperature_F)),
                                  Temperature_F_Snow = Temperature_F*Snow,
                                  Intercept = rep(1,nrow(data))
                                )
  
  
  y = data_select["Social.ST.min"] %>% as.matrix()
  y= y[2:length(y),]
  x= data_select %>% select(-Social.ST.min) %>% as.matrix()
  x= x[2:(dim(x)[1]),c("Intercept",
                       "Social.ST.min.lag1",
                       "Stay.late",
                       "Duration.per.use",
                       "Temperature_F",
                       "Snow","Weekday",
                       "Semester",
                       "Steps","Step_Week",
                       "Temperature_F_Snow"
  )]
  return(list(x=x,y=y))
}













