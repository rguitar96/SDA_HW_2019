library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(ggpubr)

#Climograph
climograph <- data.frame(
  Month=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  precipitation=c(5,2,7,15,303,547,558,602,368,206,60,7),
  average_temp <- c(25.05,26.9,28.8,30.7,29.2,27.4,26.9,26.85,27.3,27.9,27.2,25.25))

correct_order <- c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep","Oct","Nov","Dec")

plot_climograph <-  ggplot(data = climograph, mapping = aes(x = Month, y = precipitation, group = 1)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  geom_line(mapping = aes(y = average_temp*10),
            color = "red",
            size = 1.2) +
  scale_y_continuous(
    "Precipitation [mm]",
    sec.axis = sec_axis ( ~ . *0.1, name = "Temperature [°C]")) +
  scale_x_discrete(limits = correct_order) +
  ggtitle("Climograph")
plot_climograph

#Temperature
temp_mat <- matrix(c(32.2,34.5,36,37,33.4,30.2,29.7,29.6,30.4,31.5,32,31.5,25.05,26.9,28.8,30.7,29.2,27.4,26.9,26.85,27.3,27.9,27.2,25.25,17.9,19.3,21.6,24.3,25,24.5,24.1,24.1,24.2,24.2,22.4,19),
                   nrow = 12,ncol = 3)

mat.melted <- melt(temp_mat)
print(mat.melted)

plot_temp<-ggplot(mat.melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "red")+
  geom_text(aes(label = value))+
 scale_x_discrete(limit = c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
scale_y_discrete(limit = c("Maximum", "Average", "Minimum"))+
  labs(x = "Month", y = "Average temperature (Cº)", title = "Average Temperatures")

#Precipitation
  prec_df <- data.frame(
    month=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
    data=c(5,2,7,15,303,547,559,602,368,206,60,7,62,66,69,66,73,85,86,87,85,78,75,71,0,0,1,2,14,23,26,25,20,10,3,1),
    Measurement=rep(c("Precipitation (mm)","Relative humidity (%)","Wet days (>0.1mm)"),each=12))
  
  plot_prec<-ggplot(prec_df, aes(x=month, y=data, group=Measurement)) +
    geom_line(aes(color=Measurement))+
    geom_point(aes(color=Measurement),size=2.5)+
    scale_color_brewer(palette="Paired")+
    theme_minimal()+
    labs(x = "Month", y = "Value", title = "Precipitation Information")+
    scale_x_discrete(limits = correct_order) 

  #Other (Sun and Wind)
  other_df <- data.frame(
    month=c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
    data = c(9.7,8.8,9.4,9.4,5.8,2.6,2.5,3,3.1,6.5,9,9.3,11.2,11.6,12,12.5,12.9,13.1,12.8,12.7,12.2,11.7,11.3,11.1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,2,2,2,1,1,1,1,1),
    Measurement=rep(c("Average Sunlight Hours/ Day","Day Lenght (Hours)","Days with Frost","Average Wind Speed (Beaufort)"),each=12))

  plot_other<-ggplot(other_df, aes(x=month, y=data, group=Measurement)) +
    geom_line(aes(color=Measurement))+
    geom_point(aes(color=Measurement),size=2.5)+
    scale_color_brewer(palette="RdYlBu")+
    theme_minimal()+
    labs(x = "Month", y = "Value", title = "Sun and Wind Information")+
    scale_x_discrete(limits = correct_order) 
  
  plot_combined<-ggarrange(plot_climograph,plot_temp,plot_prec,plot_other,nrow=2,ncol = 2)+
    labs(title = "Rangoon, Yangon, Myanmar Climate Graph (Altitude 15m)",x = "", y="", fill="")+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  plot_combined
  