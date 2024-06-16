library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(stringr)
library(gganimate)
library(scales)
library(ggalt)
library(rbokeh)

property_org <- data.frame(read.csv("10_Property_stolen_and_recovered.csv",stringsAsFactors = T))
property_org

location_org <- data.frame(read.csv("poptable.csv", stringsAsFactors = T))
location_org[,1] = NULL
# Backup the original data and apply transformations on it. The original data
# would be used as reference in case of data failures.
property<- property_org 
str(property)
location <- location_org
str(location)

#changing uppercase to lowercase
location$State.Name <- tolower(location$State.Name)
property$Area_Name <- tolower(property$Area_Name)

#replacing and to &
location$State.Name = trimws(location$State.Name,which = "right")
location$State.Name[location$State.Name == "andaman and nicobar"] = paste("andaman and nicobar", "islands")
location$State.Name <-str_replace_all(location$State.Name, " and ", " & ")
location$State.Name

#combined data
merged_data_org <- inner_join(property, location,keep = T, by = c('Area_Name' = 'State.Name'), na_matches = c('na','never'))
merged_data_org$Area_Name<-as.factor(merged_data_org$Area_Name)
# Backup the original data and apply transformations on it. The original data
# would be used as reference in case of data failures.
merged_data <- merged_data_org


#1.Univariate Analysis
par(mfrow=c(2,2))
hist(merged_data$Cases_Property_Recovered, col = "skyblue", xlab = "Property Recovered", main = 
       "Analysis of Cases Property Recovered")
hist(merged_data$Cases_Property_Stolen, col = "Red", xlab = "Property Stolen", main = 
       "Analysis of Cases Property Stolen")
hist(merged_data$Value_of_Property_Recovered, col = "green", xlab = "Value of Property Recovered", main = 
       "Analysis of Value of Property Recovered")
hist(merged_data$Value_of_Property_Stolen, col = "pink", xlab = "Value of Property Stolen", main = 
       "Analysis of Value of Property Stolen")
par(mfrow=c(1,1))

#2.Analysis of Value of property recovered by Group Name which is number of properties
ggplot(merged_data, aes(x = Value_of_Property_Recovered)) +
  geom_histogram(fill = "blue",
                 color = "white", bins = 40) +
  facet_wrap(~Group_Name, ncol = 1) + #creating histogram in once by group name
  labs(title = "Value of Property Recovered histograms by Group Name",
       y="Count",
       x="Value of Property Recovered")

#3.generating the bar graph, the total number of property cases reported in each state of India by Year
plotdata <- data.frame(merged_data %>% group_by(Area_Name))
ggplot(plotdata, 
       aes(x = Area_Name, 
           y = Cases_Property_Recovered,fill= factor(Year))) +
  geom_bar(stat = "identity",position = 'dodge') +
  labs(title = "Total number of recovered property cases",
       x = "Area",
       y = "Property Recovered") + 
  scale_fill_discrete(name="Topographical Feature",breaks=c(1, 2)) #giving levels in the scale +
  theme(axis.text.x = element_text(angle = 110)) #change the angle of label on y-axis

#4.generating the bar graph, the total number of property stolen cases reported in each state of India
merged_data %>% ggplot(aes(Area_Name, Cases_Property_Stolen/(10000), fill= Area_Name))+ geom_boxplot() +
  labs(subtitle="Analysis of Property Stolen Area Wise",
       y="Cases of Stolen Property",
       x="Area Name",
       title = "Boxplot") +
  #below theme function use to customize the legends, axis, color, title & subtitle
  theme(axis.title.x = element_text(colour="Blue", size = 15),
        axis.title.y = element_text(colour="Red", size = 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 20, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 15))+
  theme(axis.text.x = element_text(angle = 90)) #change the angle of label on y-axis


#5.Analysis of number of property recovered cases and it's value by line and point together
ggplot(merged_data, 
       aes(x = Cases_Property_Recovered, 
           y = Value_of_Property_Recovered/(100000), group = Area_Name)) +
  geom_line(aes(linetype=Area_Name))+
  geom_point(aes(color=Area_Name)) +
  labs(subtitle="Analysis of Property Recovered and it's value",
       y="Cases of Property Recovered",
       x="Value of Property",
       title = "Lineplot with Scatterplot") +
  theme(axis.title.x = element_text(colour="Red", size = 15),
        axis.title.y = element_text(colour="Black", size = 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 20, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 15))


#6.Analysis of number of property stolen cases and it's value based on Area Name
ggplot(merged_data, 
       aes(Cases_Property_Stolen,Value_of_Property_Stolen/(100000)
           , color = Area_Name)) + geom_point(color= "steelblue") +
  geom_smooth(method = lm) + #showing regression line on scatterplot
  labs(subtitle="Analysis of Property Stolen and it's value",
       y="Vaue of Stolen Property",
       x="Cases of Stolen Property",
       title = "Scatterplot") +
  theme(axis.title.x = element_text(colour="Orange", size = 15),
        axis.title.y = element_text(colour="Red", size = 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 20, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 15))+
  theme(axis.text.x = element_text(angle = 110)) #change the angle of label on y-axis


#7.Creating 3D plot : Analysis of Property recovered and its value
Dynamic_Plot <- plot_ly(merged_data, x = ~Cases_Property_Recovered, y = ~Value_of_Property_Recovered, z = ~Area_Name) %>%
  add_markers(color = ~Area_Name) #generating color by area name 
Dynamic_Plot

#creating web link for chart by plotly
ggplotly(Dynamic_Plot)
Sys.setenv("plotly_username"="NupurR00195712")
Sys.setenv("plotly_api_key"="lBTQjSHjc7L4enYNisIL")
chart_link <- api_create(Dynamic_Plot,filename = "Animation")
chart_link

#8.checking the total number of property recovered cases reported by Year

Data <- plot_ly(merged_data, type='bar', x = ~Area_Name, y = ~Cases_Property_Recovered, text = ~Value_of_Property_Recovered, name="",
                hovertemplate = paste('%{x}', '<br>Value_of_Property_Recovered: %{text:.2s}<br>'),
                texttemplate = '%{y:.2s}', textposition = 'outside')
figure <- Data %>% layout(uniformtext=list(minsize=10, mode='hide'))
figure

#creating web link for chart
ggplotly(figure)
Sys.setenv("plotly_username"="NupurR00195712")
Sys.setenv("plotly_api_key"="SkeP1BaJBMVNmCJPjBsd")
chart_link <- api_create(figure,filename = "Animation")
chart_link


#9.Dynamic Scatter Plot : #checking the total number of property stolen cases reported by Year
myplot <- ggplot(merged_data, aes(x=Year, y=Cases_Property_Stolen)) + 
  geom_point(aes(color=Area_Name, size=3)) 

#the animating the plot by gganimate
animate <- myplot + transition_time(Year) +
  shadow_mark() + scale_x_continuous(limits = c(2001,2010)) + 
  xlab("Year") + ylab("Property Stolen")

#With the help of transition time each point moves by the variable and passes through it.
#and it is done by gganimate function
animate(animate, width = 600, height = 400)
anim_save("scatterplot.gif") #saving plot as gif in system

#10.Analysis of Number of properties as per Area Name of India
ggplot(merged_data, 
       aes(x = Area_Name, 
           fill = Group_Name)) + 
  geom_bar(position = "stack") + theme_bw() +
  transition_states(
    Year,
    transition_length = 1,
    state_length = 1
  ) +
  ease_aes('sine-in-out') +
  labs(subtitle="Analysis of Property as per Area",
       y="Count of Porperty",
       x="Area Name",
       title = "Barplot") +
  theme(axis.title.x = element_text(colour="Green", size = 15),
        axis.title.y = element_text(colour="Red", size = 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 20, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 15))+
  theme(axis.text.x = element_text(angle = 90))

#saving barplot as gif
anim_save("Barplot.gif")


#11.Analysis of Property of cases recovered by Group Name
figure <- plot_ly(merged_data, type='pie', labels = ~Area_Name, values = ~Cases_Property_Recovered, textposition = 'inside')
Final <- figure %>% layout(uniformtext=list(minsize=14, mode='hide'),title = "The labels are automatic based on the Area")
Final

#creating web link for the plot
ggplotly(Final)
Sys.setenv("plotly_username"="NupurR00195712")
Sys.setenv("plotly_api_key"="0oAlHkCRfUC7kelxbEMp")
chart_link <- api_create(Final,filename = "Animation")
chart_link

#12.Animated Scatterplot by plotly function

data <- merged_data
plot <- data %>%
  plot_ly(
    x = ~Cases_Property_Stolen, 
    y = ~Cases_Property_Recovered, 
    size = ~Value_of_Property_Recovered, 
    color = ~Group_Name, 
    frame = ~Year, 
    text = ~Area_Name, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
plot <- plot %>% layout(
  xaxis = list(
    type = "log"
  )
)
plot <- plot %>% animation_opts(
  1000, easing = "elastic", redraw = FALSE
)
#play button to the right handside of the plot
plot <- plot %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)
#slider to generate plot by year
plot <- plot %>% animation_slider(
  currentvalue = list(prefix = "YEAR ", font = list(color="blue"))
)

plot

#generating weblink for graph
ggplotly(plot)
Sys.setenv("plotly_username"="NupurR00195712")
Sys.setenv("plotly_api_key"="hnHLAyLzBuOdza1UQaRK")
chart_link <- api_create(fig,filename = "playbutton")
chart_link


#13 Analysing Property Recovered, Stolen and It's Values Across the States of India
library(rbokeh)
figure() %>%
  ly_points(Cases_Property_Recovered, Cases_Property_Stolen, data=merged_data,
            color = Area_Name, glyph = Area_Name, xlab = "Property Recovered", ylab = "Property Stolen",
            hover = list(Group_Name, Sub_Group_Name,Value_of_Property_Recovered,Value_of_Property_Stolen))


#14.Dumbbell Chart
library(ggalt)
ggplot(merged_data, 
       aes(y = Area_Name,
           x = Cases_Property_Recovered,
           xend = Cases_Property_Stolen)) +  
  #will generate dot plot
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red",
                show.legend = NA) +
  theme_minimal() + 
  labs(title = "Analysis of Property Recovered & Property Stolen",
       subtitle = "2001 to 2010",
       x = "Property Recovered",
       y = "Area Name") 



#15.Showing the number of property cases recovered by sub group name
leaflet() %>% addTiles() %>% addMarkers(data = merged_data,lng= ~longitude
                                        ,lat= ~latitude
                                        ,popup = ~paste(Sub_Group_Name,': ',Cases_Property_Recovered,sep = ''))

#popup function will display the number of cases property recovered by subgroup when we click on area name



