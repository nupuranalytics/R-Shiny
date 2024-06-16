
mydata <- read.csv("Accidents_categorical.csv", header = T)
View(mydata)
library(ggplot2)
library(dplyr)
sum(is.na(mydata))  #to check the number of missing values in dataset
attach(mydata)

#Univariate Analysis
#Quantitative (Histogram)
#par(mfrow=c(6,3))
hist(Driver_IMD_Decile, col = "skyblue", xlab = "x-axis", main = 
       "Histogram of Driver IMD Decile")

ggplot(mydata, aes(x = Speed_limit)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Speed limit", 
       subtitle = "number of bins = 20",
       x = "speed")

# Create a kernel density plot of Season
ggplot(mydata, aes(x = Season)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Accidents by Season")

# plot the age distribution using a dotplot
ggplot(mydata, aes(x = Month_of_Year)) +
  geom_dotplot(fill = "pink", 
               color = "black") + 
  labs(title = "Month Of Year",
       y = "Proportion",
       x = "Month of Year")

# Use fixed-width bins
ggplot(mydata, aes(x = Number_of_Vehicles)) +
  geom_dotplot(method="histodot", binwidth = 0.1)


hist(Year, col = "darkblue", xlab = "Years", main = 
       "Histogram of number of years")
hist(Day_of_Month, col = "darkblue", xlab = "Years", main = 
       "Histogram of day of month")
hist(Day_of_Week, col = "yellow", xlab = "Years", main = 
       "Histogram of number of years")
hist(Hour_of_Day, col = "darkblue", xlab = "Years", main = 
       "Histogram of number of years")
hist(Age_of_Driver, col = "darkblue", xlab = "Age", main = 
       "Histogram of Age of Driver")
hist(Age_of_Vehicle, col = "darkblue", xlab = "Age", main = 
       "Histogram of Age of Vehicle")
hist(Age_of_Driver, col = "darkblue", xlab = "Years", main = 
       "Histogram of number of years")

#Categorical Variables

dev.off()  #to null the device

ggplot(mydata, aes(x = Urban_or_Rural_Area)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Area", 
       y = "Count", 
       title = "Accidents by Area")

ggplot(mydata, aes(x = Weather)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Area", 
       y = "Count", 
       title = "Accidents by Area")

# plot the distribution as percentages
ggplot(mydata, 
       aes(x = Region, 
           y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "Region", 
       y = "Percent",
       title  = "Accidents by Regions") +
  scale_y_continuous(labels = scales::percent)

# plot the bars with numeric labels
ggplot(mydata, 
       aes(x = Urban_or_Rural_Area, 
           y = n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")


# horizontal bar chart
ggplot(mydata, aes(x = X1st_Road_Class)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Road Class") +
  coord_flip() + theme(axis.text.x = element_text(angle = 45, 
                                                  hjust = 1))

ggplot(mydata, aes(x = High_Wind)) + 
  geom_bar() +
  labs(x = "High Wind",
       y = "Count",
       title = "Road Class")


# create a basic ggplot2 pie chart
plotdata <- mydata %>%
  count(Road_Type) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = Road_Type)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()

# create a treemap of Road_Type
install.packages("treemapify")
library(treemapify)
plotdata <- mydata %>%
  count(Road_Surface_Conditions)

ggplot(plotdata, 
       aes(fill = Road_Surface_Conditions, 
           area = n,
           label = Road_Surface_Conditions)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Accidents by Road Type") +
  theme(legend.position = "none")


#Barplot of lights
mydata %>% ggplot(aes(x = Lights, fill= factor(Lights)))+ geom_bar(position = "dodge") +
  scale_fill_discrete(name = "Heart Disease") +
  labs(subtitle="Analysis of Accidents by presence & absence of light",
       y="count",
       x="Lights",
       title = "Barplot")+
  theme(axis.title.x = element_text(colour="DarkGreen", size = 20),
        axis.title.y = element_text(colour="Red", size = 20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.title = element_text(colour = "DarkBlue", size = 30, family = "Courier"),
        plot.subtitle = element_text(colour = "black",size = 20))

# create a basic ggplot2 pie chart Vehicle Category
plotdata <- mydata %>%
  count(Vehicle_Category) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = Vehicle_Category)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()


ggplot(mydata, aes(x = Vehicle_Manoeuvre)) + 
  geom_bar() +
  labs(x = "High Wind",
       y = "Count",
       title = "Road Class")


ggplot(mydata, aes(x = Accident_Severity)) + 
  geom_bar() +
  labs(x = "High Wind",
       y = "Count",
       title = "Road Class")

#Bivariate Variables

ggplot(mydata, 
       aes(x = Region, 
           fill = Road_Type)) + 
  geom_bar(position = "dodge")


ggplot(mydata, 
       aes(x = Region, 
           fill = Road_Surface_Conditions)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

ggplot(mydata, 
       aes(x = Season, 
           fill = High_Wind)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Season, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Year, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Region, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Urban_or_Rural_Area, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Weather, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = High_Wind, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")

ggplot(mydata, 
       aes(x = Weather,
           y = Season)) +
  geom_violin() +
  labs(title = "Salary distribution by rank")


ggplot(mydata, 
       aes(x = Weather, 
           y = Season)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Salary distribution by rank")


library(ggplot2)
install.packages("ggridges")
library(ggridges)

ggplot(mydata, 
       aes(x = Weather, 
           y = Season, 
           fill = Season)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme(legend.position = "none")


ggplot(mydata, 
       aes(x = Lights, 
           fill = Accident_Severity)) + 
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank")

ggplot(mydata, 
       aes(x = Vehicle_Manoeuvre, 
           fill = Accident_Severity)) + 
  geom_bar(position = "dodge")


ggplot(mydata, 
       aes(x = Number_of_Vehicles, 
           y = Season)) +
  geom_point()



ggplot(mydata, 
       aes(x = Year, 
           y = Number_of_Vehicles)) +
  geom_point()

ggplot(mydata,
       aes(x = Age_of_Vehicle), 
           y = Number_of_Vehicles) +
  geom_point(color= "steelblue")


ggplot(mydata, 
       aes(x = Year,
           y = Number_of_Vehicles)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")


library(dplyr)
plotdata <- mydata %>%
  filter(Urban_or_Rural_Area == "Urban" & 
           Year == 2014)

# basic Cleveland plot of life expectancy by country
ggplot(plotdata, 
       aes(x= Month_of_Year, y = Accident_Severity)) +
  geom_point()
  
