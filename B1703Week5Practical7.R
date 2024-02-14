# ----- B1703 Week 5 | Basic Plots in R | 14.02.2024 -----

# ----- 1. Basic Plots in R -----
##### 1.1 Loading in Data #####
library(tidyverse)
load("/Users/seanmccrone/Downloads/Practical 4 R Data.RData")

##### 1.2. Creating Bar chart #####
Countries2016 <- CountryDatawithPop %>%
  filter(Year=="2016")%>%
  arrange(desc(TotalMedals))

Barchart <- Countries2016[1:15,]%>%
  ggplot(aes(x=Country.x, y=TotalMedals))+
  geom_bar(stat="identity", aes(fill=Country.x))

Barchart

##### 1.3. Improvements? #####

# Change x and y axis around
# Remove the colour from country this has no added benefit
# Sort the data on number of medals
# Remove the grey background
# Remove y-axis grid lines and x-axis minor grid lines
# Rename the axis

##### 1.4. Making these Improvements #####

Barchart <-Countries2016[1:15,] %>%
  ggplot(aes(x=reorder(Country.x,TotalMedals), y=TotalMedals)) +
  geom_bar(stat="identity", fill="steelblue")+coord_flip() +
  theme_minimal() +
  labs(x= "Country", y="Total Number of Medals in 2016") +
  theme(panel.grid.minor= element_blank(),panel.grid.major.y = element_blank())
Barchart

ggplotly(Barchart, tooltip="Total")

##### 1.5. Creating Lollipop Chart #####

Lollieplot <- Countries2016[1:15,]%>%
  ggplot(aes(x=reorder(Country.x,TotalMedals), y=TotalMedals)) +
  geom_segment(aes(xend=Country.x,yend=0),color="lightblue") +
  geom_point(size=2,color="steelblue")+coord_flip()+theme_minimal() +
  labs(x= "Country", y="Total Number of Medals in 2016") +
  theme(panel.grid.minor= element_blank(),panel.grid.major.y = element_blank())
Lollieplot

# ----- 2. Treemap, Bubble charts and Word clouds -----

#Creating a treemap using the treemapify package
library(treemapify)
Treemap <- Countries2016[1:10,]%>%
  ggplot(aes(area=TotalMedals,fill=Country.x, label=Country.x)) +
  geom_treemap()+
  geom_treemap_text()
Treemap

# Creating a bubble chart
BubbleChart <- Countries2016[1:10,]%>%
  ggplot(aes(x=Country.x,y=TotalMedals, size=TotalMedals)) +
  geom_point(alpha=0.5, colour="orange")+theme_minimal()
BubbleChart

# Creating a wordcloud using the package ggwordcloud
#install.packages("ggwordcloud")
library(ggwordcloud)  
Wordcloud <- Countries2016[1:10,]%>%
  ggplot(aes(label=Country.x,size=TotalMedals, color=Country.x)) +
  geom_text_wordcloud()+theme_minimal()
Wordcloud

library(packcircles)
# Creating a bubble chart without axis using the packcircles and ggplot2 packages

Top10Countries <- subset(Countries2016[1:10,])
packing <- circleProgressiveLayout(Top10Countries$TotalMedals, sizetype='area')
data <- cbind(Top10Countries, packing)

dat.gg <- circleLayoutVertices(packing, npoints=50)

BubbleChart2 <- ggplot() + 
  geom_polygon(data=dat.gg, aes(x,y, group = id, fill = as.factor(id)), colour = "blue", alpha=0.6) +
  geom_text(data=data, aes(x, y, size=TotalMedals, label=Country.x))+
  scale_size_continuous(range=c(1,4)) +
  theme_void()+
  theme(legend.position="none")+
  coord_equal()
BubbleChart2

# ----- 3. Pie Charts -----

# Creating Pie Chart ranked by Population Level Quartile Rank 
Tert <- quantile(Countries2016$Population, probs=seq(0,1,1/3),na.rm=TRUE)

Countries2016 <- Countries2016 %>%
  mutate(PopLevel= ifelse(Population>Tert[3], 1, ifelse(Population>Tert[2],2,3)))

Tert <- quantile(Countries2016$Population, probs=seq(0,1,1/3),na.rm=TRUE)

Countries2016 <- Countries2016 %>%
  mutate(PopLevel= ifelse(Population>Tert[3], 1, ifelse(Population>Tert[2],2,3)))

Countries2016 <- Countries2016 %>% 
  arrange(desc(PopLevel)) %>%
  mutate(prop = TotalMedals / sum(Countries2016$TotalMedals) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

Pie<-na.omit(Countries2016) %>%
  group_by(PopLevel) %>%
  reframe(TotalMed=sum(TotalMedals))%>%
  ggplot(aes("", TotalMed, fill=as.factor(PopLevel))) + 
  geom_bar(stat="identity", width=1, color="white")+ 
  coord_polar("y",start=0)+
  theme_void()+
  theme(legend.position="none") +
  geom_text(aes(y = c(450,140,15), label = c("High","Mid","Low")), color = "white", size=6) +
  scale_fill_manual(values = c("steelblue", "lightblue","blue"))+
  ggtitle("Proportion of 2016 medals by population level")

Pie

# ----- 4. Histograms -----

# Creating histogram of Athlete Height
AthletesData <- DatabyAthletePerYear %>%
  filter(Year=="2016")%>%
  arrange(desc(Height))

Histogram <- AthletesData%>%
  ggplot(aes())+
  geom_histogram(aes(x=Height))
Histogram

# Creating histogram of height density of each sex 
colors <- c("blue", "lightblue") # assign the grouping colours
FacetHisto <- AthletesData%>%
  ggplot(aes())+
  geom_histogram(aes(y=after_stat(density),x=Height, fill=Sex))+ #I choose to use density instead of the standard count to ensure fair comparison between the two (there are more men in the sample so using count would have resulted in a much higher histogram for men then women)
  facet_wrap(~Sex) + #use facetwrap to show two graphs within one visualisation
  scale_fill_manual(name="Sex", labels=levels(AthletesData$Sex),values=setNames(colors, levels(AthletesData$Sex))) #tell R which color belongs to which group

FacetHisto

# Now we are layering the two histograms over the general population for easier comparison of differences
# First we will need to create a dataset without the Sex variable, this is to ensure this variable can't be used for grouping.
AthletesData2 <- AthletesData %>%
  select(-Sex)

Histo2 <- FacetHisto +
  geom_histogram(data=AthletesData2, aes(y=after_stat(density),x=Height), alpha=0.5)
Histo2

# ----- 5. Boxplots -----

# First we will need to create a dataset without the Sex variable, this is to ensure this variable can't be used for grouping.
Mediantest<-median(AthletesData$Height,na.rm=TRUE) # calculate median of the group so we can add a reference line.

Boxplot <- AthletesData%>%
  drop_na(Height)%>% #I drop all rows which have missing height data to ensure the ordering of my plot works
  ggplot(aes(y = reorder(Sport, Height, median), x = Height, fill = Sport)) + #reorder let's me reorder Height on it's median
  geom_boxplot() +
  theme(legend.position = "none")+
  geom_vline(aes(xintercept=Mediantest, colour="red"))+ #Median line to compare
  ylab("Sport")
Boxplot

# ----- 6. Saving Files -----

save(DatabyAthleteAvg, DatabyAthletePerYear, TotalsPerCountryYear, CountryDatawithPop, file="Practical7.RData")


