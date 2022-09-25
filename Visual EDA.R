library(dplyr)
library(ggplot2)
library(plotly)

getwd()
setwd("C:/Users/Admin/Downloads")


# Reading the Dataset
policing <- read.csv('Prepped Data.csv')
head(policing, 20) #  Viewing the first 20 rows of the data set

# Removing the first row of the data set which contains the duplicate column names
policing <- policing[-1,]
head(policing, 10)

#printing the class of the data
sapply(policing, class)

#changing class of the data

policing$OFFICER_ID <- as.integer(as.character(policing$OFFICER_ID))
policing$OFFICER_YEARS_ON_FORCE<- as.integer(as.character(policing$OFFICER_YEARS_ON_FORCE))
sapply(policing, class)

policing[, 12]     <- as.numeric(policing[, 12])
policing[, 20]     <- as.numeric(policing[, 20])
policing[, 21]     <- as.numeric(policing[, 21])
policing[, 22]     <- as.numeric(policing[, 22])
policing[, 32]     <- as.numeric(policing[, 32])
policing[, 33]     <- as.numeric(policing[, 33])
policing[, 5]     <- as.factor(policing[, 5])
policing[, 6]     <- as.factor(policing[, 6])
policing[, 9]     <- as.factor(policing[, 9])
policing[, 11]     <- as.factor(policing[, 11])
policing[, 13]     <- as.factor(policing[, 13])
policing[, 14]     <- as.factor(policing[, 14])
policing[, 15]     <- as.factor(policing[, 15])
policing[, 17]     <- as.factor(policing[, 17])
policing[, 18]     <- as.factor(policing[, 18])
policing[, 23]     <- as.factor(policing[, 23])
policing[, 24]     <- as.factor(policing[, 24])
policing[, 34]     <- as.factor(policing[, 34])
policing[, 35]     <- as.factor(policing[, 35])


# Looking at the summary statistics of the data set
summary(policing)
colnames(policing) # Column names of the data set
str(policing) # class of features and observation count of data set
dim(policing) # dim is used to print out the number of features and observations
names(policing)

#Select the numeric columns and store result
policing.numeric <- policing[,sapply(policing, is.numeric)]
policing.numeric
dim(policing.numeric)

# plotting the distribution of the data

par(mfrow=c(3, 3))
colnames <- dimnames(policing.numeric)[[2]]
for (i in 1:8) {
  hist(policing.numeric[,i], main=colnames[i], probability=TRUE, col="gray", border="white")
}
dev.off()


# dealing with the missing values

library(finalfit)
sum(is.na(policing))

# Type casting the date feature
library(lubridate)

new_date <- as.Date(policing$INCIDENT_DATE, "%m/%d/%Y")
sapply(new_date, class)
new_date

policing['new_date'] <- new_date

hiringbyy <- as.Date(policing$OFFICER_HIRE_DATE, "%m/%d/%Y")
sapply(hiringbyy, class)

policing['yearwisehiring'] <- year(hiringbyy)

dim(policing)


#Visualization includes several different plots with different findings


xgennew <- policing

xgen <- distinct(policing, OFFICER_ID, OFFICER_RACE, .keep_all= TRUE) # making subset of the data for distinct values

#Correlation

policing.numeric
corr.dataset = policing.numeric %>% select(SECTOR, BEAT, REPORTING_AREA, SUBJECT_ID, OFFICER_ID, OFFICER_YEARS_ON_FORCE)
corr = cor(corr.dataset)
require(ggcorrplot)
corr.p = cor_pmat(corr)
corr.p

View(corr)

(G = ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
                outline.col = "black", ggtheme = ggplot2::theme_bw(),
                colors = c("yellow", "green", "blue")))

#presenting officers genders with respect to their count using bar plot

gender = ggplot(data = policing, aes(x=OFFICER_GENDER))
gender1 = gender + geom_bar(fill="#fb8072", alpha=0.4) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "Blue") +
  xlab("GENDER") + ylab("Number of Officers")
gender2 = gender1 + theme_bw() + theme(axis.title = element_text(size = 12, color = "blue"), axis.text = element_text(size = 10, face = "bold"))
gender2

# Plotting distinct count of officers

distgenderplot <- policing %>%
  group_by(OFFICER_GENDER) %>%
  summarise(n=n_distinct(OFFICER_ID)) %>% 
  ggplot(.,aes(OFFICER_GENDER, n)) + geom_bar(stat = 'identity', fill="#fb8072", alpha=0.5)  + ylim(0,1000) +
  xlab("Gender Segregation") + ylab("Number of Officers")
distgenderplot1 = distgenderplot + theme_bw() + theme(axis.title = element_text(size = 12, color = "blue"), axis.text = element_text(size = 10, face = "bold"))
distgenderplot1



#plotting the numbers of years served 
#Using interactive histogram plot
(hist(x=xgen$OFFICER_YEARS_ON_FORCE, breaks = 4, ylim = c(0,1000), col = 2:6, main = "Yearly Grouped Segregation of number of years served", ylab = "Number of officers", xlab = "No of Years", border = "White", labels = TRUE))
text(5, 325, "Majority", srt = 90)

yearsserved = ggplot(data=xgen, aes(x=OFFICER_YEARS_ON_FORCE))
noofyears = yearsserved + geom_histogram(binwidth = 1, fill="#2b8cbe", alpha=0.6) +
  xlab("Years Served") + ylab("Number of officers") + xlim(0,40) + ylim(0,120) 
ggplotly(noofyears)

# Plotting the male and female officers with respect to their race

racegender <- table(policing$OFFICER_GENDER, policing$OFFICER_RACE)
racegender

library(RColorBrewer)

racegender1 <- table(xgen$OFFICER_GENDER, xgen$OFFICER_RACE)
racegender1

xgender <- barplot(as.matrix(t(racegender1)), beside = TRUE, main = "barplot",
                   legend.text = TRUE,
                   args.legend=list(bty="7", x = "topleft" , ncol = 3), ylim = c(0,800),
                   col=brewer.pal(7,"Set1"), border="Black")
ygender<-as.matrix(t(racegender1))
text(xgender, ygender+100, labels = as.character(ygender))



xgen <- distinct(policing, OFFICER_ID, OFFICER_RACE, .keep_all= TRUE)
dim(xgen)

# Plotting the division & district wise Distribution of Dallas Police

table(policing$DIVISION)

distgen <- xgen %>%
  group_by(DIVISION) %>% summarise(n_distinct(OFFICER_ID))
distgen

# number of districts covered by single division

table(policing$LOCATION_DISTRICT)

locindiv <- policing %>% group_by(DIVISION) %>% summarise(noofdistricts = n_distinct(LOCATION_DISTRICT)) %>% arrange(desc(noofdistricts))
locindiv

DIVISIONS <- locindiv$DIVISION
clrgenre <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69', '#000000')
ggplot(locindiv, aes(x=noofdistricts, fill=DIVISIONS)) + geom_histogram(binwidth=1) + scale_fill_manual(values = clrgenre) +
  labs(x = "Number of Districts",
       y = "Number of Divisions Covering Districts",
       title = "Graph Visualizing number of districts covered by divisions")

# Density plot visualizing the officers injury with respect to their years of service

ggplot(policing, 
       aes(x = OFFICER_YEARS_ON_FORCE, 
           fill = OFFICER_INJURY)) +
  geom_density(alpha = 0.4) +
  labs(title = "Years of experience distribution by officer injury")

# box plot using whiskers visualizing the officers hospitalization with respect to their years of service

library(ggpol)
library(scales)
ggplot(policing, 
       aes(x = factor(OFFICER_HOSPITALIZATION,
                      labels = c("Not Hospitalized",
                                 "Hospitalized")), 
           y = OFFICER_YEARS_ON_FORCE, 
           fill=OFFICER_HOSPITALIZATION)) +
  geom_boxjitter(color="black",
                 jitter.color = "darkgrey",
                 errorbar.draw = TRUE) +
  labs(title = "Officer Hospitalization by Years of Experience", 
       subtitle = "9-month salary for 2008-2009",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Number of arrests made by officers

require(dplyr)
install.packages("lubridate")
require(lubridate)




datenew1 <- datenew1 %>% mutate(hiringbyyear= case_when(
  hyear >= 2000 & hyear <= 2010  ~ "2001 to 2010",
  hyear > 1978 & hyear < 2000 ~ "till 1990",
  TRUE ~ "after 2010")
)

G = ggplot(datenew1, aes(hyear, OFFICER_YEARS_ON_FORCE, color = hiringbyyear)) + geom_point()
G = G + scale_color_manual(values = c("blue", "yellow", "red"))

New_names <- c(`Yes` = "Made Arrest", `No` = "Did Not Make Arrest")
arrestsmade <- G + facet_wrap(~ SUBJECT_WAS_ARRESTED, labeller = as_labeller(New_names)) + labs(title = "Officer Arrest Status", 
                                                                                                subtitle = "Categorized Hiring Date & their arrest record",
                                                                                                x = "Hiring Year",
                                                                                                y = "Years of Experience")
arrestsmade

# Hospitalization and injury of officers with respect to their years of experience and year of joining

library(forcats)
xgennew <- datenew1

levels(datenew1$OFFICER_HOSPITALIZATION) <- list(NotHospitalized = "No", Hospitalized = "Yes")
levels(datenew1$OFFICER_INJURY) <- list(NotInjured = "No", Injured = "Yes")

G = ggplot(xgennew, aes(x=hyear, y =OFFICER_YEARS_ON_FORCE)) + geom_boxplot(aes(group = hyear))                    
(G = G + facet_grid(OFFICER_HOSPITALIZATION ~ OFFICER_INJURY) + labs(title = "Officer Injury & Hospitalization", 
                                                                     x = "Hiring Year",
                                                                     y = "Years of Experience"))
# Total number of crimes

distcrimes <- policing %>%
  summarise(n=n_distinct(SUBJECT_ID))
distcrimes

#month wise visualization of reported crimes


new_date_crime <- as.Date(policing$INCIDENT_DATE, "%m/%d/%Y")
sapply(new_date_crime, class)
new_date_crime
policing['crime_date'] <- new_date_crime
policing['crime_month'] <- month(new_date_crime)



(hist(x=policing$crime_month, breaks = 8, ylim = c(0,1000), col = 2:6, main = "Monthly count of crimes reported", ylab = "Number of crimes", xlab = "Months", border = "White", labels = TRUE))
text(1.5, 200, "High Count", srt = 90)

# Another way of plotting month-wise crimes

datenew <- policing

library(lubridate)
library(DT)

datenew <- policing %>%
  mutate(new_inc_date = mdy(INCIDENT_DATE)) %>%
  mutate(CRIMEMONTH = month(new_inc_date, label =TRUE)) %>% 
  mutate(CRIMEDAY = day(new_inc_date)) %>% 
  mutate(CRIMEWEEK = wday(new_inc_date, label = TRUE))

datenew %>%
  count(CRIMEMONTH) %>%
  mutate(`Number of Crime` = n) %>%
  select(CRIMEMONTH, `Number of Crime`)%>% 
  DT::datatable(colnames = c("Month", "Number of Crimes"), 
                options = list(pageLength = 12))



###### subject gender reported for crimes



# Create test data.
subgen <- policing %>% 
  group_by(SUBJECT_GENDER) %>%
  summarise(n=n_distinct(SUBJECT_ID))
subgen

# Compute percentages
subgen$fraction <- subgen$n / sum(subgen$n)

# Compute the cumulative percentages (top of each rectangle)
subgen$ymax <- cumsum(subgen$fraction)

# Compute the bottom of each rectangle
subgen$ymin <- c(0, head(subgen$ymax, n=-1))

# Compute label position
subgen$labelPosition <- (subgen$ymax + subgen$ymin) / 2

# Compute a good label
subgen$label <- paste0(subgen$SUBJECT_GENDER, "\n value: ", subgen$n)

subgen
newsubgen <- subgen[-c(3, 4), ]
newsubgen


# 5ake the plot

ggplot(newsubgen, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=SUBJECT_GENDER)) +
  geom_rect() +
  geom_text( x=5.5, aes(y=labelPosition, label=label, color=SUBJECT_GENDER), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=5) +
  scale_color_brewer(palette=5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("No of Female and Male reported for crime") 



######################new#####################


################ subject races and crimes percentage

require(rAmCharts)
library(dplyr)
piedata <- policing %>% 
  group_by(SUBJECT_RACE) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percentage = `n` / sum(`n`)) %>% 
  arrange(percentage) %>%
  mutate(labels = scales::percent(percentage))

piedata %>% dplyr::select(label=SUBJECT_RACE, value = n) %>%
  amPie(., inner_radius = 25, 
        depth = 6, 
        show_values = TRUE,
        legend = TRUE,
        export = TRUE,
        main = "Percent of criminal by Race")


# arrested to injury
table(policing$SUBJECT_INJURY)

arrtoinj <- policing %>% filter(SUBJECT_WAS_ARRESTED == 'Yes') %>% group_by(SUBJECT_INJURY) %>% summarise(length(SUBJECT_WAS_ARRESTED))

arrtoinj

# offense types reported in 2016

library(dplyr)
library(ggplot2)

datenew1 %>% count(SUBJECT_DESCRIPTION) %>%
  ggplot(aes(x= reorder(SUBJECT_DESCRIPTION, n), y = n)) +
  geom_col(fill = "#756bb1") +
  labs(x = "Offense Type",
       y = "Number of Crimes",
       title = paste0("Offense types reported in 2016")) +
  coord_flip() +
  theme_minimal()
# Map Plotting of total crimes in Dallas


policing %>% 
  leaflet(.) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~LOCATION_LONGITUDE,
                   lat = ~LOCATION_LATITUDE, 
                   radius=0.01,
                   fillOpacity = 0.001)

# Specific Plot of Dallas defining the crime spots

sbbox <- make_bbox(lon = c(policing$LOCATION_LONGITUDE), lat = c(policing$LOCATION_LATITUDE), f = .1)
dallas = get_map(location=sbbox, zoom=10, maptype="terrain")
dallas1 = ggmap(dallas)

dallas1 +
  geom_point(data = policing, mapping = aes(x =LOCATION_LONGITUDE, y =LOCATION_LATITUDE), 
             color = "red") +
  geom_text(data = policing, 
            mapping = aes(x = LOCATION_LONGITUDE+0.1,
                          y = LOCATION_LATITUDE,
                          label = "Crime Spots"),
            size = 2, color = "gray20", 
            fontface = "bold", 
            check_overlap = T) 

# Categorization of the map



datenew <- formap


map <- map %>% addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Mentally unstable",], 
           group = "Mentally unstable",col="#d73027")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Alcohol",],
             group = "Alcohol",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Unknown",], 
             group = "Unknown",col="#fdae61")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Unknown Drugs",], 
             group = "Unknown Drugs",col="#fee090")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="None detected",], 
             group = "None detected",col="#ffffbf")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Alcohol and unknown drugs",], 
             group = "Alcohol and unknown drugs",col="#e0f3f8")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Unknown if Armed",], 
             group = "FD-Unknown if Armed",col="#abd9e9")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="NULL",], 
             group = "NULL",col="#74add1")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Marijuana",], 
             group = "Marijuana",col="#4575b4")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Suspect w/ Gun",],
             group = "FD-Suspect w/ Gun",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Suspect Unarmed",],
             group = "FD-Suspect Unarmed",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Suspect w/ Other Weapon",],
             group = "FD-Suspect w/ Other Weapon",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Motor Vehicle",],
             group = "FD-Motor Vehicle",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="FD-Animal",],
             group = "FD-Animal",col="#f46d43")%>%
  addCircles(data = datenew1[datenew1$SUBJECT_DESCRIPTION=="Animal",],
             group = "Animal",col="#f46d43")%>%
  addCircleMarkers(lng = ~long,
                   lat = ~LOCATION_LATITUDE, 
                   radius=1,
                   fillOpacity = 0.001,
                   group = SUBJECT_DESCRIPTION)
map




