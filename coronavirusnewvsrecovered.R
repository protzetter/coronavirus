
# coronavirus evolution of new vs recovered cases over time using the Johns Hopkins data set
# 3.03.2020, protzetter@bluewin.ch, https://www.linkedin.com/in/rotzetter/

library(gganimate)
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gifski)
library(av)



# load data from Johns Hopkins github

confirmedCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
deathCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
recoveredCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')


# set system locale for date conversion purpose
Sys.setlocale("LC_TIME", "English")


#convert data sets into columns and remove unwanted columns

confirmedCases<-confirmedCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
confirmedCases<-confirmedCases%>%group_by(`Country/Region`,variable)%>%summarise(Confirmed=sum(value))

deathCases<-deathCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
deathCases<-deathCases%>%group_by(`Country/Region`,variable)%>%summarise(Deaths=sum(value))

recoveredCases<-recoveredCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
recoveredCases<-recoveredCases%>%group_by(`Country/Region`,variable)%>%summarise(Recovered=sum(value))


# rename table columns
colnames(confirmedCases)<-c("Country","Date","Confirmed")
colnames(deathCases)<-c("Country","Date","Death")
colnames(recoveredCases)<-c("Country","Date","Recovered")

# merge all atbles together

mergedCases<-merge(confirmedCases,deathCases, by.y=c("Country","Date"))
mergedCases<-merge(mergedCases,recoveredCases, by.y=c("Country","Date"))

# convert factors to date format

mergedCases$Date<-as.Date(mergedCases$Date,"%m/%d/%y")

# summarize cases by date
df1<-mergedCases %>% group_by(Date) %>% summarise_at(c("Confirmed","Recovered","Death"),sum)

dff<-as.data.frame(rbind( c(0,0,0),apply(df1[-1] , 2 , diff )))
colnames(dff)<-c("New","New recovered","New Death")

df1<-cbind(df1,dff)

# stack columns together and add state columns to each case
df2 <- data.frame(Date=rep(df1$Date, 2), 
                  cases=c(df1$New, df1$`New recovered`), 
                  State=rep(c("New","Recovered"), each=nrow(df1)))


# retrieve last update date for title
lastDate<-max(df1$Date)

# define plot object
p<-ggplot(df2, aes(x=Date, y=cases, group=State, color=State)) +
  geom_line() +
  geom_segment(aes(xend=max(Date), yend = cases), linetype=2, colour='blue') +
  geom_point(size = 3) + 
  geom_text(aes(x = max(Date)+.1, label = sprintf("%5.0f", cases)), hjust=-0.5) +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  xlab("Day") +
  ylab("Number of daily cases") + ggtitle(paste("Daily new cases versus recovered cases",lastDate)) +
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8))+
  theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  plot.margin = margin(5.5, 40, 5.5, 5.5))

# create animation gif file
animate(p, fps=5,renderer = gifski_renderer("virusevolutiondaily.gif"), end_pause=10)

#create mp4 file
animate(p, fps=5,renderer=av_renderer('virusevolutiondaily.mp4'),  end_pause = 10)
