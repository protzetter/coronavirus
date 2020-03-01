
# coronavirus evolution of cases over time using the Johns Hopkins data set
# 1.03.2020, protzetter@bluewin.ch, https://www.linkedin.com/in/rotzetter/

library(gganimate)
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gifski)
library(av)

data(coronavirus)
coronavirus %>%filter(type == "confirmed") %>%group_by(Country.Region) %>%summarise(total = sum(cases)) %>%arrange(-total) %>%head(20)


confirmedCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
deathCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
recoveredCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')


Sys.setlocale("LC_TIME", "English")





switzerlandConfirmed<-confirmedCases%>% select(-c(Lat,Long,'Province/State'))%>%filter(`Country/Region`=='Switzerland')
switzerlandDeath<-deathCases%>% filter(`Country/Region`=='Switzerland')
switzerlandrecovered<-recoveredCases%>% filter(`Country/Region`=='Switzerland')
switzerlandConfirmed<-melt(switzerlandConfirmed, id='Country/Region')

confirmedCases<-confirmedCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
confirmedCases<-confirmedCases%>%group_by(`Country/Region`,variable)%>%summarise(Confirmed=sum(value))

deathCases<-deathCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
deathCases<-deathCases%>%group_by(`Country/Region`,variable)%>%summarise(Deaths=sum(value))

recoveredCases<-recoveredCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
recoveredCases<-recoveredCases%>%group_by(`Country/Region`,variable)%>%summarise(Recovered=sum(value))

colnames(confirmedCases)<-c("Country","Date","Confirmed")
colnames(deathCases)<-c("Country","Date","Death")
colnames(recoveredCases)<-c("Country","Date","Recovered")

mergedCases<-merge(confirmedCases,deathCases, by.y=c("Country","Date"))
mergedCases<-merge(mergedCases,recoveredCases, by.y=c("Country","Date"))

mergedCases$Date<-as.Date(mergedCases$Date,"%m/%d/%y")

p<-mergedCases %>% group_by(Date) %>% summarise_at(c("Confirmed","Recovered","Death"),sum)%>% ggplot(aes(x=Date)) + geom_line(aes(y=Confirmed, color="Confirmed")) + 
  geom_line(aes(y=Death, color="Deaths")) + 
  geom_line(aes(y=Recovered, color="Recovered")) +
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = y ), hjust = 0) + 
  transition_reveal(Date) + 
  coord_cartesian(clip = 'off') + 
  xlab("Day") +
  ylab("Number of cases") + ggtitle("Evolution of cases over time") +
  theme_classic()
file_renderer(dir = ".", prefix = "gganim_plot", overwrite = TRUE)
animate(p, fps=10,renderer = gifski_renderer("virusevolution.gif"))

df1<-mergedCases %>% group_by(Date) %>% summarise_at(c("Confirmed","Recovered","Death"),sum)
df2 <- data.frame(Date=rep(df1$Date, 3), 
                  act_noact=c(df1$Confirmed, df1$Death,df1$Recovered), 
                  State=rep(c("Confirmed","Deaths", "Recovered"), each=nrow(df1)))


lastDate<-max(df1$Date)
p <- ggplot(df2, aes(x=Date, y=act_noact, group=State, color=State)) +
  geom_line() +
  geom_segment(aes(xend=max(Date), yend = act_noact), linetype=2, colour='blue') +
  geom_point(size = 3) + 
  geom_text(aes(x = max(Date)+.1, label = sprintf("%5.0f", act_noact)), hjust=-0.5) +
  transition_reveal(Date) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  xlab("Day") +
  ylab("Number of cases") + ggtitle(paste("Evolution of cases over time as of ",lastDate)) +
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8))+
  theme(panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  plot.margin = margin(5.5, 40, 5.5, 5.5))

animate(p, fps=5,renderer = gifski_renderer("virusevolution.gif"))
