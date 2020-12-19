library(ggplot2)
library(dplyr)
library(plyr)

setwd("~/RProjects/R_courses/Statistica/Part2")
suicide_rate <-read.csv('final_project/suicide_rate.csv')

by_counties <- suicide_rate %>%
  select(-country.year, -sex, -HDI.for.year, -generation, -gdp_for_year....) %>% 
  filter(country == c('Brazil', 'Russian Federation'), year > 1989) %>%
  group_by(year, country, age) %>% 
  summarize_all(sum)

by_counties %>% 
  ggplot()+
  geom_col(mapping = aes(x = year, y = suicides.100k.pop, fill = country), position = 'dodge')+
  ylab('Suicides per 100k')

by_counties %>% 
  ggplot()+
  geom_line(mapping = aes(x = year, y = gdp_per_capita...., col = country))+
  ylab('gdp per capita')

gpl <- by_counties %>% ggplot() +
  geom_col(mapping = aes(x = year, y = suicides.100k.pop * 3.65e+05 / 270, fill = country), position = 'dodge')+
  geom_line(mapping = aes(x = year, y = gdp_per_capita...., col = country))+
  geom_point(mapping = aes(x = year, y = gdp_per_capita...., col = country))+
  scale_y_continuous('suicides per 100k', limits = c(80, 265))
gpl %+% scale_y_continuous(sec.axis = sec_axis(~. * 270 / 3.65e+05)) 