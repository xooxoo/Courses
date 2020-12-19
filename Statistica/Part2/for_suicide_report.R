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

rus <- suicide_rate %>% 
  select(-country.year, -HDI.for.year, -generation, -gdp_for_year....) %>% 
  filter(country == 'Russian Federation', age != '5-14 years')

g1 <- rus %>% 
  group_by(sex, age) %>% 
  dplyr::summarise(`mean suicides no` = mean(suicides.100k.pop), 
                   `max suicides no` = max(suicides.100k.pop), 
                   `min suicides no` = min(suicides.100k.pop),
                   `year with max suicides` = year[which.max(suicides.100k.pop)],
                   `year with min suicides` = year[which.min(suicides.100k.pop)])

g1
rus$sex <- revalue(x = rus$sex, c('female' = 'женщины', 'male' = 'мужчины'))
rus %>% 
  ggplot()+
  geom_point(aes(year, suicides.100k.pop, col = age))+
  geom_line(aes(year, suicides.100k.pop, col = age, group = age))+
  facet_grid(~sex)+
  ylab('Кол-во самоубийств на 100 тыс. человек')+
  xlab('Годы')+
  scale_color_brewer(labels = c('15 - 24 лет',
                                '25 - 34 лет',
                                '35 - 54 лет',
                                '55 - 74 лет',
                                '75+ лет') , type = 'qual',palette = 6)+
  theme(legend.position = 'top', 
        legend.title = element_blank(),
        legend.direction = 'horizontal', 
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_line(color = 'gray80'),
        panel.grid.major.x = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80', linetype = 'dashed'),
        panel.border = element_rect(fill = NA, colour = 'black'),
        strip.background = element_rect(fill = 'white'))
  
?scale_color_brewer
  
  
man_woman  
man_woman %+% 
  geom_line(aes(year, gdp_per_capita.... * 120/17100), linetype = 'dashed', alpha = 0.5)+
  scale_y_continuous(sec.axis = sec_axis(~. * 17000/120)) 
rus %>% 
  ggplot(aes(year, gdp_per_capita.... * 120/17100))+
  geom_point()+
  geom_line()+
  ylab('ВВП на душу населения, $')+
  xlab('Годы')


#t-test

g2 <- rus %>% 
  group_by(age, sex) %>% 
  do({
    tt <- t.test(.$suicides.100k.pop, alternative = 'two.sided', mu = 0, conf.level = 0.95)
    tibble(Pvalue = tt$p.value,
           ConfidentLimit = tt$conf.int[2])
  })

g2
rus
man <- rus %>%
  filter(sex == 'мужчины')
summary(man)

man_g <- man %>%
  ggplot()+
  geom_point(aes(year, suicides.100k.pop, col = age))+
  geom_line(aes(year, suicides.100k.pop, col = age, group = age))+
  ylab('Кол-во самоубийств на 100 тыс. человек')+
  xlab('Годы')+
  scale_color_brewer(labels = c('15 - 24 лет',
                                '25 - 34 лет',
                                '35 - 54 лет',
                                '55 - 74 лет',
                                '75+ лет') , type = 'qual',palette = 6)+
  theme(legend.position = 'top', 
        legend.title = element_blank(),
        legend.direction = 'horizontal', 
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_line(color = 'gray80'),
        panel.grid.major.x = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80', linetype = 'dashed'),
        panel.border = element_rect(fill = NA, colour = 'black'),
        strip.background = element_rect(fill = 'white'))
  geom_line(aes(year, gdp_per_capita.... * 120/17100), lty = 'longdash')
man_g

man %>% 
  group_by(age) %>% 
  dplyr::summarise(R = cor.test(suicides_no, gdp_per_capita....)$estimate[[1]])
man
v <- cor.test(c(12,123,123,123,123,234,234), c(12,345,56,12,12,123,34))
v$statistic
v$estimate[1]
