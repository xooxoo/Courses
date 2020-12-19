library(tidyr)
library(ggplot2)
library(dplyr)

setwd("~/RProjects/R_courses/Statistica/Part2")
glacier <- read.csv('glacier_data.csv', na.strings = "..")
glacier <- glacier %>% 
  select(REF_DATE, GEO, VALUE, Type.of.measure) %>% 
  filter(Type.of.measure == 'Annual mass balance') %>% 
  separate(GEO, c('Name', 'Location'), sep = ' - ')

# descriptive analisys
g1 <- glacier %>% 
  group_by(Name) %>% 
  summarise(YearsObserved = n(),
            MeanChange = mean(VALUE, na.rm = T),
            WorstChange = min(VALUE, na.rm = T),
            WorstYear = REF_DATE[which.min(VALUE)])


#t-test
g2 <- glacier %>% 
  group_by(Name) %>% 
  do({
    tt <- t.test(.$VALUE, alternative = 'less', mu = 0, conf.level = 0.99)
    tibble(Pvalue = tt$p.value,
               ConfidentLimit = tt$conf.int[2])
  })

#ggplot

ggplot(glacier, aes(REF_DATE, VALUE))+
  geom_line()+
  geom_hline(data = g1, aes(yintercept = MeanChange), color = 'coral',
             lty = 'dashed', alpha = 0.8)+
  facet_wrap(~Name)
?mean_cl_boot
?t.test

left_join(g1, g2, by = 'Name') %>% 
  knitr::kable(caption = 'descriptive statistics', digits = c(0,0,2,0,0,10,2))
