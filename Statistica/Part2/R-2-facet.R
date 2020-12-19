library(ggplot2)
library(dplyr)
ggplot(diamonds, aes(carat, fill = clarity))+
  geom_density(alpha = 0.2) +
  facet_grid(cut~.)
glimpse(diamonds)


mtcars %>% 
  mutate(am = factor(am, labels = c('A', 'M')), vs = factor(vs, labels = c('V', 'S'))) %>% 
  ggplot(aes(hp))+
  geom_dotplot()+
  facet_grid(am ~ vs, margins = T)

mtcars %>% 
  mutate(am = factor(am, labels = c('A', 'M')), 
         vs = factor(vs, labels = c('V', 'S')),
         cyl = factor(cyl)) %>% 
  ggplot(aes(hp, mpg))+
  geom_point(aes(col = cyl))+
  facet_grid(am ~ vs)+
  geom_smooth(method = 'lm')

ggplot(diamonds, aes(carat))+
  geom_density(alpha = 0.1) +
  facet_wrap(~cut + color, nrow = 5)

# Задание 4

movies <- read.csv('https://stepik.org/media/attachments/course/724/myMovieData.csv')
ggplot(movies, aes(Type, Budget))+
  geom_boxplot()+
  facet_grid(.~Year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
