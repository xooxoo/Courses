library(ggplot2)
library(dplyr)
library(ggthemes)

seq_x <- (seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 11))
ggplot(mtcars, aes(mpg, hp, col = factor(am))) +
  geom_point() +
  scale_x_continuous(name = 'Miles per gallon',
                     breaks = c(1, seq(10, 35, 5)),
                     limits = c(1, 35),
                     expand = c(1,1))+
  scale_color_manual(name = 'Transmission type',
                     labels = c('Auto', 'Manual'),
                     values = c('Coral', 'SeaGreen'))
#xlab('Miles per gallon')
#xlim(1, 35)

ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c('Coral', 'SeaGreen'))

ggplot(mtcars, aes(hp, mpg, size = disp, shape = factor(vs)))+
  geom_point()+
  scale_size_continuous(name = 'Имя легенды',
                        breaks = seq(100, 400, 50))+
  scale_shape_discrete(name = 'Имечко',
                       labels = c('Auto', 'Manual'),
                       solid = F)
ggplot(mtcars, aes(factor(cyl), hp))+
  geom_boxplot()+
  scale_x_discrete(name = 'Цилиндры',
                   labels = c('4 цилиндра',
                              '6 цилиндров',
                              '8 цилиндров'))

# Задание 1
ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_discrete(name = 'Вид цветка',
                     labels = c('Ирис щетинистый',
                                'Ирис разноцветный',
                                'Ирис виргинский')) +
  scale_y_continuous(name = 'Длина лепестка',
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_x_continuous(name = 'Длина чашелистика',
                     limits = c(4, 8),
                     breaks = seq(4, 8, 1))

ggplot(mtcars, aes(factor(am), hp))+
  geom_boxplot(aes(fill = factor(cyl)))+
  scale_shape_discrete()+
  scale_fill_brewer(type = 'qual', palette = 3)+
  scale_color_brewer(type = 'qual', palette = 3)+
  theme_bw()

ggplot(mtcars, aes(hp, mpg, col = factor(cyl)))+
  geom_point()+
  theme_economist()
?scale_fill_brewer
?shape


