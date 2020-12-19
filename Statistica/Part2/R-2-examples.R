library(ggplot2)
library(grid)

d <- read.csv('https://stepic.org/media/attachments/course/724/example_data.csv')

plot <- ggplot(d, aes(date, percent, col = system, group = system))+
  geom_line(size = 1.5)+
  geom_point(shape = 21, size = 3.5)+
  geom_point(shape = 21, size = 3)+
  geom_point(shape = 21, size = 2.5, fill = 'black')+
  geom_vline(xintercept = 7.5, color = 'white',
             linetype = 'dotted')+
  scale_color_manual(values = c('red', 
                                'cyan',
                                'yellow2',
                                'springgreen2',
                                'orangered1'))+
  scale_y_continuous(breaks = c(0, .04, .08, .11, .15),
                     limits = c(0, 0.15),
                     labels = scales::percent) +
  xlab('') +
  ylab('')+
  ggtitle('Top 5 Stasik distributions (% of all)')+
  theme_classic()

plot + theme(legend.title = element_blank(),
             legend.position = 'top', 
             legend.text = element_text(color = 'white', family = 'ubuntu', size = 12),
             plot.background = element_rect(color = 'black',
                                            fill = 'black'),
             legend.background = element_rect(color = 'black',
                                              fill = 'black'),
             axis.text = element_text(color = 'white'),
             panel.background = element_rect(fill = 'black', color = 'black'),
             panel.grid.major = element_line(color = 'gray30',
                                             linetype = 'longdash'),
             axis.text.x = element_text(face = 'bold', family = 'ubuntu', size = 14),
             axis.text.y = element_text(face = 'bold', family = 'ubuntu', size = 12),
             plot.title = element_text(face = 'bold', family = 'ubuntu', size = 14, color = 'white', hjust = 0.5)
             )

             

plot + my_theme

grid.text('Data sourse: The DistroWatch Page Hit Ranking (Nov. 23, 2011)',
          x = .02, y = 0.01, just = c('left', 'bottom'),
          gp = gpar(col = 'white', fontsize = 9))

grid.text('ввв.стасон.рф', x = .98, y = 0.01, just = c('right', 'bottom'),
          gp = gpar(col = 'white', fontsize = 9, fontface = 'bold'))
