library(xtable)
library(stargazer)
library(readxl)


fit1 <- lm(mpg ~ cyl + disp, mtcars)
fit2 <- lm(mpg ~ am * vs, mtcars)

fit1_table <- xtable(fit1)
fit2_table <- xtable(fit2)

print(fit1_table, type = 'html', file = 'fit_table1.html')
print(fit2_table, type = 'html', file = 'fit_table2.html')

stargazer(fit1, type = 'html',
          dep.var.labels = 'mpg',
          covariate.labels = c('cyl', 'disp'), out = 'models1.html')
