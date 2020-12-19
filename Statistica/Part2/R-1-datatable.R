library(data.table)
setwd("~/RProjects/R_courses/Statistica/Part2/dataset_5")

products <- fread('products.csv')
?fread
# Измерим с помощью системнго времени, сколько времени нам потребуется,
# чтобы прочесть файл 5 раз с помошью read.table
system.time({
  for (i in seq_len(5)){
    read.table('products.csv', header = T, sep = ';')
  }
})

# А теперь с помошью fread
system.time({
  for(i in seq_len(5)){
    fread('products.csv')
  }
})

products[price > 10000 &
           brand %in% c('Epson','Apple')] # выборка по нескольким критериям
products[available == TRUE] # всегда надо указывать аргумент
products[3] # всегда выдает строчку
products[3,] # и так тоже
products[,3] # выбор стоблцов всегда происходит по ВТОРОМУ аргументу
products[, list(name,
                price.1k = price / 1000)] # сформировали новую таблицу на основе старых данных
products[order(price, decreasing = T)] # упорядоченный по строкам по ценам table
products[order(price, decreasing = T), 
         list(name, price.1k = paste(price / 1000, 'тыс. руб'))] # а теперь сделаем преобразование и упорядочивание

# Короткий вариант записи

products[order(-price),
         .(name, price.1k = paste(price / 1000, 'тыс. руб'))]
# Посчитаем теперь сумму всех товаров в каталоге и закинем в data.table
products[, .(price = sum(price))]

# Используем теперь сортировку и фильтрацию

a <- products[, .(name.with.brand = paste(name,'-',brand))]
a[order(name.with.brand)]
# Все это можно записать в одну строчку

products[, .(name.with.brand = paste(name, '-', brand))][order(name.with.brand)]

# Можно делать более сложные трансформации

products[, .(price = {
  a <-  mean(price)
  b <-  median(price)
  c(paste(min(price), 'мин'), paste(max(price), 'макс'), paste(a/b, 'отношение a/b'))
})]

# Агрегация

# x[i, j , by, with = TRUE]
# SELECT j FROM
# WHERE i
# GROUP by
# Можно провести аналогию синтаксиса с SQL
products[, .(mean.price = mean(price)), by = brand]

# Например, нужна таблица с тремя самыми дорогими продуктами каждого бренда

products[order(-price), .(name = head(name, 3), price = head(price, 3)), by = brand]

# Задание 1

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))

filter.expensive.available <- function(products, brands){
  dt <- products[price >= 500000 & brand %in% brands & available == T]
  # или фильтрацию можно сделать при помощи скобок
  # products[brand %in% brands][price >= 500000][available = T]
  return(dt)
}
filter.expensive.available(sample.products, c('a','b'))

# Задание 2

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
ordered.short.purchase.data <- function(purchases) {
  return(purchases[quantity > 0][order(-price), .(ordernumber, product_id)])
}

# Задание 3

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
purchases.median.order.price <- function(purchases) {
  a <- purchases[quantity >= 0][, .(pq = price * quantity), by = ordernumber]
  return(median(a[, sum(pq), by = ordernumber]$V1))
  # median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)
}

median(sample.purchases[quantity >= 0][, .(w = sum(price *  quantity)), by = ordernumber]$w)


# для выбора сабдаты по всем столбикам можно использовать .SD

products[order(-price), .(name = head(name, 3), 
                          price = head(price, 3)), by = brand]
products[order(-price), head(.SD, 3), by = brand]

# переменная .N содержит количество элементов в группе

products[price > 1000, .(expencive.price = .N), by = brand]

# также можно взять nrow(.SD) или length(<col>)
# Для изменения data.table без создания копии используется оператор :=

# x[, new.col := expr]
# x[, c(col1, col2) := list(expr1, expr2)]
# x[, `:=` (col1 = expr1, col2 = expr2)]

# можно применять совместно с фильтрацией и агрегацией

# x[i, new.col := expr, by]    := == set()

products[price < 1000, name.with.price := paste0(name, " (", price, ' руб.', ')')]
products[,price := as.double(price)]
products[order(-price)]
products[, price := price / max(price), by = brand]


# также как и в SQL в data.table можно создавать ключи

products <- fread('products.csv')
purchases <- fread('purchases.csv')

setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

key(products)

# аналог join из SQL - merge

a <- merge(purchases, products, by = 'product_id')
# merge(purchases, products, by = c(col1, col2))
merge(purchases, products, by.x = 'product_id', by.y = 'product_id')
merge(purchases, products, all.x = T, all.y = F)
# можно записать через квадратные скобочки

b <- purchases[products, on = 'product_id']

# Для поиска в data.table существует три функции J, SJ и CJ

products[J(c(9, 45, 698, 95))]
products[SJ(c(158, 45 , 85, 95))]
key(SJ(c(89, 85, 75)))
products[CJ(c(45, 456, 65, 232, 4545),
            c('Supra', 'Func'))]
# Немного практики. Найдем теперь бренды, у которых высокая лояльность

products <- fread('products.csv')
purchases <- fread('purchases.csv')

purchases.with.brands <- merge(purchases, products[, .(product_id, brand)], by = 'product_id')
# выделим 20 самых популярных брэндов, для этого посчитаем длинну вектора уникальных пользователей этого бренда
pop.20.brands <- head(purchases.with.brands[, .(total.brand.users = length(unique(externalsessionid))), 
                                            by = brand], 20)
# посчитаем статистику по пользователю

users <- purchases.with.brands[, .(unique.brands = length(unique(brand)), items = .N, brand = brand[1]), 
                               by = externalsessionid]
# Далее оставим пользователей, купивших только один бренд более одного раза 
brand.loyal.user <- users[items > 1][unique.brands == 1][, .(total.loyal.orders = .N), by = brand]

# Соединим две получившиеся таблицы

brand.stats <- merge(pop.20.brands, brand.loyal.user, by = 'brand')

# Посчитаем далю лояльных пользователей

brand.stats[, loyal := total.loyal.orders / total.brand.users * 100]
brand.stats[order(-loyal)]

# Задание 4

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))

purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))

get.category.ratings <- function(purchases, product.category){
  setkey(product.category, product_id)
  setkey(purchases, product_id)
  products.with.cat <- merge(purchases, product.category, by = 'product_id')
  return(products.with.cat[,.(sum.by.cat = sum(totalcents), quantity = sum(quantity)), by = category_id])
}

get.category.ratings(purchases, product.category)

# Задание 5

sample.purchases <- data.table(
  product_id=c(817, 1580, 104, 1104, 1022, 1025),
  price=c(18892183.96, 551068.81, 1673828.09, 27630473.3, 635973178.19, 111707152.35),
  quantity=c(1, 1, 2, 1, 1, 2),
  ordernumber=c(9, 9, 14, 14, 22, 22),
  price.portion=c("97.00", "3.00", "11.00", "89.00", "64.00", "23.00"))
mark.position.portion <- function(purchases) {
  return(print(purchases[quantity > 0][, price.portion := format(round(price * 100 * quantity /sum(price * quantity),digits = 2), nsmall = 2), by = ordernumber]))
}

b <- sample.purchases[quantity > 0][, .(sum.by.order = sum(price)), by = ordernumber]
sample.purchases[quantity > 0][, price.portion := format(round(price * 100 * quantity /sum(price * quantity)),digits =  nsmall = 2), by = ordernumber]
