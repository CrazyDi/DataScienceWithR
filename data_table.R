library(data.table)

# Создние объекта data.table
t <- as.data.table(iris)
t <- data.table(col1 = c(1:3), col2 = letters[1:3])
products <- fread("E96/products.csv", encoding = 'UTF-8')

# Выбор строк
products[1:10,]
products[products$price > 10000]
with(iris, iris[Species == 'virginica',])
products[price > 10000]
products[(price > 10000) & (brand %in% c("Epson", "Apple"))]
products[available == TRUE,]

# Выбор столбцов
products[, list(name, price.1k = price / 1000)]

# Сортировка
order(products$price, decreasing = T)
products[order(price, decreasing = T)]
products[order(price, decreasing = T), list(name, price.1k = price / 1000)]
products[order(price, decreasing = T), list(name, price.1k = paste0(price / 1000, " тыс. руб."))]

products[, list(name, price)]
products[, .(name, price)]
products[, c("name", "price"), with = F]

products[order(-price), .(name = head(name), price = head(price))]
products[, .(price = sum(price))]

a <- products[, list(name.with.brand = paste0(brand, " - ", name))]
a[order(name.with.brand)]
products[, list(name.with.brand = paste0(brand, " - ", name))][order(name.with.brand)]

products[, .(price = {
    a <- mean(price)
    b <- median(price)
    c(min(price), max(price), a/b)
})]

# Выбор данных
# x[i, j, by, with = TRUE, ...]
# select j from ...
# where i
# group by by

# Группировка
products[, .(mean.price = mean(price)), by = brand]
products[order(-price), .(name = head(name, 3), price = head(price, 3)), by = brand]

# ЗАДАЧА
# Напишите функцию filter.expensive.available, которая принимает на вход products (объект типа data.table) 
# и вектор названий брендов, и возвращает только те строчки, которые соответствуют товарам, 
# цена которых больше или равна 5000 рублей, доступны на складе, и принадлежат одному из переданных брендов.

filter.expensive.available <- function(products, brands) {
    products[(price / 100 >= 5000) & (available == TRUE) & (brand %in% brands)]
}

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))
filter.expensive.available(sample.products, c("a", "c", "d"))


# ЗАДАЧА
# Создайте функцию ordered.short.purchase.data, которая будет принимать purchases, объект data.table, 
# и возвращать таблицу только со столбцами с номером заказа и ID продукта.
# Упорядочите результат по убыванию стоимости купленного товара. 
# Возвраты (записи с отрицательным количеством предметов в позиции) надо удалить.
ordered.short.purchase.data <- function(purchases) {
    purchases[quantity > 0, ][order(-price), .(ordernumber, product_id)]
}

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
ordered.short.purchase.data(sample.purchases)


# зАДАЧА
# Напишите функцию purchases.median.order.price, у которой один аргумент: purchases, 
# и которая возвращает медианную стоимость заказа (число).
# Группировку стоит проводить с помощью data.table. 
# Записи с неположительным количеством купленных товаров (возвраты) игнорировать.
purchases.median.order.price <- function(purchases) {
    as.numeric(purchases[quantity > 0, .(ordernumber, order.price = price * quantity)][, .(sum.price = sum(order.price)), by = ordernumber][, .(median.price = median(sum.price))])
}
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
purchases.median.order.price(sample.purchases)


# .sd - содержит data.table с подвыборкой группы
products[order(-price), .(name = head(name, 3), price = head(price, 3)), by = brand]
products[order(-price), head(.SD, 3), by=brand]

# .N - количество элементов в группе
products[price > 1000, .(expensive.items = .N), by=brand]

# := создает/изменяет столбец без создания копии объекта
# Изменить тип столбца нельзя!
# x[, new.column := expr]
# x[, c("col1", "col2") := list(expr1, expr2)]
# x[, ':=' list(col1 = expr1, col2 = expr2)]
# Можно применять соместно с фильтрацией и агрегацией!
# x[i, new.column := expr, by]
# Функция set делает то же самое
# x[i, j := value]
# set(x, j, value)
products[price < 1000, name.with.price := paste0(name, "(", price, " руб.)")]
products[order(-price)]

products[, price := price / max(price), by = brand]


# Ключи
purchases <- fread("E96/purchases.csv")
setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

key(purchases)
key(products)

# Объединение таблиц
merge(purchases, products, by = "product_id")
merge(purchases, products, by.x = "product_id", by.y = "product_id")
merge(purchases, products, by = "product_id", all.x = T, all.y = F)
purchases[products, on = "product_id"]

# поиск J, SJ, CJ
products[J(c(158, 208, 10001, 826355, 958238))]
products[data.table(c(158, 208, 10001, 826355, 958238))]
products[list(c(158, 208, 10001, 826355, 958238))]
products[.(c(158, 208, 10001, 826355, 958238))]

print(SJ(c(158, 208, 10001, 826355, 958238)))

# Найти бренды, к которым у пользователя высокая лояльность
# Лояльность - это когда пользователь покупает товары только этого бренда
purchases <- fread("E96/purchases.csv", encoding = "UTF-8")
products <- fread("E96/products.csv", encoding = "UTF-8")

purchases.with.brands <- merge(
    purchases,
    products[, list(product_id, brand)],
    by="product_id"
)

pop.20.brands <- head(purchases.with.brands[, list(total.brand.users = length(unique(externalsessionid))), by=brand][order(-total.brand.users)], 20)

users <- purchases.with.brands[, list(unique.brands = length(unique(brand)), items = .N, brand = brand[1]), by=externalsessionid]

brand.loyal.users <- users[items > 1][unique.brands == 1][, list(total.loyal.orders = .N), by=brand]

brand.stats <- merge(pop.20.brands, brand.loyal.users, by="brand")

brand.stats[, loyal :=  total.loyal.orders / total.brand.users]
brand.stats[order(-loyal)]


# ЗАДАЧА
# Создайте функцию get.category.ratings, которая будет возвращать суммарный оборот (с учетом скидок) каждой категории , 
# и количество купленных предметов по таблице покупок и таблице принадлежности товара к категории. 
# Если купленный товар принадлежит нескольким категориям, его необходимо учитывать во всех. При решении используйте ключи.
get.category.ratings <- function(purchases, product.category) {
    setkey(purchases, product_id)
    setkey(product.category, product_id)
    
    product.purchases <- merge(purchases, product.category, by="product_id")
    product.purchases[, .(totalcents = sum(totalcents), quantity = sum(quantity)), by = "category_id"]
}
product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
get.category.ratings(purchases, product.category)


# ЗАДАЧА
# Напишите функцию, которая будет с помощью := добавлять столбец «price.portion», 
# содержащий процент стоимости товара в заказе, с двумя знаками после запятой (нули после запятой не опускать). 
# Проверяться будет возвращаемая из функции таблица. Тип нового столбца - character (строка). 
# Записи с неположительным количеством товаров убрать перед расчётом.
mark.position.portion <- function(purchases) {
    purchases <- purchases[quantity > 0, ]
    purchases[, price.portion := formatC((price * quantity) / sum(price * quantity) * 100, digits = 2, format = "f", flag = "0"), by = "ordernumber"]
    return(purchases[])
}
sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)

sample.purchases[quantity > 0, price.portion := (price * quantity) / sum(price * quantity) * 100, by = "ordernumber"]

mark.position.portion(sample.purchases)
head(sample.purchases, 5)