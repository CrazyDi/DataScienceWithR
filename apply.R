library(ggplot2)
data(diamonds)

# Найти минимум по каждой строке из x, y, z
# Вариант 1
min_size <- c()

for (i in 1:nrow(diamonds)){
    min_size <- c(min_size, min(diamonds[i, 8:10]))
}

# Вариант 2
min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)){
    min_size[i] <- min(diamonds[i, 8:10])
}

# Вариант 3
min_size_2 <- apply(diamonds[, 8:10], 1, min)

?apply(array, margin, ...)

d<- matrix(rnorm(30), nrow = 5)

# По строкам
apply(d, 1, sd)

# По столбцам
apply(d, 2, sd)

# ЗАДАЧА
# В переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# При помощи функции apply найдите максимальной значение в каждой строке. 
# Сохраните результат (вектор максимальных значений) в переменную row_max.
row_max <- apply(my_df, 1, max)

# ЗАДАЧА
# В переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# Рассчитайте медиану для всех столбцов с количественными переменными. 
# В переменную col_median сохраните вектор полученных значений. 
col_median <- apply(my_df, 2, median)

# Если функция, применяемся в apply возвращает только одно значение, то apply возвращает вектор.
# Если значений несколько (фиксированное количество), то мы получаем матрицу
my_range <- apply(d, 2, range)

# Если же значений неопределенное количество, то получаем список.
outliers_count <- function(x){
    outliers <- x[abs(x - mean(x)) > 2 * sd(x)]
    if (length(outliers) > 0){
        return(outliers)
    } else {
        return("There are no outliers")
    }
}

iris_num <- iris[, 1:4]

apply(iris_num, 2, outliers_count)


# Добавление параметров к функции
head(airquality)

apply(airquality, 2, mean, na.rm = T)





set.seed(42)

d <- as.data.frame(matrix(rnorm(30), nrow = 5))

# Необходимо получить все отрицательные наблюдения
# Вариант 1
my_list <- list()
for (i in seq_along(d)){
    temp_col <- d[, i]
    neg_numbers <- temp_col[temp_col < 0]
    my_list[[i]] <- neg_numbers
}
names(my_list) <- colnames(d)
my_list

# Вариант 2
find_negative <- function(x) {
    x[x < 0]
}

apply(d, 2, find_negative)

# Вариант 3
apply(d, 2, FUN = function(x) x[x < 0])

# ЗАДАЧА
# Давайте завершим и слегка модифицируем задачу из предыдущей лекции. 
# Напишите функцию get_negative_values, которая получает на вход dataframe произвольного размера. 
# Функция должна для каждой переменной в данных проверять, есть ли в ней отрицательные значения. 
# Если в переменной отрицательных значений нет, то эта переменная нас не интересует, 
# для всех переменных, в которых есть отрицательные значения мы сохраним их в виде 
# списка или матрицы, если число элементов будет одинаковым в каждой переменной (смотри пример работы функции).
get_negative_values <- function(test_data){
    res <- apply(test_data, 2, function(x) x[(x < 0)&(!is.na(x))])
    res_null <- c()
    
    for (i in 1:length(res)) {
        if (length(res[[i]]) == 0) {
            res_null <- c(res_null, i * -1)
        }
    }
    
    if (!is.null(res_null)) {
        res <- res[res_null]
    }
    
    l <- length(res[[1]])
    for (i in 1:length(res)) {
        if (length(res[[i]]) != l) {
            l <- 0
        }
    }
    
    if (l > 0) {
        return(as.matrix(as.data.frame(res)))
    } else {
        return(res)
    }
}

test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))

test_data <- as.data.frame(list(V1 = c(9.3, 11.1, 9.9, 9.3, 9.9, NA, NA), V2 = c(-9.3, -10.2, -10.3, NA, -10.2, NA, NA), V3 = c(-9.1, -9.6, -10.9, NA, -8.7, NA, NA), V4 = c(-10.1, -9.8, -8.6, -10.8, -10, NA, NA), V5 = c(-11.1, -8.5, -10, -9.8, -10.6, NA, NA), V6 = c(-10.8, -9.1, -9.1, -8.7, -10.4, NA, NA), V7 = c(-8.9, -9.8, -11.1, -8.1, -7.3, NA, NA), V8 = c(-9, -10, -8.6, -11.2, -11.7, NA, NA)))

test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

res <- get_negative_values(test_data)

# Пример правильного решения
get_negative_values <- function(test_data){    
    negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
    return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))
}

# Статистики
data(iris)
aov(Sepal.Length ~ Species, iris)

aov_result <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))
norm_test <- apply(iris[, 1:4], 2, function(x) shapiro.test(x))
norm_test_p <- apply(iris[, 1:4], 2, function(x) shapiro.test(x)$p.value)


# Задача
# Напишите функцию na_rm которая заменяет все пропущенные значения в столбцах dataframe на соответствующее среднее значение. 
# То есть все NA в первом столбце заменяются на среднее значение первого столбца (рассчитанного без учета NA). 
# Все NA второго столбца заменяются на среднее значение второго столбца и т.д.  
# Функция na_rm на вход получает dataframe произвольной размерности, состоящий из количественных переменных. 
# Функция должна возвращать  dataframe с замененными NA. Ни порядок столбцов, ни порядок строк в dataframe изменять не нужно.
# Вы можете создавать вспомогательные функции для решения этой задачи. 
# Напоминаю, что для проверки является ли наблюдение NA нужно использовать функцию is.na()
na_replace <- function (x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
}


V1 <- c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9)
na_replace(V1)

test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

as.data.frame(apply(test_data, 2, function(x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
} ))


# lapply - всегда возвращает список
my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)

# sapply - возвращает или вектор, или матрицу, или список
sapply(my_list, mean, na.rm = T)
sapply(my_list, range, na.rm = T)
sapply(my_list, function(x) x * 2)

# ЗАДАЧА
# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список.
positive_sum <- function(test_data) {
    lapply(as.list(test_data), function(x) sum(x[(x > 0)&(!is.na(x))]))
}
d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
positive_sum(d)

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"
grepl("Mazda", "Mazda RX4")

cars[sapply(cars, function(x) grepl(x, car))]

iris_num <- iris[sapply(iris, is.numeric)]
sapply(iris[1:4], sd)
apply(iris[1:4], 2, sd)

sapply(iris, is.numeric)
apply(iris, 2, is.numeric)

tapply(mtcars$mpg, mtcars$am, mean)
aggregate(mpg ~ am, mtcars, mean)
by(iris[1:4], iris$Species, colMeans)
by(iris[1:4], iris$Species, function(x) sapply(x, function(col) shapiro.test(col)))
aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, 1:4, 1:4)

x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)

m <- matrix(rnorm(100 * 200), nrow = 100)
m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")
str(m_names)


get_sd <- function(x) {
    num_var <- sapply(x, is.numeric)
    sapply(x[, num_var], sd)
}
get_sd(iris)
my_df <- data.frame(x = 1:10, y = letters[1:10])
get_sd(my_df)

get_sd <- function(x) {
    num_var <- sapply(x, is.numeric)
    sapply(x[num_var], sd)
}
get_sd(my_df)


# ЗАДАЧА
# Предположим у нас есть dataframe с двумя переменными name - название гена, expression - уровень экспрессии. Например:

# my_data
#   name expression
# 1 p1@HPS1       120
# 2 p2@HPS2       89
# 3 p@GOT1        45


# Обратите внимание, что само название гена спрятано внутри строки и указано после символа @. 
# Напишите функцию my_names, которая получает на вход  датафрейм и вектор с именами тех генов, 
# для которых мы хотим отобрать наблюдения уровня экспрессии. 
# Допустим, мы хотим отобрать наблюдения только для генов 'HPS1' и 'GOT1', тогда результат работы функции будет следующий:
    
# names =c('HPS1', 'GOT1')
# my_names(my_data, names)
# name expression
# 1 p1@HPS1        120
# 3  p@GOT1         45

test_data <- as.data.frame(list(name = c('p1@HPS1', 'p2@HPS2', 'p@GOT1'), expression = c(120, 89, 45)))
names <- c('HPS1', 'GOT1')

my_names <- function(dataset, names) {
    items <- sapply(dataset$name, function(x) sapply(names, function(n) grepl(n, x)))

    if (!is.null(dim(items))) {
        items <- apply(items, 2, function(x) as.logical(max(x)))
    }

    dataset[items,]
}
test_data <- as.data.frame(list(name = c("p5@HPS1", "p3@HPS2", "p7@HPS3", "p7@HPS4", "p8@HPS5", "p8@HPS6", "p9@HPS7", "p9@HPS8", "p12@HPS9"), expression = c(57.19, 112.26, 102.36, 90.99, 144.01, 83.87, 59.82, 101.14, 83.27)))
names = c("HPS1")
my_names(test_data, names)

# Правильное решение
my_names <- function (dataset, names){    
    dataset[as.numeric(lapply(names, function(x) which(grepl(x, dataset$name)))),]
}


# ЗАДАЧА
# Напишите функцию find_outliers, которая получает на вход dataframe с одной количественной переменной 
# и произвольным числом факторных переменных. Факторные переменные разбивают все наши наблюдения на 
# определенное число групп. Например, если мы посмотрим на данные mtcars и возьмем в качестве 
# группирующих переменных: am - две градации и cyl три градации, то получим 6 групп наблюдений 
# на пересечении градаций этих переменных.
# Итак, ваша задача — создать в данных новую числовую переменную is_outlier, 
# которая будет принимать значение 1, если наблюдение в этой строке является выбросом в своей группе, и 0, если не является.
aggregate(. ~ am, mtcars, mean)

by(iris[1:4], iris$Species, colMeans)

tapply(mtcars[2:3], mtcars$am, mean)

tapply(mtcars$mpg, mtcars$am, mean)
aggregate(mpg ~ am, mtcars, mean)
by(iris[1:4], iris$Species, colMeans)
by(iris[1:4], iris$Species, function(x) sapply(x, function(col) shapiro.test(col)))
aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)

aggregate(x = iris, by = iris[5], FUN = mean)


mtcars


aggregate(x = test_data, by = test_data[c(2, 3)], "mean")
aggregate(mpg ~., test_data, sd)

aggregate(x = test_data, by = list(test_data$cyl, test_data$am), mean)


test_data <- mtcars[c(1, 2, 9)]
test_data$cyl <- as.factor(test_data$cyl)
test_data$am <- as.factor(test_data$am)

find_outliers <- function(t){
    # Индексы факторов
    factor_item <- sapply(t, is.factor)
    
    # Переименовываем колонку численного показателя
    names(t)[sapply(t, is.numeric)] <- "num"

    # Находим средние по группе и сразу именуем колонку
    t_mean <- aggregate(x = t$num, by = t[factor_item], mean)
    names(t_mean)[sapply(t_mean, is.numeric)] <- "mean"
    
    # Находим отклонение по группе и сразу именуем колонку
    t_sd <- aggregate(x = t$num, by = t[factor_item], sd)
    names(t_sd)[sapply(t_sd, is.numeric)] <- "sd"

    # В исходный data.frame добавляем среднее и отклонение
    t <- merge(t, t_mean, by = names(t)[sapply(t, is.factor)])
    t <- merge(t, t_sd, by = names(t)[sapply(t, is.factor)])

    # Рассчитываем результат
    t$is_outlier <- ifelse((abs(t$num - t$mean) > 2 * t$sd), 1, 0)
    t
}
test_data <- mtcars[c(1, 2, 9)]
test_data$cyl <- as.factor(test_data$cyl)
test_data$am <- as.factor(test_data$am)
find_outliers(test_data)

data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose)
find_outliers(ToothGrowth)

test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
find_outliers(test_data)


# ЗАДАЧА
# Напишите функцию smart_lm, которая получает на вход data.frame с произвольным числом количественных переменных. 
# Первая колонка в данных - это зависимая переменная, все остальные - предикторы. 
# На первом этапе вы должны отобрать предикторы для модели.
# Функция возвращает в виде вектора коэффициенты линейной регрессии построенной 
# только для отобранных предикторов (условие нормальности распределения). 
# Если таких предикторов в данных не оказалось, то функция возвращает предупреждение 
# "There are no normal variables in the data".
smart_lm <- function(x) {
    res <- x[c(TRUE, apply(x[-1], 2, function(col) shapiro.test(col)$p.value > 0.05))]
    
    if (length(res) == 1) {
        return("There are no normal variables in the data")
    } else {
        names(res)[1] <- "First"
        t <- lm(First~., res)
        t$coefficients
    }
}

smart_lm(swiss)
test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
smart_lm(test_data)

test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)
smart_lm(test_data)

test_data <- as.data.frame(list(V1 = c(21.7, 16.9, 16.4, 18.2, 23.1, 17.8, 20.1, 16.8, 20.9, 20.8, 19.8, 20, 17.7, 18.8, 24.3, 21.2, 18, 21.3, 19.1, 19.3, 19.2, 24.4, 20.9, 20.8, 16.1, 18.8, 19.8, 21, 19.8, 19.3), V2 = c(23.9, 23.1, 19, 20.4, 21.6, 23.1, 18.2, 23.4, 17.9, 18.6, 19.9, 19, 20.1, 19.2, 18.7, 20, 21.1, 21.4, 22.3, 23.2, 20.6, 20.6, 18.2, 20.4, 20.7, 22.3, 21.3, 19.2, 20.5, 18.9)))
t <- smart_lm(test_data)
t$coefficients


# ЗАДАЧА
# Напишите функцию one_sample_t, которая получает на вход два аргумента:
# 1. Dataframe произвольного размера с произвольным числом переменных различного типа.
# 2. Числовое значение среднего в генеральной совокупности.
# Ваша функция должна применять одновыборочный t - test к каждой числовой переменной в данных, 
# и сравнивать среднее значение этой переменной с указанным значением среднего в генеральной совокупности (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент это вектор, состоящий из t - значения, 
# числа степеней свобод (df) и значения p - value.
res <- t.test(iris[1], mu=4)
res$statistic
res$parameter
res$p.value

iris[sapply(iris, is.numeric)]

one_sample_t <- function(test_data, general_mean) {
    lapply(test_data[sapply(test_data, is.numeric)], function(x) {
        res <- t.test(x, mu=general_mean)
        c(res$statistic, res$parameter, res$p.value)
    })
}
one_sample_t(iris[, 1:4], 4)


# ЗАДАЧА
# Итак, ваша задача, написать функцию get_p_value, 
# которая получает на вход список (назовем его главным списком), 
# каждый элемент этого списка тоже список - результат выполнения функции shapiro.test (смотри пример normality_tests). 
# Ваша задача из каждого элемента главного списка вытащить только p - value. 
# В итоге функция возвращает список где каждый элемент - одно значение - p - value (как в примере normality_tests_p).
normality_tests <- lapply(iris[, 1:4], shapiro.test)
lapply(normality_tests, function(x) x$p.value)
test_1 <- shapiro.test(iris$Sepal.Length)
test_1$p.value

