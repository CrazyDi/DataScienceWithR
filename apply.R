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
