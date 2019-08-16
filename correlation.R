df <- mtcars

# Расчет коэффициента корреляции
fit <- cor.test(x = df$mpg, y = df$hp)

# через формулу
cor.test(~mpg + hp, df)

# визуализация корреляции
plot(x = df$mpg, y = df$hp)

library(ggplot2)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl))) + 
    geom_point(size = 1)

# Попарные сравнения
df_numeric <- df[, c(1, 3:7)]

pairs(df_numeric)

cor(df_numeric)

library(psych)
fit <- corr.test(df_numeric)
fit$r

x <- mtcars[, c(1, 5)]
res <- cor.test(x = x[, 1], y = x[, 2])
res$p.value

# ЗАДАЧА
# Напишите функцию corr.calc, которая на вход получает 
# data.frame с двумя количественными переменными, 
# рассчитывает коэффициент корреляции Пирсона и 
# возвращает вектор из двух значений: 
# коэффициент корреляции и p - уровень значимости.
corr.calc <- function(x){
    res <- cor.test(x = x[, 1], y = x[, 2])
    return(c(res$estimate, res$p.value))
}

# ЗАДАЧА
# Напишите функцию filtered.cor которая на вход 
# получает data.frame с  произвольным количеством 
# переменных (как количественными, так и любых 
# других типов), рассчитывает коэффициенты 
# корреляции Пирсона между всеми парами 
# количественных переменных и возвращает 
# наибольшее по модулю значение коэффициента 
# корреляции. (То есть функция может вернуть -0.9, 
# если это наибольшая по модулю  корреляция).
df_test <- read.csv('https://stepic.org/media/attachments/lesson/11504/step6.csv')
filtered.cor <- function(x){
    df_numeric <- x[, sapply(x, function(i) is.numeric(i))]
    df_cor <- cor(df_numeric)
    diag(df_cor) <- 0
    return(df_cor[which.max(abs(df_cor))])
}
filtered.cor(iris)
iris$Petal.Length <- -iris$Petal.Length

# ЗАДАЧА
# Напишите функцию smart_cor, которая получает на вход dataframe 
# с двумя количественными переменными. Проверьте с помощью теста 
# Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному 
# распределению.

# Если хотя бы в одном векторе распределение переменной отличается от 
# нормального (p - value меньше 0.05), то функция должна возвращать 
# коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).

# Если в обоих векторах распределение переменных от нормального значимо 
# не отличается, то функция должна возвращать коэффициент корреляции Пирсона.
?cor.test
smart_cor <- function(x){
    if ((shapiro.test(x[, 1])$p.value < 0.05) | (shapiro.test(x[, 2])$p.value < 0.05)) {
        res <- cor.test(~., x, method = "spearman")    
    } else {
        res <- cor.test(~., x, method = "pearson")    
    }
    
    return(res$estimate)
}

df_test <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(df_test)


