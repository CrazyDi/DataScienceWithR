library(dplyr)

my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))
my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))
my.data
my_data

library(ggplot2)
diamonds <- as_data_frame(diamonds)
diamonds


my_data_2 <- data_frame("My var"= rnorm(10))
my_data_2


my_data_2 <- data_frame(x = rnorm(10), y = abs(x))

# select - выбирает колонки
select(diamonds, cut, price)
select(diamonds, cut:price)
select(diamonds, -cut)
select(diamonds, 1, 2, 3)
select(diamonds, starts_with("c"))

# slice - выбирает строки
slice(diamonds, 2)
slice(diamonds, 2:10)
slice(diamonds, c(1, 4, 5))

# filter - выбирает данные по условию
filter(diamonds, carat > 0.3, color == "J")

# arrange - сортировка
arrange(diamonds, price, depth)
arrange(diamonds, desc(price))

# rename - переименование колонки
rename(diamonds, new_cut = cut)

# mutate - добавление\преобразование колонки
mutate(diamonds, sqrt_price = sqrt(price), log_carat = log(carat))
mutate(mtcars, am = factor(am), vs = factor(vs))

# ЗАДАЧА
# В переменную d сохраните только нeчетные строчки исходных данных diamonds.
d <- slice(diamonds, seq(from = 1, to = nrow(diamonds), by = 2))

# ЗАДАЧА
# Потренируемся использовать изученные функции. 
# Из данных mtcars отберите только четыре переменные: mpg, hp, am, vs. 
# Оставьте только те наблюдения, для которых значения mpg > 14 и hp > 100. 
# Отсортируйте получившиеся данные по убыванию переменной mpg и возьмите только первые 10 строчек. 
# Переменную mpg переименуйте в Miles per gallon, а переменную hp в  Gross horsepower 
# (обратите внимание, dplyr позволит нам создать пременные с пробелами в названии). 
# Получившийся dataframe сохраните в переменную my_df.
my_df <- mtcars %>% 
    select(mpg, hp, am, vs) %>% 
    filter(mpg > 14, hp > 100) %>% 
    arrange(desc(mpg)) %>% 
    slice(1:10) %>% 
    rename("Miles per gallon" = mpg, "Gross horsepower" = hp)


rename(mtcars, mpg = Miles)

# mutate.each - преобразование с каждой колонкой
d <- as_data_frame(matrix(rnorm(30), ncol = 5))
mutate_each(d, funs(ifelse(. < 0, 0, .)))

# ЗАДАЧА
# Напишите функцию, all_to_factor, которая преобразует dataframe, переводя все его переменные в фактор.
all_to_factor <- function(x) {
    mutate_each(x, funs(as.factor))
}

# ЗАДАЧА
# Ваша задача написать функцию, которая получает на вход dataframe  с произвольным числом переменных разных типов. 
# На первом этапе функция должна выполнить предобработку числовых переменных. 
# Т.к. значение логарифма мы можем рассчитать только для положительных чисел. 
# Для этого сделаем центрирование всех переменных (Rescaling), только еще добавим единичку, чтобы у нас не осталось нулей:

# x=(x−xmin)/(xmax−xmin)+1 

# После того как мы масштабировали каждую переменную, 
# осталось рассчитать значение натурального логарифма каждого наблюдения (функция log) и вернуть новый dataframe. 
rescaling <- function(x) {
    if (is.numeric(x)) {
        return(log((x - min(x)) / (max(x) - min(x)) + 1))
    } else {
        return(x)
    }
}

log_transform <- function(test_data) {
    mutate_each(test_data, funs(rescaling))
}
test_data <- data_frame(v1 = c(1.5, -0.1, 2.5, -0.3, -0.8), v2 = c(-0.9, -0.3, -2.4, 0.0, 0.4), v3 = c(-2.8, -3.1, -1.8, 2.1, 1.9), v4 = c("A", "B", "B", "B", "B"))
log_transform(test_data)


# group_by - разделение по группам
gr_diamonds <- group_by(diamonds, cut)
sample_n(diamonds, 2)
sample_n(gr_diamonds, 2)

slice(gr_diamonds, 1)

# summarise - преобразовывает множество значений в одно
summarise(mtcars, mean_disp = mean(disp), sd_disp = sd(disp))
summarise(diamonds, mean(price))
summarise(gr_diamonds, mean(price))
summarise(gr_diamonds, n = n())
summarise(gr_diamonds, n = n(), great_price = sum(price > 5000))

# summarise_all - все переменные в группированном data.frame
gr_mtcars <- group_by(mtcars, am, vs)
summarise_all(gr_mtcars, funs(mean))
summarise_all(gr_mtcars, funs(sum(.>10)))

group_by(iris, Species) %>% 
    summarise_all(funs(sd, mean))

# Использование dplyr, если мы не знаем имен переменных
var_to_select <- "hp"
select_(mtcars, var_to_select)
mtcars$am <- factor(mtcars$am)
mtcars$vs <- factor(mtcars$vs)
factor_vars <- names(which(sapply(mtcars, is.factor)))
mtcars %>% 
    group_by_(.dots = factor_vars) %>% 
    summarise(n = n())

mutate(mtcars, new_var = (hp - mean(hp)) / sd(hp))
mini_mtcars <- select(mtcars, hp, am, vs)
mini_mtcars <- mini_mtcars %>% mutate(am = factor(am), vs = factor(vs))
mutate_(mini_mtcars, new_var = ~ (hp - mean(hp)) / sd(hp))

library(lazyeval)
num_var <- names(which(sapply(mini_mtcars, is.numeric)))
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))

var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summarise <- "cyl"
group_by_(mtcars, .dots = var_for_group) %>% 
    filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
    arrange_(var_for_arrange) %>% 
    mutate_(new_var = interp(~ifelse(var > mean(var), 1, 0), var = as.name(var_for_mutate))) %>% 
    summarise_(max = interp(~max(var), var = as.name(var_for_summarise)))

by_species <- iris %>% group_by(Species)
by_species %>% summarise_if(is.numeric, mean)
by_species %>% summarise_if(function(col) {if(!is.numeric(col)) return(FALSE) else mean(col) > 2}, mean)
by_species %>% summarise_at(vars(Petal.Width), mean)
by_species %>% summarise_at(vars(matches("Width")), mean)
by_species %>% summarise_at(c("Sepal.Width", "Petal.Width"), mean)
by_species %>% summarise_at(c(1, 3), mean)
by_species %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
by_species %>% summarise_at(vars(matches("Width")), funs(min, max))
select_if(iris, is.numeric) %>% summarise_all(funs(sd))
