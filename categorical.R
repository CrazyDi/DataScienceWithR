# Считываем данные
df <- read.csv("https://stepic.org/media/attachments/lesson/11502/grants.csv")
str(df)

# Переделаем поле status в фактор
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

# Построим таблицу по данным
t1 <- table(df$status)
t1

# Построим таблицу по двум переменным
t2 <- table(status = df$status, field = df$field)
t2

# Процентное содержание таблиц
prop.table(t1)
prop.table(t2)

# Процентное содержание по строке
prop.table(t2, 1)

# Процентное содержание по столбцу
prop.table(t2, 2)

# Трехмерные таблицы
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

# ЗАДАЧА
# HairEyeColor - таблица с данными, встроенными в R. 
# Посмотрите на неё в R. Команда dimnames(HairEyeColor) 
# позволит нам посмотреть, какие измерения есть в этой 
# таблице и как они называются. Например, чтобы обратиться к 
# части таблицы, в которой хранятся данные только о мужчинах, 
# нам нужно выполнить следующую команду: 

# HairEyeColor[ , ,'Male']

# Ваша задача в переменную red_men сохранить долю 
# рыжеволосых (Red) от общего числа голубоглазых мужчин.
red_men <- prop.table(HairEyeColor[, , 'Male'], 2)['Red', 'Blue']

# ЗАДАЧА
# С таблицами, как и с матрицами, можно совершать 
# разные арифметические операции, например, суммировать 
# все элементы таблицы.

# Напишите число зеленоглазых женщин в наборе данных 
# HairEyeColor.
sum(HairEyeColor[, 'Green', 'Female'])

# Графики
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# ЗАДАЧА
# Постройте столбчатую диаграмму распределения цвета глаз 
# по цвету волос только у женщин из таблицы HairEyeColor. 
# По оси X должен идти цвет волос, цвет столбиков 
# должен отражать цвет глаз. По оси Y - количество 
# наблюдений.
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
dataFemale <- subset(mydata, Sex == "Female")

obj <- ggplot(data = dataFemale, aes(x = Hair, y = Freq, fill = Eye)) + 
    geom_bar(stat="identity", position = "dodge") + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

# Биномиальный тест - посволяет ответить на вопрос, на сколько 
# эмпирическое распределение некотрого ряда событий, которые
# могут заканчиваться либо одним, либо другим исходом, 
# отличаются от теоретически предсказанного биномиального 
# распределения с заданной вероятностью
# Например, у нас есть монетка, мы подкидываем ее 20 раз 
# и у нас выпадает 5 орлов и 15 решек,
# биномиальный тест позволяет нам ответить на вопрос,
# является ли данная монетка нормальной монеткой с равновероятным
# выпадением орла и решки или она является скошенной
# в сторону решки
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)

# Тест хи-квадрат Пирсона
# Для простой таблички он проделат ту же операцию, что и биномиальный тест
chisq.test(t1)
chi <- chisq.test(t1)
chi$expected
chi$observed

chisq.test(t2)

# Точный критерий Фишера
fisher.test(t2)

# ЗАДАЧА
# На основе таблицы HairEyeColor создайте ещё одну таблицу, 
# в которой хранится информация о распределении цвета глаз 
# у женщин-шатенок (Hair = 'Brown'). Проведите тест 
# равномерности распределения цвета глаз у шатенок 
# и выведите значение хи-квадрата для этого теста.
dataFemaleBrown <- HairEyeColor['Brown', , 'Female']
dataFemaleBrown

chisq.test(dataFemaleBrown)


# ЗАДАЧА
# Воспользуемся данными diamonds из библиотеки ggplot2. 
# При помощи критерия Хи - квадрат проверьте гипотезу 
# о взаимосвязи качества огранки бриллианта (сut) 
# и его цвета (color). В переменную main_stat сохраните 
# значение статистики критерия Хи - квадрат. Обратите внимание, 
# main_stat должен быть вектором из одного элемента, 
# а не списком (листом).
data("diamonds")
table(diamonds)
tD <- table(diamonds$cut, diamonds$color)
chD <- chisq.test(tD)
main_stat <- chD$statistic
is.list(main_stat)

# ЗАДАЧА
# Опять воспользуемся данными diamonds из библиотеки ggplot2. 
# При помощи критерия Хи - квадрат проверьте гипотезу о 
# взаимосвязи цены (price) и каратов (carat) бриллиантов. 
# Для этого сначала нужно перевести эти количественные 
# переменные в формат пригодный для Хи - квадрат. 
# Создайте две новые переменные в данных diamonds:
# factor_price - где будет 1, если значение цены больше 
# либо равно чем среднее, и 0, если значение цены ниже 
# среднего цены по выборке.
# factor_carat - где будет 1, если число карат больше либо 
# равно чем среднее,  и 0, если ниже среднего числа карат по 
# выборке.
diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)
diamonds$factor_price <- as.factor(diamonds$factor_price)
diamonds$factor_price <- as.factor(diamonds$factor_price)
tD <- table(diamonds$factor_price, diamonds$factor_carat)
chD <- chisq.test(tD)
main_stat <- chD$statistic

# ЗАДАЧА
# При помощи точного критерия Фишера проверьте гипотезу 
# о взаимосвязи типа коробки передач (am) и типа двигателя (vs) 
# в данных mtcars. Результат выполнения критерия сохраните 
# в переменную.Получившийся p - уровень значимости сохраните 
# в переменную fisher_test.
data("mtcars")
table_mtcars <- table(mtcars$am, mtcars$vs)
test_mtcars <- fisher.test(table_mtcars)
test_mtcars
fisher_test <- test_mtcars$p.value
