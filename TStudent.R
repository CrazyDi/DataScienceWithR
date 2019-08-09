df <- iris

str(df)

df1 <- subset(df, Species != "setosa")
table(df1$Species)

# Мы хотим сравнить Sepal.Length по двум группам цветка
hist(df1$Sepal.Length)

library(ggplot2)
ggplot(df1, aes(x = Sepal.Length)) +
    geom_histogram(fill = "white", col = "black", binwidth = 0.4) +
    facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species)) + 
    geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length)) + 
    geom_boxplot()

# Мы хотим убедиться, что наша выборка подходит для анализа критерия t-Стьюдента
# Проверка нормальности 
shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

# Проверка гомогенности дисперсий
bartlett.test(Sepal.Length ~ Species, df1)

# Проведем тест t-критерий Стьюдента
t.test(Sepal.Length ~ Species, df1)
test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

t.test(Sepal.Length ~ Species, df) # нельзя, только 2 уровня

t.test(Sepal.Length ~ Species, df1, var.equal = T)

mean(df1$Sepal.Length)

# Одноуровневый тест
t.test(df1$Sepal.Length, mu = 8)

# Проверка зависимых переменных
# Проверим гипотезу, что длина и ширина лепестка на самом деле не равны
t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# ЗАДАЧА
# Воспользуемся еще одним встроенным набором данных в 
# R  - ToothGrowth. Данные позволяют исследовать рост зубов 
# у морских свинок в зависимости от дозировки витамина C 
# и типа потребляемых продуктов.
# Сравните среднее значение длины зубов свинок, 
# которые потребляли апельсиновый сок (OJ) с дозировкой 
# 0.5 миллиграмм, со средним значением длины зубов свинок, 
# которые потребляли аскорбиновую кислоту (VC) с 
# дозировкой 2 миллиграмма. 
?ToothGrowth
tg <- ToothGrowth
tg1 <- subset(tg, (supp == "OJ" & dose == 0.5) | (supp == "VC" & dose == 2.0))
t_test <- t.test(len ~ supp, tg1)
t_stat <- t_test$statistic

# ЗАДАЧА
# По всем испытуемым сравните показатель давления до 
# начала лечения (Pressure_before) с показателем давления 
# после лечения (Pressure_after) при помощи 
# t - критерия для зависимых выборок. 
lek <- read.csv("https://stepic.org/media/attachments/lesson/11504/lekarstva.csv")
t.test(lek$Pressure_before, lek$Pressure_after, paired = T)

# ВИЗУАЛИЗАЦИЯ
install.packages("Hmisc")
library(Hmisc)
# Изобразим среднее и доверительные интервалы
ggplot(df1, aes(Species, Sepal.Length)) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
    stat_summary(fun.y = mean, geom = "point", size = 4)


ggplot(df1, aes(Species, Sepal.Length)) +
    stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)
    
# Непараметрический аналог критерия t-Стьюдента
test2 <- wilcox.test(Petal.Length ~ Species, df1)
test2$statistic

ggplot(df1, aes(Species, Petal.Length)) +
    geom_boxplot()

# зависимые выборки
wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

# ЗАДАЧА
# В этом задании нужно проверить гипотезу о равенстве средних двух 
# выборок, загрузив набор данных (нажмите начать решать задание) и 
# выполнив все необходимые операции на вашем компьютере.
# Сначала с помощью теста Бартлетта проверьте гомогенность 
# дисперсий двух выборок. В случае, если дисперсии значимо не 
# отличаются (с уровнем 0.05), примените тест Стьюдента, 
# иначе - непараметрический тест (Манна-Уитни). В поле для 
# ответа введите получившийся p-value, с точностью четыре 
# знака после запятой.
df <- read.table("d:/Git/DataScienceWithR/dataset_11504_15.txt")

b_stat <- bartlett.test(V1 ~ V2, df)$p.value
if (b_stat < 0.05) {
    res <- wilcox.test(V1 ~ V2, df)$p.value
} else {
    res <- t.test(V1 ~ V2, df, var.equal = TRUE)$p.value
}
res

# ЗАДАЧА
# В данных сохранены две количественные переменные, 
# проверьте гипотезу о равенстве средних этих переменных 
# при помощи t- теста для независимых выборок.

# Если обнаружены значимые различия (p< 0.05), то введите 
# через пробел три числа: среднее значение первой переменной, 
# среднее значение второй переменной, p - уровень значимости. 
df <- read.table("d:/Git/DataScienceWithR/dataset_11504_16.txt")

t.test(df$V1, df$V2, paired = TRUE)
