?mtcars
df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

# Простейшие описательные статистики
median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

# ЗАДАЧА
#  Рассчитайте среднее значение времени разгона (qsec) 
# для автомобилей, число цилиндров (cyl) у которых 
# не равняется 3 и показатель количества миль на галлон 
# топлива (mpg) больше 20.
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# Расчет статистик по группам
?aggregate

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

# Несколько статистик по группам
aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median)
aggregate(x = df[, c(1, 3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# ЗАДАЧА
# При помощи функции aggregate рассчитайте стандартное отклонение 
# переменной hp (лошадиные силы) и переменной disp (вместимости 
# двигателя)  у машин с автоматической и ручной коробкой передач. 
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)

install.packages("psych")
install.packages("ggplot2")

library(psych)
?describe
describe_df <- describe(x = df)

?describeBy()
descr2 <- describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1)

descr3 <- describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, fast = T)

# Пропущенные значения
sum(is.na(df$mpg))
sum(is.na(df))

df$mpg[1:10] <- NA

mean(df$mpg)
mean(df$mpg, na.rm = T)

aggregate(mpg ~ am, df, sd)

describe()

# ЗАДАЧА 
# Воспользуемся встроенными данными airquality. 
# В новую переменную сохраните subset исходных данных, 
# оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество 
# непропущенных наблюдений по переменной Ozone в 7, 8 и 9 
# месяце. Для определения количества наблюдений используйте 
# функцию length(). 
# Результат выполнения функции aggregate сохраните в переменную result.
v <- subset(airquality, Month %in% c(7, 8, 9))
result <- aggregate(Ozone ~ Month, v, FUN = length)

result <- aggregate(Ozone ~ Month, airquality, subset = Month %in% c(7, 8, 9), length)

# ЗАДАЧА
# Примените функцию describeBy к количественным переменным данных 
# airquality, группируя наблюдения по переменной Month.  
# Чему равен коэффициент асимметрии (skew) переменной Wind в восьмом месяце?
describeBy(airquality, group = airquality$Month, na.rm = T)

# ЗАДАЧА
# В переменной my_vector сохранен вектор с пропущенными значениями. 
# Вам нужно создать новый вектор fixed_vector, в котором все 
# пропущенные значения вектора my_vector будут заменены на среднее 
# значение по имеющимся наблюдениям.
my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA

fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))
