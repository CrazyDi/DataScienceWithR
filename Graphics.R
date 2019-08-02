df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

hist(df$mpg, breaks = 20, xlab = "MPG")

boxplot(mpg ~ am, df, ylab = "MPG")

plot(df$mpg, df$hp)

library(ggplot2)

ggplot(df, aes(x = mpg))+
    geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df, aes(x = mpg, fill = am))+
    geom_dotplot()

ggplot(df, aes(x = mpg, fill = am))+
    geom_density(alpha = 0.5)

ggplot(df, aes(x = am, y = hp, col = vs))+
    geom_boxplot()

ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
    geom_point()

my_plot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
    geom_point()

my_plot2 <- ggplot(df, aes(x = am, y = hp, col = vs))

my_plot2 + geom_boxplot()

# ЗАДАЧА
# При помощи функции ggplot() или boxplot() постройте 
# график boxplot, используя встроенные в R данные airquality. 
# По оси x отложите номер месяца, по оси y — значения 
# переменной Ozone.

# На графике boxplot отдельными точками отображаются 
# наблюдения, отклоняющиеся от 1 или 3 квартиля 
# больше чем на полтора межквартильных размаха. 
# Сколько таких наблюдений присутствует в сентябре (месяц №9)?
boxplot(Ozone ~ Month, airquality)
ggplot(airquality, aes(x = Month, y = Ozone, group = Month))+
    geom_boxplot()

# ЗАДАЧА
# Нужно построить scatterplot с помощью ggplot из ggplot2, 
# по оси x которого будет mpg, по оси y - disp, 
# а цветом отобразить переменную (hp).
plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp))+
    geom_point()