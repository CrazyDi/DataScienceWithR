library(ggplot2)
ggplot(diamonds, aes(x = price)) +
    geom_histogram()

ggplot(diamonds, aes(x = price, y = carat)) + 
    geom_point() +
    geom_smooth()

ggplot(diamonds) + 
    geom_point(aes(x = price, y = carat)) +
    geom_smooth(aes(x = price, y = carat))

ggplot(diamonds, aes(x = price, y = carat, color = cut)) + 
    geom_point() +
    geom_smooth()

ggplot(diamonds, aes(x = price, y = carat)) + 
    geom_point(aes(color = cut)) +
    geom_smooth()

ggplot(diamonds, aes(x = price, y = carat)) + 
    geom_point(size = 0.5) +
    geom_smooth(size = 2, color = "red")

data("airquality")
str(airquality)
library(dplyr)
glimpse(airquality)

gr_airquality <- group_by(airquality, Month)
t <- summarise(gr_airquality, mean_temp = mean(Temp), mean_wind = mean(Wind))

ggplot(t, aes(Month, mean_temp)) + 
    geom_point() +
    geom_line()

ggplot(t, aes(Month, mean_temp)) + 
    geom_line() +
    geom_point(aes(size = mean_wind), color = "blue") 
    
ggplot(t, aes(Month, mean_temp)) + 
    geom_line() +
    geom_point(aes(size = mean_wind), color = "blue") +
    geom_hline(yintercept = 75, linetype = "dotted")

gr_mtcars <- group_by(mtcars, am, vs)
se_data <- summarise(gr_mtcars, 
                     mean_mpg = mean(mpg), 
                     y_max = mean(mpg) + 1.96 * sd(mpg) / sqrt(length(mpg)), 
                     y_min = mean(mpg) - 1.96 * sd(mpg) / sqrt(length(mpg)))

ggplot(se_data, aes(x = factor(am), y = mean_mpg, color = factor(vs))) +
    geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2) +
    geom_point(shape = 21, size = 3, fill = "white")

ggplot(se_data, aes(x = factor(am), y = mean_mpg, color = factor(vs), group = factor(vs))) +
    geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2) +
    geom_line() +
    geom_point(shape = 21, size = 3, fill = "white")


ggplot(se_data, aes(x = factor(am), y = mean_mpg)) +
    geom_pointrange(aes(ymin = y_min, ymax = y_max), size = 2)

ggplot(mtcars, aes(factor(am), mpg, col = factor(vs), group = factor(vs))) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = mean_cl_boot, geom = "point") +
    stat_summary(fun.data = mean_cl_boot, geom = "line")

sd_error <- function(x) {
    c(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}

ggplot(mtcars, aes(factor(am), mpg, col = factor(vs), group = factor(vs))) +
    stat_summary(fun.data = sd_error, geom = "errorbar", width = 0.2) +
    stat_summary(fun.data = sd_error, geom = "point") +
    stat_summary(fun.data = sd_error, geom = "line")

ggplot(mtcars, aes(x = factor(am), y = mpg)) +
    geom_violin() +
    geom_boxplot(width = 0.2) 
    
sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")

# ЗАДАЧА
# Отобразите взаимосвязь между доходом (income) и числом продаж (sale), цветом точек указав номер магазина (shop)
ggplot(sales, aes(x = income, y = sale)) +
    geom_point(aes(color = shop)) +
    geom_smooth()

# ЗАДАЧА
# При помощи функции stat_summary постройте график с доверительными интервалами для демонстрации различий в доходах двух магазинов с учетом времени года:
# переменная shop - ось x;
# переменная income - ось y;
# переменная season - цвет;
# geom pointrange.
ggplot(sales, aes(x = shop, y = income, color = season)) +
    stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))

# ЗАДАЧА
# Теперь давайте отобразим на графике различия в продажах (переменная sale), в зависимости от:
# года (date) - ось x;
# и номера магазина (shop) - цвет.
ggplot(sales, aes(x = date, y = sale, color = shop)) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.2)) +
    stat_summary(fun.data = mean_cl_boot, geom = "point", position = position_dodge(0.2)) +
    stat_summary(fun.data = mean_cl_boot, geom = "line", position = position_dodge(0.2))

# FACET
ggplot(mtcars, aes(hp, mpg, col = factor(am))) +
    geom_point()

data(diamonds)

ggplot(diamonds, aes(carat, fill = color)) +
    geom_density(alpha = 0.2)

ggplot(diamonds, aes(carat)) +
    geom_density() +
    facet_grid(color ~ cut)

# по столбцам
ggplot(diamonds, aes(carat)) +
    geom_density() +
    facet_grid(. ~ cut)

# по строкам
ggplot(diamonds, aes(carat)) +
    geom_density() +
    facet_grid(cut ~ .)

ggplot(diamonds, aes(carat, fill = color)) +
    geom_density(alpha = 0.2) +
    facet_grid(cut ~ .)

mtcars <- mutate(mtcars, am = factor(am, labels = c("A", "M")), vs = factor(vs, labels = c("V", "S")))

ggplot(mtcars, aes(hp)) +
    geom_dotplot() +
    facet_grid(am ~ vs, margins = TRUE)

ggplot(mtcars, aes(hp, mpg)) +
    geom_point(aes(col = factor(cyl))) +
    facet_grid(am ~ vs, margins = TRUE)

ggplot(diamonds, aes(carat)) +
    geom_density() +
    facet_wrap(~ cut + color)

ggplot(diamonds, aes(carat, price)) +
    geom_smooth() +
    facet_wrap( ~ color)

# ЗАДАЧА
# Потренируемся с разбиением графика на подгруппы! Используя facet_grid постройте следующий график и сохраните его в переменную mpg_facet.
# ось x - переменная mpg
# facet - переменная am по строчкам и vs по столбцам
ggplot(mtcars, aes(mpg)) +
    geom_dotplot() +
    facet_grid(am ~ vs)

# ЗАДАЧА
# Используя данные iris, постройте график плотности для переменной Sepal.Length. Разбейте график на части по переменной Species при помощи facet_wrap.
ggplot(iris, aes(Sepal.Length)) +
    geom_density() +
    facet_wrap( ~ Species)

# ЗАДАЧА
# Используя данные Iris, постройте график, иллюстрирующий взаимосвязь переменных Sepal.Length и Sepal.Width внутри каждого вида (переменной Species),
# при помощи facet_wrap().
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ Species)

# ЗАДАЧА
# Вы можете скачать данные myMovieData (жмите на ссылку), в которых представлена различная информация о голливудских фильмах с 2002 по 2005: 
# тип жанр, бюджет и год выхода на экраны. 
# Давайте построим следующий график, чтобы выяснить есть ли различия в бюджетах фильмов разного жанра из года в год. 
movie <- read.csv("https://stepik.org/media/attachments/course/724/myMovieData.csv")

ggplot(movie, aes(Type, Budget)) +
    geom_boxplot() +
    facet_grid(. ~ Year)

# Scale и Theme
ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon") 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    xlab("Miles/(US) gallon") 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = c(10, 20, 30, 31, 32, 33)) 

seq_x <- round(seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 10))

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = seq_x) 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = c(1, seq(10, 35, 5)), limits = c(1, 35)) 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    xlab("Miles/(US) gallon") +
    xlim(c(1, 35)) 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = c(1, seq(10, 35, 5)), limits = c(1, 35)) +
    scale_y_continuous(limits = c(50, 400)) 

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = c(1, seq(10, 35, 5)), limits = c(1, 35)) +
    scale_y_continuous(limits = c(50, 400)) +
    scale_color_discrete(name = "Transmission", labels = c("Automatic", "Manual"))

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) +
    geom_point() +
    scale_x_continuous(name = "Miles/(US) gallon", breaks = c(1, seq(10, 35, 5)), limits = c(1, 35)) +
    scale_y_continuous(limits = c(50, 400)) +
    scale_color_manual(values = c("red", "blue"), name = "Transmission", labels = c("Automatic", "Manual"))

ggplot(mtcars, aes(hp, fill = factor(am))) +
    geom_density(alpha = 0.2) +
    scale_fill_manual(values = c("Red", "Green"), name = "Тип коробки", labels = c("Автоматическая", "Ручная"))

ggplot(mtcars, aes(hp, mpg, size = disp)) +
    geom_point() +
    scale_size_continuous(name = "Легенда")

ggplot(mtcars, aes(factor(cyl), hp)) +
    geom_boxplot() +
    scale_x_discrete(name = "Цилиндры")

# ЗАДАЧА
# В этом задании мы построим график используя данные Iris. Наша цель отобразить взаимосвязь переменных Sepal.Length (ось X) и Petal.Length (ось Y) внутри трех групп по переменной Species. Для этого постройте scaterplot, отобразите цветом значения переменной Species и добавьте линейное сглаживание в каждой группе.

# Далее от вас потребуется привести график к более завершенному виду. Мы переведем на русский название осей, название легенды и ее расшифровку:
    
# Ось X - "Длина чашелистика".
# Ось Y - "Длина лепестка".
# Название легенды - "Вид цветка".
# Расшифровка легенды: "Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский".
# Также мы чуть измени отображение значений по осям.

# Значения по оси X должны начинаться с 4 и заканчиваться на 8 с шагом в единицу.
# Значения по оси Y должны начинаться с 1 и заканчиваться на 7 с шагом в единицу.
ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    scale_x_continuous(name = "Длина чашелистика", breaks = seq(4, 8, 1), limits = c(4, 8)) +
    scale_y_continuous(name = "Длина лепестка", breaks = seq(1, 7, 1)) +
    scale_color_discrete(name = "Вид цветка", labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))


ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl))) +
    geom_boxplot() +
    scale_fill_brewer(type = "qual", palette = 3) +
    theme_bw()

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = 6) +
    theme_bw()

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = 6) +
    theme(text = element_text(size = 14), axis.line.x = element_line(size = 2))

install.packages("ggthemes")    
library("ggthemes")
ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) +
    geom_point(size = 2) +
    theme_economist()

# ПРИМЕР
d <- read.csv("https://stepic.org/media/attachments/course/724/example_data.csv")
p <- ggplot(d, aes(date, percent, col = system, group = system)) +
    geom_line(size = 1.3) +
    geom_point(shape = 21, size = 4, stroke = 2, fill = "black") +
    geom_vline(xintercept = 7.5, col = "white", linetype = "dotted") +
    scale_color_manual(values = c("orangered1", "red", "cyan", "yellow1", "springgreen2")) +
    scale_y_continuous(breaks = c(0, 0.04, 0.08, 0.11, 0.15), 
                       limits = c(0, 0.15),
                       labels = scales::percent) +
    xlab("") +
    ylab("") +
    ggtitle("Top 5 Linux distribution") +
    theme_classic()


my_theme <- theme(legend.title = element_blank(), 
          legend.position = "top", 
          plot.background = element_rect(color = "black", fill = "black"),
          panel.background = element_rect(fill = "black"),
          legend.background = element_rect(fill = "black"),
          text = element_text(color = "white"),
          panel.grid.major.y = element_line(color = "gray50", linetype = "longdash"), 
          axis.text.x = element_text(face = "bold", size = 16, color = "white"),
          axis.text.y = element_text(face = "bold", size = 14, color = "white"),
          legend.text = element_text(size = 14),
          title = element_text(face = "bold", size = 16, color = "white"))    

p + my_theme

library(grid)
grid.text("Data source: The DistroWatch's Page Hit Ranking", x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontface = "bold", fontsize = 9, col = "white"))
