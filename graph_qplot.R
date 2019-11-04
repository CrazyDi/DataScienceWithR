library(ggplot2)
data("diamonds")

# по умолчанию гистограмма
qplot(x = price, data = diamonds)

# диаграмма рассеивания
qplot(x = price, y = carat, data = diamonds)

qplot(x = cut, y = carat, data = diamonds)


v <- diamonds$carat
qplot(v)

# ЗАДАЧА
# Используя функцию qplot, постройте гистограмму переменной depth из данных diamonds. Сохраните график в переменную depth_hist.
depth_hist <- qplot(diamonds$depth)
depth_hist

qplot(diamonds$carat, diamonds$price)

my_plot <- qplot(x = price, y = carat, data = diamonds)
str(my_plot)

qplot(x = price, y = carat, color = color, data = diamonds)

qplot(mpg, hp, color = factor(am), shape = factor(cyl), size = I(5), data = mtcars)
qplot(mpg, hp, color = "green", shape = factor(cyl), size = I(5), data = mtcars)
qplot(mpg, hp, color = I("green"), shape = factor(cyl), size = I(5), data = mtcars)

# ЗАДАЧА
# Постройте диаграмму рассеивания (scatter plot) как в указанном ниже примере, результат сохраните в переменную price_carat_clarity_points.
# данные - diamonds
# ось x - carat
# ось y - price
# цвет точек - clarity
price_carat_clarity_points <- qplot(x = carat, y = price, color = clarity, data = diamonds)
price_carat_clarity_points

qplot(mpg, hp, color = I("green"), shape = factor(cyl), size = I(5), alpha = I(0.3), data = mtcars)

qplot(x = price, data = diamonds, fill = color, col = I("black"), geom = "density", alpha = I(0.2))

# ЗАДАЧА
# Используя функцию qplot, постройте график плотности переменной x из данных diamonds. Сохраните график в переменную x_density.
x_density <- qplot(x = x, data = diamonds, geom = "density")
x_density

# ЗАДАЧА
# Усложним задачу, постройте график плотности переменной x для каждой группы наблюдений по переменной cut из данных diamonds. 
# Таким образом за цвет графика теперь отвечает переменная cut. Сохраните результат в переменную x_cut_density.
x_cut_density <- qplot(x = x, data = diamonds, color = cut, geom = "density")
x_cut_density

# ЗАДАЧА
# Давайте построим график violin plot для переменной price в каждой группе наблюдений по переменной color. 
# Сохраните результа в переменную price_violin.
price_violin <- qplot(x = color, y = price, data = diamonds, geom = "violin")
price_violin
