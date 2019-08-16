df <- mtcars
df_numeric <- df[, c(1, 3:7)]

# Построим регрессионную модель
fit <- lm(mpg ~ hp, df)
fit
summary(fit)

ggplot(df, aes(hp, mpg)) + 
    geom_point(size = 2) +
    geom_smooth(method = 'lm') + 
    facet_grid(.~cyl)

# Предсказанные значения
fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100, 150, 120, 300))
new_hp$mpg <- predict(fit, new_hp)

# Номинативные данные в качестве предиктора
my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit)

aggregate(mpg ~ cyl, my_df, mean)

ggplot(my_df, aes(cyl, mpg)) + 
    geom_point() +
    theme(axis.text = element_text(size = 25),
          axis.title = element_text(size = 25, face = 'bold'))

# ЗАДАЧА
# Скачайте набор данных - dataframe с двумя количественными 
# переменными (вспомните при необходимости, как задавать 
# разделитель и другие параметры функции read.table), 
# постройте линейную регрессию, где - первая переменная - 
# зависимая, вторая - независимая. В ответ укажите значения 
# регрессионных коэффициентов сначала intercept затем  slope.
df_test <- read.table('dataset_11508_12.txt')
fit_test <- lm(V1 ~ V2, df_test)
fit_test$coefficients

# ЗАДАЧА
# Воспользуемся уже знакомыми данными diamonds из библиотеки 
# ggplot2. Только для бриллиантов класса Ideal (переменная 
# cut) c числом карат равным 0.46 (переменная carat) 
# постройте линейную регрессию, где в качестве зависимой 
# переменной выступает price, в качестве предиктора - 
# переменная  depth. Сохраните коэффициенты регрессии в 
# переменную fit_coef.
test_df <- diamonds[diamonds$cut == 'Ideal' & diamonds$carat == 0.46, ]
fit_coef <- lm(price ~ depth, test_df)$coefficients

# ЗАДАЧА
# Напишите функцию regr.calc, которая на вход получает dataframe 
# c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости 
# для коэффициента корреляции Пирсона меньше 0.05), то функция строит 
# регрессионную модель, где первая переменная - зависимая, 
# вторая - независимая. Затем создает в dataframe новую переменную 
# с назанием fit, где сохраняет предсказанные моделью значения 
# зависимой переменной. В результате функция должна возвращать 
# исходный dataframe с добавленной новой переменной fit.

# Если две переменные значимо не коррелируют, то функция возвращает 
# строчку "There is no sense in prediction"
regr.calc <- function(x){
    fit_cor <- cor.test(x = x[, 1], y = x[, 2])
    if (fit_cor$p.value < 0.05) {
        fit_lm <- lm(x[, 1] ~ x[, 2])
        x$fit <- fit_lm$fitted.values
        return(x)
    } else {
        return('There is no sense in prediction')
    }
}

# ЗАДАЧА
# Постройте scatterplot по данным iris, сохранив его в 
# переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы 
# наблюдений по переменной Species.
my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = Species)) +
    geom_point() + 
    geom_smooth(method = 'lm')
