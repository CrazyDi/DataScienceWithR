swiss <- data.frame(swiss)
str(swiss)

hist(swiss$Fertility, col='red')

# Предскажем рождаемость от физ. подготовки и верисповедания
fit <- lm(Fertility ~ Examination + Catholic, data=swiss)
summary(fit)

# Взаимодействие переменных
fit2 <- lm(Fertility ~ Examination * Catholic, data=swiss)
summary(fit2)

# Доверительные интервалы
confint(fit2)

# ЗАДАЧА
# Напишите функцию fill_na, которая принимает на вход 
# данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя 
# только наблюдения, в которых нет пропущенных значений, 
# мы построим регрессионную модель (без взаимодействий), 
# где  y — зависимая переменная, x_1 и x_2 — независимые 
# переменные. Затем, используя построенную модель, мы 
# заполним пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой 
# переменной  y_full. Сохраните в нее переменную y, 
# в которой пропущенные значения заполнены предсказанными 
# значениями построенной модели.
fill_na <- function(x){
    fit_test <- lm(y ~ x_1 + x_2, data=x, na.action=na.omit)
    x$y_full <- ifelse(is.na(x$y), predict(fit_test, x[, c(1, 2)]), x$y)
    return(x)
}
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na(test_data)

# ЗАДАЧА
# В переменной df сохранен subset данных mtcars только с 
# переменными "wt", "mpg", "disp", "drat", "hp". 
# Воспользуйтесь множественным регрессионным анализом, 
# чтобы предсказать вес машины (переменная "wt"). 
# Выберите такую комбинацию независимых переменных 
# (из "mpg", "disp", "drat", "hp"), чтобы значение R^2 
# adjusted было наибольшим. Взаимодействия факторов учитывать 
# не надо. 
df <- subset(mtcars, select=c(wt, mpg, disp, drat, hp))
fit_max <- lm(wt ~ mpg + disp + hp, data=df)
summary(fit_max)

# ЗАДАЧА
# Воспользуйтесь встроенным датасетом attitude, 
# чтобы предсказать рейтинг (rating) по переменным 
# complaints и critical. Каково t-значение для взаимодействия 
# двух факторов?
test_df <- attitude
fit_test <- lm(rating ~ complaints * critical, test_df)
summary(fit_test)

# Категориальные предикторы
hist(swiss$Catholic, col="red")
swiss$Religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$Religious <- as.factor(swiss$Religious)

fit3 <- lm(Fertility ~ Examination + Religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ Examination * Religious, data = swiss)
summary(fit4)


# Графики
ggplot(swiss, aes(x = Examination, y = Fertility)) + 
    geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
    geom_point() +
    geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
    geom_point() +
    geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = Religious)) + 
    geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility, col = Religious)) + 
    geom_point() + 
    geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = Religious)) + 
    geom_point() + 
    geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ Religious * Infant.Mortality * Examination, data = swiss)
summary(fit5)

# ЗАДАЧА
# В этом примере будем работать с хорошо вам известным 
# встроенным датасетом mtcars. Переменная am говорит о том, 
# какая коробка передач используется в машине: 
# 0 - автоматическая, 1 - ручная. 

# Сделаем эту переменную факторной. 
# Теперь постройте линейную модель, в которой в качестве 
# зависимой переменной выступает расход топлива (mpg), 
# а в качестве независимых - вес машины (wt) и 
# коробка передач (модифицированная am), 
# а также их взаимодействие. Выведите summary этой модели.
# Что отражает значение intercept в данной модели?
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
fit_test <- lm(mpg ~ am * wt, mtcars)
summary(fit_test)
ggplot(mtcars, aes(x = wt, y = mpg, col = am)) +
    geom_smooth(method = 'lm')

# Отбор моделей
rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~., swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

# Автоматический отбор моделей
optimal_fit <- step(fit_full, direction = 'backward')
summary(optimal_fit)

# ЗАДАЧА
# C помощью функции step найдите оптимальную модель для 
# предсказания rating в датасете attitude. Model_full и 
# model_null уже созданы. Сохраните команду с функцией step в 
# переменную ideal_model. 
attitude <- data.frame(attitude)
model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)
optimal_fit <- step(model_null, scope = list(lower = model_null, upper = model_full), direction = 'forward')

# ЗАДАЧА
# Сравните полную модель из предыдущего степа и оптимальную 
# модель с помощью функции anova. Введите получившееся 
# F-значение.
anova(model_full, optimal_fit)

# ЗАДАЧА
# Напоследок потренируемся в эффективном написании формул. 
# В этой задаче будем работать со встроенным датасетом 
# LifeCycleSavings. Попытаемся предсказать значение sr 
# на основе всех остальных переменных в этом датасете. 
# Вспомните способы сокращения формул и напишите команду, 
# которая создаёт линейную регрессию с главными эффектами 
# и всеми возможными взаимодействиями второго уровня. 
# Сохраните модель в переменную model.
LifeCycleSavings <- data.frame(LifeCycleSavings)
model <- lm(sr ~ (pop15 + pop75 + dpi + ddpi) ^ 2, LifeCycleSavings)
summary(model)
