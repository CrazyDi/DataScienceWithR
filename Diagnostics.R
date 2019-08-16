data(swiss)
str(swiss)

pairs(swiss)

ggplot(swiss, aes(x=Examination, y=Education)) +
    geom_point() +
    theme(axis.text = element_text(size=25),
          axis.title = element_text(size=25, face="bold"))

# Выбросы
ggplot(swiss, aes(x=Examination, y=Education)) +
    geom_point() +
    theme(axis.text = element_text(size=25),
          axis.title = element_text(size=25, face="bold")) + 
    geom_smooth(method = 'lm')

# Нормальность распределения
ggplot(swiss, aes(x = Examination)) + 
    geom_histogram()

ggplot(swiss, aes(x = log(Education))) + 
    geom_histogram()

# ЗАДАЧА
# Функция scale() позволяет совершить стандартизацию вектора, 
# то есть делает его среднее значение равным нулю, а стандартное 
# отклонение - единице (Z-преобразование). 
# Стандартизованный коэффициент регрессии (β) можно получить, 
# если предикторы и зависимая переменная стандартизованы.
# Напишите функцию, которая на вход получает dataframe с двумя 
# количественными переменными, а возвращает стандартизованные 
# коэффициенты для регрессионной модели, в которой первая 
# переменная датафрейма выступает в качестве зависимой, а 
# вторая в качестве независимой.
beta.coef <- function(x){
    x[, 3] <- scale(x[, 1])[, 1]
    x[, 4] <- scale(x[, 2])[, 1]
    
    fit <- lm(x[, 3] ~ x[, 4])
    return(fit$coefficients)
}
beta.coef(mtcars[,c(1,3)])

# ЗАДАЧА
# Напишите функцию normality.test, которая получает на 
# вход dataframe с количественными переменными, проверяет 
# распределения каждой переменной на нормальность с помощью 
# функции shapiro.test. Функция должна возвращать вектор 
# с значениями p - value, полученного в результате 
# проверки на нормальность каждой переменной. Названия 
# элементов вектора должны совпадать с названиями 
# переменных. 
t <- mtcars
v <- sapply(t, function(x) shapiro.test(x)$p.value)

# Линейная зависимость
ggplot(data = swiss, aes(Examination, Education)) +
    geom_point() +
    geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_squared <- (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted.values
swiss$lm2_fitted <- lm2$fitted.values
swiss$lm1_resid <- lm1$residuals
swiss$lm2_resid <- lm2$residuals
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
    geom_point(size = 3) +
    geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd = 1) +
    geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd = 1)
    
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, col = 'red', lwd = 1)

# Независимость остатков
ggplot(swiss, aes(x = obs_number, y = lm1_resid)) +
    geom_point(size = 3) +
    geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) +
    geom_point(size = 3) +
    geom_smooth()

# Гомоскедастичность
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) +
    geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) +
    geom_point(size = 3)

df <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
fit <- lm(DV ~ IV, df)
install.packages('gvlma')
library(gvlma)
x <- gvlma(fit)
summary(x)

# Нормальность распределения остатков
ggplot(swiss, aes(x = lm1_resid)) +
    geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

ggplot(swiss, aes(x = lm2_resid)) +
    geom_histogram(binwidth = 4, fill = 'white', col = 'black')


qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

# ЗАДАЧА
# Напишите функцию resid.norm, которая тестирует 
# распределение остатков от модели на нормальность при 
# помощи функции shapiro.test и создает гистограмму при 
# помощи функции ggplot() с красной заливкой "red", 
# если распределение остатков значимо отличается от 
# нормального (p < 0.05), и с зелёной заливкой 
# "green" - если распределение остатков значимо не 
# отличается от нормального. 
# На вход функция получает регрессионную модель. 
# Функция возвращает переменную, в которой сохранен 
# график ggplot.
resid.norm  <- function(fit){
    resid <- data.frame(fit$residuals)
    if (shapiro.test(resid$fit.residuals)$p.value < 0.05) {
        res_col <- 'red'
    } else {
        res_col <- 'green'
    }
    return(ggplot(resid, aes(x = fit.residuals)) +
               geom_histogram(fill = res_col))
}
fit <- lm(mpg ~ disp, mtcars)
resid <- data.frame(fit$residuals)
ggplot(resid, aes(x = fit.residuals)) +
    geom_histogram(fill = 'red')
my_plot <- resid.norm(fit)
my_plot
fit <- lm(mpg ~ wt, mtcars)

# ЗАДАЧА
# Напишите функцию high.corr, которая принимает на вход 
# датасет с произвольным числом количественных переменных 
# и возвращает вектор с именами двух переменных с 
# максимальным абсолютным значением коэффициента 
# корреляции . 
high.corr <- function(x){
    m <- cor(x)
    diag(m) <- 0
    index <- which.max(abs(m))
    count_dim <- length(dimnames(m)[[1]])
    numrow <- (index - 1) %/% count_dim + 1
    numcol <- index %% count_dim
    if (numcol == 0) {
        numcol <- count_dim
    }
    return(c(dimnames(m)[[1]][numrow], dimnames(m)[[2]][numcol]))
}

high.corr(iris[,-5])
m <- cor(iris[,-5])
diag(m) <- 0
index <- which.max(abs(m))
count_dim <- length(dimnames(m)[[1]])
