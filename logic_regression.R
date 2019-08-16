my_df <- read.csv('https://stepic.org/media/attachments/lesson/10226/train.csv', sep = ';')

ggplot(my_df, aes(read, math, col = gender)) +
    geom_point() + 
    facet_grid(.~hon)
       
fit <- glm(hon ~ read + math + gender, my_df, family = 'binomial')          
summary(fit)

fit$coefficients
exp(fit$coefficients)

head(predict(object = fit, type = 'response'))

my_df$prob <- predict(object = fit, type = 'response')

# ЗАДАЧА
# Используем данные mtcars. Сохраните в переменную 
# логистическую регрессионную модель, где в качестве 
# зависимой переменной выступает тип коробки передач (am), 
# в качестве предикторов переменные disp, vs, mpg.
data(mtcars)
mtcars$am <- as.factor(mtcars$am)

fit <- glm(am ~ disp + vs + mpg, mtcars, family = 'binomial')
log_coef <- fit$coefficients

# ЗАДАЧА
# Дополните предложенный в задании код, чтобы построить 
# следующий график по данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в 
# различных условиях дозировки и типа потребляемого продукта.
# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.

data(ToothGrowth)
ggplot(data = ToothGrowth, aes(x = supp, y = len, group = factor(dose))) + 
    geom_boxplot(aes(fill = factor(dose))) + 
    facet_grid(.~supp)

# ROCR кривые
install.packages('ROCR')
library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit, "tpr", "fpr")
plot(perf_fit, colorize = 1, print.cutoffs.at = seq(0, 1, by = 0.1))

auc <- performance(pred_fit, measure = "auc")
str(auc)

perf3 <- performance(pred_fit, x.measure = 'cutoff', measure = 'spec')
plot(perf3, col = 'red', lwd = 2)

perf4 <- performance(pred_fit, x.measure = 'cutoff', measure = 'sens')
plot(add = T, perf4, col = 'green', lwd = 2)

perf5 <- performance(pred_fit, x.measure = 'cutoff', measure = 'acc')
plot(add = T, perf5, lwd = 2)

my_df$pred_resp <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)

ggplot(my_df, aes(prob, fill = factor(correct))) + 
    geom_dotplot()
    

test_df <- read.csv("https://stepic.org/media/attachments/lesson/10226/test.csv", sep = ';')
test_df$hon <- NA

test_df$hon <- predict(fit, newdata = test_df, type = 'response')

# ЗАДАЧА
# По имеющимся данным в переменной admit постройте 
# логистическую регрессионную модель, предсказывающую 
# результат поступления по престижности учебного заведения 
# среднего образования (переменная rank, 1 — наиболее 
# престижное, 4 — наименее престижное) и результатов GPA 
# (переменная gpa) с учётом их взаимодействия. 
# Примените эту модель к той части данных, где результат 
# поступления неизвестен.
# Ответом в задаче будет предсказанное моделью число 
# поступивших из тех, для кого результат поступления был 
# неизвестен. Считаем человека поступившим, когда 
# вероятность его поступления не меньше 0.4.
my_df <- read.csv('https://stepic.org/media/attachments/lesson/11478/data.csv')
df_fill <- subset(my_df, !is.na(admit))
df_empty <- subset(my_df, is.na(admit))

fit <- glm(admit ~ rank*gpa, data = df_fill, family = 'binomial')
df_empty$prob <- predict(object = fit, newdata = df_empty, type = 'response')
df_empty$admit <- ifelse(df_empty$prob < 0.4, 0, 1)
sum(df_empty$admit)
