library(ggplot2)
DV ~ IV # One-way
DV ~ IV1 + IV2 # Two-way
DV ~ IV1:IV2 # Two-way interaction
DV ~ IV1 + IV2 + IV1:IV2 # Main effects + interation
DV ~ IV1 * IV2 # The same: Main effects + interation
DV ~ (IV1 + IV2 + IV3)^2 # main effects and all possible interation
DV ~ IV1 + Error(subject/IV1) # repeated measures

df <- read.csv("https://stepic.org/media/attachments/lesson/11505/shops.csv")

# Однофакторный анализ
# Исследуем зависимость цены от производителя
boxplot(price ~ origin, data=df)
ggplot(df, aes(x = origin, y = price)) +
    geom_boxplot()

fit <- aov(price ~ origin, data=df)
summary(fit)

# Двухфакторный анализ
# Исследуем зависимость цены от производителя и магазина
fit <- aov(price ~ origin + store, data=df)
summary(fit)

model.tables(fit, 'means')

# Анализ взаимодействия
ggplot(df, aes(x=store, y=price, color=origin, group=origin)) +
    stat_summary(fun.data=mean_cl_boot,
                 geom='errorbar', width=0.1,
                 position=position_dodge(width=0.2)) +
    stat_summary(fun.data=mean_cl_boot,
                 geom='line', size=1,
                 position=position_dodge(width=0.2)) +
    stat_summary(fun.data=mean_cl_boot,
                 geom='point', shape='square',
                 size=3, position=position_dodge(width=0.2)) +
    theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=df)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=df)
summary(fit4)

# ЗАДАЧА
# Воспользуемся встроенными данными npk, иллюстрирующими влияние 
# применения различных удобрений на урожайность гороха (yield). 
# Нашей задачей будет выяснить, существенно ли одновременное 
# применение азота (фактор N) и фосфата (фактор P). Примените 
# дисперсионный анализ, где будет проверяться влияние фактора 
# применения азота (N), влияние фактора применения фосфата (P) 
# и их взаимодействие.
test_df <- npk
test_fit <- aov(yield ~ N * P, data=test_df)
summary(test_fit)

# ЗАДАЧА
# Теперь проведите трехфакторный дисперсионный анализ, 
# где зависимая переменная - это урожайность (yield), 
# а три фактора - типы удобрений (N, P, K). 
# После проведения данного анализа вы получите три значения 
# p - уровня значимости (о значимости каждого из факторов).
test_fit3 <- aov(yield ~ N + P + K, data=test_df)
summary(test_fit3)

# Попарные сравнения
ggplot(df, aes(x = food, y = price)) +
    geom_boxplot()

fit5 <- aov(price ~ food, data=mydata)
summary(fit5)
TukeyHSD(fit5)

# ЗАДАЧА 
# Проведите однофакторный дисперсионный анализ на встроенных 
# данных iris. Зависимая переменная - ширина чашелистика 
# (Sepal.Width), независимая переменная - вид (Species). 
# Затем проведите попарные сравнения видов. Какие виды 
# статистически значимо различаются по ширине чашелистика 
# (p < 0.05)?
test_df <- iris
test_fit <- aov(Sepal.Width ~ Species, data=test_df)
TukeyHSD(test_fit)

# Дисперсионный анализ с повторным наблюдением
df2 <- read.csv("https://stepic.org/media/attachments/lesson/11505/therapy_data.csv")
df2$subject <- as.factor(df2$subject)

fit1 <- aov(well_being ~ therapy, data=df2)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data=df2)
summary(fit1b)

fit2 <- aov(well_being ~ therapy * price, data=df2)
summary(fit2)

ggplot(df2, aes(x = price, y = well_being)) + 
    geom_boxplot()

fit2b <- aov(well_being ~ therapy * price + Error(subject/(therapy*price)), data=df2)
summary(fit2b)

ggplot(df2, aes(x = price, y = well_being)) + 
    geom_boxplot() +
    facet_grid(~subject)

fit3 <- aov(well_being ~ therapy * price * sex, data=df2)
summary(fit3)

fit3b <- aov(well_being ~ therapy * price * sex + Error(subject/(therapy*price)), data=df2)
summary(fit3b)


# ЗАДАЧА
# В этой задаче вам дан набор данных, в котором представлена 
# информация о температуре нескольких пациентов, которые лечатся 
# разными таблетками и у разных врачей.

# Проведите однофакторный дисперсионный анализ с повторными 
# измерениями: влияние типа таблетки (pill) на 
# температуру (temperature) с учётом испытуемого (patient). 
# Каково p-value для влияния типа таблеток на температуру?
test_df <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")
test_df$patient <- as.factor(test_df$patient)

test_fit <- aov(temperature ~ pill + Error(patient/pill), data=test_df)
summary(test_fit)

# ЗАДАЧА
# Теперь вашей задачей будет провести двухфакторный 
# дисперсионный анализ с повторными измерениями: влияние 
# факторов doctor, влияние фактора pill и их взаимодействие 
# на temperature. Учтите обе внутригрупповые переменные: 
# и тот факт, что один и тот же больной принимает разные 
# таблетки, и тот факт, что  один и тот же больной лечится у 
# разных врачей! Каково F-значение для взаимодействия факторов 
# доктора (doctor) и типа таблеток (pill)?
test_fit <- aov(temperature ~ doctor * pill + Error(patient/(doctor*pill)), data=test_df)
summary(test_fit)

# ЗАДАЧА
# Вспомните графики из лекций и дополните шаблон графика 
# в поле для ответа так (не добавляя еще один geom) , 
# чтобы объединить линиями точки, принадлежащие разным 
# уровням фактора supp. Не забудьте подключить нужный для 
# построение графика пакет.
# Пожалуйста, сохраните график в переменную obj.
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
    stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
    stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
