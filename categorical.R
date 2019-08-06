# Считываем данные
df <- read.csv("https://stepic.org/media/attachments/lesson/11502/grants.csv")
str(df)

# Переделаем поле status в фактор
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

# Построим таблицу по данным
t1 <- table(df$status)
t1

# Построим таблицу по двум переменным
t2 <- table(status = df$status, field = df$field)
t2

# Процентное содержание таблиц
prop.table(t1)
prop.table(t2)

# Процентное содержание по строке
prop.table(t2, 1)

# Процентное содержание по столбцу
prop.table(t2, 2)

# Трехмерные таблицы
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3
