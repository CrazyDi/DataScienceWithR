my_calc <- function(x, y, z = 0){
    s <- x + y + z
    d <- x - y - z
    return(c(s, d))
}

my_calc(3, 4)

distr1 <- rnorm(1000)
hist(distr1)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1, na.rm = T)

d1 <- rnorm(2000)
d2 <- runif(2000)
d1[1:10] <- NA
d2[1:10] <- NA

source("my_na_rm.R")

d1 <- my_na_rm(d1)
d2 <- my_na_rm(d2)

NA.position <- function(x) {
    return(which(is.na(my_vector)))
}

my_vector <- c(1, 2, 3, NA, NA)
my_vector[is.na(my_vector)]
length(which(is.na(my_vector)))

NA.counter <- function(x){
    # put your code here  
    return(length(which(is.na(x))))
}

dir("Grants data", pattern = "*.csv")

grants <- data.frame()

for (i in dir("Grants data", pattern = "*.csv")) {
    temp_df <- read.csv(paste("Grants data/", i, sep = ""))
    grants <- rbind(temp_df, grants)
}

read_data <- function(){
    df <- data.frame()
    
    for (i in dir("Grants data", pattern = "*.csv")) {
        temp_df <- read.csv(paste("Grants data/", i, sep = ""))
        df <- rbind(temp_df, df)
    }
    return(df)
}

grants2 <- read_data()

filtered.sum <- function(x){
    # put your code here  
    
}

v <- c(1, -2, 3, NA, NA)

sum(v[v>0], na.rm = T)


# ЗАДАЧА
# Напишите функцию outliers.rm, которая находит и удаляет 
# выбросы. Для обнаружения выбросов воспользуемся самым 
# простым способом, с которым вы не раз встречались, 
# используя график Box plot. 

# Выбросами будем считать те наблюдения, которые 
# отклоняются от 1 или 3 квартиля больше чем на 
# 1,5 *  IQR, где  IQR  - межквартильный размах.

# На вход функция получает числовой вектор x. 
# Функция должна возвращать модифицированный вектор x 
# с удаленными выбросами. 
v <- rnorm(100)
boxplot(v)

q <- quantile(v, probs = c(0.25, 0.75))
q[1]
q[2]
iqr <- IQR(v)
v[(v > q[1] - iqr * 1.5) & (v < q[2] + iqr * 1.5)]
