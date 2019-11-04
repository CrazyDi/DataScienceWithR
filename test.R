library(dplyr)
library(tidyr)
library(ggplot2)

glacier <- read.csv("data/glacier.csv", na.strings = "..", comment.char = "#")
names(glacier)[1] <- "REF_DATE"
glacier <- glacier %>% 
    select(REF_DATE, GEO, Type.of.measure, VALUE) %>% 
    filter(Type.of.measure == "Annual mass balance") %>% 
    separate(GEO, c("Name", "Location"), sep = ' - ')

# descriptive analysis
g1 <- glacier %>% 
    group_by(Name) %>% 
    summarise(YearsObserved = n(),
              MeanChange = mean(VALUE, na.rm = T),
              WorstChange = min(VALUE, na.rm = T),
              WorstYear = REF_DATE[which.min(VALUE)])

# t-test
g2 <- glacier %>% 
    group_by(Name) %>% 
    do({
        tt <- t.test(.$VALUE, alternative = 'less', mu = 0, conf.level = 0.99)
        data.frame(PValue = tt$p.value, ConfidenceLimit = tt$conf.int[2])
    })

left_join(g1, g2, by = "Name") %>% 
    knitr::kable(caption = "Descriptive statistics and confidence intervals", digits = c(0, 0, 2, 0, 0, 10, 2))

#ggplot
ggplot(glacier, aes(REF_DATE, VALUE)) +
    geom_line() +
    geom_hline(data = g1, aes(yintercept = MeanChange),
               color = "red", linetype = "dashed", alpha = 0.8) +
    facet_wrap(~Name, nrow = 2)
