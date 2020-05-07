read.csv(file.choose())-> data


data$REAC.R
t.test(data$REAC.R,data$REAC.U)$conf


t.test(data$REAC.R,data$REAC.U, paired = FALSE)$conf

library(dplyr)

data %>% mutate(difference = REAC.R-REAC.U) -> data2

t.test(data2$difference)$conf

t.test(data$REAC.R,data$REAC.U, paired =TRUE)$conf

