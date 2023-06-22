### FIN2104319

library("FinTS")
library("tseries")
library("tidyverse")
library("dplyr")
library("plyr")
library("readxl")
library("ggplot2")
library("sjPlot")
library("sjmisc")
library("lubridate")
library("broom")
library("faraway")
library("corrplot")
library("DescTools")
library("stats")
library("lmtest")
library("orcutt")

### Deviation Model
deviation <- read_excel("C:\\Users\\USER\\Python Programme\\uni\\fe_assignment\\deviation.xlsx")

deviation
m <- lm(ln_de ~ ., data = deviation)
summary(m)


### Equality Test
equality_increase <- read_excel("C:\\Users\\USER\\Python Programme\\uni\\fe_assignment\\price_increase_decrease_period.xlsx", sheet = 1)
equality_decrease <- read_excel("C:\\Users\\USER\\Python Programme\\uni\\fe_assignment\\price_increase_decrease_period.xlsx", sheet = 2)


equality_increase$Dates <- NULL
result_increase <- t.test(equality_increase$Deviation, equality_increase$`Roll's Spread`,
    var.equal = TRUE
)

equality_decrease$Dates <- NULL
result_decrease <- t.test(equality_decrease$Deviation, equality_decrease$`Roll's Spread`,
    var.equal = TRUE
)
result_increase
result_decrease
