library(survival)
library(lubridate)
regjering <- read.csv("regjering.csv",
                      colClasses=c("character",
                                   "Date",
                                   "Date",
                                   "integer",
                                   "factor"))
regjering[is.na(regjering)] <- today()
regjering$År <- decimal_date(regjering$Sluttdato) - 
    decimal_date(regjering$Startdato)
regjering.fit  <- survfit(with(regjering, Surv(År, Avskjed)) ~ 1)
plot(regjering.fit)
