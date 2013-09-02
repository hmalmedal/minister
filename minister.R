library(survival)
library(lubridate)
regjering <- read.csv("regjering.csv",
                      colClasses=c("character",
                                   "Date",
                                   "Date",
                                   "integer",
                                   "factor"))
regjering[is.na(regjering)] <- today()
regjering$Dager <- as.numeric(regjering$Sluttdato - regjering$Startdato)
regjering.fit  <- survfit(with(regjering, Surv(Dager, Avskjed)) ~ 1)
plot(regjering.fit)
