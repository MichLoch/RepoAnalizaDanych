## Reguły do sortowania danych
# biblioteki

library(readr)
library(dplyr)
library(editrules)
library(VIM)

dane <- read_csv("apartments_pl_2024_06.csv")

dane <- data.frame(id=1:5,
                   wiek=c(25,-5, 30, 40, 156),
                   dochod=c(3000,4000,-500,6000,7000)
                   #reguły:
                   reguly <editset(c(
                     "floor">=0,
                     "buildYear"<=120,
                     "dochod">=0
                   ))
                   #walidacja
                   summary(violatedEdits(reguly,dane))
                   bledy <- violatedEdits(reguly,dane)
View(dane)
bledy <- violatedEdits(reguly.dane)

# zastosowanie reguły do walidacji
summary(violatedEdits(reguly,dane))

# podsumuj na wykresie
bledy <- violatedEdits(reguly.dane)

dane <- read_csv("apartments_pl_2024_06.csv")
# poprawiam bledy w danych
dane[localizeErrors(reguly,dane)$adapt] <- NA
# coś zostało?
any(violatedEdits(reguly,dane),na.rm=TRUE)
# nic

# imputacja braków danych
library(VIM)
czyste_dane <- hotdeck(dane)

