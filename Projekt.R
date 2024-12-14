## Reguły do sortowania danych
# biblioteki

library(readr)
library(dplyr)
library(editrules)
library(VIM)

install.packages("naniar")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("prettydoc")
install.packages("mice")
install.packages("shape")
install.packages("jomo")
install.packages("pan")
install.packages("tidyverse")
install.packages("dlookr")
install.packages("editrules") #reguły
install.packages("VIM")
install.packages("validate")
install.packages("editrules")
install.packages("errorlocate")
library(errorlocate)
library(mice)
library(naniar)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(validate)
library(editrules)

library(readr)
dane <- read_csv("apartments_pl_2024_06.csv")

n_miss(dane)
# jest 37990 NA w danych

n_complete(dane)
# jest 564038  pełnych wartości w danych


dane <- read_csv("apartments_pl_2024_06.csv")

dane <- data.frame(id=1:5,
                   floor=c(25,-5, 30, 40, 156),
                   buildingYear=c(3000,4000,-500,6000,7000)
                   #reguły:
                   reguly <editset(c(
                     "floor">=0,
                     "buildYear"<=120,
                     "buildingMaterial"
                     "condition"
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

