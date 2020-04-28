# Козак Е.Л., 124 гр. 2 задание
## дневные потоки паров воды за летний период 2013 года по данным измерений методом турбулентной пульсации
# проверка рабочей директории
getwd()
setwd("C:/group_124/kozak/MathMod/MathMod")

library("tidyverse")# Всё обо всем
library("readr")# функция read_csv()
library("stringr")# функция str_replace_all
library("dplyr")# функции: filter(),arrange(),select(),mutate(),summarize(),group_by(),sample_n()
library("ggplot2")# графики функций qplot()

#считываем файл
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))
#готовим данные
# Удаляем ненужную пустую первую строку
eddypro = eddypro [-1,]
# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro,-(roll))
# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character,factor)
#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
#Возвратим столбцы таблицы в виде векторов для проверки,посмотрим
glimpse(eddypro)
#Удалим строки в которых содержится NA
eddypro = drop_na(eddypro)
# Отфильтруем по заданию данные только за летний период. 
eddypro = filter(eddypro,DOY >= 151 & DOY < 242)
# Отфильтруем данные по заданию только за дневное время
eddypro = filter(eddypro, daytime ==TRUE)
# Получим таблицу, состоящую только из чисел для работы с ней
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]
# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]
# Создадим модель добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod = lm(h2o_flux~ (.) , data = teaching_tbl)
