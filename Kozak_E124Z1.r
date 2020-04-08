#Козак Екатерина  – для региона 79 Еврейская автономная область
#ЗАДАНИЕ: для региона 79 рассчитайте урожайность пшеницы в 2013 году, взяв для рассчета средние суммы активных температур за текущий год, с 14 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 30 градусов

# проверка рабочей директории
getwd()
setwd("C:/group_124/kozak/MathMod")


#Устанавливаем пакеты
library(tidyverse) ; 
library(rnoaa)

station_data = ghcnd_stations() 
#Может занять несколько минут лучше выполнить один раз в месте с хорошим интернетом и сохранить результат (1 раз)
write.csv(station_data, file = "station_data.csv")
station_data = read.csv("station_data.csv")
# ФОРМИРОВАНИЕ СПИСКА МЕТЕОСТАНЦИЙ
#После получения списка всех станций, выберем из него список станций ближайших к Биробиджана,создав таблицу с именем региона и координатами его столицы
#координаты должны быть в десятых градусов
birobidjan = data.frame(id = "birobidjan", latitude = 48.47011,  longitude = 132.55015)
#прочитаем справку команды meteo_nearby_stations
? meteo_nearby_stations
#можно выбирать метеостанции в некотором фиксированном радиусе от Биробиджана 
#или конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
birobidjan_around = meteo_nearby_stations(lat_lon_df = birobidjan, station_data = station_data,
                                    limit = 14, var = c("PRCP", "TAVG"),
                                    year_min = 2013, year_max = 2013)
#birobidjan_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций, отсортиров-ых по их удаленности от Биробиджана 
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Биробиджана, его то мы и попытаемся получить
birobidjan_id = birobidjan_around[["birobidjan"]][["id"]][1]
summary (birobidjan_id)
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте #след. команду
all_birobidjan_data=meteo_tidy_ghcnd(stationid=birobidjan_id)
#2)чтобы получить таблицу всех метеостанций вокруг Биробиджана нужно выбрать целиком первый объект из списка
birobidjan_table = birobidjan_around[[1]]
summary(birobidjan_table)
#в таблице birobidjan_table оказалось 14 объектов, ранжированных по расстоянию от Биробиджана
#нужно убедится, что этот список включает нужные по условию задачи метеостанции

birobidjan_stations = birobidjan_table 
str(birobidjan_stations)
#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
birobidjan_stations$id

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_birobidjan_meteodata = data.frame()
#Цикл для всех метеостанций
# Цикл для всех метеостанций
for ( i  in  1 : 14 )
{ 
  print ( i )
  print(birobidjan_id)
  # выберем нужные свойства
  all_i  = meteo_tidy_ghcnd ( stationid  = birobidjan_id[i],var="TAVG",
                              data_min="2013-01-01", data_max="2013-12-31")
  all_i  =  all_i [, c ( " id " , " date " , " tavg "  )]
  
  # с помощью команды rbind соединяем данные,
  all_birobidjan_meteodata  = rbind ( all_birobidjan_meteodata , all_i )
}


# ## Записываем полученные результаты ###
write.csv ( all_birobidjan_meteodata , " all_birobidjan_meteodata.csv " )
## Cчитываем данные из файла all_birobidjan_meteodata.csv
all_birobidjan_meteodata = read.csv("all_birobidjan_meteodata.csv")

#посмотрим на данные
str(all_birobidjan_meteodata)

# Добавим год, месяц, день
all_birobidjan_meteodata = mutate(all_birobidjan_meteodata, year = year(date), 
                               month = month(date), day = day(date))
#проверим результат
str(all_birobidjan_meteodata)

#отфильтруем данные за 2013 год
years_birobidjan_meteodata =filter(all_birobidjan_meteodata, year %in% c(2013:2013))
#проверим результат
str(years_birobidjan_meteodata)
summary(years_birobidjan_meteodata)

# Превратим в нули все NA и где  5<tavg>30 

years_birobidjan_meteodata [is.na(years_birobidjan_meteodata$tavg), "tavg"] = 0
years_birobidjan_meteodata [years_birobidjan_meteodata$tavg<5, "tavg"] = 0
years_birobidjan_meteodata [years_birobidjan_meteodata$tavg>30, "tavg"] = 0

#проверяем, что температура получилась или 0, или больше 5 градусов
summary(years_birobidjan_meteodata)

# Расчитаем суммарную температуру за месяц для всех станций 
# группируем по метеостанциям, годам и месяцам
#??group_by
alldays = group_by(years_birobidjan_meteodata,id,year,month)
#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_birobidjan = summarize(alldays, tsum = sum(tavg))
# максимальная суммарная температура за месяц 656,4, следовательно,еслиподелить на 30 будет 21,88,что является приемлемым
summary(sumT_alldays_birobidjan) 

# Сгруппируем данные по месяцам  
groups_birobidjan_months = group_by(sumT_alldays_birobidjan,month)
groups_birobidjan_months
# найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_birobidjan_months, St = mean(tsum))
sumT_months

###### Подготовка к расчету по формуле Урожая
### Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# константы выше взяты из первой таблицы
y1 = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры; 
# Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y1*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield
# Ответ: 18,3 ц/га
#По данным аналитики на  2013 года средняя урожайность в ЕАО составляла 17,1 ц/га
