# Загрузка необходимых пакетов
install.packages('readxl')
install.packages('ade4')
install.packages('dplyr')
install.packages('factoextra')
install.packages('cluster')
install.packages('fpc')

# Распаковка необходимых пакетов
library(readxl)
library(ade4)
library(dplyr)
library(factoextra)
library(cluster)
library(fpc)

# Работники домов престарелых
ltcfpers_vac_db <- read_excel('db_for_clustering_2_ltcfpers.xlsx') # данные со стэком помесячных данных в одну строку
ltcfpers_vac_db <- tibble::column_to_rownames(ltcfpers_vac_db, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
ltcfpers_distance <- dist.binary(ltcfpers_vac_db, method = 1) # для подсчёта расстояний используем индекс Жаккарда
ltcfpers_distance[is.na(ltcfpers_distance)] <- 0 # так как почему-то расстояние между Эстонией и Исландие не посчиталось

# Иерархическая кластеризация; метод полной связи
ltcfpers_clusters <- hclust(ltcfpers_distance, method = 'complete')
plot(ltcfpers_clusters, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
ltcfpers_groups2 <- cutree(ltcfpers_clusters, k = 2)
ltcfpers_groups3 <- cutree(ltcfpers_clusters, k = 3)
ltcfpers_groups4 <- cutree(ltcfpers_clusters, k = 4)
ltcfpers_groups5 <- cutree(ltcfpers_clusters, k = 5)
ltcfpers_groups6 <- cutree(ltcfpers_clusters, k = 6)
ltcfpers_groups7 <- cutree(ltcfpers_clusters, k = 7)
ltcfpers_groups8 <- cutree(ltcfpers_clusters, k = 8)
ltcfpers_groups9 <- cutree(ltcfpers_clusters, k = 9)
ltcfpers_groups10 <- cutree(ltcfpers_clusters, k = 10)
ltcfpers_sil_2 <- silhouette(ltcfpers_groups2, dist = ltcfpers_distance)
ltcfpers_sil_3 <- silhouette(ltcfpers_groups3, dist = ltcfpers_distance)
ltcfpers_sil_4 <- silhouette(ltcfpers_groups4, dist = ltcfpers_distance)
ltcfpers_sil_5 <- silhouette(ltcfpers_groups5, dist = ltcfpers_distance)
ltcfpers_sil_6 <- silhouette(ltcfpers_groups6, dist = ltcfpers_distance)
ltcfpers_sil_7 <- silhouette(ltcfpers_groups7, dist = ltcfpers_distance)
ltcfpers_sil_8 <- silhouette(ltcfpers_groups8, dist = ltcfpers_distance)
ltcfpers_sil_9 <- silhouette(ltcfpers_groups9, dist = ltcfpers_distance)
ltcfpers_sil_10 <- silhouette(ltcfpers_groups10, dist = ltcfpers_distance)

fviz_silhouette(ltcfpers_sil_2)
fviz_silhouette(ltcfpers_sil_3)
fviz_silhouette(ltcfpers_sil_4)
fviz_silhouette(ltcfpers_sil_5)
fviz_silhouette(ltcfpers_sil_6)
fviz_silhouette(ltcfpers_sil_7)
fviz_silhouette(ltcfpers_sil_8)
fviz_silhouette(ltcfpers_sil_9)
fviz_silhouette(ltcfpers_sil_10)

# Лучшее разбиение без кластеров-одиночек - на 3 группы (силуэт = 0,45), 
# с кластерами-одиночками - на 7 групп, 2 из которых одиночки (силуэт = 0,9)
# Рассматриваем разбиение на 3 кластера, оцениваем для этих кластеров стабильность по clusterboot()
ltcfpers_cboot <- clusterboot(ltcfpers_distance, 
                              clustermethod = disthclustCBI, 
                              method = 'complete', 
                              k = 3, 
                              seed = 38)
ltcfpers_groups_cboot <- ltcfpers_cboot$result$partition
ltcfpers_groups_cboot == ltcfpers_groups3 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
ltcfpers_cboot$bootmean # Вектор стабильности кластеров => 
# 1 кластер стабилен, 2 высоко стабилен, а 3 нестабилен
ltcfpers_cboot$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичные
# Это разбиение нельзя использовать, так как третий кластер не стабилен, поэтому рассмотрим разбиение на 7 кластеров
ltcfpers_cboot_7 <- clusterboot(ltcfpers_distance, 
                                clustermethod = disthclustCBI, 
                                method = 'complete', 
                                k = 7, 
                                seed = 38)
ltcfpers_groups_cboot_7 <- ltcfpers_cboot_7$result$partition
ltcfpers_groups_cboot_7 == ltcfpers_groups7 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
ltcfpers_cboot_7$bootmean # Вектор стабильности кластеров => 
# 1 кластер стабилен, 2 высоко стабилен, 3 стабилен, 4 высоко стабилен,
# 5 слабо стабилен, но выделяются паттерны, 6 аналогично, 7 стабилен
ltcfpers_cboot_7$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичны
# разбиение на 7 групп сильно лучше разбиения на 3, но 5 и 6 кластеры слабо стабильны
# 5 кластер состоит из одной Германии, 6 - из одной Ирландии
# исключим их как выбросы и рассмотрим 5 кластеров
ltcfpers_vac_db_woGerIre <- ltcfpers_vac_db[-c(8,10), ]
# Рассчёт матрицы расстояний между объектами
ltcfpers_distance_woGerIre <- dist.binary(ltcfpers_vac_db_woGerIre, method = 1) # для подсчёта расстояний используем индекс Жаккарда
ltcfpers_distance_woGerIre[is.na(ltcfpers_distance_woGerIre)] <- 0 # так как почему-то расстояние между Эстонией и Исландие не посчиталось
# Иерархическая кластеризация; метод полной связи
ltcfpers_clusters_woGerIre <- hclust(ltcfpers_distance_woGerIre, method = 'complete')
plot(ltcfpers_clusters_woGerIre, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
rect.hclust(ltcfpers_clusters_woGerIre, k = 5, border = 2:5)
ltcfpers_cboot_5_woGerIre <- clusterboot(ltcfpers_distance_woGerIre, 
                                         clustermethod = disthclustCBI, 
                                         method = 'complete', 
                                         k = 5, 
                                         seed = 38)
ltcfpers_groups_cboot_5 <- ltcfpers_cboot_5_woGerIre$result$partition
ltcfpers_cboot_5_woGerIre$bootmean
# Рассмотрим подробно стратегии вакцинации в этих кластерах
ltcfpers_vac_db_woGerIre$groups <- factor(ltcfpers_groups_cboot_5)
ltcfpers_vac_db_woGerIre %>% filter(groups == 1) %>% View
ltcfpers_vac_db_woGerIre %>% filter(groups == 2) %>% View
ltcfpers_vac_db_woGerIre %>% filter(groups == 3) %>% View
ltcfpers_vac_db_woGerIre %>% filter(groups == 4) %>% View
ltcfpers_vac_db_woGerIre %>% filter(groups == 5) %>% View

ltcfeld_vac_db_woEst %>% group_by(groups) %>% 
  summarise_all(.funs = funs(mean)) %>% 
  View

# Оценим теперь избыточную смертность в каждом кластере
ltcfpers_cluster1_em <- data_em %>% filter(year == 2021 & 
                                             (country_name == 'Austria' & time > 4 & time < 31 | 
                                                country_name == 'Sweden' & time > 4 & time < 31))
ltcfpers_cluster1_exc_deaths <- sum(ltcfpers_cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

ltcfpers_cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                             country_name == 'Croatia' & time > 4 & time < 31 |
                                                             country_name == 'Czechia' & time > 4 & time < 31 |
                                                             country_name == 'Luxembourg' & time > 4 & time < 31 |
                                                             country_name == 'Malta' & time > 4 & time < 31 ))
ltcfpers_cluster2_exc_deaths <- sum(ltcfpers_cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

ltcfpers_cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Estonia' & time > 4 & time < 31 |
                                                             country_name == 'Iceland' & time > 4 & time < 31
))
ltcfpers_cluster3_exc_deaths <- sum(ltcfpers_cluster3_em$excess.deaths) # число изб.смертей в 3 кластере

ltcfpers_cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'Finland' & time > 4 & time < 31 |
                                                             country_name == 'France' & time > 4 & time < 31 |
                                                             country_name == 'Lithuania' & time > 4 & time < 31 |
                                                             country_name == 'Netherlands' & time > 4 & time < 31 |
                                                             country_name == 'Portugal' & time > 4 & time < 31 |
                                                             country_name == 'Romania' & time > 4 & time < 31 |
                                                             country_name == 'Spain' & time > 4 & time < 31 ))
ltcfpers_cluster4_exc_deaths <- sum(ltcfpers_cluster4_em$excess.deaths) # число изб.смертей в 4 кластере

ltcfpers_cluster5_em <- data_em %>% filter(year == 2021 & (country_name == 'Poland' & time > 4 & time < 31 |
                                                             country_name == 'Latvia' & time > 4 & time < 31))
ltcfpers_cluster5_exc_deaths <- sum(ltcfpers_cluster5_em$excess.deaths) # число изб.смертей в 5 кластере

# Численность населения по кластерам (на 1 января 2020 года):
ltcfpers_pops_cluster1 <- pops %>% filter(Country == 'Austria' |
                                            Country == 'Sweden')
ltcfpers_pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                            Country == 'Croatia' |
                                            Country == 'Czechia' |
                                            Country == 'Luxembourg' |
                                            Country == 'Malta')
ltcfpers_pops_cluster3 <- pops %>% filter(Country == 'Estonia'|
                                            Country == 'Iceland')
ltcfpers_pops_cluster4 <- pops %>% filter(Country == 'Finland'|
                                            Country == 'France'|
                                            Country == 'Lithuania'|
                                            Country == 'Netherlands' |
                                            Country == 'Portugal' |
                                            Country == 'Romania' |
                                            Country == 'Spain' )
ltcfpers_pops_cluster5 <- pops %>% filter(Country == 'Poland' |
                                            Country == 'Latvia')
ltcfpers_population_cluster1 <- sum(ltcfpers_pops_cluster1$Population)*1000
ltcfpers_population_cluster2 <- sum(ltcfpers_pops_cluster2$Population)*1000
ltcfpers_population_cluster3 <- sum(ltcfpers_pops_cluster3$Population)*1000
ltcfpers_population_cluster4 <- sum(ltcfpers_pops_cluster4$Population)*1000
ltcfpers_population_cluster5 <- sum(ltcfpers_pops_cluster5$Population)*1000

# Избыточная смертность по кластерам
ltcfpers_excess_mortality_cluster1 <- ltcfpers_cluster1_exc_deaths/ltcfpers_population_cluster1 * 100000
ltcfpers_excess_mortality_cluster2 <- ltcfpers_cluster2_exc_deaths/ltcfpers_population_cluster2 * 100000
ltcfpers_excess_mortality_cluster3 <- ltcfpers_cluster3_exc_deaths/ltcfpers_population_cluster3 * 100000
ltcfpers_excess_mortality_cluster4 <- ltcfpers_cluster4_exc_deaths/ltcfpers_population_cluster4 * 100000
ltcfpers_excess_mortality_cluster5 <- ltcfpers_cluster5_exc_deaths/ltcfpers_population_cluster5 * 100000
