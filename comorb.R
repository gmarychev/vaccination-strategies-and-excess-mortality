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

# Люди с хроническими заболеваниями
comorb_vac_db <- read_excel('db_for_clustering_2_comorb.xlsx') # данные со стэком помесячных данных в одну строку
comorb_vac_db <- tibble::column_to_rownames(comorb_vac_db, var = "Country") # столбец "Country" - 
# названия наблюдений

# Рассчёт матрицы расстояний между объектами
comorb_distance <- dist.binary(comorb_vac_db, method = 1) # для подсчёта расстояний используем индекс Жаккарда
comorb_distance[is.na(comorb_distance)] <- 0 # так как почему-то расстояние между Латвие, Польшей и Румынией не посчиталось

# Иерархическая кластеризация; метод полной связи
comorb_clusters <- hclust(comorb_distance, method = 'complete')
plot(comorb_clusters, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы

# Подбор лучшего разрезания дендрограммы на кластеры по силуэту
comorb_groups2 <- cutree(comorb_clusters, k = 2)
comorb_groups3 <- cutree(comorb_clusters, k = 3)
comorb_groups4 <- cutree(comorb_clusters, k = 4)
comorb_groups5 <- cutree(comorb_clusters, k = 5)
comorb_groups6 <- cutree(comorb_clusters, k = 6)
comorb_groups7 <- cutree(comorb_clusters, k = 7)
comorb_groups8 <- cutree(comorb_clusters, k = 8)
comorb_groups9 <- cutree(comorb_clusters, k = 9)
comorb_groups10 <- cutree(comorb_clusters, k = 10)
comorb_groups11 <- cutree(comorb_clusters, k = 11)
comorb_sil_2 <- silhouette(comorb_groups2, dist = comorb_distance)
comorb_sil_3 <- silhouette(comorb_groups3, dist = comorb_distance)
comorb_sil_4 <- silhouette(comorb_groups4, dist = comorb_distance)
comorb_sil_5 <- silhouette(comorb_groups5, dist = comorb_distance)
comorb_sil_6 <- silhouette(comorb_groups6, dist = comorb_distance)
comorb_sil_7 <- silhouette(comorb_groups7, dist = comorb_distance)
comorb_sil_8 <- silhouette(comorb_groups8, dist = comorb_distance)
comorb_sil_9 <- silhouette(comorb_groups9, dist = comorb_distance)
comorb_sil_10 <- silhouette(comorb_groups10, dist = comorb_distance)
comorb_sil_11 <- silhouette(comorb_groups11, dist = comorb_distance)

fviz_silhouette(comorb_sil_2)
fviz_silhouette(comorb_sil_3)
fviz_silhouette(comorb_sil_4)
fviz_silhouette(comorb_sil_5)
fviz_silhouette(comorb_sil_6)
fviz_silhouette(comorb_sil_7)
fviz_silhouette(comorb_sil_8)
fviz_silhouette(comorb_sil_9)
fviz_silhouette(comorb_sil_10)
fviz_silhouette(comorb_sil_11)

# Все разбиения получаются с кластерами-одиночками. Наилучшее - на 10 кластеров, из которых 5 одиночек
# Рассматриваем разбиение на 10 кластеров, оцениваем для этих кластеров стабильность по clusterboot()
comorb_cboot <- clusterboot(comorb_distance, 
                            clustermethod = disthclustCBI, 
                            method = 'complete', 
                            k = 10, 
                            seed = 38)
comorb_groups_cboot <- comorb_cboot$result$partition
comorb_groups_cboot == comorb_groups10 # Проверка, что мы нигде не опрофанились и разбиение на кластеры совпадает
comorb_cboot$bootmean # Вектор стабильности кластеров => 
# 1 кластер стабилен, 2 стабилен, 3 высоко стабилен, 4 стабилен, 
# 5 слабо стабилен, 6 тоже, 7 стабилен, 8 стабилен, 9 не стабилен, 10 слабо стабилен
# одиночки: 5 (Германия), 6 (Исландия), 8 (Нидерланды), 9 (Норвегия) и 10 (Швеция)
comorb_cboot$bootbrd # вектор, показывающий, сколько раз "распался" каждый кластер; 
# выводы аналогичные

# исключим 5 стран как выбросы и рассмотрим оставшиеся 5 кластеров
comorb_vac_db_wo5 <- comorb_vac_db[-c(9,10,16,17,23), ]
# Рассчёт матрицы расстояний между объектами
comorb_distance_wo5 <- dist.binary(comorb_vac_db_wo5, method = 1) # для подсчёта расстояний используем индекс Жаккарда
comorb_distance_wo5[is.na(comorb_distance_wo5)] <- 0 # так как почему-то расстояние между Польшей, Латвией и Румынией не посчиталось
# Иерархическая кластеризация; метод полной связи
comorb_clusters_wo5 <- hclust(comorb_distance_wo5, method = 'complete')
plot(comorb_clusters_wo5, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # зарисовка дендрограммы
rect.hclust(comorb_clusters_wo5, k = 5, border = 2:5)
comorb_cboot_5_wo5 <- clusterboot(comorb_distance_wo5, 
                                  clustermethod = disthclustCBI, 
                                  method = 'complete', 
                                  k = 5, 
                                  seed = 38)
comorb_groups_cboot_5 <- comorb_cboot_5_wo5$result$partition
comorb_cboot_5_wo5$bootmean # все высоко стабильны
# Рассмотрим подробно стратегии вакцинации в этих кластерах
comorb_vac_db_wo5$groups <- factor(comorb_groups_cboot_5)
comorb_vac_db_wo5 %>% filter(groups == 1) %>% View
comorb_vac_db_wo5 %>% filter(groups == 2) %>% View
comorb_vac_db_wo5 %>% filter(groups == 3) %>% View
comorb_vac_db_wo5 %>% filter(groups == 4) %>% View
comorb_vac_db_wo5 %>% filter(groups == 5) %>% View

# Оценим теперь избыточную смертность в каждом кластере
comorb_cluster1_em <- data_em %>% filter(year == 2021 & 
                                           (country_name == 'Austria' & time > 4 & time < 31 | 
                                              country_name == 'Portugal' & time > 4 & time < 31))
comorb_cluster1_exc_deaths <- sum(comorb_cluster1_em$excess.deaths) # число изб.смертей в 1 кластере

comorb_cluster2_em <- data_em %>% filter(year == 2021 & (country_name == 'Belgium' & time > 4 & time < 31 |
                                                           country_name == 'Ireland' & time > 1 & time < 8 |
                                                           country_name == 'Luxembourg' & time > 4 & time < 31))
comorb_cluster2_exc_deaths <- sum(comorb_cluster2_em$excess.deaths) # число изб.смертей во 2 кластере

comorb_cluster3_em <- data_em %>% filter(year == 2021 & (country_name == 'Croatia' & time > 4 & time < 31 |
                                                           country_name == 'Czechia' & time > 4 & time < 31 |
                                                           country_name == 'Finland' & time > 4 & time < 31 |
                                                           country_name == 'Lithuania' & time > 4 & time < 31 |
                                                           country_name == 'Slovakia' & time > 4 & time < 31 |
                                                           country_name == 'Spain' & time > 4 & time < 31
))
comorb_cluster3_exc_deaths <- sum(comorb_cluster3_em$excess.deaths) # число изб.смертей в 3 кластере

comorb_cluster4_em <- data_em %>% filter(year == 2021 & (country_name == 'Denmark' & time > 4 & time < 31 |
                                                           country_name == 'Estonia' & time > 4 & time < 31 |
                                                           country_name == 'France' & time > 4 & time < 31 |
                                                           country_name == 'Malta' & time > 4 & time < 31 ))
comorb_cluster4_exc_deaths <- sum(comorb_cluster4_em$excess.deaths) # число изб.смертей в 4 кластере

comorb_cluster5_em <- data_em %>% filter(year == 2021 & (country_name == 'Poland' & time > 4 & time < 31 |
                                                           country_name == 'Romania' & time > 4 & time < 31 |
                                                           country_name == 'Latvia' & time > 4 & time < 31))
comorb_cluster5_exc_deaths <- sum(comorb_cluster5_em$excess.deaths) # число изб.смертей в 5 кластере

# Численность населения по кластерам (на 1 января 2020 года):
comorb_pops_cluster1 <- pops %>% filter(Country == 'Austria' |
                                          Country == 'Portugal')
comorb_pops_cluster2 <- pops %>% filter(Country == 'Belgium' |
                                          Country == 'Ireland' |
                                          Country == 'Luxembourg')
comorb_pops_cluster3 <- pops %>% filter(Country == 'Croatia' |
                                          Country == 'Czechia' |
                                          Country == 'Finland' |
                                          Country == 'Lithuania' |
                                          Country == 'Slovakia' |
                                          Country == 'Spain')
comorb_pops_cluster4 <- pops %>% filter(Country == 'Denmark' |
                                          Country == 'Estonia' |
                                          Country == 'France' |
                                          Country == 'Malta' )
comorb_pops_cluster5 <- pops %>% filter(Country == 'Poland' |
                                          Country == 'Romania' |
                                          Country == 'Latvia')
comorb_population_cluster1 <- sum(comorb_pops_cluster1$Population)*1000
comorb_population_cluster2 <- sum(comorb_pops_cluster2$Population)*1000
comorb_population_cluster3 <- sum(comorb_pops_cluster3$Population)*1000
comorb_population_cluster4 <- sum(comorb_pops_cluster4$Population)*1000
comorb_population_cluster5 <- sum(comorb_pops_cluster5$Population)*1000

# Избыточная смертность по кластерам
comorb_excess_mortality_cluster1 <- comorb_cluster1_exc_deaths/comorb_population_cluster1 * 100000
comorb_excess_mortality_cluster2 <- comorb_cluster2_exc_deaths/comorb_population_cluster2 * 100000
comorb_excess_mortality_cluster3 <- comorb_cluster3_exc_deaths/comorb_population_cluster3 * 100000
comorb_excess_mortality_cluster4 <- comorb_cluster4_exc_deaths/comorb_population_cluster4 * 100000
comorb_excess_mortality_cluster5 <- comorb_cluster5_exc_deaths/comorb_population_cluster5 * 100000
