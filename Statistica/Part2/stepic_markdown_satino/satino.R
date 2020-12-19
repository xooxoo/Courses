### Import modules
library(dplyr)
library(tidyr)
library(data.table)
library(ggvis)
library(kableExtra)
library(scales)


### Import data frame
rocks <- fread("satino_rocks.csv", header = T, sep = ";", dec = ",",
               stringsAsFactors = T)
rocks <- rocks %>% separate(Link, c("Observer", "Year"), ", ")

levels(rocks$Place) <- c("Волчий овраг", "Егоров овраг", 
                         "Излучина реки Протвы", "Обцарский овраг", 
                         "Пойма реки Протвы", "Рыжковское кладбище", 
                         "Ручей Язвицы")

rocks$Place <- factor(rocks$Place, levels(rocks$Place)[c(3, 5, 6, 7, 1, 2, 4)])

avtohton <- c("габбро", "гранит", "гранит-пегматит", "гранито-гнейс",
              "гранито-диорит", "диабаз", "основная гп", "шокс. кварцит",
              "шокш. кварцит")

rocks$P_location <- as.factor(sapply(rocks$P, function(x) ifelse(is.element(x,avtohton), "AV", "AL")))

rocks$KI <- (rocks$A + rocks$B) / (2 * rocks$C)
rocks$KL <- 1 - rocks$B/rocks$A
rocks$KH <- 1 - rocks$C/rocks$B


### Table 1 - Morphometric characteristics
table_1 <- rocks %>%
  group_by("Разрез" = Place) %>%
  summarize(N = n(),
            AL = sum(P_location == "AL"),
            AV = sum(P_location == "AV"),
            A = mean(A),
            B = mean(B),
            C = mean(C),
            KI = mean(KI),
            KL = mean(KL),
            KH = mean(KH))

kable(data.frame(table_1), align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  add_header_above(c("Таблица 1. Основные морфометрические характеристики обломков" = ncol(table_1))) %>% 
  column_spec(1, italic = T) %>%
  footnote(number = c("N - количество обломков; ", 
                      "AL - количество местных обломков; ", 
                      "AV - количество принесенных обломков"))


### Table 2 - AOV summary
aov_table <- function(dep_var, title_name){
  tmp_table <- as.data.table(unclass(summary(aov(
    as.formula(paste(dep_var, "~", "Place")), rocks))), 
    check.names = FALSE, stringsAsFactors = FALSE)
  
  tmp_table[is.na(tmp_table)] <- "-"
  tmp_table[1, 4] <- round(as.numeric(tmp_table[1, 4]), digits = 2)
  tmp_table[1, 5] <- scientific_format()(as.numeric(tmp_table[1, 5]))
  tmp_table <- cbind(rep(title_name, times = 2), tmp_table)
  
  colnames(tmp_table) <- c("Коэффициент", "Число степеней свободы", 
                           "Сумма квадратов", "Средний квадрат", 
                           "F-значение", "Уровень значимости")
  return(tmp_table)
}

result_aov_table <-  rbind(aov_table("KI", "Изометричности KI"),
                           aov_table("KL", "Удлиненности KL"),
                           aov_table("KH", "Уплощенности KH"))

kable(result_aov_table, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  add_header_above(c("Таблица 2. Результаты дисперсионного анализа для морфометрических коэффициентов" = ncol(result_aov_table))) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "middle")


### Table 3 - TukeyHSD
Tukey_table <- function(dep_var){
  Tukey_tmp <- TukeyHSD(aov( as.formula(paste(dep_var, "~", "Place")), rocks))
  Tukey_table_tmp <- as.data.table(
    Tukey_tmp$Place[, 4][Tukey_tmp$Place[, 4] < 0.05],
    row.names(names(Tukey_tmp$Place[, 4][Tukey_tmp$Place[, 4] < 0.05])))
  Tukey_table_tmp[, 2] <- sapply(Tukey_table_tmp[, 2], 
                                 function(x) scientific_format()(as.numeric(x)))
  return(Tukey_table_tmp)
}

Tukey_KI <- Tukey_table("KI")
Tukey_KL <- Tukey_table("KL")
Tukey_KH <- Tukey_table("KH")

result_Tukey_table <-  rbind(Tukey_KI, Tukey_KL, Tukey_KH)
colnames(result_Tukey_table) <- c('', 'Уровень значимости')

kable(result_Tukey_table, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  add_header_above(c("Таблица 3. Попарные сравнения мест отбора обломков, рассчитанные с учетом поправки Тьюки, с уровнем значимости меньше 0.05" = ncol(result_Tukey_table))) %>%
  group_rows(index = c("Коэффициент изометричности" = nrow(Tukey_KI), 
                       "Kоэффициент удлиненности" = nrow(Tukey_KL), 
                       "Коэффициент уплощенности" = nrow(Tukey_KH)))


### Boxplot graphics
ggvis_boxplot <- function(var_name, axis_name, domain_min, domain_max, item){
  rocks %>% 
    ggvis(~Place, prop("y", as.name(var_name))) %>%
    layer_boxplots() %>%
    add_axis("x", orient = "top", ticks = 0, title = item,
             properties = axis_props(axis = list(stroke = "white"), 
                                     labels = list(fontSize = 0))) %>%
    add_axis("x", title = "Разрез", title_offset = 35) %>%
    scale_numeric("y", domain = c(domain_min, domain_max), 
                  clamp = T, label = axis_name) %>%
    set_options(width = 900, height = 400)
}

ggvis_boxplot("KI", "Коэффициент изометричности", 0, 20, "a)")
ggvis_boxplot("KL", "Коэффициент удлиненности", -1, 1, "b)")
ggvis_boxplot("KH", "Коэффициент уплощенности", -1, 1, "c)")


### T-tests
alluvium <- c("Излучина реки Протвы", "Пойма реки Протвы", "Рыжковское кладбище", "Ручей Язвицы")
proluvium <- c("Волчий овраг", "Егоров овраг", "Обцарский овраг")

t_KI_t <- t.test(rocks$KI[rocks$Place == alluvium], rocks$KI[rocks$Place == proluvium])$stat
t_KL_t <- t.test(rocks$KL[rocks$Place == alluvium], rocks$KL[rocks$Place == proluvium])$stat
t_KH_t <- t.test(rocks$KH[rocks$Place == alluvium], rocks$KH[rocks$Place == proluvium])$stat

t_KI_p <- t.test(rocks$KI[rocks$Place == alluvium], rocks$KI[rocks$Place == proluvium])$p.val
t_KL_p <- t.test(rocks$KL[rocks$Place == alluvium], rocks$KL[rocks$Place == proluvium])$p.val
t_KH_p <- t.test(rocks$KH[rocks$Place == alluvium], rocks$KH[rocks$Place == proluvium])$p.val

t_table <- data.table(c("Коэффициент изометричности", "Коэффициент удлиненности", "Коэффициент уплощенности"), c(t_KI_t, t_KL_t, t_KH_t), c(t_KI_p, t_KL_p, t_KH_p))
colnames(t_table) <- c("Морфометрический коэффициент", "t-значение", "Уровень значимости")

kable(t_table, align = "c") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), 
                full_width = F, position = "center") %>%
  add_header_above(c("Таблица 4. Результаты сравнения аллювиальных и пролювиальных отложений при помощи t-критерия Стьюдента" = ncol(t_table)))