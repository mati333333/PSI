# ==============================================================================
# ZAJĘCIA 2: DATA SCIENCE WORKFLOW
# ==============================================================================

# --- 1. KATALOG ROBOCZY ---
# R musi wiedzieć, gdzie szukać plików.
getwd() # Sprawdź bieżący katalog roboczy

# Ustaw swój katalog roboczy (odkomentuj i dostosuj ścieżkę do swojego folderu):
# setwd("C:/Sciezka/Do/Twojego/Folderu/Zadanie_Data_science_workflow")


# --- 2. IMPORT DANYCH ---
# Prawidłowy import plików CSV z folderu:
kraje_1 <- read.table("D:/Downloads/kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 <- read.table("D:/Downloads/kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# Ładowanie niezbędnych pakietów (instalacja zakomentowana)
# install.packages("readxl") 
library(readxl)


# --- 3. PRZYGOTOWANIE DANYCH ---

# Podgląd danych
head(kraje_1)      # pierwsze 6 wierszy
head(kraje_2)
head(kraje_1, 10)  # pierwsze 10 wierszy
head(kraje_2, 10)
tail(kraje_1, 5)   # ostatnie 5 wierszy
tail(kraje_2, 5)

# Podstawowe statystyki 
summary(kraje_1)
summary(kraje_2)

mean(kraje_1$Przyrost_populacji)
median(kraje_1$Przyrost_populacji)
min(kraje_1$Przyrost_populacji)
max(kraje_1$Przyrost_populacji)

# Porządkowanie nazw kolumn
kraje_1$X <- NULL # Usuwanie zbędnej kolumny
kraje_2$X <- NULL
colnames(kraje_2) <- c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")

# Porządkowanie typów danych
is.numeric(kraje_2$Region)   # Odp. Nie
is.character(kraje_2$Region) # Odp. Tak
kraje_2$Region <- as.factor(kraje_2$Region) # Nadanie typu factor
summary(kraje_2)
levels(kraje_2$Region)

# Porządkowanie braków danych
colSums(is.na(kraje_1)) # Brak braków danych
colSums(is.na(kraje_2)) # 4 braki w Internet_proc.
sum(is.na(kraje_2$Internet_proc.)) 
kraje_2[is.na(kraje_2$Internet_proc.), ] # Podgląd wierszy z brakami (zostawiamy je)

# Czyszczenie danych (zamiana znaku & na and)
levels(kraje_2$Region)
kraje_2$Region <- gsub("&", "and", kraje_2$Region)
kraje_2$Region <- as.factor(kraje_2$Region) # Ponowne ustawienie typu factor
levels(kraje_2$Region)


# --- 4. ŁĄCZENIE (SCALANIE) RAMEK DANYCH ---
kraje <- merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")
kraje$Nazwa <- NULL # Usuwanie zbędnej kolumny po połączeniu

summary(kraje)
str(kraje)


# --- 5. PODSTAWOWA ANALIZA DANYCH (dplyr) ---
# install.packages("dplyr")
library(dplyr)

# mutate() – tworzenie nowych zmiennych
kraje <- kraje %>%
  mutate(Populacja_mln = Populacja / 1e6,
         PKB_per_capita = PKB / Populacja)

# filter() i select() 
kraje %>% filter(Urbanizacja_proc. > 50) # Kraje o urbanizacji > 50%

kraje %>% select(Panstwo, Region, PKB, Populacja_mln) # Wybór konkretnych kolumn

# arrange() – sortowanie
kraje %>% arrange(Przyrost_populacji) # Rosnąco
kraje %>% arrange(desc(Przyrost_populacji)) # Malejąco

# Filtrowanie, sortowanie i wybór w jednej sekwencji (PKB > 1 bilion)
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)

# Kraje z Afryki Subsaharyjskiej
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))

# group_by() i summarise()
# Kraje bogatsze niż średnia ich regionu
bogate <- kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))

# Wartości skrajne
kraje %>% summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))
kraje %>% summarise(
  min_populacja = min(Populacja_mln, na.rm = TRUE),
  max_populacja = max(Populacja_mln, na.rm = TRUE)
)

# Średnia populacja i liczba wszystkich krajów
kraje %>% summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))
kraje %>% summarise(liczba_krajow = n())

# Statystyki w podziale na regiony
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())

kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# --- 6. WIZUALIZACJA (ggplot2) ---
# install.packages("ggplot2")
library(ggplot2)

# Wykres 1: Prosty punktowy (Urbanizacja a PKB per capita)
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(title = "Urbanizacja a PKB per capita", x = "Urbanizacja (%)", y = "PKB per capita")

# Wykres 2: Zaawansowany punktowy
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje są bogatsze?",
    x = "Urbanizacja (% ludności miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region świata"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "bottom")

# Wykres 3: Zaawansowany punktowy (rozmiar gospodarki a populacja)
ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()

# Wykres 4: Prosty słupkowy (liczba krajów w regionach)
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Liczba krajów w regionach świata", x = "Region", y = "Liczba krajów") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# Wykres 5: Zaawansowany słupkowy poziomy (TOP 15 najbogatszych krajów)
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów świata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), axis.text.y = element_text(size = 10))

# Wykres 6: Wykres pudełkowy (dostęp do internetu według regionów)
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostęp do internetu według regionów świata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostęp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "none")

# Wykres 7: Wykres pudełkowy (przyrost populacji według regionów)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach świata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# --- 7. EKSPORT DANYCH ---
# Eksport do CSV
write.csv(kraje, "kraje_analiza.csv", row.names = FALSE)

# Eksport do Excela
# install.packages("writexl")
library(writexl)
write_xlsx(kraje, "kraje_wynik.xlsx")

# Pamiętaj, aby zapisać 7 wygenerowanych wykresów ręcznie 
# z poziomu okna RStudio (zakładka Plots -> Export -> Save as image).
