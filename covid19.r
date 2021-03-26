library(tidyverse)
library(lubridate)

# Erstmaliger Download
if(!file.exists("data.zip"))
  download.file("https://info.gesundheitsministerium.at/data/data.zip", "data.zip")

# Wenn Download aus unterschiedlicher Stunde (vgl stundenweise Updates), dann neuer Download 
if(Sys.time() %>% floor_date(unit = "hours") != file.info("data.zip")$mtime %>% floor_date(unit = "hours"))
  download.file("https://info.gesundheitsministerium.at/data/data.zip", "data.zip")

# Daten entpacken
unzip("data.zip", files = c("Epikurve.csv", "GenesenTimeline.csv", "TodesfaelleTimeline.csv"), overwrite = TRUE)

# Einlesen Fälle
FAELLE <- 
read_csv2("Epikurve.csv",
          skip = 1,
          col_types = cols(col_date(format = "%d.%m.%Y"), col_integer(), col_datetime()),
          col_names = c("Datum", "Fälle", "Stand")) %>% 
  mutate(Fälle = cumsum(Fälle))

# Einlesen Genesene
GENESEN <- 
read_csv2("GenesenTimeline.csv",
          skip = 1,
          col_types = cols(col_date(format = "%d.%m.%Y"), col_integer(), col_character()),
          col_names = c("Datum", "Genesen", "RM")) %>% 
  select(-RM)

# Einlesen Todesfälle
TOD <- 
read_csv2("TodesfaelleTimeline.csv",
          skip = 1,
          col_types = cols(col_date(format = "%d.%m.%Y"), col_integer(), col_character()),
          col_names = c("Datum", "Verstorben", "RM")) %>% 
  select(-RM)

# Datensatz erstellen
Covid19 <- 
  FAELLE %>%
  left_join(GENESEN, by = "Datum") %>% 
  left_join(TOD, by = "Datum") %>% 
  replace_na(list(Genesen = 0, Verstorben = 0)) %>% 
  mutate(Krank = Fälle - Genesen - Verstorben)

# Plot
Covid19 %>% 
  select(Datum, Krank, Genesen, Verstorben) %>% 
  pivot_longer(cols = -Datum, names_to = "Kategorie", values_to = "Anzahl") %>% 
  mutate(Kategorie = factor(Kategorie, levels = c("Verstorben", "Genesen", "Krank"))) %>% 
  ggplot(mapping = aes(x = Datum, y = Anzahl, group = Kategorie)) +
  geom_col(mapping = aes(fill = Kategorie), color = "white", alpha = 1/4) +
  geom_line(mapping = aes(color = Kategorie), size = 2) +
  scale_y_continuous(name = "Anzahl [in 1.000]", labels = function(x) x / 1000) +
  scale_x_date(date_labels = "%d. %b") +
  labs(title = "COVID19 - Österreich",
       subtitle = paste0("zuletzt aktualisiert: ", Covid19$Stand[1])) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") -> p

# Ausgabe
windows(16, 9)
plot(p)

# Aufräumen
rm(FAELLE, GENESEN, TOD, p)
  