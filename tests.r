# Thomas' COVID19 Tests

# Laden der Libraries ----
library(tidyverse)
library(lubridate)

# Zeitraum fuer Darstellung (in Tagen) ----
range <- c(-56, 0, 3) # 56 Tage = 8 Wochen
frame <- now() %>%
  floor_date(unit = "10 minute") + c(days(range[1]), days(range[2]), days(range[3]))

# Datenerfassung ----
## Tests ----
tests <- tibble(
  Data = c("5/12/2020/15/40 Ag BM", "23/12/2020/16/31 Ag MA", "9/1/2021/10/33 Ag BM",  "17/1/2021/9/18 Ag BM",  #  1
           "23/1/2021/9/20 Ag MA",  "30/1/2021/9/25 Ag MA",   "7/2/2021/15/59 Ag MA",  "13/2/2021/9/48 Ag MA",  #  5
           "20/2/2021/9/55 Ag MA",  "25/2/2021/18/4 Ag MA",   "27/2/2021/9/57 Ag MA",  "6/3/2021/9/44 Ag MA",   #  9
           "12/3/2021/16/26 Ag MA", "18/3/2021/4/46 PCR LH",  "20/3/2021/9/48 Ag MA",  "21/3/2021/18/8 Ag MA",  # 13
           "23/3/2021/4/58 PCR LH", "25/3/2021/5/1 PCR LH",   "28/3/2021/9/20 Ag MA",  "29/3/2021/7/42 PCR LH", # 17
           "1/4/2021/6/21 PCR LH",  "3/4/2021/9/38 Ag MA",    "5/4/2021/9/03 Ag MA",   "7/4/2021/6/21 PCR LH",  # 21
           "10/4/2021/6/51 PCR LH", "13/4/2021/4/56 PCR LH",  "16/4/2021/4/53 PCR LH", "19/4/2021/4/55 PCR LH", # 25
           "22/4/2021/4/51 PCR LH", "24/4/2021/6/13 PCR LH",  "26/4/2021/5/12 PCR LH", "28/4/2021/5/2 PCR LH",  # 29
           "30/4/2021/4/55 PCR LH", "3/5/2021/5/3 PCR LH",    "6/5/2021/4/56 PCR LH",  "8/5/2021/6/5 PCR LH",   # 33
           "10/5/2021/4/57 PCR LH", "12/5/2021/4/51 PCR LH",  "15/5/2021/7/7 PCR LH",  "17/5/2021/4/54 PCR LH", # 37
           "20/5/2021/4/52 PCR LH", "22/5/2021/6/15 PCR LH",  "25/5/2021/4/59 PCR LH", "27/5/2021/5/1 PCR LH",  # 41
           "29/5/2021/8/22 PCR LH", "31/5/2021/4/57 PCR LH",  "2/6/2021/4/54 PCR LH",  "5/6/2021/6/27 PCR LH",  # 45
           "7/6/2021/5/10 PCR LH",  "10/6/2021/5/7 PCR LH",   "12/6/2021/6/54 PCR LH", "14/6/2021/4/55 PCR LH", # 49
           "17/6/2021/4/49 PCR LH", "19/6/2021/7/15 PCR LH",  "21/6/2021/5/6 PCR LH",  "24/6/2021/4/52 PCR LH", # 54
           "26/6/2021/7/14 PCR LH", "28/6/2021/4/57 PCR LH",  "1/7/2021/5/3 PCR LH",   "3/7/2021/6/45 PCR LH",  # 59
           "5/7/2021/4/54 PCR LH",  "8/7/2021/4/56 PCR LH",   "10/7/2021/7/20 PCR LH", "12/7/2021/5/4 PCR LH",  # 63
           "14/7/2021/5/3 PCR LH",  "17/7/2021/6/56 PCR LH",  "19/7/2021/5/15 PCR LH", "22/7/2021/4/55 PCR LH", # 67
           "24/7/2021/7/22 PCR LH", "26/7/2021/5/2 PCR LH",   "29/7/2021/4/57 PCR LH", "31/7/2021/7/25 PCR LH", # 71
           "2/8/2021/4/49 PCR LH",  "5/8/2021/4/55 PCR LH",   "7/8/2021/6/40 PCR LH",  "9/8/2021/5/7 PCR LH",   # 75
           "16/8/2021/4/46 PCR LH", "19/8/2021/5/21 PCR LH",  "23/8/2021/7/18 PCR LH", "25/8/2021/7/2 PCR LH",  # 79
		   "27/8/2021/6/36 PCR LH", "30/8/2021/7/39 PCR LH",  "1/9/2021/7/8 PCR LH")) %>% 
  separate(Data, into = c("Zeit", "Art", "Anbieter"), sep = " ") %>% 
  rownames_to_column(., var = "Lfnr") %>% # Zeilennummern - spaeter Ableitung der y-Koordinate im Plot (Range-Bars)
  mutate(Lfnr = as.numeric(Lfnr),
         Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"),
         Dauer = case_when(Art == "Ag" ~ 48, # Geltungsdauern
                           Art == "PCR" ~ 72),
         Ende = Zeit + lubridate::hours(Dauer),
         Key = strftime(Zeit, "%y%m%d"), # Fuer spaetere Befundzuordnung
         Anbieter = factor(Anbieter, levels = c("BM", "MA", "LH"),
                           labels = c('Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz, "Österreich testet"',
                                      'MA 15 - Stadt Wien Gesundheitsdienst, Teststraße',
                                      'LEAD Horizon, "Alles gurgelt!"')))

## Impfung ----
impf <- tribble(~Zeit, ~Name,
                "10/5/2021/13/10", "Moderna (1)\n10. Mai",
                "15/6/2021/8/40", "Moderna (2)\n15. Juni") %>% 
  mutate(Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"))

# Verfuegbare Befunde zuordnen ----
tests <- left_join(tests,
                   tibble(Befund = dir("Befunde/")) %>%
                     mutate(Key = str_sub(Befund, 1, 6),
                            Befund = paste0("Befunde/", Befund)),
                   by = "Key")

# Verschiebe Testanbieter ans Ende ----
tests <- tests %>% 
  relocate(Anbieter, .after = last_col())

# Auswahl relevanter Tests fuer Darstellung ----
testungen <- tests %>% 
  filter(Zeit > frame[1]) %>% 
  mutate(Zeit = case_when(frame[1] %within% interval(Zeit, Ende) ~ frame[1], # Abgeschnittene Testzeitraeume bei frame[1] -> Verscheiebe Testzeitpunkt
                          TRUE ~ Zeit))

# Bereinige ueberlappende Zeitraeume fuer Darstellung ----
zeitraeume <- testungen %>%
  mutate(Lfnr = c(0, cumsum(as.numeric(lead(Zeit)) >
                              cummax(as.numeric(Ende)))[-n()])) %>%
  group_by(Lfnr) %>%
  summarise(Von = min(Zeit),
            Ende = max(Ende),
            Anzahl = n())

# Plot vorbereiten ----
ggplot(data = testungen) +
  ggfx::with_blur(
    geom_rect(mapping = aes(xmin = Zeit, xmax = Ende, ymin = 0, ymax = 1, fill = Art), alpha = .75),
    sigma = 2.5) +
  geom_vline(data = impf, mapping = aes(xintercept = Zeit), linetype = "solid", size = 1, color = "orangered") +
  geom_vline(xintercept = frame[2], linetype = "dotted", size = 1, color = "orangered") +
  geom_label(data = impf, mapping = aes(x = Zeit, label = Name), y = .95, size = 2.5, color = "orangered") +
  geom_label(x = frame[2], y = .85, label = strftime(x = frame[2], format = "%e. %B\n%H:%M"), size = 4, color = "orangered") +
  ggfx::with_shadow(
    geom_errorbarh(mapping = aes(y = ((Lfnr - 1) %% 3 + 9) * .05,
                                 xmin = Zeit, xmax = Ende), height = .02, size = 1.1, color = "steelblue"),
    color = "steelblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  ggfx::with_shadow(
    geom_errorbarh(data = zeitraeume, mapping = aes(y = .15, xmin = Von, xmax = Ende), height = .03, size = 1.5, color = "royalblue"),
    color = "royalblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  scale_x_datetime(name = paste0("Zeit [", range[1], "d ... jetzt ... +", range[3], "d]"),
                   date_labels = "%e. %B", minor_breaks = NULL,
                   limits = frame[c(1, 3)], expand = c(.01, .01)) + 
  scale_y_continuous(name = "", breaks = c(0, 1), labels = NULL, minor_breaks = NULL, expand = c(.015, .015)) +
  scale_fill_manual(name = "Testart", values = set_names(x = RColorBrewer::brewer.pal(3, "YlGn")[-1],
                                                         nm = c("Ag", "PCR"))) +
  labs(title = "COVID-19 Tests", subtitle = "Thomas") + 
  theme_minimal(base_size = 13)-> p

# Plot ----
windows(16, 5)
plot(p)

# Aufraeumen ----
rm(range, frame, impf, testungen, zeitraeume, p)
