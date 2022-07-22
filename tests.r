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
  #        "_#/_#/####/_#/_# _## ##", "_#/_#/####/_#/_# _## ##", "_#/_#/####/_#/_# _## ##", "_#/_#/####/_#/_# _## ##", "_#/_#/####/_#/_# _## ##", # __#
  Data = c("5/12/2020/15/40 Ag BM",   "23/12/2020/16/31 Ag MA",  "9/1/2021/10/33 Ag BM",    "17/1/2021/9/18 Ag BM",    "23/1/2021/9/20 Ag MA",    #   5
           "30/1/2021/9/25 Ag MA",    "7/2/2021/15/59 Ag MA",    "13/2/2021/9/48 Ag MA",    "20/2/2021/9/55 Ag MA",    "25/2/2021/18/4 Ag MA",    #  10
           "27/2/2021/9/57 Ag MA",    "6/3/2021/9/44 Ag MA",     "12/3/2021/16/26 Ag MA",   "18/3/2021/4/46 PCR AG",   "20/3/2021/9/48 Ag MA",    #  15
           "21/3/2021/18/8 Ag MA",    "23/3/2021/4/58 PCR AG",   "25/3/2021/5/1 PCR AG",    "28/3/2021/9/20 Ag MA",    "29/3/2021/7/42 PCR AG",   #  20
           "1/4/2021/6/21 PCR AG",    "3/4/2021/9/38 Ag MA",     "5/4/2021/9/03 Ag MA",     "7/4/2021/6/21 PCR AG",    "10/4/2021/6/51 PCR AG",   #  25
           "13/4/2021/4/56 PCR AG",   "16/4/2021/4/53 PCR AG",   "19/4/2021/4/55 PCR AG",   "22/4/2021/4/51 PCR AG",   "24/4/2021/6/13 PCR AG",   #  30
           "26/4/2021/5/12 PCR AG",   "28/4/2021/5/2 PCR AG",    "30/4/2021/4/55 PCR AG",   "3/5/2021/5/3 PCR AG",     "6/5/2021/4/56 PCR AG",    #  35
           "8/5/2021/6/5 PCR AG",     "10/5/2021/4/57 PCR AG",   "12/5/2021/4/51 PCR AG",   "15/5/2021/7/7 PCR AG",    "17/5/2021/4/54 PCR AG",   #  40
           "20/5/2021/4/52 PCR AG",   "22/5/2021/6/15 PCR AG",   "25/5/2021/4/59 PCR AG",   "27/5/2021/5/1 PCR AG",    "29/5/2021/8/22 PCR AG",   #  45
           "31/5/2021/4/57 PCR AG",   "2/6/2021/4/54 PCR AG",    "5/6/2021/6/27 PCR AG",    "7/6/2021/5/10 PCR AG",    "10/6/2021/5/7 PCR AG",    #  50
           "12/6/2021/6/54 PCR AG",   "14/6/2021/4/55 PCR AG",   "17/6/2021/4/49 PCR AG",   "19/6/2021/7/15 PCR AG",   "21/6/2021/5/6 PCR AG",    #  55
           "24/6/2021/4/52 PCR AG",   "26/6/2021/7/14 PCR AG",   "28/6/2021/4/57 PCR AG",   "1/7/2021/5/3 PCR AG",     "3/7/2021/6/45 PCR AG",    #  60
           "5/7/2021/4/54 PCR AG",    "8/7/2021/4/56 PCR AG",    "10/7/2021/7/20 PCR AG",   "12/7/2021/5/4 PCR AG",    "14/7/2021/5/3 PCR AG",    #  65
           "17/7/2021/6/56 PCR AG",   "19/7/2021/5/15 PCR AG",   "22/7/2021/4/55 PCR AG",   "24/7/2021/7/22 PCR AG",   "26/7/2021/5/2 PCR AG",    #  70
           "29/7/2021/4/57 PCR AG",   "31/7/2021/7/25 PCR AG",   "2/8/2021/4/49 PCR AG",    "5/8/2021/4/55 PCR AG",    "7/8/2021/6/40 PCR AG",    #  75
           "9/8/2021/5/7 PCR AG",     "16/8/2021/4/46 PCR AG",   "19/8/2021/5/21 PCR AG",   "23/8/2021/7/18 PCR AG",   "25/8/2021/7/2 PCR AG",    #  80
           "27/8/2021/6/36 PCR AG",   "30/8/2021/7/39 PCR AG",   "1/9/2021/7/8 PCR AG",     "4/9/2021/7/22 PCR AG",    "8/9/2021/10/21 PCR AG",   #  85
           "11/9/2021/7/48 PCR AG",   "18/9/2021/7/0 PCR AG",    "20/9/2021/7/14 PCR AG",   "22/9/2021/6/50 PCR AG",   "24/9/2021/8/35 PCR AG",   #  90
           "5/10/2021/5/10 PCR AG",   "9/10/2021/7/56 PCR AG",   "15/10/2021/10/40 PCR AG", "19/10/2021/11/2 PCR AG",  "25/10/2021/6/43 PCR AG",  #  95
           "30/10/2021/7/39 PCR AG",  "6/11/2021/8/17 PCR AG",   "8/11/2021/11/52 PCR AG",  "10/11/2021/11/16 PCR AG", "13/11/2021/8/36 PCR AG",  # 100
           "16/11/2021/11/7 PCR AG",  "18/11/2021/11/4 PCR AG",  "20/11/2021/8/4 PCR AG",   "22/11/2021/11/16 PCR AG", "25/11/2021/11/9 PCR AG",  # 105
           "29/11/2021/11/10 PCR AG", "1/12/2021/11/13 PCR AG",  "6/12/2021/8/8 PCR AG",    "9/12/2021/11/10 PCR AG",  "13/12/2021/10/46 PCR AG", # 110
           "16/12/2021/11/1 PCR AG",  "18/12/2021/7/40 PCR AG",  "20/12/2021/10/49 PCR AG", "22/12/2021/11/19 PCR AG", "24/12/2021/8/6 PCR AG",   # 115
           "27/12/2021/10/54 PCR AG", "29/12/2021/11/3 PCR AG",  "31/12/2021/8/23 PCR AG",  "3/1/2022/11/0 PCR AG",    "5/1/2022/11/7 PCR AG",    # 120
           "7/1/2022/11/15 PCR AG",   "10/1/2022/9/47 PCR AG",   "12/1/2022/10/5 PCR AG",   "14/1/2022/10/23 PCR AG",  "17/1/2022/10/26 PCR AG",  # 125
           "19/1/2022/10/24 PCR AG",  "21/1/2022/10/19 PCR AG",  "24/1/2022/18/57 PCR RB",  "26/1/2022/10/24 PCR AG",  "28/1/2022/10/25 PCR AG",  # 130
           "29/1/2022/7/16 PCR AG",   "31/1/2022/10/26 PCR AG",  "3/2/2022/9/43 PCR AG",    "5/2/2022/8/24 PCR AG",    "7/2/2022/10/8 PCR AG",    # 135
           "9/2/2022/10/29 PCR AG",   "10/2/2022/8/56 PCR AG",   "12/2/2022/8/7 PCR AG",    "14/2/2022/8/12 PCR AG",   "16/2/2022/10/29 PCR AG",  # 140
           "18/2/2022/10/11 PCR AG",  "21/2/2022/10/14 PCR AG",  "24/2/2022/10/28 PCR AG",  "28/2/2022/10/22 PCR AG",  "3/3/2022/7/25 PCR AG",    # 145
           "7/3/2022/10/36 PCR AG",   "10/3/2022/9/38 PCR AG",   "14/3/2022/10/16 PCR AG",  "17/3/2022/10/32 PCR AG",  "21/3/2022/10/38 PCR AG",  # 150
           "23/3/2022/10/21 PCR AG",  "25/3/2022/10/32 PCR AG",  "28/3/2022/10/26 PCR AG",  "31/3/2022/8/19/ PCR AG",  "4/4/2022/6/49 PCR AG",    # 155
           "11/4/2022/6/53 PCR AG",   "18/4/2022/6/56 PCR AG",   "25/4/2022/9/29 PCR AG",   "26/4/2022/7/32 PCR AG",   "2/5/2022/10/3 PCR AG",    # 160
           "9/5/2022/6/23 PCR AG",    "17/5/2022/10/18 PCR AG",  "23/5/2022/9/20 PCR AG",   "30/5/2022/10/24 PCR AG",  "7/6/2022/6/56 PCR AG",    # 165
           "13/6/2022/10/3 PCR AG",   "20/6/2022/6/59 PCR AG",   "27/6/2022/6/30 PCR AG",   "4/7/2022/7/1 PCR AG",     "8/7/2022/9/20 PCR AG")) %>% 
  separate(Data, into = c("Zeit", "Art", "Anbieter"), sep = " ") %>% 
  rownames_to_column(., var = "Lfnr") %>% # Zeilennummern - spaeter Ableitung der y-Koordinate im Plot (Range-Bars)
  mutate(Lfnr = as.numeric(Lfnr),
         Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"),
         Prot = case_when(Zeit >= 1630454400 ~ 1, # Protokoll für Geltungsdauern (2021/09/01T0000 = 1630454400)
                               TRUE ~ 0),
         Dauer = case_when(Art == "Ag" & Prot == 0 ~ 48,
                           Art == "Ag" & Prot == 1 ~ 24,
                           Art == "PCR" & Prot == 0 ~ 72,
                           Art == "PCR" & Prot == 1 ~ 48),
         Ende = Zeit + lubridate::hours(Dauer),
         Key = strftime(Zeit, "%y%m%d"), # Fuer spaetere Befundzuordnung
         Anbieter = factor(Anbieter, levels = c("BM", "MA", "AG", "RB"),
                           labels = c('Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz, "Österreich testet"',
                                      'MA 15 - Stadt Wien Gesundheitsdienst, Teststraße',
                                      'Lifebrain - "Alles gurgelt!"',
                                      'Spa Therme Blumau, SYNLAB IMCL')))

## Ereignisse ----
event <- tribble(~Zeit, ~Name,
                "10/5/2021/13/49", "Dosis 1\n(Moderna)\n10. Mai",
                "15/6/2021/8/55", "Dosis 2\n(Moderna)\n15. Juni",
                "16/11/2021/8/49", "Dosis 3\n(Moderna)\n16. November",
                "27/6/2022/15/35", "Auffrischung\n(Pfizer-Biontech)\n27. Juni",
                "18/7/2022/6/13", "Infektion\n(Ct = 21.5)\n18. Juli",
                "22/7/2022/6/8", "Freitesten\n(Ct = 34.21, 34.89)\n22. Juli") %>% 
  mutate(Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"))

# Verfuegbare Befunde zuordnen ----
tests <- left_join(tests,
                   tibble(Befund = dir("Befunde/")) %>%
                     mutate(Key = str_sub(Befund, 1, 6),
                            Befund = paste0("Befunde/", Befund)),
                   by = "Key")

# Verschiebe Testanbieter ans Ende ----
tests <- tests %>% 
  relocate(Prot, .after = Zeit) %>%
  relocate(Anbieter, .after = last_col())
    
# Auswahl relevanter Tests fuer Darstellung ----
testungen <- tests %>% 
  filter(Ende > frame[1]) %>% 
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
  geom_vline(data = event, mapping = aes(xintercept = Zeit), linetype = "solid", size = 1, color = "orangered") +
  geom_vline(xintercept = frame[2], linetype = "dotted", size = 1, color = "orangered") +
  geom_label(data = event, mapping = aes(x = Zeit, label = Name), y = .925, size = 2.5, color = "orangered") +
  geom_label(x = frame[2], y = .8, label = strftime(x = frame[2], format = "%e. %B\n%H:%M"), size = 4, color = "orangered") +
  ggfx::with_shadow(
    geom_errorbarh(mapping = aes(y = ((Lfnr - 1) %% 3 + 9) * .05,
                                 xmin = Zeit, xmax = Ende), height = .02, size = 1.1, color = "steelblue"),
    color = "steelblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  ggfx::with_shadow(
    geom_errorbarh(data = zeitraeume, mapping = aes(y = .15, xmin = Von, xmax = Ende), height = .03, size = 1.5, color = "royalblue"),
    color = "royalblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  scale_x_datetime(name = paste0("Zeit [", range[1], "d ... jetzt ... +", range[3], "d]"),
                   date_labels = "%e. %B %y", minor_breaks = NULL,
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
rm(range, frame, testungen, zeitraeume, p)
