# Tracking Thomas' COVID19 test

library(tidyverse)
library(lubridate)

tests <- tribble(~Zeit, ~Art, ~Befund,
                 "5/12/2020 15.40", "Ag", NA_character_,
                 "23/12/2020 16.31", "Ag", NA_character_, 
                 "9/1/2021 10.33", "Ag", NA_character_,
                 "17/1/2021 9.18", "Ag", NA_character_,
                 "23/1/2021 9.20", "Ag", NA_character_,
                 "30/1/2021 9.25", "Ag", NA_character_,
                 "7/2/2021 15.59", "Ag", NA_character_,
                 "13/2/2021 9.48", "Ag", NA_character_,
                 "20/2/2021 9.55", "Ag", NA_character_,
                 "25/2/2021 18.04", "Ag", NA_character_,
                 "27/2/2021 9.57", "Ag", NA_character_,
                 "6/3/2021 9.44", "Ag", NA_character_,
                 "12/3/2021 16.26", "Ag", NA_character_,
                 "18/3/2021 4.46", "PCR", "Befunde/0318_L21031800195.pdf",
                 "20/3/2021 9.48", "Ag", "Befunde/0320_U07.1-2117023-2021_AGLabCluster_800010648264_1_1000000913223840693451__.pdf",
                 "21/3/2021 18.08", "Ag", "Befunde/0321_U07.1-2117023-2021_AGLabCluster_800010702190_1_1000000931823120263007__.pdf",
                 "23/3/2021 4.58", "PCR", "Befunde/0323_L21032300178.pdf",
                 "25/3/2021 5.01", "PCR", "Befunde/0325_L21032500256.pdf",
                 "28/3/2021 9.20", "Ag", "Befunde/0328_U07.1-2117023-2021_AGLabCluster_800010817875_1_1000001052716230980622__.pdf",
                 "29/3/2021 7.42", "PCR", "Befunde/0329_da08e473-a55b-4d5e-be10-8b20bef6c653.pdf",
                 "1/4/2021 6.21", "PCR", "Befunde/0401_d2674b08-5c33-4313-8737-78d2d4e43945.pdf",
                 "3/4/2021 9.38", "Ag", "Befunde/0403_U07.1-2517090-2021_AGLabCluster_800010921161_1_1000001191178180040093__.pdf",
                 "5/4/2021 9.03", "Ag", "Befunde/0405_U07.1-2517090-2021_AGLabCluster_800011000503_1_1000001217569280666552__.pdf") %>%
  mutate(Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"),
         Dauer = case_when(Art == "Ag" ~ 48,
                           TRUE ~ 72),
         Ende = Zeit + lubridate::hours(Dauer)) %>%
  rownames_to_column(., var = "Lfnr") %>%
  mutate(He = ((as.numeric(Lfnr) - 1) %% 3 + 9) * .05) %>% 
  relocate(Befund, .after = Ende)

# Zeitraum fuer Darstellung (in Tagen)
range <- c(-50, 0, 5)
frame <- now() %>%
  floor_date(unit = "15 minute") + c(days(range[1]), days(range[2]), days(range[3]))

# Auswahl relevanter Tests fuer Darstellung
testungen <- tests %>% 
  filter(Zeit > frame[1])

# Bereinige ueberlappende Zeitraeume fuer Darstellung
zeitraeume <- testungen %>%
  mutate(Lfnr = c(0, cumsum(as.numeric(lead(Zeit)) >
                              cummax(as.numeric(Ende)))[-n()])) %>%
  group_by(Lfnr) %>%
  summarise(Von = min(Zeit),
            Ende = max(Ende),
            Anzahl = n())

ggplot(data = testungen) +
  ggfx::with_blur(
    geom_rect(mapping = aes(xmin = Zeit, xmax = Ende, ymin = 0, ymax = 1, fill = Art), alpha = .75),
    sigma = 2.5) +
  geom_vline(xintercept = frame[2], linetype = "dotted", size = 1, color = "orangered") +
  ggfx::with_shadow(
    geom_errorbarh(mapping = aes(y = He, xmin = Zeit, xmax = Ende), height = .02, size = 1.1, color = "steelblue"),
    color = "steelblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  ggfx::with_shadow(
    geom_errorbarh(data = zeitraeume, mapping = aes(y = .15, xmin = Von, xmax = Ende), height = .03, size = 1.5, color = "royalblue"),
    color = "royalblue", x_offset = 0, y_offset = 4, sigma = 2.5) +
  geom_label(x = frame[2], y = .85, label = strftime(x = frame[2], format = "%e. %B\n%H:%M"),
                        size = 4, color = "orangered") +
  scale_x_datetime(name = paste0("Zeit [", range[1], "d ... jetzt ... +", range[3], "d]"),
                   date_labels = "%e. %B", minor_breaks = NULL,
                   limits = frame[c(1, 3)], expand = c(.01, .01)) + 
  scale_y_continuous(name = "", breaks = c(0, 1), labels = NULL, minor_breaks = NULL, expand = c(.015, .015)) +
  scale_fill_manual(name = "Testart", values = set_names(x = RColorBrewer::brewer.pal(3, "YlGn")[-1],
                                                         nm = c("Ag", "PCR"))) +
  labs(title = "COVID-19 Tests", subtitle = "Thomas") + 
  theme_minimal(base_size = 13)-> p

# Plot
windows(16, 5)
plot(p)

# Aufraeumen
rm(range, frame, testungen, zeitraeume, p)
