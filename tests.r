# Tracking Thomas' COVID19 tests

library(tidyverse)
library(lubridate)

tests <- tribble(~Zeit, ~Art,
                 "5/12/2020 15.40", "Ag",
                 "23/12/2020 16.31", "Ag", 
                 "9/1/2021 10.33", "Ag",
                 "17/1/2021 9.18", "Ag",
                 "23/1/2021 9.20", "Ag",
                 "30/1/2021 9.25", "Ag",
                 "7/2/2021 15.59", "Ag",
                 "13/2/2021 9.48", "Ag",
                 "20/2/2021 9.55", "Ag",
                 "25/2/2021 18.04", "Ag",
                 "27/2/2021 9.57", "Ag",
                 "6/3/2021 9.44", "Ag",
                 "12/3/2021 16.26", "Ag",
                 "18/3/2021 4.46", "PCR",
                 "20/3/2021 9.48", "Ag",
                 "21/3/2021 18.08", "Ag",
                 "23/3/2021 4.58", "PCR",
                 "25/3/2021 5.01", "PCR",
                 "28/3/2021 9.20", "Ag",
                 "29/3/2021 7.42", "PCR",
                 "1/4/2021 6.21", "PCR",
                 "3/4/2021 9.38", "Ag",
                 "5/4/2021 9.03", "Ag") %>% 
  mutate(Zeit = dmy_hm(Zeit, tz = "Europe/Vienna"),
         Dauer = case_when(Art == "Ag" ~ 48,
                           TRUE ~ 72),
         Ende = Zeit + lubridate::hours(Dauer)) %>%
  rownames_to_column(., var = "Lfnr") %>%
  mutate(Horizont = ((as.numeric(Lfnr) - 1) %% 3 + 9) * .05,
         Key = strftime(Zeit, "%y%m%d"))

# Verfügbare Befunde zuordnen
tests <- left_join(tests,
                   tibble(Befund = dir("Befunde/")) %>%
                     mutate(Key = str_sub(Befund, 1, 6),
                     Befund = paste0("Befunde/", Befund)),
                   by = "Key") 

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
    geom_errorbarh(mapping = aes(y = Horizont, xmin = Zeit, xmax = Ende), height = .02, size = 1.1, color = "steelblue"),
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
