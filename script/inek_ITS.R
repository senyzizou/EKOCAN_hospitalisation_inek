# ==============================================================================
# ==============================================================================
# ==============================================================================

# PROJECT TITLE:  EKOCAN
# CODE AUTHOR:    SENADIN
# DATE STARTED:   260119

# ==============================================================================
# ==============================================================================
# ==============================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

packages = c(
  "data.table", "ggplot2", "ggthemes", "Hmisc", "mgcv", "DBI", "RMariaDB", 
  "lubridate", "dplyr", "tidyr", "ISOweek", "stringr", "flextable",
  "officer", "svglite")

# Install packages not yet installed
installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages == F)) {
  install.packages(packages[!installed_packages])
}
# Load packages
invisible(lapply(packages, library, character.only = T))

# current date:
DATE = format(Sys.Date(), "%Y%m%d")

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)

# plasma colors: https://waldyrious.net/viridis-palette-generator/
pcol = viridis::plasma(1)

readRenviron(".Renviron")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) IMPORT DATA
# ______________________________________________________________________________________________________________________

# connect to DB
con = dbConnect(
  MariaDB(),
  dbname = Sys.getenv("MYSQL_DB"),
  host = Sys.getenv("MYSQL_HOST"),
  port = 3306,
  user = Sys.getenv("MYSQL_USER"),
  password = Sys.getenv("MYSQL_PW")
)

input1 = data.table(
  dbGetQuery(
    con, "SELECT * FROM inek_merkmale_weekly;"))
input2 = data.table(
  dbGetQuery(
    con, "SELECT * FROM inek_diagnosen_weekly;"))
input3 = data.table(
  dbGetQuery(
    con, "SELECT * FROM inek_fallzahlen_weekly;"))

dbDisconnect(con); rm(con)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________
## intervention
intervention_date = as.Date("2024-04-01")

## F12.0 & T40.7 transformation
dat2 = input2 %>%
  mutate(
    icd_code = if_else(
      subset == "F12.0_T40.7" & 
        icd_code == "Rest" & str_detect(diagnose, "^Rest"),
      "T40.7", icd_code),
    diagnose = if_else(
      subset == "F12.0_T40.7" & 
        str_detect(diagnose, "^Rest") & icd_code == "T40.7",
      "Vergiftung: Cannabis (-Derivate)", diagnose))

## key variables
key_vars = c("jahr", "kw", "altergruppe")

## all-cause totals
all = input3 %>%
  filter(subset == "all") %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(N_all = sum(fallzahl, na.rm = T), .groups = "drop")

## outcomes
y_prim = dat2 %>%
  filter(subset == "F12.X_T40.7") %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_primary = sum(fallzahl, na.rm = T), .groups = "drop")

y_intox = dat2 %>%
  filter(subset == "F12.0_T40.7") %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_intox = sum(fallzahl, na.rm = T), .groups = "drop")

y_psy = dat2 %>%
  filter(subset == "F12.5") %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_psych = sum(fallzahl, na.rm = T), .groups = "drop")

## F12.2
y_dep = dat2 %>%
  filter(
    subset == "F12.X_T40.7",
    icd_code == "F12.2", 
    altergruppe == "adult") %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_dep = sum(fallzahl, na.rm = T), .groups = "drop")

## tertiary outcomes
y_schizo = dat2 %>%
  filter(
    altergruppe == "adult",
    str_detect(icd_code, "^F2")) %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_schizo = sum(fallzahl, na.rm = T), .groups = "drop")

y_alc = dat2 %>%
  filter(
    altergruppe == "adult",
    str_detect(icd_code, "^F10") | str_detect(icd_code, "^T51")) %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_alc = sum(fallzahl, na.rm = T), .groups = "drop")

y_anxiety = dat2 %>%
  filter(
    altergruppe == "adult",
    str_detect(icd_code, "^F40") |
      str_detect(icd_code, "^F41") |
      icd_code %in% c("F48.8", "F48.9")) %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_anxiety = sum(fallzahl, na.rm = T), .groups = "drop")

y_depress = dat2 %>%
  filter(
    altergruppe == "adult",
    str_detect(icd_code, "^F32") |
      str_detect(icd_code, "^F33")) %>%
  group_by(across(all_of(key_vars))) %>%
  summarize(Y_depress = sum(fallzahl, na.rm = T), .groups = "drop")

## join 
dt = all %>%
  left_join(y_prim, by = key_vars) %>%
  left_join(y_intox, by = key_vars) %>%
  left_join(y_psy, by = key_vars) %>%
  left_join(y_dep, by = key_vars) %>%
  left_join(y_schizo, by = key_vars) %>%
  left_join(y_alc, by = key_vars) %>%
  left_join(y_anxiety, by = key_vars) %>%
  left_join(y_depress, by = key_vars) %>%
  mutate(
    Y_primary = replace_na(Y_primary, 0L),
    Y_intox = replace_na(Y_intox, 0L),
    Y_psych = replace_na(Y_psych, 0L),
    Y_dep = replace_na(Y_dep, 0L),
    Y_schizo = replace_na(Y_schizo, 0L),
    Y_alc = replace_na(Y_alc, 0L),
    Y_anxiety = replace_na(Y_anxiety, 0L),
    Y_depress = replace_na(Y_depress, 0L))

## check up
dt_check = dt %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6)

n_total = dt_check %>% distinct(jahr, kw) %>% nrow()

n_pre = dt_check %>%
  filter(week_start < intervention_date) %>%
  distinct(jahr, kw) %>%
  nrow()

n_post = dt_check %>%
  filter(week_start >= intervention_date) %>%
  distinct(jahr, kw) %>%
  nrow()

latest_week_end = max(dt_check$week_end, na.rm = T)
earliest_week_start = min(dt_check$week_start, na.rm = T)

c(
  weeks_total = n_total,
  weeks_before_2024_04_01 = n_pre,
  weeks_from_2024_04_01 = n_post
)

c(
  earliest_week_start = as.character(earliest_week_start),
  latest_week_end = as.character(latest_week_end)
)

## percent of diagnosis
pct_adults = 100 * (sum(dt$Y_intox[dt$altergruppe == "adult"], na.rm = T) +
                      sum(dt$Y_psych[dt$altergruppe == "adult"], na.rm = T)) /
  sum(dt$Y_primary[dt$altergruppe == "adult"], na.rm = T)

pct_adults

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) ITS: primary analysis: F12.X + T40.7 rate
# ______________________________________________________________________________________________________________________

## adults <---------------------------------------------------------------------
itsadult = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 observations

## intervention
intervention_date = as.Date("2024-04-01")

itsadult = itsadult %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>% 
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

## t0
t0 = itsadult %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

# check
stopifnot(length(t0) == 1, !is.na(t0))

itsadult = itsadult %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

# control
itsadult %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  select(year, woy, week_start, week_end, t_idx, post, time_after)

## ITS GAM <--------------------------------------------------------------------
prim_adult = gam(
  Y_primary ~ post + time_after + t_idx + 
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsadult,
  method = "REML",
  knots = list(woy = c(0.5, 52.5)),
)

cf = coef(prim_adult); V = vcov(prim_adult); se = sqrt(diag(V))

eff_h = \(h) {
  b  = cf["post"] + h * cf["time_after"]
  se = sqrt(V["post","post"] + h ^ 2 * V["time_after","time_after"] + 
              2 * h * V["post","time_after"])
  tibble(
    h_weeks = h, 
    RR = exp(b), 
    RR_l = exp(b - 1.96 * se), 
    RR_u = exp(b + 1.96 * se))
}

bind_rows(eff_h(0), eff_h(13), eff_h(26), eff_h(52))

## rate ratios
rr_adult = tibble(
  term = names(cf),
  est = cf, 
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_adult

coef(prim_adult)[c("t_idx","post","time_after")]
gam.check(prim_adult)
acf(residuals(prim_adult, type = "pearson"), 
    na.action = na.pass, main = "ACF der Residuen (Primary)")
pacf(residuals(prim_adult, type = "pearson"), 
    na.action = na.pass, main = "pACF der Residuen (Primary)")
acf(residuals(prim_adult, type = "pearson"), 
    plot = F, na.action = na.pass)$acf[1:20]
pacf(residuals(prim_adult, type = "pearson"),
     plot = F, na.action = na.pass)$acf[1:20]

itsadult1 = itsadult %>%
  mutate(mu_hat = predict(prim_adult, type = "response"))

## counterfactual
itsadult_cf = itsadult1
itsadult_cf$post[itsadult_cf$t_idx >= t0] = 0L
itsadult_cf$time_after[itsadult_cf$t_idx >= t0] = 0L

itsadult1 = itsadult1 %>%
  mutate(
    mu_cf = predict(prim_adult, newdata = itsadult_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_primary / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

prim_adult_pt = ggplot(itsadult1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
    ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); prim_adult_pt

# svg
ggsave(
  filename = paste0("plots/GAM_InEK_primary_", DATE, ".svg"),
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/GAM_InEK_primary_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

## adolescents <----------------------------------------------------------------
itsminor = copy(dt) %>%
  filter(altergruppe == "minor") %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 observations

## intervention
intervention_date = as.Date("2024-04-01")

itsminor = itsminor %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>% 
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

## t0
t0 = itsminor %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

# check
stopifnot(length(t0) == 1, !is.na(t0))

itsminor = itsminor %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

# control
itsminor %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  select(year, woy, week_start, week_end, t_idx, post, time_after)

## ITS GAM <--------------------------------------------------------------------
prim_minor = gam(
  Y_primary ~ post + time_after + t_idx + 
    s(woy, bs = "cc", k = 50) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsminor,
  method = "REML",
  knots = list(woy = c(0.5, 52.5)),
)

cf = coef(prim_minor); V = vcov(prim_minor); se = sqrt(diag(V))

bind_rows(eff_h(0), eff_h(13), eff_h(26), eff_h(52))

## rate ratios
rr_minor = tibble(
  term = names(cf),
  est = cf, 
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_minor

coef(prim_minor)[c("t_idx","post", "time_after")]
gam.check(prim_minor)
acf(residuals(prim_minor, type = "pearson"), 
    na.action = na.pass, main = "ACF of residuals (Primary, minor)")
pacf(residuals(prim_minor, type = "pearson"), 
     na.action = na.pass, main = "pACF of residuals (Primary, minor)")

acf(residuals(prim_minor, type = "pearson"), 
    plot = F, na.action = na.pass)$acf[1:20]
pacf(residuals(prim_minor, type = "pearson"),
     plot = F, na.action = na.pass)$acf[1:20]

itsminor1 = itsminor %>%
  mutate(mu_hat = predict(prim_minor, type = "response"))

## counterfactual
itsminor_cf = itsminor1
itsminor_cf$post[itsminor_cf$t_idx >= t0] = 0L
itsminor_cf$time_after[itsminor_cf$t_idx >= t0] = 0L

itsminor1 = itsminor1 %>%
  mutate(
    mu_cf = predict(prim_minor, newdata = itsminor_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_primary / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

prim_minor_pt = ggplot(itsminor1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions",
    #title = "ITS-GAM: Rates of cannabis-related main diagnosis for adolescents",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); prim_minor_pt

# svg
ggsave(
  filename = paste0("plots/GAM_InEK_primary_minor_", DATE, ".svg"),
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/GAM_InEK_primary_minor_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) ITS: secondary analysis 1: F12.0 + T40.7 rate
# ______________________________________________________________________________________________________________________
itsintox = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 observations

## intervention
intervention_date = as.Date("2024-04-01")

itsintox = itsintox %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>% 
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

## t0
t0 = itsintox %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

# check
stopifnot(length(t0) == 1, !is.na(t0))

itsintox = itsintox %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

# control
itsintox %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  select(year, woy, week_start, week_end, t_idx, post, time_after)

## ITS GAM <--------------------------------------------------------------------
sec_adult = gam(
  Y_intox ~ post + time_after + t_idx + 
    s(woy, bs = "cc", k = 52) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsintox,
  method = "REML",
  knots = list(woy = c(0.5, 52.5)),
)

cf = coef(sec_adult); V = vcov(sec_adult); se = sqrt(diag(V))

eff_h = \(h) {
  b  = cf["post"] + h * cf["time_after"]
  se = sqrt(V["post","post"] + h ^ 2 * V["time_after","time_after"] + 
              2 * h * V["post","time_after"])
  tibble(
    h_weeks = h, 
    RR = exp(b), 
    RR_l = exp(b - 1.96 * se), 
    RR_u = exp(b + 1.96 * se))
}

bind_rows(eff_h(0), eff_h(13), eff_h(26), eff_h(52))

## rate ratios
rr_adult_sec = tibble(
  term = names(cf),
  est = cf, 
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_adult_sec

coef(sec_adult)[c("t_idx","post","time_after")]
gam.check(sec_adult)
acf(residuals(sec_adult, type = "pearson"), 
    na.action = na.pass, main = "ACF der Residuen (Intox)")
pacf(residuals(sec_adult, type = "pearson"), 
     na.action = na.pass, main = "pACF der Residuen (Intox)")

acf(residuals(sec_adult, type = "pearson"), 
    plot = F, na.action = na.pass)$acf[1:20]
pacf(residuals(sec_adult, type = "pearson"),
     plot = F, na.action = na.pass)$acf[1:20]

itsintox1 = itsintox %>%
  mutate(mu_hat = predict(sec_adult, type = "response"))

## counterfactual
itsintox_cf = itsintox1
itsintox_cf$post[itsintox_cf$t_idx >= t0] = 0L
itsintox_cf$time_after[itsintox_cf$t_idx >= t0] = 0L

itsintox1 = itsintox1 %>%
  mutate(
    mu_cf = predict(sec_adult, newdata = itsintox_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_intox / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

sec_pt = ggplot(itsintox1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); sec_pt

# svg
ggsave(
  filename = paste0("plots/GAM_InEK_secondary_intox_", DATE, ".svg"),
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/GAM_InEK_secondary_intox_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) ITS: secondary analysis 2: F12.5 rate
# ______________________________________________________________________________________________________________________
itspsych = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 observations

## intervention
intervention_date = as.Date("2024-04-01")

itspsych = itspsych %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>% 
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

## t0
t0 = itspsych %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

# check
stopifnot(length(t0) == 1, !is.na(t0))

itspsych = itspsych %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

# control
itspsych %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  select(year, woy, week_start, week_end, t_idx, post, time_after)

## ITS GAM <--------------------------------------------------------------------
sec2_adult = gam(
  Y_psych ~ post + time_after + t_idx + 
    s(woy, bs = "cc", k = 20) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itspsych,
  method = "REML",
  knots = list(woy = c(0.5, 52.5)),
)

cf = coef(sec2_adult); V = vcov(sec2_adult); se = sqrt(diag(V))

eff_h = \(h) {
  b  = cf["post"] + h * cf["time_after"]
  se = sqrt(V["post","post"] + h ^ 2 * V["time_after","time_after"] + 
              2 * h * V["post","time_after"])
  tibble(
    h_weeks = h, 
    RR = exp(b), 
    RR_l = exp(b - 1.96 * se), 
    RR_u = exp(b + 1.96 * se))
}

bind_rows(eff_h(0), eff_h(13), eff_h(26), eff_h(52))

## rate ratios
rr_adult_sec2 = tibble(
  term = names(cf),
  est = cf, 
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_adult_sec2

coef(sec2_adult)[c("t_idx","post","time_after")]
gam.check(sec2_adult)
acf(residuals(sec2_adult, type = "pearson"), 
    na.action = na.pass, main = "ACF der Residuen (Psych)")
pacf(residuals(sec2_adult, type = "pearson"), 
     na.action = na.pass, main = "pACF der Residuen (Psych)")

acf(residuals(sec2_adult, type = "pearson"), 
    plot = F, na.action = na.pass)$acf[1:20]
pacf(residuals(sec2_adult, type = "pearson"),
     plot = F, na.action = na.pass)$acf[1:20]

itspsych1 = itspsych %>%
  mutate(mu_hat = predict(sec2_adult, type = "response"))

## counterfactual
itspsych_cf = itspsych1
itspsych_cf$post[itspsych_cf$t_idx >= t0] = 0L
itspsych_cf$time_after[itspsych_cf$t_idx >= t0] = 0L

itspsych1 = itspsych1 %>%
  mutate(
    mu_cf = predict(sec2_adult, newdata = itspsych_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_psych / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

sec2_pt = ggplot(itspsych1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); sec2_pt

# svg
ggsave(
  filename = paste0("plots/GAM_InEK_secondary_psych_", DATE, ".svg"),
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/GAM_InEK_secondary_psych_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

rr_adult; rr_adult_sec; rr_adult_sec2
prim_adult_pt; prim_minor_pt; sec_pt; sec2_pt

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5b) POST-HOC ANALYSIS: F12.2 (adults only)
# ______________________________________________________________________________________________________________________
itsdep = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 observations

itsdep = itsdep %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>%
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

t0 = itsdep %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

stopifnot(length(t0) == 1, !is.na(t0))

itsdep = itsdep %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

## ITS GAM <--------------------------------------------------------------------
dep_adult = gam(
  Y_dep ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsdep,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

cf = coef(dep_adult); V = vcov(dep_adult); se = sqrt(diag(V))

rr_dep_adult = tibble(
  term = names(cf),
  est = cf,
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_dep_adult

coef(dep_adult)[c("t_idx", "post", "time_after")]
gam.check(dep_adult)

acf(residuals(dep_adult, type = "pearson"),
    na.action = na.pass, main = "ACF of residuals (F12.2, adults)")
pacf(residuals(dep_adult, type = "pearson"),
     na.action = na.pass, main = "pACF of residuals (F12.2, adults)")

acf(residuals(dep_adult, type = "pearson"), 
    plot = F, na.action = na.pass)$acf[1:20]
pacf(residuals(dep_adult, type = "pearson"),
     plot = F, na.action = na.pass)$acf[1:20]

## plot
itsdep1 = itsdep %>%
  mutate(mu_hat = predict(dep_adult, type = "response"))

itsdep_cf = itsdep1
itsdep_cf$post[itsdep_cf$t_idx >= t0] = 0L
itsdep_cf$time_after[itsdep_cf$t_idx >= t0] = 0L

itsdep1 = itsdep1 %>%
  mutate(
    mu_cf = predict(dep_adult, newdata = itsdep_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_dep / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

dep_pt = ggplot(itsdep1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); dep_pt

# svg
ggsave(
  filename = paste0("plots/GAM_InEK_posthoc_F12_2_adults_", DATE, ".svg"),
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/GAM_InEK_posthoc_F12_2_adults_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5c) TERTIARY ANALYSIS: Schizophrenia (F2x, adults only)
# ______________________________________________________________________________________________________________________
itsschizo = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(32:39)))

itsschizo = itsschizo %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>%
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

t0 = itsschizo %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

stopifnot(length(t0) == 1, !is.na(t0))

itsschizo = itsschizo %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

## ITS GAM <--------------------------------------------------------------------
schizo_adult = gam(
  Y_schizo ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsschizo,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

cf = coef(schizo_adult); V = vcov(schizo_adult); se = sqrt(diag(V))

rr_schizo_adult = tibble(
  term = names(cf),
  est = cf,
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_schizo_adult

## plot
itsschizo1 = itsschizo %>%
  mutate(mu_hat = predict(schizo_adult, type = "response"))

itsschizo_cf = itsschizo1
itsschizo_cf$post[itsschizo_cf$t_idx >= t0] = 0L
itsschizo_cf$time_after[itsschizo_cf$t_idx >= t0] = 0L

itsschizo1 = itsschizo1 %>%
  mutate(
    mu_cf = predict(schizo_adult, newdata = itsschizo_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_schizo / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

schizo_pt = ggplot(itsschizo1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); schizo_pt

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5d) TERTIARY ANALYSIS: Alcohol (F10/T51, adults only)
# ______________________________________________________________________________________________________________________
itsalc = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(32:39)))

itsalc = itsalc %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>%
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

t0 = itsalc %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

stopifnot(length(t0) == 1, !is.na(t0))

itsalc = itsalc %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

## ITS GAM <--------------------------------------------------------------------
alc_adult = gam(
  Y_alc ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsalc,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

cf = coef(alc_adult); V = vcov(alc_adult); se = sqrt(diag(V))

rr_alc_adult = tibble(
  term = names(cf),
  est = cf,
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_alc_adult

## plot
itsalc1 = itsalc %>%
  mutate(mu_hat = predict(alc_adult, type = "response"))

itsalc_cf = itsalc1
itsalc_cf$post[itsalc_cf$t_idx >= t0] = 0L
itsalc_cf$time_after[itsalc_cf$t_idx >= t0] = 0L

itsalc1 = itsalc1 %>%
  mutate(
    mu_cf = predict(alc_adult, newdata = itsalc_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_alc / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

alc_pt = ggplot(itsalc1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); alc_pt

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5e) TERTIARY ANALYSIS: Anxiety disorders (adults only)
# ______________________________________________________________________________________________________________________
itsanxiety = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(32:39)))

itsanxiety = itsanxiety %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>%
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

t0 = itsanxiety %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

stopifnot(length(t0) == 1, !is.na(t0))

itsanxiety = itsanxiety %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

## ITS GAM <--------------------------------------------------------------------
anxiety_adult = gam(
  Y_anxiety ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsanxiety,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

cf = coef(anxiety_adult); V = vcov(anxiety_adult); se = sqrt(diag(V))

rr_anxiety_adult = tibble(
  term = names(cf),
  est = cf,
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_anxiety_adult

## plot
itsanxiety1 = itsanxiety %>%
  mutate(mu_hat = predict(anxiety_adult, type = "response"))

itsanxiety_cf = itsanxiety1
itsanxiety_cf$post[itsanxiety_cf$t_idx >= t0] = 0L
itsanxiety_cf$time_after[itsanxiety_cf$t_idx >= t0] = 0L

itsanxiety1 = itsanxiety1 %>%
  mutate(
    mu_cf = predict(anxiety_adult, newdata = itsanxiety_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_anxiety / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

anxiety_pt = ggplot(itsanxiety1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); anxiety_pt

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5f) TERTIARY ANALYSIS: Depression (F32-F33, adults only)
# ______________________________________________________________________________________________________________________
itsdepress = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(32:39)))

itsdepress = itsdepress %>%
  mutate(
    week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
    week_end = week_start + 6) %>%
  rename(woy = kw, year = jahr, agegroup = altergruppe) %>%
  arrange(week_start) %>%
  mutate(t_idx = row_number())

t0 = itsdepress %>%
  filter(week_start <= intervention_date & intervention_date <= week_end) %>%
  summarize(t0 = min(t_idx)) %>%
  pull(t0)

stopifnot(length(t0) == 1, !is.na(t0))

itsdepress = itsdepress %>%
  mutate(
    post = as.integer(t_idx >= t0),
    time_after = pmax(0L, t_idx - t0)) %>%
  mutate(time_after = if_else(t_idx < t0, 0L, time_after))

## ITS GAM <--------------------------------------------------------------------
depress_adult = gam(
  Y_depress ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 30) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsdepress,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

cf = coef(depress_adult); V = vcov(depress_adult); se = sqrt(diag(V))

rr_depress_adult = tibble(
  term = names(cf),
  est = cf,
  se = se,
  lcl = cf - 1.96 * se,
  ucl = cf + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_depress_adult

## plot
itsdepress1 = itsdepress %>%
  mutate(mu_hat = predict(depress_adult, type = "response"))

itsdepress_cf = itsdepress1
itsdepress_cf$post[itsdepress_cf$t_idx >= t0] = 0L
itsdepress_cf$time_after[itsdepress_cf$t_idx >= t0] = 0L

itsdepress1 = itsdepress1 %>%
  mutate(
    mu_cf = predict(depress_adult, newdata = itsdepress_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_depress / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf = 1000 * mu_cf / N_all)

depress_pt = ggplot(itsdepress1, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf), linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); depress_pt

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) ITS plots & result table
# ______________________________________________________________________________________________________________________
## harmonize plots
p1 = as.data.table(itsadult1)[, .(
  week_start,
  panel = "Primary (adults)",
  rate_obs, rate_hat, rate_cf
)]

p2 = as.data.table(itsminor1)[, .(
  week_start,
  panel = "Primary (adolescents)",
  rate_obs, rate_hat, rate_cf
)]

p3 = as.data.table(itsintox1)[, .(
  week_start,
  panel = "Secondary 1",
  rate_obs, rate_hat, rate_cf
)]

p4 = as.data.table(itspsych1)[, .(
  week_start,
  panel = "Secondary 2",
  rate_obs, rate_hat, rate_cf
)]

plot_dt = rbindlist(list(p1, p2, p3, p4), use.names = T)

plot_long = melt(
  plot_dt,
  id.vars = c("week_start", "panel"),
  measure.vars = c("rate_obs", "rate_hat", "rate_cf"),
  variable.name = "series",
  value.name = "rate"
)

plot_long[, series := factor(
  series,
  levels = c("rate_obs", "rate_hat", "rate_cf"),
  labels = c("Observed", "Fitted", "Counterfactual")
)]

plot_long[, panel := factor(
  panel,
  levels = c("Primary (adults)", "Primary (adolescents)", 
             "Secondary 1", "Secondary 2")
)]

## facet plot
itsa_facet = ggplot(plot_long, aes(x = week_start, y = rate)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(
    data = plot_long[series == "Observed"],
    linewidth = 0.7, alpha = 0.30, color = pcol) +
  geom_line(
    data = plot_long[series == "Fitted"],
    linewidth = 1.0, alpha = 1.00, color = pcol) +
  geom_line(
    data = plot_long[series == "Counterfactual"],
    linewidth = 1.0, alpha = 1.00, color = pcol, linetype = "22") +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

itsa_facet

# svg
ggsave(
  filename = paste0("plots/ITSA_facets_4panels_", DATE, ".svg"),
  plot = itsa_facet,
  width = 14,
  height = 9
)

# tiff
ggsave(
  filename = paste0("plots/ITSA_facets_4panels_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

## Result table <---------------------------------------------------------------
## help fcts
fmt_stars = \(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    T ~ "")
}

get_term_rr_star = \(mod, term) {
  cf = coef(mod)
  V = vcov(mod)
  pt = summary(mod)$p.table
  b = unname(cf[term])
  se = sqrt(unname(V[term, term]))
  p = unname(pt[term, "Pr(>|z|)"])
  rr = exp(b)
  l = exp(b - 1.96 * se)
  u = exp(b + 1.96 * se)
  sprintf("%.3f (%.3f–%.3f)%s", rr, l, u, fmt_stars(p))
}

tab2 = data.table(
  Population = c("Adults", "Adolescents", "Adults", "Adults"),
  Outcome = c("Primary", "Primary", "Secondary 1", "Secondary 2"),
  Details = c(
    "All cannabis-specific\ndiagnoses¹)",
    "All cannabis-specific\ndiagnoses¹)",
    "Cannabis\nintoxication or\npoisoning²)",
    "Cannabis-induced\npsychosis³)"),
  level_change = c(
    get_term_rr_star(prim_adult, "post"),
    get_term_rr_star(prim_minor, "post"),
    get_term_rr_star(sec_adult, "post"),
    get_term_rr_star(sec2_adult, "post")),
  slope_change = c(
    get_term_rr_star(prim_adult, "time_after"),
    get_term_rr_star(prim_minor, "time_after"),
    get_term_rr_star(sec_adult, "time_after"),
    get_term_rr_star(sec2_adult, "time_after")))

note_tab2 = paste(
  "Note. 1) Primary outcome: rate of admissions with a cannabis-specific ICD-10 main diagnosis",
  "(F12.0-F12.9; T40.7) per 1,000 all-cause admissions in the population",
  "(adults vs. adolescents); 2) Secondary outcome 1: rate of admissions with acute intoxication",
  "(F12.0) or poisoning (T40.7) per 1,000 all-cause admissions among adults;",
  "3) Secondary outcome 2: rate of admissions with cannabis-induced psychoses (F12.5)",
  "per 1,000 all-cause admissions among adults. Detailed model results are reported in",
  "Supplementary Table 4. * p < 0.05, ** p < 0.01, *** p < 0.001."
)

ft2 = tab2 %>%
  flextable(
    col_keys = c(
      "Population", "Outcome", "Details", "level_change", "slope_change")) %>%
  set_header_labels(
    Population = "Population",
    Outcome = "Outcome",
    Details = "Details",
    level_change = "Level change",
    slope_change = "Slope change") %>%
  autofit() %>%
  add_footer_lines(values = note_tab2)

ft2
# save_as_docx(ft2, path = paste0("Table_model_results_", DATE, ".docx"))

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6b) ITS plots & result table - Tertiary Analysis
# ______________________________________________________________________________________________________________________
## harmonize tertiary plots
p5 = as.data.table(itsschizo1)[, .(
  week_start,
  panel = "Schizophrenia",
  rate_obs, rate_hat, rate_cf
)]

p6 = as.data.table(itsalc1)[, .(
  week_start,
  panel = "Alcohol",
  rate_obs, rate_hat, rate_cf
)]

p7 = as.data.table(itsanxiety1)[, .(
  week_start,
  panel = "Anxiety",
  rate_obs, rate_hat, rate_cf
)]

p8 = as.data.table(itsdepress1)[, .(
  week_start,
  panel = "Depression",
  rate_obs, rate_hat, rate_cf
)]

plot_dt_ter = rbindlist(list(p5, p6, p7, p8), use.names = T)

plot_long_ter = melt(
  plot_dt_ter,
  id.vars = c("week_start", "panel"),
  measure.vars = c("rate_obs", "rate_hat", "rate_cf"),
  variable.name = "series",
  value.name = "rate"
)

plot_long_ter[, series := factor(
  series,
  levels = c("rate_obs", "rate_hat", "rate_cf"),
  labels = c("Observed", "Fitted", "Counterfactual")
)]

plot_long_ter[, panel := factor(
  panel,
  levels = c("Alcohol", "Anxiety", "Depression", "Schizophrenia")
)]

## facet plot
itsa_facet_ter = ggplot(plot_long_ter, aes(x = week_start, y = rate)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(
    data = plot_long_ter[series == "Observed"],
    linewidth = 0.7, alpha = 0.30, color = pcol) +
  geom_line(
    data = plot_long_ter[series == "Fitted"],
    linewidth = 1.0, alpha = 1.00, color = pcol) +
  geom_line(
    data = plot_long_ter[series == "Counterfactual"],
    linewidth = 1.0, alpha = 1.00, color = pcol, linetype = "22") +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause admissions") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

itsa_facet_ter
rr_alc_adult; rr_anxiety_adult; rr_depress_adult; rr_schizo_adult

# svg
ggsave(
  filename = paste0("plots/ITSA_facets_tertiary_4panels_", DATE, ".svg"),
  plot = itsa_facet_ter,
  width = 14,
  height = 9
)

# tiff
ggsave(
  filename = paste0("plots/ITSA_facets_tertiary_4panels_", DATE, ".tiff"),
  plot = itsa_facet_ter,
  width = 14,
  height = 9,
  dpi = 300
)

## tertiary result table <-------------------------------------------------------
tab2_ter = data.table(
  Outcome = c(
    "Alcohol use disorders",
    "Anxiety disorders",
    "Depression",
    "Schizophrenia"),
  level_change = c(
    get_term_rr_star(alc_adult, "post"),
    get_term_rr_star(anxiety_adult, "post"),
    get_term_rr_star(depress_adult, "post"),
    get_term_rr_star(schizo_adult, "post")),
  slope_change = c(
    get_term_rr_star(alc_adult, "time_after"),
    get_term_rr_star(anxiety_adult, "time_after"),
    get_term_rr_star(depress_adult, "time_after"),
    get_term_rr_star(schizo_adult, "time_after")))

note_tab2_ter = paste(
  "Note. Values are rate ratios with 95% confidence intervals.",
  "* p < 0.05, ** p < 0.01, *** p < 0.001.")

ft2_ter = tab2_ter %>%
  flextable(col_keys = c("Outcome", "level_change", "slope_change")) %>%
  set_header_labels(
    Outcome = "Outcome",
    level_change = "level change: RR (95% CI)",
    slope_change = "slope change: RR (95% CI)") %>%
  autofit() %>%
  add_footer_lines(values = note_tab2_ter)

ft2_ter
save_as_docx(ft2_ter, path = paste0("supp_table_tertiary_model_results_", DATE, ".docx"))

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6c) ABSOLUTE AND RELATIVE 12-MONTH EFFECTS
# ______________________________________________________________________________________________________________________
post_12m_end = as.Date("2025-03-31")

## adults primary
absrel_12m_adult = itsadult1 %>%
  filter(
    week_start >= intervention_date,
    week_start <= post_12m_end) %>%
  summarize(
    fitted_12m = sum(mu_hat, na.rm = T),
    cf_12m = sum(mu_cf, na.rm = T),
    extra_12m = fitted_12m - cf_12m,
    extra_per_week = mean(mu_hat - mu_cf, na.rm = T),
    pct_more_vs_cf = 100 * (fitted_12m - cf_12m) / cf_12m)

## adolescents primary
absrel_12m_minor = itsminor1 %>%
  filter(
    week_start >= intervention_date,
    week_start <= post_12m_end) %>%
  summarize(
    fitted_12m = sum(mu_hat, na.rm = T),
    cf_12m = sum(mu_cf, na.rm = T),
    extra_12m = fitted_12m - cf_12m,
    pct_more_vs_cf = 100 * (fitted_12m - cf_12m) / cf_12m)

## secondary 1
absrel_12m_intox = itsintox1 %>%
  filter(
    week_start >= intervention_date,
    week_start <= post_12m_end) %>%
  summarize(
    fitted_12m = sum(mu_hat, na.rm = T),
    cf_12m = sum(mu_cf, na.rm = T),
    extra_12m = fitted_12m - cf_12m,
    extra_per_week = mean(mu_hat - mu_cf, na.rm = T),
    pct_more_vs_cf = 100 * (fitted_12m - cf_12m) / cf_12m)

## secondary 2
absrel_12m_psych = itspsych1 %>%
  filter(
    week_start >= intervention_date,
    week_start <= post_12m_end) %>%
  summarize(
    fitted_12m = sum(mu_hat, na.rm = T),
    cf_12m = sum(mu_cf, na.rm = T),
    extra_12m = fitted_12m - cf_12m,
    extra_per_week = mean(mu_hat - mu_cf, na.rm = T),
    pct_more_vs_cf = 100 * (fitted_12m - cf_12m) / cf_12m)

absrel_12m_adult; absrel_12m_minor; absrel_12m_intox; absrel_12m_psych

## rounded values for manuscript text
round(absrel_12m_adult$pct_more_vs_cf, 1)
round(absrel_12m_minor$pct_more_vs_cf, 1)
round(absrel_12m_intox$pct_more_vs_cf, 1)
round(absrel_12m_psych$pct_more_vs_cf, 1)

# p-value F12.2
summary(dep_adult)$p.table[c("post", "time_after"), "Pr(>|z|)"]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) TABLE 1: Descriptives
# ______________________________________________________________________________________________________________________
dt0 = copy(dt) %>%
  filter(!(jahr == 2025 & kw %in% c(38:39))) # remove last 2 obs.

dt0 = as.data.table(dt0)
dt0[, date := ISOweek2date(sprintf("%d-W%02d-1", jahr, kw))]

## totals
dt_total = dt0[
  , 
  .(N_all = sum(N_all, na.rm = T),
    Y_primary = sum(Y_primary, na.rm = T),
    Y_intox = sum(Y_intox, na.rm = T),
    Y_psych = sum(Y_psych, na.rm = T)
    ),
  by = .(jahr, kw, date)
  ][, altergruppe := "total"]

dt0 = rbindlist(list(dt0, dt_total), use.names = T, fill = T)

## columns of tab1
age_labs = c(
  minor = "Adolescents",
  adult = "Adults",
  total = "Total"
)

dt0[, altergruppe_lab := age_labs[altergruppe]]

## actual date version <--------------------------------------------------------
## pre/post
intervention_date = as.Date("2024-04-01")
pre_start = as.Date("2023-04-01")
pre_end = as.Date("2024-03-31")
post_start = intervention_date
post_end = as.Date("2025-03-31")

## long format
dt_long = rbindlist(list(
  dt0[, .(altergruppe_lab, date, y = Y_primary, pop = N_all,
          outcome = "Any cannabis-specific diagnosis admissions (primary outcome)")],
  dt0[, .(altergruppe_lab, date, y = Y_intox,   pop = N_all,
          outcome = "Acute intoxication admissions (secondary outcome 1)")],
  dt0[, .(altergruppe_lab, date, y = Y_psych,   pop = N_all,
          outcome = "Cannabis-induced psychosis admissions (secondary outcome 2)")]
), use.names = T)

## rate per 1000
dt_long[, rate := (y / pop) * 1e3]

setorder(dt_long, outcome, altergruppe_lab, date)

## Table 1
tab1_long = dt_long[, {
  
  d = .SD
  d_pre = d[date >= pre_start  & date < pre_end]
  d_post = d[date >= post_start & date < post_end]
  
  rbind(
    data.table(
      block = .BY$outcome,
      row = "Total N",
      value = sum(d$y, na.rm = T)),
    data.table(
      block = .BY$outcome,
      row = "Weekly N: mean (SD)",
      value = {
        m = mean(d$y, na.rm = T)
        s = sd(d$y, na.rm = T)
        sprintf("%.2f (%.2f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "Weekly rate: mean (SD)",
      value = {
        m = mean(d$rate, na.rm = T)
        s = sd(d$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "12 months before 1 April 2024: weekly rate: mean (SD)",
      value = {
        m = mean(d_pre$rate, na.rm = T)
        s = sd(d_pre$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "12 months after 1 April 2024: weekly rate: mean (SD)",
      value = {
        m = mean(d_post$rate, na.rm = T)
        s = sd(d_post$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      })
  )
}, by = .(outcome, altergruppe_lab)]

## wide format
tab1 = dcast(
  tab1_long,
  block + row ~ altergruppe_lab,
  value.var = "value"
)

## sort
tab1[, block := factor(block, levels = c(
  "Any cannabis-specific diagnosis admissions (primary outcome)",
  "Acute intoxication admissions (secondary outcome 1)",
  "Cannabis-induced psychosis admissions (secondary outcome 2)"
))]

tab1[, row := factor(row, levels = c(
  "Total N",
  "Weekly N: mean (SD)",
  "Weekly rate: mean (SD)",
  "12 months before 1 April 2024: weekly rate: mean (SD)",
  "12 months after 1 April 2024: weekly rate: mean (SD)"
))]

setorder(tab1, block, row)

tab1

## week of year version <-------------------------------------------------------
dt0[, iso_week_id := jahr * 53L + kw]

## intervention
intervention_year = 2024L
intervention_kw = 14L
intervention_id = intervention_year * 53L + intervention_kw

## pre/post
pre_min_id = intervention_id - 52L
pre_max_id = intervention_id - 1L
post_min_id = intervention_id
post_max_id = intervention_id + 51L

## long format
dt_long = rbindlist(list(
  dt0[, .(altergruppe_lab, jahr, kw, iso_week_id, y = Y_primary, pop = N_all,
          outcome = "Any cannabis-specific diagnosis admissions (primary outcome)")],
  dt0[, .(altergruppe_lab, jahr, kw, iso_week_id, y = Y_intox,   pop = N_all,
          outcome = "Acute intoxication admissions (secondary outcome 1)")],
  dt0[, .(altergruppe_lab, jahr, kw, iso_week_id, y = Y_psych,   pop = N_all,
          outcome = "Cannabis-induced psychosis admissions (secondary outcome 2)")]
), use.names = T)

## rate per 1000
dt_long[, rate := (y / pop) * 1e3]

setorder(dt_long, outcome, altergruppe_lab, iso_week_id)

## Table 1
tab1_long = dt_long[, {
  
  d = .SD
  d_pre = d[iso_week_id >= pre_min_id  & iso_week_id <= pre_max_id]
  d_post = d[iso_week_id >= post_min_id & iso_week_id <= post_max_id]
  
  rbind(
    data.table(
      block = .BY$outcome,
      row = "Total N",
      value = sum(d$y, na.rm = T)),
    data.table(
      block = .BY$outcome,
      row = "Weekly N: mean (SD)",
      value = {
        m = mean(d$y, na.rm = T)
        s = sd(d$y, na.rm = T)
        sprintf("%.2f (%.2f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "Weekly rate: mean (SD)",
      value = {
        m = mean(d$rate, na.rm = T)
        s = sd(d$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "12 months before 1 April 2024: weekly rate: mean (SD)",
      value = {
        m = mean(d_pre$rate, na.rm = T)
        s = sd(d_pre$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      }),
    data.table(
      block = .BY$outcome,
      row = "12 months after 1 April 2024: weekly rate: mean (SD)",
      value = {
        m = mean(d_post$rate, na.rm = T)
        s = sd(d_post$rate, na.rm = T)
        sprintf("%.3f (%.3f)", m, s)
      }))
}, by = .(outcome, altergruppe_lab)]

## wide format
tab1 = dcast(
  tab1_long,
  block + row ~ altergruppe_lab,
  value.var = "value"
)

## sort
tab1[, block := factor(block, levels = c(
  "Any cannabis-specific diagnosis admissions (primary outcome)",
  "Acute intoxication admissions (secondary outcome 1)",
  "Cannabis-induced psychosis admissions (secondary outcome 2)"
))]

tab1[, row := factor(row, levels = c(
  "Total N",
  "Weekly N: mean (SD)",
  "Weekly rate: mean (SD)",
  "12 months before 1 April 2024: weekly rate: mean (SD)",
  "12 months after 1 April 2024: weekly rate: mean (SD)"
))]

setorder(tab1, block, row)

tab1

## tab 1 extension <------------------------------------------------------------
## long format
dt_long = rbindlist(list(
  dt0[, .(altergruppe_lab, date, y = Y_primary, pop = N_all,
          outcome = "Any cannabis-specific diagnosis admissions")],
  dt0[, .(altergruppe_lab, date, y = Y_intox,   pop = N_all,
          outcome = "Acute intoxication admissions")],
  dt0[, .(altergruppe_lab, date, y = Y_psych,   pop = N_all,
          outcome = "Cannabis-induced psychosis admissions")]
), use.names = T)

## rate per 1000 all-cause admissions
dt_long[, rate := (y / pop) * 1e3]

setorder(dt_long, outcome, altergruppe_lab, date)

## Table 1
tab1_long = dt_long[, {
  d = .SD
  d_pre  = d[date >= pre_start  & date <  pre_end]
  d_post = d[date >= post_start & date <  post_end]
  out_lab = .BY$outcome
  row_lab_rate = fifelse(
    out_lab == "Any cannabis-specific diagnosis admissions",
    "Weekly rate¹: mean (SD)",
    fifelse(
      out_lab == "Acute intoxication admissions",
      "Weekly rate²: mean (SD)",
      "Weekly rate³: mean (SD)"))
  row_lab_rate_pre = fifelse(
    out_lab == "Any cannabis-specific diagnosis admissions",
    "12 months before 1 April 2024: weekly rate¹: mean (SD)",
    fifelse(
      out_lab == "Acute intoxication admissions",
      "12 months before 1 April 2024: weekly rate²: mean (SD)",
      "12 months before 1 April 2024: weekly rate³: mean (SD)"))
  row_lab_rate_post = fifelse(
    out_lab == "Any cannabis-specific diagnosis admissions",
    "12 months after 1 April 2024: weekly rate¹: mean (SD)",
    fifelse(
      out_lab == "Acute intoxication admissions",
      "12 months after 1 April 2024: weekly rate²: mean (SD)",
      "12 months after 1 April 2024: weekly rate³: mean (SD)"))
  rbind(
    data.table(
      block = out_lab,
      row = "Total N",
      value = as.character(sum(d$y, na.rm = T))),
    data.table(
      block = out_lab,
      row = "Total N: 12 months before 1 April 2024",
      value = as.character(sum(d_pre$y, na.rm = T))),
    data.table(
      block = out_lab,
      row = "Total N: 12 months after 1 April 2024",
      value = as.character(sum(d_post$y, na.rm = T))),
    data.table(
      block = out_lab,
      row = "Weekly N: mean (SD)",
      value = sprintf("%.2f (%.2f)",
                      mean(d$y, na.rm = T),
                      sd(d$y, na.rm = T))),
    data.table(
      block = out_lab,
      row = row_lab_rate,
      value = sprintf("%.2f (%.2f)",
                      mean(d$rate, na.rm = T),
                      sd(d$rate, na.rm = T))),
    data.table(
      block = out_lab,
      row = row_lab_rate_pre,
      value = sprintf("%.2f (%.2f)",
                      mean(d_pre$rate, na.rm = T),
                      sd(d_pre$rate, na.rm = T))),
    data.table(
      block = out_lab,
      row = row_lab_rate_post,
      value = sprintf("%.2f (%.2f)",
                      mean(d_post$rate, na.rm = T),
                      sd(d_post$rate, na.rm = T))))
}, by = .(outcome, altergruppe_lab)]

## wide format
tab1 = dcast(
  tab1_long,
  block + row ~ altergruppe_lab,
  value.var = "value")

## order
tab1[, block := factor(block, levels = c(
  "Any cannabis-specific diagnosis admissions",
  "Acute intoxication admissions",
  "Cannabis-induced psychosis admissions"))]

tab1[, row := factor(row, levels = c(
  "Total N",
  "Total N: 12 months before 1 April 2024",
  "Total N: 12 months after 1 April 2024",
  "Weekly N: mean (SD)",
  "Weekly rate¹: mean (SD)",
  "12 months before 1 April 2024: weekly rate¹: mean (SD)",
  "12 months after 1 April 2024: weekly rate¹: mean (SD)",
  "Weekly rate²: mean (SD)",
  "12 months before 1 April 2024: weekly rate²: mean (SD)",
  "12 months after 1 April 2024: weekly rate²: mean (SD)",
  "Weekly rate³: mean (SD)",
  "12 months before 1 April 2024: weekly rate³: mean (SD)",
  "12 months after 1 April 2024: weekly rate³: mean (SD)"))]

setorder(tab1, block, row)

tab1_out = copy(tab1)

sec_rows = tab1_out$block %in% c(
  "Acute intoxication admissions",
  "Cannabis-induced psychosis admissions")

tab1_out[sec_rows, Adolescents := "/"]
tab1_out[sec_rows, Total := "/"]

tab1_out
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 8) SENSITIVITY ANALYSES
# ______________________________________________________________________________________________________________________
intervention_date = as.Date("2024-04-01")
placebo_shifts = -10L:10L

# facet order 
outcome_levels = c(
  "Primary (adults)",
  "Primary (adolescents)",
  "Secondary 1",
  "Secondary 2"
)

get_rr = \(m, term) {
  cf = coef(m); V = vcov(m)
  b  = unname(cf[term])
  se = sqrt(unname(V[term, term]))
  c(
    RR  = exp(b),
    RR_l = exp(b - 1.96 * se),
    RR_u = exp(b + 1.96 * se)
  )
}

## Stratified by source <-------------------------------------------------------
key_vars_sys = c("jahr", "kw", "altergruppe", "quelle")

# all-cause totals per system
all_sys = input3 %>%
  filter(subset == "all") %>%
  group_by(across(all_of(key_vars_sys))) %>%
  summarize(N_all = sum(fallzahl, na.rm = T), .groups = "drop")

# outcomes per system
y_prim_sys = dat2 %>%
  filter(subset == "F12.X_T40.7") %>%
  group_by(across(all_of(key_vars_sys))) %>%
  summarize(Y_primary = sum(fallzahl, na.rm = T), .groups = "drop")

y_intox_sys = dat2 %>%
  filter(subset == "F12.0_T40.7") %>%
  group_by(across(all_of(key_vars_sys))) %>%
  summarize(Y_intox = sum(fallzahl, na.rm = T), .groups = "drop")

y_psych_sys = dat2 %>%
  filter(subset == "F12.5") %>%
  group_by(across(all_of(key_vars_sys))) %>%
  summarize(Y_psych = sum(fallzahl, na.rm = T), .groups = "drop")

dt_sys = all_sys %>%
  left_join(y_prim_sys, by = key_vars_sys) %>%
  left_join(y_intox_sys, by = key_vars_sys) %>%
  left_join(y_psych_sys, by = key_vars_sys) %>%
  mutate(
    Y_primary = replace_na(Y_primary, 0L),
    Y_intox = replace_na(Y_intox, 0L),
    Y_psych = replace_na(Y_psych, 0L))

sens_sys_res = data.table()
sens_model_list = list()

for (q in sort(unique(dt_sys$quelle))) {
  
  # ---------------- primary adults ----------------
  d_ad = dt_sys %>%
    filter(quelle == q, altergruppe == "adult") %>%
    filter(!(jahr == 2025 & kw %in% c(38:39))) %>%
    mutate(
      week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
      week_end = week_start + 6) %>%
    rename(woy = kw) %>%
    arrange(week_start) %>%
    mutate(t_idx = row_number())
  
  t0_ad = d_ad %>%
    filter(week_start <= intervention_date & intervention_date <= week_end) %>%
    summarize(t0 = min(t_idx)) %>%
    pull(t0)
  
  stopifnot(length(t0_ad) == 1, !is.na(t0_ad))
  
  d_ad = d_ad %>%
    mutate(
      post = as.integer(t_idx >= t0_ad),
      time_after = if_else(t_idx < t0_ad, 0L, pmax(0L, t_idx - t0_ad)))
  
  m_ad = gam(
    Y_primary ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 30) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_ad,
    method = "REML",
    knots = list(woy = c(0.5, 52.5)))
  
  sens_model_list[[paste(q, "Primary (adults)", sep = " | ")]] = m_ad
  
  r_post = get_rr_p(m_ad, "post")
  r_ta = get_rr_p(m_ad, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(
      quelle = q, outcome = "Primary (adults)", term = "post",
      RR = r_post["RR"], RR_l = r_post["RR_l"], RR_u = r_post["RR_u"],
      p_value = r_post["p_value"]),
    data.table(
      quelle = q, outcome = "Primary (adults)", term = "time_after",
      RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"],
      p_value = r_ta["p_value"]))
  
  # ---------------- primary adolescents ----------------
  d_mi = dt_sys %>%
    filter(quelle == q, altergruppe == "minor") %>%
    filter(!(jahr == 2025 & kw %in% c(38:39))) %>%
    mutate(
      week_start = ISOweek2date(sprintf("%d-W%02d-1", jahr, kw)),
      week_end = week_start + 6) %>%
    rename(woy = kw) %>%
    arrange(week_start) %>%
    mutate(t_idx = row_number())
  
  t0_mi = d_mi %>%
    filter(week_start <= intervention_date & intervention_date <= week_end) %>%
    summarize(t0 = min(t_idx)) %>%
    pull(t0)
  
  stopifnot(length(t0_mi) == 1, !is.na(t0_mi))
  
  d_mi = d_mi %>%
    mutate(
      post = as.integer(t_idx >= t0_mi),
      time_after = if_else(t_idx < t0_mi, 0L, pmax(0L, t_idx - t0_mi)))
  
  m_mi = gam(
    Y_primary ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 50) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_mi,
    method = "REML",
    knots = list(woy = c(0.5, 52.5)))
  
  sens_model_list[[paste(q, "Primary (adolescents)", sep = " | ")]] = m_mi
  
  r_post = get_rr_p(m_mi, "post")
  r_ta = get_rr_p(m_mi, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(
      quelle = q, outcome = "Primary (adolescents)", term = "post",
      RR = r_post["RR"], RR_l = r_post["RR_l"], RR_u = r_post["RR_u"],
      p_value = r_post["p_value"]),
    data.table(
      quelle = q, outcome = "Primary (adolescents)", term = "time_after",
      RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"],
      p_value = r_ta["p_value"]))
  
  # ---------------- secondary 1 ----------------
  m_s1 = gam(
    Y_intox ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 52) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_ad,
    method = "REML",
    knots = list(woy = c(0.5, 52.5)))
  
  sens_model_list[[paste(q, "Secondary 1", sep = " | ")]] = m_s1
  
  r_post = get_rr_p(m_s1, "post")
  r_ta = get_rr_p(m_s1, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(
      quelle = q, outcome = "Secondary 1", term = "post",
      RR = r_post["RR"], RR_l = r_post["RR_l"], RR_u = r_post["RR_u"],
      p_value = r_post["p_value"]),
    data.table(
      quelle = q, outcome = "Secondary 1", term = "time_after",
      RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"],
      p_value = r_ta["p_value"]))
  
  # ---------------- secondary 2 ----------------
  m_s2 = gam(
    Y_psych ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 20) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_ad,
    method = "REML",
    knots = list(woy = c(0.5, 52.5)))
  
  sens_model_list[[paste(q, "Secondary 2", sep = " | ")]] = m_s2
  
  r_post = get_rr_p(m_s2, "post")
  r_ta = get_rr_p(m_s2, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(
      quelle = q, outcome = "Secondary 2", term = "post",
      RR = r_post["RR"], RR_l = r_post["RR_l"], RR_u = r_post["RR_u"],
      p_value = r_post["p_value"]),
    data.table(
      quelle = q, outcome = "Secondary 2", term = "time_after",
      RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"],
      p_value = r_ta["p_value"]))
}

sens_sys_res = sens_sys_res %>%
  mutate(
    RR_txt = sprintf("%.3f (%.3f–%.3f)%s", RR, RR_l, RR_u, fmt_stars(p_value)),
    outcome = factor(outcome, levels = outcome_levels)) %>%
  arrange(quelle, outcome, term)

sens_sys_res

## plots
sens_sys_plot = sens_sys_res %>%
  mutate(
    term_lab = factor(
      term,
      levels = c("post", "time_after"),
      labels = c("Level change (β_level)", "Slope change (β_trend)")),
    outcome = factor(outcome, levels = outcome_levels))

p_sys_combined = ggplot(
  sens_sys_plot,
  aes(x = quelle, y = RR, ymin = RR_l, ymax = RR_u, colour = outcome)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.35)) +
  facet_grid(term_lab ~ outcome, scales = "free_y") +
  labs(
    x = "Reimbursement system",
    y = "Rate ratio") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12)); p_sys_combined

# svg
ggsave(
  filename = paste0("plots/sensitivity_source_combined_", DATE, ".svg"),
  plot = p_sys_combined,
  width = 16,
  height = 9
)

# tiff
ggsave(
  filename = paste0("plots/sensitivity_source_combined_", DATE, ".tiff"),
  width = 14,
  height = 8,
  dpi = 300
)

## source table <---------------------------------------------------------------
sens_sys_tab = as.data.table(sens_sys_res)

sens_sys_tab[, term2 := fifelse(
  term == "post", "beta_level",
  fifelse(term == "time_after", "beta_trend", NA_character_)
)]

sens_sys_tab = sens_sys_tab[
  !is.na(term2),
  .(Source = quelle, Outcome = as.character(outcome), term2, RR_txt)
]

sens_sys_tab_wide = dcast(
  sens_sys_tab,
  Source + Outcome ~ term2,
  value.var = "RR_txt")

sens_sys_tab_wide[, Outcome := factor(Outcome, levels = c(
  "Primary (adults)", "Primary (adolescents)",
  "Secondary 1", "Secondary 2"))]

setorder(sens_sys_tab_wide, Source, Outcome)

note_sens_sys = paste(
  "Note. Values are rate ratios with 95% confidence intervals.",
  "* p < 0.05, ** p < 0.01, *** p < 0.001.")

ft_sens_sys = sens_sys_tab_wide %>%
  flextable(col_keys = c("Source", "Outcome", "beta_level", "beta_trend")) %>%
  set_header_labels(
    Source = "Source",
    Outcome = "Outcome",
    beta_level = "level change: RR (95% CI)",
    beta_trend = "slope change: RR (95% CI)") %>%
  autofit() %>%
  add_footer_lines(values = note_sens_sys)

ft_sens_sys
# save_as_docx(ft_sens_sys, path = paste0("sens_table_model_results_", DATE, ".docx"))

## model information <----------------------------------------------------------
get_model_info = \(mod, model_name, source_name = NA_character_) {
  
  sm = summary(mod)
  
  # parametric terms
  pt = as.data.frame(sm$p.table)
  pt$term = rownames(pt)
  rownames(pt) = NULL
  
  names(pt) = c("Estimate", "Std_Error", "Statistic", "p_value", "term")
  
  pt = pt %>%
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "post" ~ "β_level",
        term == "time_after" ~ "β_trend",
        term == "t_idx" ~ "time index",
        T ~ term),
      Source = source_name,
      Model = model_name,
      n = nobs(mod),
      `adj. R²` = if (!is.null(sm$r.sq)) sm$r.sq else NA_real_) %>%
    select(
      Source, Model, n, term, Estimate, Std_Error, 
      Statistic, p_value, `adj. R²`)
  
  # smooth term
  if (!is.null(sm$s.table)) {
    st = as.data.frame(sm$s.table)
    st$term = rownames(st)
    rownames(st) = NULL
    
    nms = names(st)
    names(st)[grepl("edf", nms, ignore.case = T)][1] = "edf"
    names(st)[grepl("Ref.df", nms, ignore.case = T)][1] = "Ref_df"
    names(st)[grepl("^F$|Chi.sq", nms, ignore.case = T)][1] = "Statistic"
    names(st)[grepl("p-value", nms, ignore.case = T)][1] = "p_value"
    
    smooth_row = tibble(
      Source = source_name,
      Model = model_name,
      n = nobs(mod),
      term = "calendar week",
      Estimate = NA_real_,
      Std_Error = NA_real_,
      Statistic = st$Statistic[1],
      p_value = st$p_value[1],
      `adj. R²` = if (!is.null(sm$r.sq)) sm$r.sq else NA_real_) %>%
      select(Source, Model, n, term, Estimate, Std_Error, Statistic, p_value, `adj. R²`)
    
    pt = bind_rows(pt, smooth_row)
  }
  pt
}

## main model
main_model_info = bind_rows(
  get_model_info(prim_adult, "Primary (adults)"),
  get_model_info(prim_minor, "Primary (adolescents)"),
  get_model_info(sec_adult, "Secondary 1"),
  get_model_info(sec2_adult, "Secondary 2")
)
main_model_info = main_model_info[,-1]

ft_main_info = main_model_info %>%
  mutate(
    Estimate = round(Estimate, 4),
    Std_Error = round(Std_Error, 4),
    Statistic = round(Statistic, 3),
    `adj. R²` = round(`adj. R²`, 3),
    p_value = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      T ~ sprintf("%.3f", p_value))) %>%
  flextable() %>%
  set_header_labels(
    model = "Model",
    n = "n",
    term = "Term",
    Estimate = "Estimate",
    Std_Error = "Std. Error",
    Statistic = "Statistic",
    p_value = "p-value",
    `adj. R²` = "adj. R²") %>%
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1)) %>%
  border_inner_h(part = "all", border = fp_border(color = "black", width = 0.8)) %>%
  border_inner_v(part = "all", border = fp_border(color = "black", width = 0.8)) %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  autofit()
ft_main_info

#save_as_docx(
#  ft_main_info,
#  path = paste0("supp_table_main_model_info_", DATE, ".docx")
#)

## sensitivity info <-----------------------------------------------------------
sens_model_info = bind_rows(lapply(names(sens_model_list), \(nm) {
  parts = strsplit(nm, " \\| ")[[1]]
  src = parts[1]
  modlab = parts[2]
  
  get_model_info(
    mod = sens_model_list[[nm]],
    model_name = modlab,
    source_name = src)
}))

sens_model_info

ft_sens_model_info = sens_model_info %>%
  mutate(
    Model = case_when(
      Model == "Primary (adults)" ~ "P (adult)",
      Model == "Primary (adolescents)" ~ "P (minor)",
      Model == "Secondary 1" ~ "S1",
      Model == "Secondary 2" ~ "S2",
      T ~ Model),
    Estimate = round(Estimate, 4),
    Std_Error = round(Std_Error, 4),
    Statistic = round(Statistic, 3),
    `adj. R²` = round(`adj. R²`, 3),
    p_value = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      T ~ sprintf("%.3f", p_value))) %>%
  flextable() %>%
  set_header_labels(
    Source = "Source",
    Model = "Model",
    n = "n",
    term = "Term",
    Estimate = "Estimate",
    Std_Error = "Std. Error",
    Statistic = "Statistic",
    p_value = "p-value",
    `adj. R²` = "adj. R²") %>%
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1)) %>%
  border_inner_h(part = "all", border = fp_border(color = "black", width = 0.8)) %>%
  border_inner_v(part = "all", border = fp_border(color = "black", width = 0.8)) %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  autofit()

ft_sens_model_info

#save_as_docx(
#  ft_sens_model_info,
#  path = paste0("sens_table_full_model_info_", DATE, ".docx")
#)

## Placebo test <---------------------------------------------------------------
intervention_date = as.Date("2024-04-01")
placebo_shifts = seq(-20L, 20L, by = 2L)

get_t0 = \(d) {
  t0 = d %>%
    filter(week_start <= intervention_date & intervention_date <= week_end) %>%
    summarize(t0 = min(t_idx)) %>%
    pull(t0)
  stopifnot(length(t0) == 1, !is.na(t0))
  t0
}

t0_adult = get_t0(itsadult)
t0_minor = get_t0(itsminor)
t0_intox = get_t0(itsintox)
t0_psych = get_t0(itspsych)

## helper fcts
run_placebo = \(data, t0, outcome_label, y_var, k_woy) {
  
  res = data.table()
  
  for (sh in placebo_shifts) {
    
    t0j = t0 + sh
    if (t0j < 1L || t0j > nrow(data)) next
    
    d_run = data %>%
      mutate(
        post = as.integer(t_idx >= t0j),
        time_after = if_else(t_idx < t0j, 0L, pmax(0L, t_idx - t0j)))
    
    form = as.formula(
      paste0(
        y_var,
        " ~ post + time_after + t_idx + ",
        "s(woy, bs = 'cc', k = ", k_woy, ") + ",
        "offset(log(N_all))"))
    
    m = gam(
      formula = form,
      family = nb(link = "log"),
      data = d_run,
      method = "REML",
      knots = list(woy = c(0.5, 52.5)))
    
    r_post = get_rr(m, "post")
    r_ta   = get_rr(m, "time_after")
    
    res = rbind(
      res,
      data.table(
        outcome = outcome_label,
        shift_weeks = sh,
        term = "post",
        RR = r_post["RR"],
        RR_l = r_post["RR_l"],
        RR_u = r_post["RR_u"]),
      data.table(
        outcome = outcome_label,
        shift_weeks = sh,
        term = "time_after",
        RR = r_ta["RR"],
        RR_l = r_ta["RR_l"],
        RR_u = r_ta["RR_u"]))
  }
  res
}

## run placebo models
placebo_res = rbindlist(list(
  run_placebo(
    data = itsadult,
    t0 = t0_adult,
    outcome_label = "Primary (adults)",
    y_var = "Y_primary",
    k_woy = 30),
  run_placebo(
    data = itsminor,
    t0 = t0_minor,
    outcome_label = "Primary (adolescents)",
    y_var = "Y_primary",
    k_woy = 50),
  run_placebo(
    data = itsintox,
    t0 = t0_intox,
    outcome_label = "Secondary 1",
    y_var = "Y_intox",
    k_woy = 52),
  run_placebo(
    data = itspsych,
    t0 = t0_psych,
    outcome_label = "Secondary 2",
    y_var = "Y_psych",
    k_woy = 20)), use.names = T)

placebo_res = placebo_res %>%
  mutate(
    outcome = factor(outcome, levels = outcome_levels),
    term = factor(term, levels = c("post", "time_after"))) %>%
  arrange(outcome, shift_weeks, term)

placebo_res

## plots: level change
p_placebo_level = ggplot(
  placebo_res %>% filter(term == "post"),
  aes(x = shift_weeks, y = RR, colour = outcome)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = RR_l, ymax = RR_u), width = 0.5) +
  geom_point() +
  geom_line(aes(group = outcome)) +
  facet_wrap(~ outcome, ncol = 2) +
  scale_x_continuous(breaks = placebo_shifts) +
  labs(
    x = "Weeks relative to legalisation (0 = 01/04/2024)",
    y = "Rate ratio") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13))

p_placebo_level

# svg
ggsave(
  filename = paste0("plots/placebo_level_", DATE, ".svg"),
  plot = p_placebo_level,
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/placebo_level_", DATE, ".tiff"),
  plot = p_placebo_level,
  width = 14,
  height = 8,
  dpi = 300
)

## plots: slope change
p_placebo_trend = ggplot(
  placebo_res %>% filter(term == "time_after"),
  aes(x = shift_weeks, y = RR, colour = outcome)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = RR_l, ymax = RR_u), width = 0.5) +
  geom_point() +
  geom_line(aes(group = outcome)) +
  facet_wrap(~ outcome, ncol = 2) +
  scale_x_continuous(breaks = placebo_shifts) +
  labs(
    x = "Weeks relative to legalisation (0 = 01/04/2024)",
    y = "Rate ratio") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13))

p_placebo_trend

# svg
ggsave(
  filename = paste0("plots/placebo_trend_", DATE, ".svg"),
  plot = p_placebo_trend,
  width = 14,
  height = 8
)

# tiff
ggsave(
  filename = paste0("plots/placebo_trend_", DATE, ".tiff"),
  plot = p_placebo_trend,
  width = 14,
  height = 8,
  dpi = 300
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 9) SUPPLEMENT
# ______________________________________________________________________________________________________________________

## ACF, pACF, QQ plots <--------------------------------------------------------
labs = c(
  primary_adult = "Primary (adults)",
  primary_minor = "Primary (adolescents)",
  secondary1 = "Secondary 1",
  secondary2 = "Secondary 2"
)

mods = list(
  primary_adult = prim_adult,
  primary_minor = prim_minor,
  secondary1 = sec_adult,
  secondary2 = sec2_adult
)

# get residuals
get_rsd = \(m, outcome_name) {
  if (outcome_name == "secondaryA") {
    rsd = m$std.rsd
    if (is.null(rsd)) rsd = residuals(m, type = "pearson")
  } else {
    rsd = residuals(m, type = "pearson")
  }
  rsd
}

max_lag = 24 # months
dir.create("plots", showWarnings = F, recursive = T)

## ACF
# svg
svglite(
  file = paste0(
    "plots/ACF_primary_analysis_", DATE, ".svg"),
  width = 12, height = 7
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  acf(rsd, lag.max = max_lag, na.action = na.pass, 
      main = paste0("ACF: ", labs[[nm]]))
}

par(op)
dev.off()

# tiff
tiff(
  filename = paste0(
    "plots/ACF_primary_analysis_", DATE, ".tiff"),
  width = 12, height = 7, units = "in", res = 300
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  acf(rsd, lag.max = max_lag, na.action = na.pass,
      main = paste0("ACF: ", labs[[nm]]))
}

par(op)
dev.off()

## pACF
# svg
svglite(
  file = paste0(
    "plots/pACF_primary_analysis_", DATE, ".svg"),
  width = 12, height = 7
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  pacf(rsd, lag.max = max_lag, na.action = na.pass, 
       main = paste0("pACF: ", labs[[nm]]))
}

par(op)
dev.off()

# tiff
tiff(
  filename = paste0(
    "plots/pACF_primary_analysis_", DATE, ".tiff"),
  width = 12, height = 7, units = "in", res = 300
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  pacf(rsd, lag.max = max_lag, na.action = na.pass,
       main = paste0("pACF: ", labs[[nm]]))
}

par(op)
dev.off()

## QQ 
# svg
svglite(
  file = paste0(
    "plots/QQ_primary_analysis_", DATE, ".svg"),
  width = 12, height = 7
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  qqnorm(rsd, main = paste0("QQ: ", labs[[nm]]))
  qqline(rsd)
}

par(op)
dev.off()

# tiff
tiff(
  filename = paste0(
    "plots/QQ_primary_analysis_", DATE, ".tiff"),
  width = 12, height = 7, units = "in", res = 300
)

op = par(no.readonly = T)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (nm in names(mods)) {
  rsd = get_rsd(mods[[nm]], nm)
  qqnorm(rsd, main = paste0("QQ: ", labs[[nm]]))
  qqline(rsd)
}

par(op)
dev.off()

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 10) SUPPLEMENTARY TABLE 1
# ______________________________________________________________________________________________________________________
supp_exclusion = data.table(
  Analysis_group = c(
    "Primary outcome",
    "Secondary outcome 1",
    "Secondary outcome 2",
    "Tertiary outcomes"),
  Population = c(
    "Adults and adolescents",
    "Adults only",
    "Adults only",
    "Adults only"),
  Weeks_removed_end_of_series = c(
    "2025-W38 to 2025-W39",
    "2025-W38 to 2025-W39",
    "2025-W38 to 2025-W39",
    "2025-W32 to 2025-W39"),
  N_removed_weeks = c(2L, 2L, 2L, 8L))

supp_exclusion

note_suppl1 = paste(
  "Note. Because the public InEK data only appear to cover completed hospitalisations,",
  "end-of-series observations may become increasingly incomplete for diagnoses with longer",
  "lengths of stay. To determine an appropriate end-of-series restriction for these",
  "additional analyses, we reviewed diagnosis-group-specific length-of-stay information",
  "from separate InEK extracts for early 2025 and for August/September 2024.",
  "Based on these supplementary data, we conservatively excluded the final weeks shown above",
  "from the respective analyses.")

ft_suppl1 = supp_exclusion %>%
  flextable(col_keys = c(
    "Analysis_group",
    "Population",
    "Weeks_removed_end_of_series",
    "N_removed_weeks")) %>%
  set_header_labels(
    Analysis_group = "Analysis group",
    Population = "Population",
    Weeks_removed_end_of_series = "Excluded calendar weeks in 2025",
    N_removed_weeks = "No. of excluded weeks") %>%
  autofit() %>%
  add_footer_lines(values = note_suppl1)

ft_suppl1
save_as_docx(ft_suppl1, path = paste0("supp_table_exclusion_results_", DATE, ".docx"))
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 11) SUPPLEMENTARY TABLE 3
# ______________________________________________________________________________________________________________________
all_codes_main = tibble(
  icd_code = c(paste0("F12.", 0:9), "T40.7")
)

supp_diag_main = all_codes_main %>%
  left_join(
    dat2 %>%
      filter(
        subset == "all",
        icd_code %in% c(paste0("F12.", 0:9), "T40.7"),
        !(jahr == 2025 & kw %in% c(38:39))) %>%
      group_by(icd_code) %>%
      summarize(
        Total = sum(fallzahl, na.rm = T),
        Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
        Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
        DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
        PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T),
        .groups = "drop"), by = "icd_code") %>%
  mutate(
    Section = "Primary/secondary outcomes",
    Diagnosis = icd_code) %>%
  select(Section, Diagnosis, Total, Adolescents, Adults, DRG, PEPP)

## tertiary outcomes: grouped control diagnoses
supp_diag_ter = bind_rows(
  dat2 %>%
    filter(
      subset == "all",
      str_detect(icd_code, "^F2"),
      !(jahr == 2025 & kw %in% c(32:39))) %>%
    summarize(
      Total = sum(fallzahl, na.rm = T),
      Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
      Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
      DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
      PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T)) %>%
    mutate(Diagnosis = "Schizophrenia: F2x"),
  
  dat2 %>%
    filter(
      subset == "all",
      str_detect(icd_code, "^F10") | str_detect(icd_code, "^T51"),
      !(jahr == 2025 & kw %in% c(32:39))) %>%
    summarize(
      Total = sum(fallzahl, na.rm = T),
      Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
      Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
      DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
      PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T)) %>%
    mutate(Diagnosis = "Alcohol: F10; T51"),
  
  dat2 %>%
    filter(
      subset == "all",
      str_detect(icd_code, "^F40") |
        str_detect(icd_code, "^F41") |
        icd_code %in% c("F48.8", "F48.9"),
      !(jahr == 2025 & kw %in% c(32:39))) %>%
    summarize(
      Total = sum(fallzahl, na.rm = T),
      Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
      Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
      DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
      PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T)) %>%
    mutate(Diagnosis = "Anxiety: F40-F41; F48.8; F48.9"),
  
  dat2 %>%
    filter(
      subset == "all",
      str_detect(icd_code, "^F32") |
        str_detect(icd_code, "^F33"),
      !(jahr == 2025 & kw %in% c(32:39))) %>%
    summarize(
      Total = sum(fallzahl, na.rm = T),
      Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
      Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
      DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
      PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T)) %>%
    mutate(Diagnosis = "Depression: F32-F33")) %>%
  mutate(Section = "Tertiary outcomes") %>%
  select(Section, Diagnosis, Total, Adolescents, Adults, DRG, PEPP)

## combine
supp_diag = bind_rows(supp_diag_main, supp_diag_ter)
supp_diag

