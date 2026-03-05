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

## join 
dt = all %>%
  left_join(y_prim, by = key_vars) %>%
  left_join(y_intox, by = key_vars) %>%
  left_join(y_psy, by = key_vars) %>%
  left_join(y_dep, by = key_vars) %>%
  mutate(
    Y_primary = replace_na(Y_primary, 0L),
    Y_intox = replace_na(Y_intox, 0L),
    Y_psych = replace_na(Y_psych, 0L),
    Y_dep = replace_na(Y_dep, 0L))

## check up
dt_check = dt %>%
  filter(!(jahr == 2025 & kw %in% c(20:21))) %>%
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
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 observations

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
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 observations

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
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 observations

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
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 observations

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
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 observations

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
fmt_p = \(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}

get_term_rr = \(mod, term) {
  cf = coef(mod)
  V = vcov(mod)
  b = unname(cf[term])
  se = sqrt(unname(V[term, term]))
  rr = exp(b)
  l = exp(b - 1.96 * se)
  u = exp(b + 1.96 * se)
  sprintf("%.3f (%.3f–%.3f)", rr, l, u)
}

get_term_se = \(mod, term) {
  V = vcov(mod)
  se = sqrt(unname(V[term, term]))
  sprintf("%.3f", se)
}

get_swoy_p = \(mod) {
  st = summary(mod)$s.table
  if (is.null(st) || nrow(st) == 0) return(NA_character_)
  rn = rownames(st)
  i = grep("s\\(woy", rn)
  if (length(i) == 0) return(NA_character_)
  fmt_p(st[i[1], "p-value"])
}

##
tab2 = data.table(
  Outcome = c(
    "Primary (adults)",
    "Primary (adolescents)",
    "Secondary 1",
    "Secondary 2"),
  n = c(
    nobs(prim_adult),
    nobs(prim_minor),
    nobs(sec_adult),
    nobs(sec2_adult)),
  β_level = c(
    get_term_rr(prim_adult, "post"),
    get_term_rr(prim_minor, "post"),
    get_term_rr(sec_adult, "post"),
    get_term_rr(sec2_adult, "post")),
  β_trend = c(
    get_term_rr(prim_adult, "time_after"),
    get_term_rr(prim_minor, "time_after"),
    get_term_rr(sec_adult, "time_after"),
    get_term_rr(sec2_adult, "time_after"))
)

ft2 = tab2 %>%
  flextable(col_keys = c("Outcome", "n", "β_level", "β_trend")) %>%
  set_header_labels(
    Outcome = "Outcome",
    n = "n",
    beta_level = "β_level: RR (95% CI)",
    beta_trend = "β_trend: RR (95% CI)") %>%
  autofit()

ft2
# save_as_docx(ft2, path = paste0("Table_model_results_", DATE, ".docx"))

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) TABLE 1: Descriptives
# ______________________________________________________________________________________________________________________
dt0 = copy(dt) %>%
  filter(!(jahr == 2025 & kw %in% c(20:21))) # remove last 2 obs.

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
names(input2)
names(dt)
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
    filter(!(jahr == 2025 & kw %in% c(20:21))) %>%
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
    knots = list(woy = c(0.5, 52.5))
  )
  sens_model_list[[paste(q, "Primary (adults)", sep = " | ")]] = m_ad
  r_post = get_rr(m_ad, "post")
  r_ta   = get_rr(m_ad, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(quelle = q, outcome = "Primary (adults)", term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], RR_u = r_post["RR_u"]),
    data.table(quelle = q, outcome = "Primary (adults)", term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
  
  # ---------------- primary adolescents ----------------
  d_mi = dt_sys %>%
    filter(quelle == q, altergruppe == "minor") %>%
    filter(!(jahr == 2025 & kw %in% c(20:21))) %>%
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
    knots = list(woy = c(0.5, 52.5))
  )
  sens_model_list[[paste(q, "Primary (adolescents)", sep = " | ")]] = m_mi
  r_post = get_rr(m_mi, "post")
  r_ta = get_rr(m_mi, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(quelle = q, outcome = "Primary (adolescents)", term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(quelle = q, outcome = "Primary (adolescents)", 
               term = "time_after", RR = r_ta["RR"], RR_l = r_ta["RR_l"], 
               RR_u = r_ta["RR_u"])
  )
  
  # ---------------- secondary 1 ----------------
  m_s1 = gam(
    Y_intox ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 52) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_ad,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  sens_model_list[[paste(q, "Secondary 1", sep = " | ")]] = m_s1
  r_post = get_rr(m_s1, "post")
  r_ta = get_rr(m_s1, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(quelle = q, outcome = "Secondary 1", term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(quelle = q, outcome = "Secondary 1", term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
  
  # ---------------- secondary 2 ----------------
  m_s2 = gam(
    Y_psych ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 20) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_ad,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  sens_model_list[[paste(q, "Secondary 2", sep = " | ")]] = m_s2
  r_post = get_rr(m_s2, "post")
  r_ta = get_rr(m_s2, "time_after")
  
  sens_sys_res = rbind(
    sens_sys_res,
    data.table(quelle = q, outcome = "Secondary 2", term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(quelle = q, outcome = "Secondary 2", term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
}

sens_sys_res = sens_sys_res %>%
  mutate(
    RR_txt = sprintf("%.3f (%.3f–%.3f)", RR, RR_l, RR_u),
    outcome = factor(outcome, levels = outcome_levels)
  ) %>%
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
  fifelse(term == "time_after", "beta_trend", NA_character_))]

# keep only the two terms we want
sens_sys_tab = sens_sys_tab[
  !is.na(term2), .(Source = quelle, 
                   Outcome = as.character(outcome), term2, RR_txt)]

# wide
sens_sys_tab_wide = dcast(
  sens_sys_tab,
  Source + Outcome ~ term2,
  value.var = "RR_txt"
)

# order
sens_sys_tab_wide[, Outcome := factor(Outcome, levels = c(
  "Primary (adults)",
  "Primary (adolescents)",
  "Secondary 1",
  "Secondary 2"
))]
setorder(sens_sys_tab_wide, Source, Outcome)

ft_sens_sys = sens_sys_tab_wide %>%
  flextable(col_keys = c("Source", "Outcome", "beta_level", "beta_trend")) %>%
  set_header_labels(
    Source = "Source",
    Outcome = "Outcome",
    beta_level = "\u03B2_level: RR (95% CI)",
    beta_trend = "\u03B2_trend: RR (95% CI)"
  ) %>%
  autofit()

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
  autofit(); ft_main_info

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

placebo_res = data.table()

# ---- Primary adults ----
for (sh in placebo_shifts) {
  
  t0j = t0_adult + sh
  if (t0j < 1L || t0j > nrow(itsadult)) next
  
  d_run = itsadult %>%
    mutate(
      post = as.integer(t_idx >= t0j),
      time_after = if_else(t_idx < t0j, 0L, pmax(0L, t_idx - t0j)))
  
  m = gam(
    Y_primary ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 30) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_run,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  
  r_post = get_rr(m, "post")
  r_ta = get_rr(m, "time_after")
  
  placebo_res = rbind(
    placebo_res,
    data.table(outcome = "Primary (adults)", 
               shift_weeks = sh, term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(outcome = "Primary (adults)", 
               shift_weeks = sh, term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
}

# ---- Primary adolescents ----
for (sh in placebo_shifts) {
  
  t0j = t0_minor + sh
  if (t0j < 1L || t0j > nrow(itsminor)) next
  
  d_run = itsminor %>%
    mutate(
      post = as.integer(t_idx >= t0j),
      time_after = if_else(t_idx < t0j, 0L, pmax(0L, t_idx - t0j)))
  
  m = gam(
    Y_primary ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 50) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_run,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  
  r_post = get_rr(m, "post")
  r_ta = get_rr(m, "time_after")
  
  placebo_res = rbind(
    placebo_res,
    data.table(outcome = "Primary (adolescents)", 
               shift_weeks = sh, term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(outcome = "Primary (adolescents)", 
               shift_weeks = sh, term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
}

# ---- Secondary 1 (adults) ----
for (sh in placebo_shifts) {
  
  t0j = t0_intox + sh
  if (t0j < 1L || t0j > nrow(itsintox)) next
  
  d_run = itsintox %>%
    mutate(
      post = as.integer(t_idx >= t0j),
      time_after = if_else(t_idx < t0j, 0L, pmax(0L, t_idx - t0j)))
  
  m = gam(
    Y_intox ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 52) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_run,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  
  r_post = get_rr(m, "post")
  r_ta = get_rr(m, "time_after")
  
  placebo_res = rbind(
    placebo_res,
    data.table(outcome = "Secondary 1", 
               shift_weeks = sh, term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(outcome = "Secondary 1", 
               shift_weeks = sh, term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
}

# ---- Secondary 2 (adults) ----
for (sh in placebo_shifts) {
  
  t0j = t0_psych + sh
  if (t0j < 1L || t0j > nrow(itspsych)) next
  
  d_run = itspsych %>%
    mutate(
      post = as.integer(t_idx >= t0j),
      time_after = if_else(t_idx < t0j, 0L, pmax(0L, t_idx - t0j)))
  
  m = gam(
    Y_psych ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 20) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = d_run,
    method = "REML",
    knots = list(woy = c(0.5, 52.5))
  )
  
  r_post = get_rr(m, "post")
  r_ta = get_rr(m, "time_after")
  
  placebo_res = rbind(
    placebo_res,
    data.table(outcome = "Secondary 2", 
               shift_weeks = sh, term = "post",
               RR = r_post["RR"], RR_l = r_post["RR_l"], 
               RR_u = r_post["RR_u"]),
    data.table(outcome = "Secondary 2", 
               shift_weeks = sh, term = "time_after",
               RR = r_ta["RR"], RR_l = r_ta["RR_l"], RR_u = r_ta["RR_u"])
  )
}

placebo_res = placebo_res %>%
  mutate(outcome = factor(outcome, levels = outcome_levels)) %>%
  arrange(outcome, shift_weeks, term)

## plots
p_placebo_level = ggplot(
  placebo_res %>% filter(term == "post"),
  aes(x = shift_weeks, y = RR, colour = outcome)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = RR_l, ymax = RR_u), width = 0.4) +
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
    strip.text = element_text(size = 13)); p_placebo_level

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
  width = 14,
  height = 8,
  dpi = 300
)

p_placebo_trend = ggplot(
  placebo_res %>% filter(term == "time_after"),
  aes(x = shift_weeks, y = RR, colour = outcome)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = RR_l, ymax = RR_u), width = 0.4) +
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
    strip.text = element_text(size = 13)); p_placebo_trend

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

# 10) SUPPLEMENTARY TABLE 2
# ______________________________________________________________________________________________________________________
supp_diag = dat2 %>%
  filter(
    icd_code %in% c(paste0("F12.", 0:9), "T40.7"),
    !(jahr == 2025 & kw %in% c(20:21))) %>%
  group_by(icd_code, diagnose) %>%
  summarize(
    Total = sum(fallzahl, na.rm = T),
    Adolescents = sum(fallzahl[altergruppe == "minor"], na.rm = T),
    Adults = sum(fallzahl[altergruppe == "adult"], na.rm = T),
    DRG = sum(fallzahl[quelle == "DRG"], na.rm = T),
    PEPP = sum(fallzahl[quelle == "PEPP"], na.rm = T),
    .groups = "drop") %>%
  arrange(icd_code)

supp_diag

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# X) REST
# ______________________________________________________________________________________________________________________

all_source = dat3 %>% # differentiation between DRG & PEPP
  filter(subset == "all") %>%
  group_by(across(all_of(c("jahr", "kw", "altergruppe", "quelle")))) %>%
  summarize(N_all = sum(fallzahl, na.rm = T), .groups = "drop") %>%
  mutate(N_all = as.integer(N_all))


prim_adultAR = gamm(
  Y_primary ~ post + time_after + t_idx + 
    s(woy, bs = "cc", k = 52) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsdat,
  method = "REML",
  knots = list(woy = c(0.5, 52.5)),
  correlation = corAR1(form = ~ t_idx)
)


## old
## ITS GAM
prim_adult = gam(
  Y_primary ~ post + time_after + 
    s(t_idx, k = 20) + 
    s(woy, bs = "cc", k = 20) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsdat,
  method = "REML",
  knots = list(woy = c(0.5, 52.5))
)

## extract coefficients
cf = coef(prim_adult); V = vcov(prim_adult); se = sqrt(diag(V))


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

## tests
cf = coef(prim_adult); V = vcov(prim_adult)

eff_h = \(h) {
  b  = cf["post"] + h * cf["time_after"]
  se = sqrt(
    V["post","post"] +
      h^2 * V["time_after","time_after"] +
      2 * h * V["post","time_after"]
  )
  tibble(
    h_weeks = h,
    RR = exp(b),
    RR_l = exp(b - 1.96 * se),
    RR_u = exp(b + 1.96 * se)
  )
}

bind_rows(eff_h(0), eff_h(13), eff_h(26), eff_h(52))
## ITS BAM <--------------------------------------------------------------------
## grid over rho
rhos = seq(0, 0.6, by = 0.01)

fit_rho = \(rho) {
  AR_start = c(T, rep(F, nrow(itsdat) - 1L))
  m = bam(
    Y_primary ~ post + time_after + t_idx +
      s(woy, bs = "cc", k = 50) +
      offset(log(N_all)),
    family = nb(link = "log"),
    data = itsdat,
    method = "fREML",
    discrete = T,
    rho = rho,
    AR.start = AR_start,
    knots = list(woy = c(0.5, 52.5))
  )
  
  rsd = m$std.rsd
  if (is.null(rsd)) rsd = residuals(m, type = "pearson")
  a1 = as.numeric(acf(rsd, plot = F, na.action = na.pass)$acf[2])
  c(
    rho = rho, 
    acf1 = a1, 
    post = coef(m)["post"], 
    time_after = coef(m)["time_after"])
}

res = t(sapply(rhos, fit_rho))
res = as.data.frame(res)

## choose rho which minimizes AR1
rho = res[which.min(abs(res$acf1)), ]$rho

AR_start = c(T, rep(F, nrow(itsdat) - 1L))

## ITS BAM with AR1
prim_adult_bamAR = bam(
  Y_primary ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 50) +
    offset(log(N_all)),
  family = nb(link = "log"),
  data = itsdat,
  method = "fREML",
  rho = rho,
  discrete = T,
  AR.start = AR_start,
  knots = list(woy = c(0.5, 52.5))
)

cf2 = coef(prim_adult_bamAR); V2 = vcov(prim_adult_bamAR); se2 = sqrt(diag(V2))

eff_h2 = \(h) {
  b  = cf2["post"] + h * cf2["time_after"]
  se = sqrt(V2["post","post"] + h ^ 2 * V2["time_after","time_after"] + 
              2 * h * V2["post","time_after"])
  tibble(
    h_weeks = h, 
    RR = exp(b), 
    RR_l = exp(b - 1.96 * se), 
    RR_u = exp(b + 1.96 * se))
}

bind_rows(eff_h2(0), eff_h2(13), eff_h2(26), eff_h2(52))

## rate ratios
rr_adult_bam = tibble(
  term = names(cf2),
  est = cf2, 
  se = se2,
  lcl = cf2 - 1.96 * se,
  ucl = cf2 + 1.96 * se) %>%
  mutate(
    RR = exp(est),
    RR_l = exp(lcl),
    RR_r = exp(ucl)) %>%
  filter(term %in% c("post", "time_after"))

rr_adult_bam

coef(prim_adult_bamAR)[c("t_idx","post","time_after")]
gam.check(prim_adult_bamAR)

acf(prim_adult_bamAR$std.rsd, plot = F, na.action = na.pass)$acf[1:8]
pacf(prim_adult_bamAR$std.rsd, plot = F, na.action = na.pass)$acf[1:8]
concurvity(prim_adult_bamAR, full = T)
summary(prim_adult_bamAR)


## ACF of residuals
acf(
  prim_adult_bamAR$std.rsd, 
  na.action = na.pass, 
  main = "ACF: std.rsd (BAM, AR1)")
pacf(
  prim_adult_bamAR$std.rsd, 
  na.action = na.pass, 
  main = "pACF: std.rsd (BAM, AR1)")

## Ljung-Box 
Box.test(prim_adult_bamAR$std.rsd, lag = 5, type = "Ljung-Box") # p=0.56
Box.test(prim_adult_bamAR$std.rsd, lag = 8, type = "Ljung-Box") # p=0.79
Box.test(prim_adult_bamAR$std.rsd, lag = 12, type = "Ljung-Box") # p=0.95
Box.test(prim_adult_bamAR$std.rsd, lag = 35, type = "Ljung-Box") # p=0.88

##
itsdat_b = itsdat %>%
  mutate(mu_hat = predict(prim_adult_bamAR, type = "response"))

itsdat_cf = itsdat_b
itsdat_cf$post[itsdat_cf$t_idx >= t0] = 0L
itsdat_cf$time_after[itsdat_cf$t_idx >= t0] = 0L

itsdat_b = itsdat_b %>%
  mutate(mu_cf = predict(prim_adult_bamAR, newdata = itsdat_cf, type = "response")) %>%
  mutate(
    rate_obs = 1000 * Y_primary / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf  = 1000 * mu_cf  / N_all
  )

ggplot(itsdat_b, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf),  linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate per 1,000 all-cause intakes",
    #title = "ITS-GAM AR(1): Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/BAM_InEK_primary_AR1_", DATE, ".svg"),
  width = 14,
  height = 8
)

## ITS BAM AR1 <----------------------------------------------------------------
prim_adultAR = gamm(
  Y_primary ~ post + time_after + t_idx +
    s(woy, bs = "cc", k = 52) +
    offset(log(N_all)),
  family = quasipoisson(link="log"),
  data = itsdat,
  correlation = corAR1(form = ~ t_idx),
  knots = list(woy = c(0.5, 52.5))
)

summary(prim_adultAR$gam)

intervals(prim_adultAR$lme)$corStruct
# oder:
summary(prim_adultAR$lme)

summary(prim_adultAR$gam)$p.table[c("post", "time_after"), "Estimate"]

# NB-AR1
b  = summary(prim_adultAR$gam)$p.table["post","Estimate"]
se = summary(prim_adultAR$gam)$p.table["post","Std. Error"]
c(RR=exp(b), L=exp(b-1.96*se), U=exp(b+1.96*se))


# Koeffizienten
coef(prim_adultAR$gam)[c("t_idx","post","time_after")]
summary(prim_adultAR$gam)$p.table[c("t_idx","post","time_after"), ]

r_gam = residuals(prim_adultAR$gam, type = "pearson")
acf(r_gam, na.action = na.pass, main = "ACF: Pearson-Residuen (gamm$gam)")

r_lme = resid(prim_adultAR$lme, type = "normalized")
acf(r_lme, na.action = na.pass, main = "ACF: normalisierte Residuen (gamm$lme, AR1)")

## fitted values (AR1-model via gamm)
itsdat2 = itsdat %>%
  mutate(mu_hat = predict(prim_adultAR$gam, type = "response"))

## counterfactual (post=0, time_after=0 ab t0)
itsdat_cf = itsdat2
itsdat_cf$post[itsdat_cf$t_idx >= t0] = 0L
itsdat_cf$time_after[itsdat_cf$t_idx >= t0] = 0L

## counterfactual predictions + rates
itsdat2 = itsdat2 %>%
  mutate(
    mu_cf = predict(prim_adultAR$gam, newdata = itsdat_cf, type = "response")
  ) %>%
  mutate(
    rate_obs = 1000 * Y_primary / N_all,
    rate_hat = 1000 * mu_hat / N_all,
    rate_cf  = 1000 * mu_cf  / N_all
  )

## plot
ggplot(itsdat2, aes(x = week_start)) +
  geom_vline(xintercept = intervention_date) +
  geom_line(aes(y = rate_obs), linewidth = 0.7, alpha = 0.35, color = pcol) +
  geom_line(aes(y = rate_hat), linewidth = 1.0, alpha = 1, color = pcol) +
  geom_line(aes(y = rate_cf),  linewidth = 1.0, linetype = "22", alpha = 1, color = pcol) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x = "",
    y = "Rate pro 1 000 all-cause Aufnahmen",
    title = "ITS-GAM AR(1): Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/GAM_InEK_primary_AR1_", DATE, ".svg"),
  width = 14,
  height = 8
)

## old plots <------------------------------------------------------------------
p_sys_level = ggplot(
  sens_sys_res %>% filter(term == "post"),
  aes(x = quelle, y = RR, ymin = RR_l, ymax = RR_u, colour = outcome)
) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(
    x = "Reimbursement system",
    y = "Rate ratio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); p_sys_level

ggsave(
  filename = paste0("plots/sensitivity_source_level_", DATE, ".svg"),
  plot = p_sys_level,
  width = 14,
  height = 8
)


p_sys_trend = ggplot(
  sens_sys_res %>% filter(term == "time_after"),
  aes(x = quelle, y = RR, ymin = RR_l, ymax = RR_u, colour = outcome)
) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  labs(
    x = "Reimbursement system",
    y = "Rate ratio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  ); p_sys_trend

ggsave(
  filename = paste0("plots/sensitivity_source_trend_", DATE, ".svg"),
  plot = p_sys_trend,
  width = 14,
  height = 8
)