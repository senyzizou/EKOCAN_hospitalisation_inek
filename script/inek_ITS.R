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
  "lubridate", "dplyr", "tidyr", "ISOweek", "stringr")

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

## join 
dt = all %>%
  left_join(y_prim, by = key_vars) %>%
  left_join(y_intox, by = key_vars) %>%
  left_join(y_psy, by = key_vars) %>%
  mutate(
    Y_primary = replace_na(Y_primary, 0L),
    Y_intox = replace_na(Y_intox, 0L),
    Y_psych = replace_na(Y_psych, 0L))

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
    y = "Rate per 1,000 all-cause intakes",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
    ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/GAM_InEK_primary_", DATE, ".svg"),
  width = 14,
  height = 8
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
    y = "Rate per 1,000 all-cause intakes",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/GAM_InEK_primary_minor_", DATE, ".svg"),
  width = 14,
  height = 8
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) ITS: secondary analysis 1: F12.0 + T40.7 rate
# ______________________________________________________________________________________________________________________
itsintox = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(20:21))) %>% # remove last 2 observations
  select(-Y_primary, -Y_psych)

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
gam.check(sec_adult)

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
    y = "Rate per 1,000 all-cause intakes",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/GAM_InEK_secondary_intox_", DATE, ".svg"),
  width = 14,
  height = 8
)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) ITS: secondary analysis 2: F12.5 rate
# ______________________________________________________________________________________________________________________
itspsych = copy(dt) %>%
  filter(altergruppe == "adult") %>%
  filter(!(jahr == 2025 & kw %in% c(20:21))) %>% # remove last 2 observations
  select(-Y_primary, -Y_intox)

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
gam.check(sec2_adult)

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
    y = "Rate per 1,000 all-cause intakes",
    #title = "ITS-GAM: Raten cannabisbezogener Hauptdiagnosen bei Erwachsenen",
    #subtitle = "Beobachtet (transparent), fitted (fett), Verlauf ohne Intervention (gestrichelt)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13)
  )

ggsave(
  filename = paste0("plots/GAM_InEK_secondary_psych_", DATE, ".svg"),
  width = 14,
  height = 8
)

rr_adult; rr_adult_sec; rr_adult_sec2
prim_adult_pt; sec_pt; sec2_pt

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
