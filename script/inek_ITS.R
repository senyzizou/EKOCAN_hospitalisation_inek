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
  "data.table", "ggplot2", "ggthemes", "Hmisc", "mgcv", "DBI", "RMariaDB")

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

dat1 = data.table(dbGetQuery(con, "SELECT * FROM usr_p117892_3.destatis_b2;"))
pdat = copy(dat1[!is.na(value)])

