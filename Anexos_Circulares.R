library(tidyverse)
library(openxlsx)
library(readr)
library(readxl)

options(scipen = 999)
options(digits=7)

#### conexiones versión database
contabilidad <- DBI::dbConnect(odbc::odbc(),
                               Driver   = "ODBC Driver 17 for SQL Server",
                               Server   = "192.168.8.14",
                               Database = "CMUNDIAL",
                               UID      = "danny2",
                               PWD      = "ReadyLove100*",
                               Port     = 1433)


con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 17 for SQL Server",
                      Server   = "172.30.149.67",
                      Database = "Sis2000",
                      UID      = "dmorales",
                      PWD      = "lamundial*2025*morales",
                      Port     = 1433)