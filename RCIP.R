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

#### Extraigo las partidas específicas para el cálculo

cuentas <- tbl(contabilidad, "SCCUENTA") |> 
  collect()

saldos <- tbl(contabilidad, "SCREN_CO") |> 
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-15")) |> 
  collect()


Contabilidad <- left_join(saldos, cuentas, by = "co_cue")

Contabilidad_consolidada <- Contabilidad |> 
  mutate(saldo = monto_d - monto_h,
         nro_recibo = str_extract(descri, "(?<=Nro_Recibo\\s|RECIBO\\s)[0-9-]+")) |> 
  select(co_cue, des_cue, nro_recibo, fec_emis, descri, monto_d, monto_h, saldo)

###### Ingresos Técnicos#########
prima_bruta <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2025-01-01"),
         fec_emis <= as.Date("2025-12-31")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Prima Bruta` = sum(abs(saldo))) |> 
  drop_na(ramo)

rrc <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2025-01-01"),
         fec_emis <= as.Date("2025-12-31")) |>
  mutate(ramo = str_extract(des_cue, "(?<=RESERVA DE RIESGOS EN CURSO -\\s|RESERVAS DE PRIMAS -\\s|RESERV. RIESGOS EN CURSO -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Reserva de Riesgo en Curso` = sum(abs(saldo))) |> 
  drop_na(ramo)

rrc_ant <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2024-01-01"),
         fec_emis <= as.Date("2024-12-31")) |>
  mutate(ramo = str_extract(des_cue, "(?<=RESERVA DE RIESGOS EN CURSO -\\s|RESERVAS DE PRIMAS -\\s|RESERV. RIESGOS EN CURSO -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Reserva de Riesgo en Curso Anterior` = sum(abs(saldo))) |> 
  drop_na(ramo)


prima_devengada <- full_join(prima_bruta, rrc, by = "ramo")


prima_dev <- prima_devengada |> 
  mutate(ramo = str_to_upper(ramo),
         ramo = str_trim(ramo),
         ramo = iconv(ramo, to="ASCII//TRANSLIT"),
         ramo = case_when(
           str_detect(ramo, "ACC. PERS. COLECTIVO") ~ "ACCIDENTES PERSONALES COLECTIVO",
           str_detect(ramo, "ACC. PERS. INDIVIDUAL") ~ "ACCIDENTES PERSONALES INDIVIDUAL",
           str_detect(ramo, "AUTO CASCO|AUTOMOVIL CASCO|AUTOMÓVIL CASCO") ~ "AUTOMOVIL CASCO",
           str_detect(ramo, "COMBINADOS|COMBINADO") ~ "COMBINADOS",
           str_detect(ramo, "DIVERSOS") ~ "DIVERSOS",
           str_detect(ramo, "FIANZAS") ~ "FIANZAS",
           str_detect(ramo, "HOSP. COLECTIVO") ~ "HOSPITALIZACION COLECTIVO",
           str_detect(ramo, "HOSP. INDIVIDUAL") ~ "HOSPITALIZACION INDIVIDUAL",
           str_detect(ramo, "RAMOS TECNIC|RAMOS TECNICOS")   ~ "RAMOS TECNICOS (TRI)",
           str_detect(ramo, "RESP CIVIL AUTOMOVIL|RESPONSABILIDAD CIVIL AUTO")   ~ "RESPONS. CIVIL DE AUTOMOVIL",
           str_detect(ramo, "RESP CIVIL GENERAL|RESP. CIVIL GENERAL")   ~ "RESPONS. CIVIL GENERAL",
           str_detect(ramo, "RESP CIVIL PATRONAL|RESP. CIVIL PATRONAL")   ~ "RESPONS. CIVIL PATRONAL",
           str_detect(ramo, "RESPONSABILIDAD CIVIL PATRONAL|RESP. CIVIL PATRONAL") ~ "RESPONS. CIVIL PROFESIONAL",
           str_detect(ramo, "SEG. FUNERARIOS|SEG. FUNERARIOS COLECTIVO") ~ "SEGUROS FUNERARIOS",
           str_detect(ramo, "SEGURO DE CREDITO") ~ "SEGUROS DE CREDITO",
           TRUE ~ "OTROS")
         ) |> 
  group_by(ramo) |> 
  summarise(`Prima Bruta` = sum(`Prima Bruta`),
            `Reserva de Riesgo en Curso` = sum(`Reserva de Riesgo en Curso`)
            )
  

####### Egresos Técnicos#########

siniestros_pagados <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2025-01-01"),
         fec_emis <= as.Date("2025-12-31")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=SINIESTROS PAGADOS -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Siniestros Pagados` = sum(abs(saldo))) |> 
  drop_na(ramo)

siniestros_pendientes <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2025-01-01"),
         fec_emis <= as.Date("2025-12-31")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=RESERVA SINIESTROS PENDIENTES -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Reserva de Siniestros Pendientes de Pago` = sum(abs(saldo))) |> 
  drop_na(ramo)

ibnr <- siniestros_pendientes |> 
  mutate(IBNR = `Reserva de Siniestros Pendientes de Pago` * .03) |> 
  select(-`Reserva de Siniestros Pendientes de Pago`)

siniestros_pendientes_ant <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2024-01-01"),
         fec_emis <= as.Date("2024-12-31")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=RESERVA SINIESTROS PENDIENTES -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Reserva de Siniestros Pendientes de Pago Anterior` = sum(abs(saldo))) |> 
  drop_na(ramo)

ibnr_ant <- siniestros_pendientes_ant |> 
  mutate(`IBNR Anterior` = `Reserva de Siniestros Pendientes de Pago Anterior` * .03) |> 
  select(-`Reserva de Siniestros Pendientes de Pago Anterior`)



write.xlsx(prima_bruta, "prima_profit.xlsx", overwrite = TRUE)
