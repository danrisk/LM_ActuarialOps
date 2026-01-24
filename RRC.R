library(tidyverse)
library(openxlsx)
library(readr)
library(readxl)
library(DBI)
library(odbc)

options(scipen = 999)
options(digits=7)

# Recibos_normalizados <- read_excel("recibos_normalizados.xlsx")

SYSIP <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "dmorales",
                        PWD      = "lamundial*2025*morales",
                        Port     = 1433)

recibos <- recibo_prima_preliminar |> 
  select(Descripción, `Fecha desde Recibo`, `Fecha hasta Recibo`, `Fecha de Cobro`, Debe, Haber, `Monto de Comisión`) |> 
  rename(Descripcion_Ramo = Descripción,
         Fecha_desde_Recibo = `Fecha desde Recibo`,
         fecha_hasta_Recibo = `Fecha hasta Recibo`,
         Fecha_Cobro = `Fecha de Cobro`,
         Comisión_BS = `Monto de Comisión`)

rrc <- Recibos_normalizados |> 
  select(Descripcion_Ramo, Fecha_desde_Recibo, Fecha_hasta_Recibo, Fecha_Cobro, Monto_Prima_Bruta, Comisión_BS) |>
  mutate(Fecha_desde_Recibo = dmy(Fecha_desde_Recibo),
         Fecha_hasta_Recibo = dmy(Fecha_hasta_Recibo),
         Fecha_Cobro = dmy(Fecha_Cobro),
         ANIO = year(Fecha_Cobro),
         Mes = month(Fecha_Cobro, label = TRUE),
         prima_neta = as.numeric(Monto_Prima_Bruta) - as.numeric(Comisión_BS),
         fecha_evaluacion = as.Date("2025-12-31"),
         dias_por_transcurrir = case_when(
           as.numeric(Fecha_hasta_Recibo) <= fecha_evaluacion ~ 0,
           as.numeric(Fecha_desde_Recibo) > fecha_evaluacion ~ as.numeric(Fecha_hasta_Recibo) - as.numeric(Fecha_desde_Recibo),
           as.numeric(Fecha_hasta_Recibo) > fecha_evaluacion ~ as.numeric(Fecha_hasta_Recibo) - as.numeric(fecha_evaluacion),
           TRUE ~ 0),
         proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(Fecha_hasta_Recibo) - as.numeric(Fecha_desde_Recibo)),
         reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(prima_neta),
         proporcion_RRC = replace_na(proporcion_RRC, 0),
         reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0)
  )


prima_ramo_mes <- rrc |> 
  group_by(Descripcion_Ramo, Mes) |> 
  summarise(Prima = sum(prima_neta)) |>
  pivot_wider(names_from = Mes, values_from = Prima, values_fn = sum, values_fill = 0)

rrc_ramo <- rrc |> 
  group_by(Descripcion_Ramo) |> 
  summarise(Prima = sum(prima_neta),
           `Reserva de Riesgo en Curso Totales` = sum(reserva_de_riesgo_en_curso)) 

write.xlsx(RRC, "anexo_17.xlsx")

#################################################################################

#Versión Local

Recibos_historico_2025 <- read_excel("recibos_normalizados.xlsx")

Recibos_historico_2025 <- Recibos_historico_2025 |> 
  rename("Fecha desde Recibo" = Fecha_desde_Recibo,
         "Fecha hasta Recibo" = Fecha_hasta_Recibo,
         Saldo = Monto_Prima_Bruta,
         "Monto de Comisión" = Comisión_BS,
         Ramo = Descripcion_Ramo,
         "Fecha de Cobro" = Fecha_Cobro) |> 
  select(Nro_Recibo, 
         `Fecha desde Recibo`, 
         `Fecha hasta Recibo`, 
         Saldo, 
         `Monto de Comisión`, 
         Ramo, 
         `Fecha de Cobro`)

Primas_auditadas <- Primas_auditadas |> 
  select(Nro_Recibo, 
         `Fecha desde Recibo`, 
         `Fecha hasta Recibo`, 
         Saldo, 
         `Monto de Comisión`, 
         Ramo, 
         `Fecha de Cobro`)


Primas_Reservas_2025 <- rbind(Recibos_historico_2025, Primas_auditadas) |> 
  drop_na(`Fecha desde Recibo`)




RRC <- Primas_Reservas_2025 |> 
  mutate(`Fecha desde Recibo`= dmy(`Fecha desde Recibo`),
         `Fecha hasta Recibo` = dmy(`Fecha hasta Recibo`),
         `Fecha de Cobro` = dmy(`Fecha de Cobro`),
         ANIO = year(`Fecha de Cobro`),
         Mes = month(`Fecha de Cobro`, label = TRUE),
         prima_neta = as.numeric(Saldo) - as.numeric(`Monto de Comisión`),
         fecha_evaluacion = as.Date("2025-12-31"),
         dias_por_transcurrir = case_when(
           as.numeric(`Fecha hasta Recibo`) <= fecha_evaluacion ~ 0,
           as.numeric(`Fecha desde Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`),
           as.numeric(`Fecha hasta Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(fecha_evaluacion),
           TRUE ~ 0),
         proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`)),
         reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(prima_neta),
         proporcion_RRC = replace_na(proporcion_RRC, 0),
         reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0),
         prima_cedida = as.numeric(prima_neta) * 0.8,
         rrc_reaseguro = as.numeric(proporcion_RRC) * prima_cedida,
         prima_retenida = as.numeric(prima_neta) - as.numeric(prima_cedida),
         rrc_retenida = as.numeric(reserva_de_riesgo_en_curso) - as.numeric(rrc_reaseguro)
  )


RRC_RAMO <- RRC |> 
  group_by(Ramo) |> 
  summarise(Prima = sum(Saldo),
            `Reserva de Riesgo en Curso Totales` = sum(reserva_de_riesgo_en_curso),
            `Prima Cedida` = sum(prima_cedida),
            `RRC Reaseguradores` = sum(rrc_reaseguro),
            `Prima Retenida` = sum(prima_retenida),
            `RRC Retenida` = sum(rrc_retenida)
            ) 



#########version con BD######

Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
  filter(
    fcobro >= "2026-01-01",
    fcobro <= "2026-01-15",
    iestadorec == "C") |> 
  collect()

maramos <- tbl(SYSIP, "MARAMOS") |> 
  collect()

Recibos_ramos <- Recibos_SYSIP |> 
  left_join(maramos, by ="cramo")

Recibos_detalle <- Recibos_ramos |> 
  select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
         cnrecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, msumabrutaext, mprimabruta, mprimabrutaext,
         pcomision, mcomision, mcomisionext, mpcedida, mpcedidaext, mpret, mpretext, mpfp, mpfpext) |> 
  rename("Nº de Póliza" = cnpoliza,
         Ramo = xdescripcion_l,
         "Fecha de Emision Recibo" = femision,
         "Fecha desde Póliza" = fdesde_pol,
         "Fecha Hasta Póliza" = fhasta_pol,
         "Cédula Tomador" = ctenedor,
         "Nro de Recibo" = cnrecibo,
         "Fecha desde Recibo" = fdesde,
         "Fecha hasta Recibo" = fhasta,
         "Fecha de Cobro" = fcobro,
         Moneda = cmoneda,
         "Tasa de Cambio" = ptasamon_pago,
         "Suma Asegurada" = msumabruta,
         "Suma Asegurada Moneda Extranjera" = msumabrutaext,
         "Prima Bruta" = mprimabruta,
         "Prima Bruta Moneda Extranjera" = mprimabrutaext,
         "Porcentaje de Comisión" = pcomision,
         "Monto de Comisión" = mcomision,
         "Monto Comision Extranjera" = mcomisionext,
         "Prima Cedida en Reaseguro" = mpcedida,
         "Prima Cedida Moneda Extranjera"= mpcedidaext,
         "Prima Cedida Facultativo" = mpfp,
         "Prima Cedida Facultativo Moneda Extranjera" = mpfpext,
         "Prima Retenida" = mpret,
         "Prima Retenida Moneda Extranjera" = mpretext)


RRC <- Recibos_detalle |> 
  mutate(`Fecha desde Recibo`= as.Date(`Fecha desde Recibo`),
         `Fecha hasta Recibo` = as.Date(`Fecha hasta Recibo`),
         `Fecha de Cobro` = as.Date(`Fecha de Cobro`),
         ANIO = year(`Fecha de Cobro`),
         Mes = month(`Fecha de Cobro`, label = TRUE),
         prima_neta = as.numeric(`Prima Bruta`) - as.numeric(`Monto de Comisión`),
         fecha_evaluacion = as.Date("2026-01-15"),
         dias_por_transcurrir = case_when(
           as.numeric(`Fecha hasta Recibo`) <= fecha_evaluacion ~ 0,
           as.numeric(`Fecha desde Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`),
           as.numeric(`Fecha hasta Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(fecha_evaluacion),
           TRUE ~ 0),
         proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`)),
         reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(prima_neta),
         proporcion_RRC = replace_na(proporcion_RRC, 0),
         reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0),
         prima_cedida = ifelse(as.numeric(prima_neta) * 0.8 < 0, 0,as.numeric(prima_neta) * 0.8),
         rrc_reaseguro = as.numeric(proporcion_RRC) * prima_cedida,
         prima_retenida = as.numeric(prima_neta) - as.numeric(prima_cedida),
         rrc_retenida = as.numeric(reserva_de_riesgo_en_curso) - as.numeric(rrc_reaseguro),
         rrc_reaseguro = replace_na(rrc_reaseguro, 0),
         prima_retenida = replace_na(prima_retenida, 0),
         rrc_retenida = replace_na(rrc_retenida, 0),
         prima_cedida = replace_na(prima_cedida, 0)
  )


RRC_RAMO <- RRC |> 
  group_by(Ramo) |> 
  summarise(Prima = sum(`Prima Bruta`),
            `Reserva de Riesgo en Curso Totales` = sum(reserva_de_riesgo_en_curso),
            `Prima Cedida` = sum(prima_cedida),
            `RRC Reaseguradores` = sum(rrc_reaseguro),
            `Prima Retenida` = sum(prima_retenida),
            `RRC Retenida` = sum(rrc_retenida)
  ) %>% 
  mutate(`Prima Cedida` = replace_na(`Prima Cedida`, 0))






write.xlsx(RRC_RAMO, "RRC_Ramo_31122025.xlsx")
