
#Importamos el reporte de depresión
#N = 117,289 registros con fecha de 07/feb/2021 a 09/09/2022
###############################################################################
depre <- read.csv("./BD/Autodiagnostico_depresion.csv", 
                  header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################
depre$SEXO[depre$FOLIO == "10317"] <- "No deseo especificar" #Corregimos un valor vacio de SEXO

#print(lapply(depre, function(x) sum(is.na(x))))

depre$RESULTADO <- factor(depre$RESULTADO, levels = c("Bajo", "Leve", "Medio", "Alto"))
depre$LGBTI <- factor(depre$LGBTI, levels = c("No", "Si"))
depre$INDIGENA <- factor(depre$INDIGENA, levels = c("No", "Si"))

#Creación de nuevas variables
###############################################################################
depre$sexo[depre$SEXO == "Hombre"] <- "Hombre"
depre$sexo[depre$SEXO == "Hombre Cisgénero"] <- "Hombre"
depre$sexo[depre$SEXO == "Hombre Trans"] <- "Hombre"
depre$sexo[depre$SEXO == "Mujer"] <- "Mujer"
depre$sexo[depre$SEXO == "Mujer Cisgénero"] <- "Mujer"
depre$sexo[depre$SEXO == "Mujer Trans"] <- "Mujer"
depre$sexo[depre$SEXO == "No deseo especificar"] <- "Otro"
depre$sexo[depre$SEXO == "Otro"] <- "Otro"
depre$sexo[depre$SEXO == "Queer"] <- "Otro"
depre$sexo[depre$SEXO == "Persona intersexual"] <- "Otro"
depre$sexo[depre$SEXO == "Persona no binaria"] <- "Otro"

depre$date <- lubridate::dmy_hm(depre$FECHA.CAPTURA, tz = "Mexico/General")

depre$puntos <- rowSums(depre[,c("R1", "R2", "R3", "R4", "R5", "R6", "R7", 
                                 "R8", "R9", "R10", "R11", "R12", "R13", "R14", 
                                 "R15", "R16", "R17", "R18", "R19", "R20", "R21")])

#Evaluamos la confiabilidad interna del instrumento de depresión (Alfa = 0.943)
ltm::cronbach.alpha(depre[, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", 
                              "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", 
                              "R18", "R19", "R20", "R21")])

depre$year <- lubridate::year(depre$date)
depre$year <- factor(depre$year, levels = c("2021","2022"))

depre$month <- lubridate::month(depre$date, label = TRUE, abbr = TRUE)
depre$month <- factor(depre$month, levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

depre$wday <- lubridate::wday(depre$date, label = TRUE, abbr = TRUE, week_start = 1)
depre$wday <- substr(depre$wday, start = 1, stop = 3)
depre$wday <- factor(depre$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

depre$añomes <- paste(lubridate::month(depre$date),lubridate::year(depre$date),sep ="-")
depre$añomes <-factor(depre$añomes, levels = c("2-2021","3-2021","4-2021","5-2021", "6-2021",
                                               "7-2021","8-2021","9-2021","10-2021","11-2021",
                                               "12-2021","1-2022", "2-2022", "3-2022", "4-2022", 
                                               "5-2022", "6-2022", "7-2022", "8-2022", "9-2022"))

depre$epiweek <- lubridate::epiweek(depre$date)

depre$epiw_year <- paste(depre$epiweek,depre$year, sep="-")

depre$hora <- lubridate::hour(depre$date)

depre$horauso <- NA
depre$horauso[depre$hora >= 0 & depre$hora < 6] <- '0:00 a 5:59 hrs'
depre$horauso[depre$hora >= 6 & depre$hora < 12] <- '6:00 a 11:59 hrs'
depre$horauso[depre$hora >= 12 & depre$hora < 18] <- '12:00 a 17:59 hrs'
depre$horauso[depre$hora >= 18 & depre$hora <= 24] <- '18:00 a 23:59 hrs'
depre$horauso <- factor(depre$horauso, levels = c('0:00 a 5:59 hrs', '6:00 a 11:59 hrs', '12:00 a 17:59 hrs', '18:00 a 23:59 hrs'))


depre$grupoedad <- NA
depre$grupoedad[depre$EDAD > 13 & depre$EDAD < 18] <-"Menor a 18 años"
depre$grupoedad[depre$EDAD >= 18 & depre$EDAD < 26] <-"18 a 25 años"
depre$grupoedad[depre$EDAD >= 25 & depre$EDAD < 46] <-"26 a 45 años"
depre$grupoedad[depre$EDAD >= 45 & depre$EDAD < 60] <-"46 a 59 años"
depre$grupoedad[depre$EDAD >= 60] <-"60 y más años"

depre$grupoedad <- factor(depre$grupoedad, levels = c("Menor a 18 años", 
                                                    "18 a 25 años",
                                                    "26 a 45 años",
                                                    "46 a 59 años",
                                                    "60 y más años"))

depre$solici_atend <- !is.na(depre$NOMBRE)
table(depre$solici_atend)
#FALSE   TRUE 
#114450  2839 = 2.48%

depre$origen <- "Depresión"
depre$EDAD <- abs(depre$EDAD)

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)
depre[paste0("R",c(1:21))]<- NULL
depre$NOMBRE <- NULL
depre$APELLIDO.UNO <- NULL
depre$APELLIDO.DOS <- NULL
depre$EMAIL <- NULL
depre$TELEFONO <- NULL
depre$SEXO <- NULL
