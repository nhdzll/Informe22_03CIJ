
#Importamos el reporte de ansiedad
#N = 24,078 Registros con fecha de 07/feb/2021 a 30/may/2022
###############################################################################
ansi <- read.csv("./BD/Autodiagnostico_ansiedad.csv", 
                 header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################

#print(lapply(ansi, function(x) sum(is.na(x))))

ansi$RESULTADO <- factor(ansi$RESULTADO, levels = c("Bajo", "Leve", "Medio", "Alto"))
ansi$LGBTI <- factor(ansi$LGBTI, levels = c("No", "Si"))
ansi$INDIGENA <- factor(ansi$INDIGENA, levels = c("No", "Si"))

#Creación de nuevas variables
###############################################################################
ansi$sexo[ansi$SEXO == "Hombre"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Hombre Cisgénero"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Hombre Trans"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Mujer"] <- "Mujer"
ansi$sexo[ansi$SEXO == "Mujer Cisgénero"] <- "Mujer"
ansi$sexo[ansi$SEXO == "Mujer Trans"] <- "Mujer"
ansi$sexo[ansi$SEXO == "No deseo especificar"] <- "Otro"
ansi$sexo[ansi$SEXO == "Otro"] <- "Otro"
ansi$sexo[ansi$SEXO == "Queer"] <- "Otro"
ansi$sexo[ansi$SEXO == "Persona intersexual"] <- "Otro"
ansi$sexo[ansi$SEXO == "Persona no binaria"] <- "Otro"

ansi$date <- lubridate::dmy_hm(ansi$FECHA.CAPTURA, tz = "Mexico/General")

ansi$puntos <- rowSums(ansi[,c("R1", "R2", "R3", "R4", "R5", "R6", "R7", 
                                 "R8", "R9", "R10", "R11", "R12", "R13", "R14", 
                                 "R15", "R16", "R17", "R18", "R19", "R20", "R21")])

#Evaluamos la confiabilidad interna del instrumento de ansiedad (Alfa = 0.94)
ltm::cronbach.alpha(ansi[, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", 
                              "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", 
                              "R18", "R19", "R20", "R21")])

ansi$year <- lubridate::year(ansi$date)
ansi$year <- factor(ansi$year, levels = c("2021","2022"))

ansi$month <- lubridate::month(ansi$date, label = TRUE, abbr = TRUE)
ansi$month <- factor(ansi$month, levels = c("ene", "feb", "mar", "abr", 
                                            "may", "jun", "jul", "ago", "sep", 
                                            "oct", "nov", "dic"))

ansi$wday <- lubridate::wday(ansi$date, label = TRUE, abbr = TRUE, week_start = 1)
ansi$wday <- substr(ansi$wday, start = 1, stop = 3)
ansi$wday <- factor(ansi$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

ansi$añomes <- paste(lubridate::month(ansi$date), lubridate::year(ansi$date),sep ="-")
ansi$añomes <-factor(ansi$añomes, levels = c("2-2021","3-2021","4-2021","5-2021", "6-2021",
                                             "7-2021","8-2021","9-2021","10-2021","11-2021",
                                             "12-2021","1-2022", "2-2022", "3-2022", "4-2022", 
                                             "5-2022", "6-2022", "7-2022", "8-2022", "9-2022"))

ansi$epiweek <- lubridate::epiweek(ansi$date)

ansi$epiw_year <- paste(ansi$epiweek, ansi$year, sep="-")

ansi$hora <- lubridate::hour(ansi$date)

ansi$horauso <- NA
ansi$horauso[ansi$hora >= 0 & ansi$hora < 6] <- '0:00 a 5:59 hrs'
ansi$horauso[ansi$hora >= 6 & ansi$hora < 12] <- '6:00 a 11:59 hrs'
ansi$horauso[ansi$hora >= 12 & ansi$hora < 18] <- '12:00 a 17:59 hrs'
ansi$horauso[ansi$hora >= 18 & ansi$hora <= 24] <- '18:00 a 23:59 hrs'
ansi$horauso <- factor(ansi$horauso, levels = c('0:00 a 5:59 hrs', '6:00 a 11:59 hrs', 
                                                '12:00 a 17:59 hrs', '18:00 a 23:59 hrs'))

ansi$grupoedad <- NA
ansi$grupoedad[ansi$EDAD > 13 & ansi$EDAD < 18] <-"Menor a 18 años"
ansi$grupoedad[ansi$EDAD >= 18 & ansi$EDAD < 26] <-"18 a 25 años"
ansi$grupoedad[ansi$EDAD >= 25 & ansi$EDAD < 46] <-"26 a 45 años"
ansi$grupoedad[ansi$EDAD >= 45 & ansi$EDAD < 60] <-"46 a 59 años"
ansi$grupoedad[ansi$EDAD >= 60] <-"60 y más años"
ansi$grupoedad <- factor(ansi$grupoedad, levels = c("Menor a 18 años", 
                                                    "18 a 25 años",
                                                    "26 a 45 años",
                                                    "46 a 59 años",
                                                    "60 y más años"))

ansi$solici_atend <- !is.na(ansi$NOMBRE)
table(ansi$solici_atend)
#FALSE   TRUE 
#23026  1052

ansi$origen <- "Ansiedad"
ansi$EDAD <- abs(ansi$EDAD)

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)
ansi[paste0("R",c(1:21))]<- NULL
ansi$NOMBRE <- NULL
ansi$APELLIDO.UNO <- NULL
ansi$APELLIDO.DOS <- NULL
ansi$EMAIL <- NULL
ansi$TELEFONO <- NULL
ansi$SEXO <- NULL