##################################################
#Visualización ENSU
#Autor: Esteban Calisaya
#Fecha: 24-06-2025
#Coordinación de Asosres
#Gobierno de la Ciudad de México
##################################################



# Verificar/instalar pacman primero
if (!require('pacman')) install.packages('pacman', dependencies = TRUE); library('pacman')

# Cargar o instalar paquetes usando pacman
p_load(tidyverse,
       factoextra,
       foreign,
       ggplot2,
       survey,
       corrplot, 
       gtsummary,
       summarytools,
       ggsurvey,
       questionr,
       scales,
       ggpubr,
       remotes,
       srvyr,
       ggrepel,
       sjlabelled)

# Instalar plotflow desde GitHub usando remotes (ya cargado por pacman)
if(!require('plotflow')) remotes::install_github("trinker/plotflow")
library(plotflow)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")



load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/03_23/BD_ENSU_0323_2.RData")

ENSU_CB_0323<- ENSU_CB_0323 %>% mutate(
  trimestre = "T1-23"
)

load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/06_23/BD_ENSU_0623_2.RData")

ENSU_CB_0623<- ENSU_CB_0623 %>% mutate(
  trimestre = "T2-23"
)


load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/09_23/BD_ENSU_0923_2.RData")

ENSU_CB_0923<- ENSU_CB_0923 %>% mutate(
  trimestre = "T3-23"
)


load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/12_23/BD_ENSU_1223_2.RData")

ENSU_CB_1223<- ENSU_CB_1223 %>% mutate(
  trimestre = "T4-23"
)


load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/03_24/BD_ENSU_0324.RData")

ENSU_CB_0324<- ENSU_CB_0324 %>% mutate(
  trimestre = "T1-24"
)


load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/06_24/BD_ENSU_0624.RData")

ENSU_CB_0624<- ENSU_CB_0624 %>% mutate(
  trimestre = "T2-24"
)

load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/09_24/BD_ENSU_0924.RData")

ENSU_CB_0924<- ENSU_CB_0924 %>% mutate(
  trimestre = "T3-24"
)


load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/12_24/BD_ENSU_1224.RData")

ENSU_CB_1224<- ENSU_CB_1224 %>% mutate(
  trimestre = "T4-24"
)

load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/03_25/BD_ENSU_0325.RData")

ENSU_CB_0325<- ENSU_CB_0325 %>% mutate(
  trimestre = "T1-25"
)

load("G:/Mi unidad/Iztapalapa/Encuestas/ENSU/06_25/BD_ENSU_0625.RData")

ENSU_CB_0625<- ENSU_CB_0625 %>% mutate(
  trimestre = "T2-25"
)

# 1. Función de recodificación de variable de confianza
recodificar_confianza <- function(var, tipo = c("escala10", "escala4")) {
  tipo <- match.arg(tipo)
  
  if (tipo == "escala10") {
    case_when(
      var %in% c("00", "01", "02") ~ "Mucha desconfianza",
      var %in% c("03", "04", "05") ~ "Algo de desconfianza",
      var %in% c("06", "07", "08", "09") ~ "Algo de confianza",
      var %in% c("10") ~ "Mucha confianza",
      var %in% c("99") ~ NA_character_,
      TRUE ~ NA_character_
    )
  } else {
    case_when(
      var == "1" ~ "Mucha confianza",
      var == "2" ~ "Algo de confianza",
      var == "3" ~ "Algo de desconfianza",
      var == "4" ~ "Mucha desconfianza",
      var %in% c("9") ~ NA_character_,
      TRUE ~ NA_character_
    )
  }
}

# 2. Cargar y procesar cada base individualmente
procesar_trimestre <- function(obj_name, trimestre, tipo_escala = NULL, vars = NULL) {
  df <- get(obj_name)
  df$trimestre <- trimestre
  
  if (is.null(tipo_escala)) {
    df$conf_pres <- NA_character_
    df$conf_gob  <- NA_character_
    df$conf_alc  <- NA_character_
  } else {
    df <- df %>%
      mutate(
        conf_pres = recodificar_confianza(.data[[vars[1]]], tipo_escala),
        conf_gob  = recodificar_confianza(.data[[vars[2]]], tipo_escala),
        conf_alc  = recodificar_confianza(.data[[vars[3]]], tipo_escala)
      )
  }
  assign(obj_name, df, envir = .GlobalEnv)
}

# 3. Aplicar a cada base
procesar_trimestre("ENSU_CB_0323", "T1-23", "escala10", c("BP4_1_1", "BP4_1_2", "BP4_1_3"))
procesar_trimestre("ENSU_CB_0623", "T2-23")  # No aplica
procesar_trimestre("ENSU_CB_0923", "T3-23", "escala10", c("BP5_1_1", "BP5_1_2", "BP5_1_3"))
procesar_trimestre("ENSU_CB_1223", "T4-23")  # No aplica

procesar_trimestre("ENSU_CB_0324", "T1-24", "escala10", c("BP4_1_1", "BP4_1_2", "BP4_1_3"))
procesar_trimestre("ENSU_CB_0624", "T2-24")  # No aplica
procesar_trimestre("ENSU_CB_0924", "T3-24", "escala4",  c("BP5_1_1", "BP5_1_2", "BP5_1_3"))
procesar_trimestre("ENSU_CB_1224", "T4-24")  # No aplica

procesar_trimestre("ENSU_CB_0325", "T1-25", "escala4",  c("BP4_1_1", "BP4_1_2", "BP4_1_3"))
procesar_trimestre("ENSU_CB_0625", "T2-25")  # No aplica

# 4. Unir todas las bases
mydflist <- mget(ls(pattern = "ENSU_CB_"))
allNms <- unique(unlist(lapply(mydflist, names)))

ENSU_CB <- do.call(rbind,
                   c(lapply(mydflist,
                            function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                               function(y) NA)))),
                     make.row.names=FALSE))

# 5. Convertir a factores ordenados
ENSU_CB <- ENSU_CB %>%
  mutate(across(starts_with("conf_"),
                ~factor(.x, levels = c("Mucha confianza", "Algo de confianza",
                                       "Algo de desconfianza", "Mucha desconfianza"))))

# 6. Verifica
table(ENSU_CB$conf_pres, useNA = "always")


#Liberación espacio
rm(list = setdiff(ls(pattern = "^ENSU_CB_|^ENSU_CS_|^ENSU_VIV_"), "ENSU_CB_long"))


####Etiquetado####

ENSU_CB <- ENSU_CB |>
  mutate(
    SEXO = case_when(SEXO == 1 ~ "Hombre",
                     SEXO == 2 ~ "Mujer")
  )

ENSU_CB <- ENSU_CB %>%
  mutate_at(vars(starts_with('BP1_1')), 
            ~recode(., '1' = 'Seguro(a)', 
                    "2" = "Inseguro(a)" ,
                    "3" = "No aplica",
                    "9" = "NS/NR"))%>%
  mutate_at(vars(starts_with('BP1_1')),
            ~factor(., levels = c("Inseguro(a)",
                                  "Seguro(a)",
                                  "No aplica",
                                  "NS/NR"))
  )
#Inseguridad por lugar
ENSU_CB <- ENSU_CB %>%
  mutate_at(vars(starts_with('BP1_2_')), 
            ~recode(., '1' = 'Seguro(a)', 
                    "2" = "Inseguro(a)" ,
                    "3" = "No aplica",
                    "9" = "NS/NR"))%>%
  mutate_at(vars(starts_with('BP1_2_')),
            ~factor(., levels = c("Inseguro(a)",
                                  "Seguro(a)",
                                  "No aplica",
                                  "NS/NR"))
  )
#Incivilidades
ENSU_CB <- ENSU_CB %>%
  mutate_at(vars(starts_with('BP1_4_')), 
            ~recode(., '1' = 'Sí', 
                    "2" = "No" ,
                    "9" = "NS/NR"))%>%
  mutate_at(vars(starts_with('BP1_4_')),
            ~factor(., levels = c("Sí",
                                  "No",
                                  "NS/NR"))
  )


##Desempeño gubernamental histórico

ENSU_CB <- ENSU_CB%>%
  mutate(
    BP3_2= recode( BP3_2, 
                   '1' = 'Muy efectivo', 
                   "2" = "Algo efectivo" ,
                   "3" = "Poco efectivo",
                   "4" = "Nada efectivo",
                   "9" = NA_character_)
  )%>%
  mutate(
    BP3_2= factor(BP3_2, levels =c('Muy efectivo', 
                                   "Algo efectivo" ,
                                   "Poco efectivo",
                                   "Nada efectivo"))
  )



ENSU_CB <- ENSU_CB %>%
  mutate(
    desemp_gub1= recode( BP3_2, 
                         'Muy efectivo' = 'Muy efectivo o algo efectivo', 
                         "Algo efectivo" = "Muy efectivo o algo efectivo" ,
                         "Poco efectivo" = "Poco efectivo o nada efectivo",
                         "Nada efectivo" = "Poco efectivo o nada efectivo")
  )%>%
  mutate(
    desemp_gub1= factor(desemp_gub1, levels =c('Muy efectivo o algo efectivo', 
                                               "Poco efectivo o nada efectivo"))
  )




#Recodificación nombres CDMX#

ENSU_CB<- ENSU_CB %>% mutate(
  nom_cdmx = case_when(NOM_CD =="ALVARO OBREGON" ~ "A.OBR",
                       NOM_CD =="AZCAPOTZALCO" ~ "AZCA",
                       NOM_CD =="BENITO JUAREZ" ~ "B.JRZ",
                       NOM_CD =="COYOACAN" ~ "COYO",
                       NOM_CD =="CUAJIMALPA DE MORELOS" ~ "CUAJ",
                       NOM_CD =="CUAUHTEMOC" ~ "CUAUH",
                       NOM_CD =="GUSTAVO A. MADERO" ~ "GAM",
                       NOM_CD =="IZTACALCO" ~ "IZTC",
                       NOM_CD =="IZTAPALAPA" ~ "IZTP",
                       NOM_CD =="LA MAGDALENA CONTRERAS" ~ "M.CONT",
                       NOM_CD =="MIGUEL HIDALGO" ~ "M.HGO",
                       NOM_CD =="MILPA ALTA" ~ "M.ALTA",
                       NOM_CD =="TLAHUAC" ~ "TLHC",
                       NOM_CD =="TLALPAN" ~ "TLPN",
                       NOM_CD =="VENUSTIANO CARRANZA" ~ "V.CAR",
                       NOM_CD =="XOCHIMILCO" ~ "XOCH",
                       TRUE ~ NA))


ENSU_CB <- ENSU_CB %>% mutate( 
  nom_cdmx1 = factor(nom_cdmx, levels = c( "XOCH", "AZCA",  "TLHC", "IZTC", 
                                           "IZTP", "A.OBR", "CUAUH", "GAM", 
                                           "M.CONT", "V.CAR", "M.ALTA", "TLPN", 
                                           "COYO", "M.HGO", "CUAJ", "B.JRZ"))
)


ENSU_CB <- ENSU_CB %>% mutate( 
  nom_cdmx2 = factor(nom_cdmx, levels = c("IZTP", "B.JRZ", "TLHC", "XOCH",
                                          "IZTC", "CUAUH", "GAM", "M.CONT",
                                          "V.CAR", "M.ALTA", "TLPN", "AZCA",
                                          "COYO", "M.HGO", "CUAJ", "A.OBR"))
)

ENSU_CB <- ENSU_CB %>% mutate(
  trimestre = factor(trimestre, levels = c ("T1-23", "T2-23", "T3-23",
                                            "T4-23", "T1-24", "T2-24", "T3-24",
                                            "T4-24", "T1-25", "T2-25"))
)


ENSU_CB <- ENSU_CB %>% mutate(
  victi = case_when(  BP1_6_1 == 1 | BP1_6_2 == 1 |
                      BP1_6_3 == 1 | BP1_6_4 == 1 |
                      BP1_6_5 == 1 | BP1_6_6 == 1 |
                      BP1_6_7 == 1 | BP1_6_8 ==1  ~ "Víctima",
                    BP1_6_1 == 2 & BP1_6_2 == 2 &
                      BP1_6_3 == 2 & BP1_6_4 == 2 &
                      BP1_6_5 == 2 & BP1_6_6 == 2 |
                      BP1_6_7 == 2 & BP1_6_8 == 2 ~ "No víctima",
                    BP1_6_1 == 9 | BP1_6_2 == 9 |
                      BP1_6_3 == 9 | BP1_6_4 == 9 |
                      BP1_6_5 == 9 | BP1_6_6 == 9 |
                      BP1_6_7 == 9 & BP1_6_8 == 9 ~ NA
                    )
)


ENSU_CB <- ENSU_CB %>%
  mutate(
    # Robo total de vehículo (BP1_6_1)
    vict_robo_veh = case_when(
      BP1_6_1 == 9 ~ NA_character_,
      BP1_6_1 == 1 ~ "Robo total vehículo", # Etiqueta el tipo de crimen
      BP1_6_1 == 2 ~ "No víctima"
    ),
    # Robo de accesorios de vehículo (BP1_6_2)
    vict_robo_acc = case_when(
      BP1_6_2 == 9 ~ NA_character_,
      BP1_6_2 == 1 ~ "Robo accesorios vehículo",
      BP1_6_2 == 2 ~ "No víctima"
    ),
    # Allanamiento morada (BP1_6_3)
    vict_allana = case_when(
      BP1_6_3 == 9 ~ NA_character_,
      BP1_6_3 == 1 ~ "Allanamiento morada",
      BP1_6_3 == 2 ~ "No víctima"
    ),
    # Robo o asalto en calle/transporte (BP1_6_4)
    vict_robo_asalto = case_when(
      BP1_6_4 == 9 ~ NA_character_,
      BP1_6_4 == 1 ~ "Robo o asalto",
      BP1_6_4 == 2 ~ "No víctima"
    ),
    # Robo en otra forma (BP1_6_5)
    vict_robo_otro = case_when(
      BP1_6_5 == 9 ~ NA_character_,
      BP1_6_5 == 1 ~ "Robo otra forma",
      BP1_6_5 == 2 ~ "No víctima"
    ),
    # Amenaza o extorsión (BP1_6_6)
    vict_amenaza_ext = case_when(
      BP1_6_6 == 9 ~ NA_character_,
      BP1_6_6 == 1 ~ "Amenaza o extorsión",
      BP1_6_6 == 2 ~ "No víctima"
    ),
    # Robo de tarjeta o fraude bancario (BP1_6_7)
    vict_fraude_banc = case_when(
      BP1_6_7 == 9 ~ NA_character_,
      BP1_6_7 == 1 ~ "Fraude bancario",
      BP1_6_7 == 2 ~ "No víctima"
    ),
    # Estafa (BP1_6_8)
    vict_estafa = case_when(
      BP1_6_8 == 9 ~ NA_character_,
      BP1_6_8 == 1 ~ "Estafa",
      BP1_6_8 == 2 ~ "No víctima"
    )
  )|>
  mutate(across(starts_with("vict_"), 
                ~factor(.x, levels = unique(c(setdiff(.x, "No víctima"), "No víctima"))))
  )

#####Acoso

ENSU_CB <- ENSU_CB %>%
  mutate(
  acoso = case_when(grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 1 | BP4_1_2 == 1 |
                        BP4_1_3 == 1 | BP4_1_4 == 1 |
                        BP4_1_5 == 1 | BP4_1_6 == 1 |
                        BP4_1_7 == 1 | BP4_1_8 == 1 |
                        BP4_1_9 == 1) ~ "Enfrentó acoso",
                    grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 2 & BP4_1_2 == 2 &
                        BP4_1_3 == 2 & BP4_1_4 == 2 &
                        BP4_1_5 == 2 & BP4_1_6 == 2 &
                        BP4_1_7 == 2 & BP4_1_8 == 2 &
                        BP4_1_9 == 2) ~ "No enfrentó acoso",
                    grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 9 | BP4_1_2 == 9 |
                        BP4_1_3 == 9 | BP4_1_4 == 9 |
                        BP4_1_5 == 9 | BP4_1_6 == 9 |
                        BP4_1_7 == 9 & BP4_1_8 == 9 |
                        BP4_1_9 == 9) ~ NA_character_
  )
)

ENSU_CB <- ENSU_CB |>
  mutate(
    # Piropos (BP4_1_1)
    aco_piropos = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_1 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_1 == 1 ~ "Enfrentó", # Etiqueta el tipo de crimen
      grepl("^(T2|T4)", trimestre) & BP4_1_1 == 2 ~ "No enfrentó"
    ),
    # Intento violación (BP4_1_2)
    aco_intento_vio = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_2 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_2 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_2 == 2 ~ "No enfrentó"
    ),
    # Oferta intercambio sexual (BP4_1_3)
    aco_intercam = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_3 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_3 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_3 == 2 ~ "No enfrentó"
    ),
    # Acoso sexual redes, llamadas, correo (BP4_1_4)
    aco_redes = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_4 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_4 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_4 == 2 ~ "No enfrentó"
    ),
    # Violación (BP4_1_5)
    aco_viola = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_5 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_5 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_5 == 2 ~ "No enfrentó"
    ),
    # Exhibicionismo (BP4_1_6)
    aco_exhi = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_6 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_6 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_6 == 2 ~ "No enfrentó"
    ),
    # Tocamientos (BP4_1_7)
    aco_toca = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_7 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_7 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_7 == 2 ~ "No enfrentó"
    ),
    # sexting no consentido(BP4_1_8)
    aco_pic = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_8 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_8 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_8 == 2 ~ "No enfrentó"
    ),
    # Mirar esenas(BP4_1_8)
    aco_escenas = case_when(
      grepl("^(T2|T4)", trimestre) & BP4_1_9 == 9 ~ NA_character_,
      grepl("^(T2|T4)", trimestre) & BP4_1_9 == 1 ~ "Enfrentó",
      grepl("^(T2|T4)", trimestre) & BP4_1_9 == 2 ~ "No enfrentó"
    )
  )|>
  mutate(across(starts_with("aco_"), 
                ~factor(.x, levels = c("Enfrentó", "No enfrentó")))
         )
  

#ENSU_CB <- ENSU_CB %>%
#  filter(grepl("^(T2|T4)", trimestre))|>
#  mutate(
#    # Piropos (BP4_1_1)
#    aco_piropos = case_when(
#      BP4_1_1 == 9 ~ NA_character_,
#      BP4_1_1 == 1 ~ "Acoso sexual con piropos", # Etiqueta el tipo de crimen
#      BP4_1_1 == 2 ~ "No enfrentó"
#    ),
#    # Intento violación (BP4_1_2)
#    aco_intento_vio = case_when(
#    BP4_1_2 == 9 ~ NA_character_,
#      BP4_1_2 == 1 ~ "Intento de violación",
#      BP4_1_2 == 2 ~ "No enfrentó"
#    ),
#    # Oferta intercambio sexual (BP4_1_3)
#    aco_intercam = case_when(
#      BP4_1_3 == 9 ~ NA_character_,
#      BP4_1_3 == 1 ~ "Oferta intercambio sexual",
#      BP4_1_3 == 2 ~ "No enfrentó"
#    ),
#    # Acoso sexual redes, llamadas, correo (BP4_1_4)
#    aco_redes = case_when(
#      BP4_1_4 == 9 ~ NA_character_,
#      BP4_1_4 == 1 ~ "Acoso sexual mediante llamada, correo o redes sociales",
#      BP4_1_4 == 2 ~ "No enfrentó"
#    ),
#    # Violación (BP4_1_5)
#    aco_viola = case_when(
#      BP4_1_5 == 9 ~ NA_character_,
#      BP4_1_5 == 1 ~ "Violación",
#      BP4_1_5 == 2 ~ "No enfrentó"
#    ),
#    # Exhibicionismo (BP4_1_6)
#    aco_exhi = case_when(
#      BP4_1_6 == 9 ~ NA_character_,
#      BP4_1_6 == 1 ~ "Exhibicionismo",
#      BP4_1_6 == 2 ~ "No enfrentó"
#    ),
#    # Tocamientos (BP4_1_7)
#    aco_toca = case_when(
#      BP4_1_7 == 9 ~ NA_character_,
#      BP4_1_7 == 1 ~ "Tocamiento, besos u otro contacto sin consentimiento",
#      BP4_1_7 == 2 ~ "No enfrentó"
#    ),
#    # sexting no consentido(BP4_1_8)
#    aco_pic = case_when(
#      BP4_1_8 == 9 ~ NA_character_,
#      BP4_1_8 == 1 ~ "Recibió contenido sexual no solicitado u ofensivo",
#      BP4_1_8 == 2 ~ "No enfrentó"
#    ),
#    # Mirar esenas(BP4_1_8)
#    aco_escenas = case_when(
#      BP4_1_9 == 9 ~ NA_character_,
#      BP4_1_9 == 1 ~ "Mirar escenas, actos sexuales o pornografía sin consentimiento",
#      BP4_1_9 == 2 ~ "No enfrentó"
#    )
#  )|>
#  mutate(across(starts_with("aco_"), 
#                ~factor(.x, levels = unique(c(setdiff(.x, "No enfrentó"), "No enfrentó"))))
#  )


ENSU_CB <- ENSU_CB %>%
  mutate(
    acos_intimi = case_when(grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 1 | BP4_1_4 == 1| BP4_1_8 == 1) ~ "Intimidación sexual",
                       grepl("^(T2|T4)", trimestre) & (BP4_1_6 == 1| BP4_1_7 == 1 | BP4_1_9 == 1 |
                       BP4_1_3 == 1 | BP4_1_2 == 1 | BP4_1_5 == 1) ~ "Otro tipo de violencia o acoso sexual",
                       grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 2 & BP4_1_4 == 2 & BP4_1_8 == 2 &
                       BP4_1_6 == 2 & BP4_1_7 == 2 & BP4_1_9 == 2 &
                       BP4_1_3 == 2 & BP4_1_2 == 2 & BP4_1_5 == 2) ~ "Ningún tipo de violencia o acoso sexual"),
    acos_abuso = case_when(grepl("^(T2|T4)", trimestre) & (BP4_1_6 == 1| BP4_1_7 == 1 | BP4_1_9 == 1) ~ "Abuso sexual",
                           grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 1 | BP4_1_4 == 1| BP4_1_8 == 1 |
                          BP4_1_3 == 1 | BP4_1_2 == 1 | BP4_1_5 == 1) ~ "Otro tipo de violencia o acoso sexual",
                          grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 2 & BP4_1_4 == 2 & BP4_1_8 == 2 &
                          BP4_1_6 == 2 & BP4_1_7 == 2 & BP4_1_9 == 2 &
                          BP4_1_3 == 2 & BP4_1_2 == 2 & BP4_1_5 == 2) ~ "Ningún tipo de violencia o acoso sexual"),
    acos_acoso = case_when(grepl("^(T2|T4)", trimestre) & (BP4_1_3 == 1) ~ "Acoso sexual",
                           grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 1 | BP4_1_4 == 1| BP4_1_8 == 1|
                          BP4_1_6 == 1 | BP4_1_7 == 1 | BP4_1_9 == 1 |
                          BP4_1_2 == 1 | BP4_1_5 == 1) ~ "Otro tipo de violencia o acoso sexual",
                          grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 2 & BP4_1_4 == 2 & BP4_1_8 == 2 &
                          BP4_1_6 == 2 & BP4_1_7 == 2 & BP4_1_9 == 2 &
                          BP4_1_3 == 2 & BP4_1_2 == 2 & BP4_1_5 == 2) ~ "Ningún tipo de violencia o acoso sexual"),
    acos_viola = case_when(grepl("^(T2|T4)", trimestre) & (BP4_1_2 == 1 | BP4_1_5 == 1) ~ "Violación o intento de violación",
                           grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 1 | BP4_1_4 == 1| BP4_1_8 == 1 |
                          BP4_1_6 == 1 | BP4_1_7 == 1 | BP4_1_9 == 1 |
                          BP4_1_3 == 1) ~ "Otro tipo de violencia o acoso sexual",
                          grepl("^(T2|T4)", trimestre) & (BP4_1_1 == 2 & BP4_1_4 == 2 & BP4_1_8 == 2 &
                          BP4_1_6 == 2 & BP4_1_7 == 2 & BP4_1_9 == 2 &
                          BP4_1_3 == 2 & BP4_1_2 == 2 & BP4_1_5 == 2) ~ "Ningún tipo de violencia o acoso sexual")
                          )|>
  mutate(across(
    starts_with("acos_"),
    ~ factor(.x, levels = c(
      setdiff(unique(.x), c("Otro tipo de violencia o acoso sexual", "Ningún tipo de violencia o acoso sexual")),
      "Otro tipo de violencia o acoso sexual",
      "Ningún tipo de violencia o acoso sexual"
    ))
  ))


ENSU_CB <- ENSU_CB %>%
  mutate(across(starts_with("BP1_9_"), 
                ~recode(., '1' = 'Mucha confianza', 
                        "2" = "Algo de confianza" ,
                        "3" = "Algo de desconfianza",
                        "4" = "Mucha desconfianza",
                        "9" = "NS/NR"))
  )%>%
  mutate(across(starts_with("BP1_9_"), 
                ~factor(.,levels =c('Mucha confianza', 
                                    "Algo de confianza" ,
                                    "Algo de desconfianza",
                                    "Mucha desconfianza",
                                    "NS/NR")))
  )

# 1. Preparar y transformar los datos a formato largo
#ENSU_CB_long <- ENSU_CB %>%
#  pivot_longer(
#    cols = starts_with("vict_"),
#    names_to = "tipo_delito_var",
#    values_to = "estado_victimizacion"
#  ) %>%
#  mutate(
#    tipo_delito_facet = case_when(
#      tipo_delito_var == "vict_robo_veh" ~ "Robo total vehículo",
#      tipo_delito_var == "vict_robo_acc" ~ "Robo accesorios vehículo",
#      tipo_delito_var == "vict_allana" ~ "Allanamiento morada",
#      tipo_delito_var == "vict_robo_asalto" ~ "Robo o asalto",
#      tipo_delito_var == "vict_robo_otro" ~ "Robo otra forma",
#      tipo_delito_var == "vict_amenaza_ext" ~ "Amenaza o extorsión",
#      tipo_delito_var == "vict_fraude_banc" ~ "Fraude bancario",
#      tipo_delito_var == "vict_estafa" ~ "Estafa",
#      TRUE ~ tipo_delito_var
#    ),
#    estado_victimizacion = factor(estado_victimizacion, levels = c(
#      "No víctima", "Robo total vehículo", "Robo accesorios vehículo", "Allanamiento morada",
#      "Robo o asalto", "Robo otra forma", "Amenaza o extorsión",
#      "Fraude bancario", "Estafa"
#    ))
#  )

ENSU_CB <- ENSU_CB |>
  mutate_at(vars(starts_with('BP1_2_')), 
            ~recode(., '1' = 'Seguro(a)', 
                    "2" = "Inseguro(a)" ,
                    "3" = "No aplica",
                    "9" = "NS/NR"))%>%
  mutate_at(vars(starts_with('BP1_2_')),
            ~factor(., levels = c("Inseguro(a)",
                                  "Seguro(a)",
                                  "No aplica",
                                  "NS/NR"))
  )


######Diseños muestrales

# Convertir el objeto survey a un objeto de diseño srvyr



dm_CB_agr<-svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_SEL, data =ENSU_CB, nest = T)

dm_srvyr <- as_survey_design(dm_CB_agr)

dm_CB_VIV_agr<-svydesign(ids = ~UPM_DIS*ID_VIV, strata = ~EST_DIS, weights = ~FAC_VIV, data =ENSU_CB, nest = T)


#dm_CB_VIV_agr_long<-svydesign(ids = ~UPM_DIS*ID_VIV, strata = ~EST_DIS, weights = ~FAC_VIV, data =ENSU_CB_long, nest = T)

#Preprocesamientos gráficas
# Calcular proporciones por demarcación, trimestre y categoría de BP1_1 utilizando srvyr
proporciones_bp1 <- dm_srvyr %>%
  group_by(nom_cdmx1, trimestre, BP1_1) %>%
  summarize(proporcion = survey_mean(na.rm = TRUE))

proporciones_bp1

# Extraer solo la proporción de la categoría "Inseguro (a)"
proporciones_inseguro <- proporciones_bp1 %>%
  filter(BP1_1 == "Inseguro(a)")

print(proporciones_inseguro)

# Calcular el total de la Ciudad de México (incluyendo todas las demarcaciones)
total_cdmx <- subset(dm_srvyr, NOM_ENT == "CIUDAD DE MEXICO") %>%
  group_by(trimestre, BP1_1) %>%
  summarize(proporcion = survey_mean(na.rm = TRUE)) %>%
  filter(BP1_1 == "Inseguro(a)") %>%
  mutate(nom_cdmx1 = "CDMX")

total_cdmx

# Combinar los resultados de las demarcaciones con el total de CDMX
proporciones_combinadas <- bind_rows(proporciones_inseguro, total_cdmx)
 
