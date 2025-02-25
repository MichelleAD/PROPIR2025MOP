# Librerías ---------------------------------------------------------------

library(ggplot2); library(dplyr) 
library(reshape); library(tools); library(readxl);
library(stringr)

# Datos -------------------------------------------------------------------



PROYECCIONES <- read_excel("estimaciones-y-proyecciones-2002-2035-comunas (1).xlsx", sheet = 1,skip = 3)
PROYECCIONES <- head(PROYECCIONES, -1)

#con el head -1, quito la última fila, creo que salían los totales, por eso es que 
#no la incluí en la base.


CUT2018 <- read_excel("cut_2018_v04.xls")
View(CUT2018)

colnames(CUT2018)[colnames(CUT2018) == "Código Comuna 2018"] <- "Código"
CUT2018$Código[1:207] <- substr(CUT2018$`Código`[1:207], 2, nchar(CUT2018$`Código`[1:207]))

#le quitamos el cero a las primeras 207 observaciones, ya que las demás bases tienen
#el código sin empezar desde cero (en el cut empiezan algunas desde cero), por lo que es necesario dejarlas en el mismo formato.




POBR_MULTI <- read_excel("Estimaciones_Indice_Pobreza_Multidimensional_Comunas_2022.xlsx", skip = 2)
POBR_MULTI <- head(POBR_MULTI,-5)


PROPIR2025 <- read_excel("Propir2025.xlsx",sheet="detalle")

POB_2017 <- read_excel("Poblacion_sexo_edad_urbana_rural.xlsx", sheet = "TOTAL 2017",skip =3)
POB_2017$TOTAL2017 <- POB_2017$Total
POB_2017$Código <- POB_2017$`CÓDIGO COMUNA`

#establecí un mismo nombre para las columnas que indiquen el código de la comuna




# Formato -----------------------------------------------------------------


# a continuación, cree una columna nueva, llamada Comunas que contiene los string con un mismo formato
# todas en mayúsculas, sin tildes de ningún tipo.


POBR_MULTI <- POBR_MULTI %>% mutate(Comunas = toupper(`Nombre comuna`))
CUT2018 <- CUT2018 %>%  mutate(Comunas = toupper(`Nombre Comuna`))
PROYECCIONES <-  PROYECCIONES %>% mutate(Comunas = toupper(`Nombre Comuna`))

POBR_MULTI <- POBR_MULTI %>%
  mutate(
    Comunas = str_replace_all(Comunas, c("Ö" = "O", "Ë" = "E", "Ï" = "I", "Ä" = "A", "Ü" = "U",
                                         "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U"))
  )


PROYECCIONES <- PROYECCIONES %>%
  mutate(
    Comunas = str_replace_all(Comunas, c("Ö" = "O", "Ë" = "E", "Ï" = "I", "Ä" = "A", "Ü" = "U",
                                         "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U"))
  )


CUT2018 <- CUT2018 %>%
  mutate(
    Comunas = str_replace_all(Comunas, c("Ö" = "O", "Ë" = "E", "Ï" = "I", "Ä" = "A", "Ü" = "U",
                                         "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U"))
  )



#junto las bases (con la llave código) en una base que contengan las proyecciones poblacionales junto a la 
#pobreza multidimensional.



PROYECCIONES$Código <- as.character(PROYECCIONES$Comuna)
PROYECCIONES_POBR_MULTI <- left_join(PROYECCIONES, POBR_MULTI, by = "Código")
PROYECCIONES_POBR_MULTI <- left_join(PROYECCIONES_POBR_MULTI,POB_2017, by = "Código")


PROYECCIONES_POBR_MULTI2 <- PROYECCIONES_POBR_MULTI #creo esta base de repuesto

summary(PROYECCIONES_POBR_MULTI) #el único NA es de la Antártica


PROYECCIONES_POBR_MULTI <- PROYECCIONES_POBR_MULTI %>% select(-Región) 
#elimino la región que está duplicada


PROPIR2025 <- PROPIR2025 %>% mutate(Comunas = COMUNAS) 
# creo la variable Comunas para después juntarlo con el cut y 
#agregar el código





# Join --------------------------------------------------------------------



#Aquí le añado el código a la base del PROPIR, utilizando Comunas como llave

PROPIR2025 <- left_join(PROPIR2025,CUT2018, by = "Comunas")


#lo anterior lo realicé para poder unir la base de las proyecciones-pobreza a través
#de la llave código

BASE <- PROPIR2025 %>%
  left_join(PROYECCIONES_POBR_MULTI , by = "Código") 






# Arreglos ----------------------------------------------------------------

#creamos dos bases que luego uniremos en el html, esto porque hay comunas que no tienen
#inversión, por lo que la BASE no tendrá incluídas algunas comunas, por lo que
#pobmulti contiene las proyecciones, y la pobreza multidimensional de estas.



orden_regiones <- c("INTERREGIONAL","MAGALLANES","AYSÉN","LOS LAGOS","LOS RÍOS","LA ARAUCANÍA","BIOBÍO","ÑUBLE","MAULE",
                    "O'HIGGINS", "METROPOLITANA","VALPARAÍSO","COQUIMBO","ATACAMA","ANTOFAGASTA","TARAPACÁ","ARICA Y PARINACOTA")



#categorizamos las regiones de la base que se encuentran en número

BASE <- BASE %>% mutate(RegionAB = case_when(REGION == "1" ~ "TARAPACÁ",
                                             REGION == "3" ~ "ATACAMA",
                                             REGION == "2" ~ "ANTOFAGASTA",
                                             REGION == "4" ~ "COQUIMBO",
                                             REGION == "5" ~ "VALPARAÍSO",
                                             REGION == "6" ~ "O'HIGGINS",
                                             REGION == "7" ~ "MAULE",
                                             REGION == "16" ~ "ÑUBLE",
                                             REGION == "8" ~ "BIOBÍO",
                                             REGION == "9" ~ "LA ARAUCANÍA",
                                             REGION == "10"  ~ "LOS LAGOS",
                                             REGION == "11" ~ "AYSÉN",
                                             REGION == "12"  ~ "MAGALLANES",
                                             REGION == "RM" ~  "METROPOLITANA" ,
                                             REGION == "14" ~ "LOS RÍOS",
                                             REGION == "15"  ~ "ARICA Y PARINACOTA",
                                             TRUE ~ "INTERREGIONAL"
))







PROYECCIONES_POBR_MULTI2 <- PROYECCIONES_POBR_MULTI2 %>% mutate(RegionAB = case_when(Región == "Tarapacá" ~ "TARAPACÁ",
                                                                                   Región == "Atacama" ~ "ATACAMA",
                                                                                   Región == "Antofagasta" ~ "ANTOFAGASTA",
                                                                                   Región == "Coquimbo" ~ "COQUIMBO",
                                                                                   Región == "Valparaíso" ~ "VALPARAÍSO",
                                                                                   Región == "O'Higgins" ~ "O'HIGGINS",
                                                                                   Región == "Maule" ~ "MAULE",
                                                                                   Región == "Ñuble" ~ "ÑUBLE",
                                                                                   Región == "Biobío" ~ "BIOBÍO",
                                                                                   Región == "La Araucanía" ~ "LA ARAUCANÍA",
                                                                                   Región == "Los Lagos"  ~ "LOS LAGOS",
                                                                                   Región == "Aysén" ~ "AYSÉN",
                                                                                   Región == "Magallanes"  ~ "MAGALLANES",
                                                                                   Región == "Metropolitana" ~  "METROPOLITANA" ,
                                                                                   Región == "Los Ríos" ~ "LOS RÍOS",
                                                                                   Región == "Arica y Parinacota"  ~ "ARICA Y PARINACOTA",
                                                                                   TRUE ~ "MAGALLANES"
))




BASE <- BASE %>% mutate(NombreComuna = Comunas)  

BASE$RegionAB <- factor(BASE$RegionAB, levels = orden_regiones)


#cambiamos los nombres de algunas columnas para no cambiar las del html

colnames(BASE)[colnames(BASE) == "SERVICIO_NOMBRE"] <- "Servicio"
colnames(BASE)[colnames(BASE) == "AGNO_ACT"] <- "Monto 2025"
colnames(BASE)[colnames(BASE) == "EJE"] <- "Eje Ministerial"


BASE <- BASE %>% mutate(Categoría = case_when(N_A == "A" ~ "Arrastre",
                                              TRUE ~ "Nuevo")) 



unique(PROYECCIONES_POBR_MULTI$Comunas.x)
PROYECCIONES_POBR_MULTI2$Comunas <- PROYECCIONES_POBR_MULTI2$Comunas.x
PROYECCIONES_POBR_MULTI2 <- PROYECCIONES_POBR_MULTI2 %>% mutate(NombreComuna = Comunas)  


#obtenemos las bases con las que trabajaremos el html

writexl::write_xlsx(BASE, "BASE_PROPIR.xlsx")


writexl::write_xlsx(PROYECCIONES_POBR_MULTI2, "PROYECCIONES_POBmultiPROPIR.xlsx")


