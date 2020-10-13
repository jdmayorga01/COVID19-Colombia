# *** COVID - 19 COLOMBIA *** # 
   # Juan Diego Mayorga #
 # Twitter:@jdmayorga01  #

# Packages ----------------------------------------------------------------

# Los paquetes que se utilizaron en este trabajo son los siguientes:

library(dplyr)
library(Hmisc)
library(plyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)

# Data set ----------------------------------------------------------------

getwd()

# URL de la base de datos del INS 
download.file("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD",
              destfile = "Datos/Datos_INS")

# Con UTF-8 logro leer la base de datos bien, dado que tiene tildes. El comando 
# fread es parte del package "data.table"

# Esta base de datos es la de INS 
COVID <- read_csv("Datos/Datos_INS",encoding = 'UTF-8',
                  cols("Fecha de notificación"=))
head(COVID)

# Esta es la base de SALUDATA de la Alcaldia de Bogotá
download.file("http://saludata.saludcapital.gov.co/osb/datos_abiertos_osb/enf-transmisibles/OSB_EnfTransm-COVID-19.csv",
              destfile = "Datos//OSB_EnfTransm-COVID-19.csv")

# Utilizo el argumento skip para saltarme las primera 6 filas que no contienen datos
BOGOTA_SALUDATA <-  fread("Data/OSB_EnfTransm-COVID-19.csv",skip = 7)

# Bogotá ------------------------------------------------------------------

## 1. CON LOS DATOS DE SALUDATA

# Primero analizo los datos de la base y veo en que formato vienen

str(BOGOTA_SALUDATA) # aquí identifico algunos problemas de la base de datos

# Utilizare el pacakge dplyr para editar las columnas que no tienen 
# el formato apropiado

BOGOTA_SALUDATA <- BOGOTA_SALUDATA %>% 
  mutate(F_INICIO=dmy(`Fecha de inicio de síntomas`),
                                              F_DIAG=dmy(`Fecha de diagnóstico`)) %>%
  select(F_DIAG,F_INICIO,Localidad=`Localidad de residencia`,Edad,Sexo,`Tipo de caso`,
         Ubicación,Estado)

# Ahora proceder a sacar los casos agrupados por día para construir los diagramas

Crecimiento_BOG <- BOGOTA_SALUDATA %>% group_by(F_INICIO) %>%
  dplyr::summarise(Total=n()) %>%
  mutate(Acumulado=cumsum(Total),log_acum=log(Acumulado)) %>%
  arrange(desc(F_INICIO))
  
ggplot(Crecimiento_BOG,aes(x=F_INICIO,y=log_acum)) + 
  geom_line(color="#CC0000",size=1) +
  labs(x="Fecha de Diagnóstico",y="Log casos") + 
  theme_classic()

# Crecimiento de casos por localidad

Crecimiento_loc <-  BOGOTA_SALUDATA %>%
  group_by(F_INICIO,Localidad) %>%
  dplyr::summarise(Total=n()) %>% 
  mutate(Acumulado=cumsum(Total),log_acum=log(Acumulado)) %>%
  arrange(desc(F_INICIO))

ggplot(Crecimiento_loc,aes(x=F_INICIO,y=log_acum, color=Localidad)) + 
  geom_line() + 
  facet_wrap(~Localidad)

## 2. CON LOS DATOS DE INS

# El codigo 11001 corresponde a la ciudad de Bogotá,
Bogota <- COVID %>% filter(`Código DIVIPOLA`==11001)



# En este código vamos a seleccionar las variables relevantes,
# además le cambiare el formato a las columnas que tengan fecha dado que vienen 
# en formato character. Esto se hace a través del package "lubridate". 
Bogota <- Bogota %>% select(`Fecha de notificación`,atención,Edad,Sexo,
                                   Estado,`Fecha de muerte`,`Fecha diagnostico`,
                                   `Fecha recuperado`,`Tipo recuperación`) %>%
  mutate(F_not=ymd_hms(`Fecha de notificación`),
               F_muerte=ymd_hms(`Fecha de muerte`),
               F_diagnos=ymd_hms(`Fecha diagnostico`),
               F_recuperado=ymd_hms(`Fecha recuperado`)) %>%
  select(-`Fecha de notificación`,-`Fecha de muerte`,
               -`Fecha diagnostico`,-`Fecha recuperado`) %>%
  arrange(desc(F_not))

## 1. En primera instancia, calcularé los casos totales por día 
# y el acumulado en el tiempo


Bogota_crecimiento <- Bogota %>% group_by(F_not)%>% # Agrupamos por fecha
  dplyr::summarise(Total=n()) %>% # Sacamos el total de casos por fecha
  mutate(Acum=cumsum(Total),log_acum=log(Acum)) %>% # Sacamos el logaritmo del acumulado
  arrange(desc(F_not)) # Con este argumento se puede analizar la ultima fecha de actualización 

# Graficamos el logaritmo del crecimiento en el tiempo de los casos en Bogotá 

ggplot(Bogota_crecimiento,aes(x=F_not,y=log_acum))+geom_line()

## 2. Analisis por estado de la persona 
# Se busca crear una tasa de muertes sobre recuperados y analizar 
# su trayectoria en el tiempo

Bogota_estado <- Bogota %>%
  group_by(F_not,atención) %>% # Agrupamos por fecha de notificación y tipo de atención
  dplyr::summarise(total=n())

# Dada la estructura de los datos, debemos extraer por un lado los casos
# recuperados y en otro los fallecidos, de esta forma unirnos a travpés de la función
# merge 

Recuperados <- Bogota_estado %>%
        filter(atención=="Recuperado")
        
Fallecidos <- Bogota_estado %>%
        filter(atención=="Fallecido")

# Se utuiliza el argumento all=T para que se realice un inner join y de esta 
# forma TODOS los datos se preserven
Rec_Fall <- merge(Recuperados,Fallecidos,by="F_not",all = T,
      suffixes = c("_Recuperado","_Fallecido"))

# A traves del verbo mutate del paquete dplyr generamos una nueva columna que 
# nos muestre la proporción de muertos sobre recuperados

Rec_Fall <- Rec_Fall %>% mutate(Per_death_rec=total_Fallecido/total_Recuperado)

ggplot(Rec_Fall,aes(x=F_not,y=Per_death_rec)) + geom_line()

## 3. 

# Colombia ----------------------------------------------------------------

# Nuevamente, como en el caso anterior, tuve que arreglar la base de datos de 
# tal forma de que los datos sean manipulables
COVID_select <- COVID %>% select(`Fecha de notificación`,Ciudad=`Ciudad de ubicación`,
                          Departamento=`Departamento o Distrito`
                          ,atención,Edad,Sexo,
                      Estado,`Fecha de muerte`,`Fecha diagnostico`,
                      `Fecha recuperado`,`Tipo recuperación`) %>%
  mutate(F_not=ymd_hms(`Fecha de notificación`),
               F_muerte=ymd_hms(`Fecha de muerte`),
               F_diagnos=ymd_hms(`Fecha diagnostico`),
               F_recuperado=ymd_hms(`Fecha recuperado`)) %>%
  select(-`Fecha de notificación`,-`Fecha de muerte`,
               -`Fecha diagnostico`,-`Fecha recuperado`)

## 1. En primera instancia, calcularé los casos totales por día 
# y el acumulado en el tiempo

COVID_crecimiento <- COVID_select %>%
  group_by(Departamento,F_not) %>% 
  dplyr::summarise(Total=n()) %>%
  mutate(Acum=cumsum(Total),log_acum=log(Acum)) 

# Graficamos el logaritmo del crecimiento en el tiempo de los casos en Colombia 
# por departamento

ggplot(COVID_crecimiento,aes(x=F_not,y=log_acum),
       colour=Departamento) +
  geom_line() +
  facet_wrap(~Departamento,scales = "free_y") # Este comando nos permite 
        # tener todos los casos en una sola gráfica



