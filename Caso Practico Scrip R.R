library(tidyverse)

titanic <- read.csv("Titanicv2.csv")

# Analisis exploratorio de los datos

# Colores personalizados para los graficos (ES NECESARIO CORRER CADA UNA UNA DE LAS LINEAS DE CODIGO PARA VER LOS GRAFICOS)
color_sexo <- c("male" = "navy", "female" = "pink")
color_clase <- c("Lower Clas" = "brown", "Middle Class" = "gray", "Upper Class" = "gold")
color_superviviente <- c("No" = "black", "Yes" = "white")


# Distribucion de pasajeros por clase
titanic %>%
  group_by(Pclass) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Pclass, y = count, fill = Pclass)) +
  geom_col() +
  scale_fill_manual(values = color_clase) +
  labs(title = "Distribucion por clase", x = "Clase", y = "Numero de Pasajeros")

# Proporcion de supervivientes por clase
titanic %>%
  filter(Survived == "Yes") %>%
  group_by(Pclass) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Pclass, y = count, fill = Pclass)) +
  geom_col() +
  scale_fill_manual(values = color_clase) +
  labs(title = "Supervivientes por Clase", x = "Clase", y = "Numero de Pasajeros")

## La proporcion de supervivientes a diferencia de los pasajeros filtrados por clases, se diferencian por la clase alta,
## se aumenta considerablemente la supervivencia en la clase alta en el accidente del Titanic.



# Diagrama de caja de tarifas por clase
titanic %>%
  ggplot(aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  scale_fill_manual(values = color_clase) +
  labs(title = "Diagrama de Caja de Tarifas por Clase", x = "Clase", y = "Tarifa")


# Supervivencia por puerto de embarque
titanic %>%
  group_by(Embarked, Survived) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Embarked, y = count, fill = Survived)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = color_superviviente) +
  labs(title = "Supervivencia por puerto de embarque", x = "Puerto", y = "Cantidad")

# Histograma de pasajeros por edad y sexo
titanic %>%
  ggplot(aes(x = Age, fill = Sex)) +
  geom_histogram(binwidth = 5, position = "stack") +
  scale_fill_manual(values = color_sexo) +
  labs(title = "Histograma de Edad por Sexo", x = "Edad", y = "Frecuencia")

## Ambos sexos tienen valores similares, las edades mas representativas son de 20 a 35 anios.

# Conteo total de pasajeros
conteo_total <- titanic %>% 
  summarise(count = n())
print(conteo_total) # 418

# Conteo de pasajeros por sexo
conteo_por_sexo <- titanic %>%
  group_by(Sex) %>%
  summarise(count = n())
print(conteo_por_sexo) # F = 152 , M = 266

# Conteo de pasajeros que sobrevivieron
conteo_superviviente <- titanic %>%
  group_by(Survived) %>%
  summarise(count = n())
print(conteo_superviviente) # Si = 152 , No = 266 

# Supervivencia por sexo
titanic %>%
  group_by(Sex, Survived) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Sex, y = count, fill = Survived)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = color_superviviente) +
  labs(title = "Supervivencia por Sexo", x = "Sexo", y = "Cantidad")

## El analisis de supervivencia por sexo es definitivo; mujeres viven, hombres mueren, independientemente de los demas factores.
 