library(tidyverse)
data("starwars")
# Lo primero que vamos a realizaar va ser seleccionar todas la columna menos la del nombre.
starwars %>% select(-name)
# Ahora solo vamos a seleccionar las que están subrayadas.
starwars %>% select(contains("_"))
# Ahora vamos a hacer lo mismo pero solo con las columnas que emiezan con la s
starwars %>% select(starts_with("s"))
# Ahora vamos a crear un data frame con lo nombres y planetas de los personajes de sw.
homeworld <- starwars %>% select(name,homeworld)
# Vamos a filtrar por la especie para que sea solo humano.
human <- starwars %>% filter(species == "Human")
#Después de filtrar or humano ahora vamos a filtrar por humanos de un planeta en específico.
starwars %>% filter(species == "Human", homeworld == "Tatooine")
#Por último vamos a crear un nuevo dataframe con todas las especies menos droides.
starwars_nodroids <- starwars %>% filter(species != "Droid")
# Vamos a agrupar y seleccionar datos a continuación.
starwars %>% group_by(species) %>% tally()
# así aparecerán todas las especies
print(starwars %>% group_by(species) %>% tally(),n=38)
#Añadiremos otra variable.
starwars %>% group_by(species, gender) %>% tally()
#Hago lo mismo para que se vean todas las lineas.
print(starwars %>% group_by(species, gender) %>% tally(),n=42)
#Tambien lo podemos guardar en un dataframe utilizando <-
table_gender <- starwars %>% group_by(species, gender) %>% tally()
#Vamos a clacular la media.
starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
#en el siguiente comando vamos a calcular la desviación estandar.
starwars %>% group_by(species) %>% summarise(sd_height = sd(height, na.rm = T),sd_mass = sd(mass,na.rm = T))
#Realizaremos un plot(gráfico) de masa/altura
ggplot(starwars, aes(height, mass)) + geom_point()
#ahora modificamos el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")
#ahora el tamaño del punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)
#ahora modifico el color y el fondo.
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light()
starwarswthJabba <- starwars %>% filter (name != "Jabba Desilijic Tiure")
ggplot(starwarswthJabba, aes(height, mass)) + geom_point(colour = "red")
#ahora voy a descargar el toy que lo he importado de la carpeta como ruta de acceso.
#voy a crear un data con cada sexo
toymujer<- toy %>% filter (Sex != "Men")
toyhombre<- toy %>% filter (Sex != "Women")
#calculo los valores medio hombre
toyhombre %>% group_by(Sex) %>%
  summarise(
    mean_Height = mean(Height_cm, na.rm = T),
    mean_Weight = mean(Weight_Kg,na.rm = T),
    mean_IMC = mean(IMC,na.rm = T),
    mean_IAS = mean(IAS, na.rm = T),
    mean_Ccintura = mean(Ccintura, na.rm =T)
  )
#calculo valores medios mujer
toymujer %>% group_by(Sex) %>%
  summarise(
    mean_Height = mean(Height_cm, na.rm = T),
    mean_Weight = mean(Weight_Kg,na.rm = T),
    mean_IMC = mean(IMC,na.rm = T),
    mean_IAS = mean(IAS, na.rm = T),
    mean_Ccintura = mean(Ccintura, na.rm =T)
  )
#La tabla ya la he creado es toymujer.
#voy a calcular las mujeres con sobrepeso.
toymujerobesas<- toymujer %>% filter (IMC_clas != "Normal")
toymujerobesas1<- toymujerobesas %>% filter (IMC_clas != "Obesity")
#voy a crear el plot
ggplot(toy, aes(IMC,Weight_Kg)) + geom_point(colour = "orange") + theme_light()
#voy a repetir con elpeso
toygordo<- toy %>% filter (IMC_clas != "Normal")
ggplot(toygordo, aes(IMC,Weight_Kg)) + geom_point(colour = "black",pch =7) 
