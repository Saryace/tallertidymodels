
# Instalacion paquetes ----------------------------------------------------

#install.packages(c("tidyverse", "tidymodels")), dependencies = TRUE)

#install.packages(c("remotes", "ggsignif")), dependencies = TRUE)

#remotes::install_github("cienciadedatos/datos")

#remotes::install_github("tidymodels/corrr")

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(remotes)
library(datos)
library(ggsignif)
library(corrr)

# estilo ggplot
theme_set(theme_bw())

# Datos -------------------------------------------------------------------
# cargar la database
pinguinos <- datos::pinguinos
# echar un vistazo
glimpse(pinguinos)
#limpieza de datos
pinguinos_db <-  pinguinos %>%
  drop_na() %>% # las observaciones con datos ausentes
  select(-anio) # la columna anio
# revisamos nuestro nuevo archivo
glimpse(pinguinos_db) 

# Exploramos datos visualmente --------------------------------------------
pinguinos_db %>% ggplot(aes(
  x = largo_aleta_mm,
  y = masa_corporal_g,
  color = sexo,
  size = masa_corporal_g)) +
  geom_point(alpha = 0.5) 

# Diferencias macho y hembra por especie ----------------------------------
pinguinos_db %>% ggplot(aes(x=sexo, y=masa_corporal_g, fill=sexo)) + 
                        geom_boxplot() +
                        facet_wrap(~especie) +
                        geom_signif(comparisons = list(c("macho", "hembra")), 
                        map_signif_level=TRUE,
                        test = "t.test")
# Correlación entre variables numéricas -----------------------------------
pinguinos_db %>% 
  select(-especie,-sexo,-isla) %>% 
  corrr::correlate() 

# Correlacion entre variables numericas -----------------------------------
pinguinos_db %>% 
  select(-especie,-sexo,-isla) %>% 
  corrr::correlate() %>% 
  rearrange() %>%  # ordena las correlaciones
  shave() %>%# limpia las correlaciones repetidas
  fashion()


# Correlacion entre variables numericas -----------------------------------
pinguinos_db %>% 
  select(-especie,-sexo,-isla) %>% 
  corrr::correlate() %>% 
  network_plot()

# Dividimos el dataset en 80% entrenamiento y 20% testeo ------------------
set.seed(1234)
division      <-  initial_split(data = pinguinos_db, prop = .8)
entrenamiento <-  training(division)
testeo        <-  testing(division)

entrenamiento %>% count(sexo) 
testeo        %>% count(sexo) 


# Estratificar datos ------------------------------------------------------
set.seed(1234)
division_strat<-  initial_split(data = pinguinos_db, prop = .8, strat = sexo)
entrenamiento_strat <-  training(division_strat)
testeo_strat        <-  testing(division_strat)

entrenamiento_strat %>% count(sexo) 
testeo_strat        %>% count(sexo) 

# Receta ------------------------------------------------------------------

masa_recipe <-recipe(masa_corporal_g ~ ., data = entrenamiento) %>% 
              step_corr(all_numeric()) %>% 
              step_dummy(all_nominal()) %>% 
              prep()

entrenamiento_juice <- masa_recipe %>%
                       juice()

testeo_bake         <- masa_recipe %>%
                       bake(testeo)


# Diferencia juice and bake -----------------------------------------------

entrenamiento_bake  <- masa_recipe %>%
                       bake(entrenamiento)

identical(entrenamiento_juice,entrenamiento_bake)

# Errores -----------------------------------------------------------------

masa_recipe_noprep   <-  recipe(masa_corporal_g ~ ., data = pinguinos_db) %>% 
                         step_normalize(all_numeric()) %>% 
                         step_dummy(all_nominal())

entrenamiento_juice_noprep <-   masa_recipe_noprep %>%
                                juice()

# Creamos nuestro modelo --------------------------------------------------

modelo_lineal <- linear_reg() %>% 
                 set_engine("lm") %>% 
                 set_mode("regression")


modelo_rf     <- rand_forest() %>% 
                 set_engine("ranger") %>% 
                 set_mode("regression") 

# Modelos -----------------------------------------------------------------

translate(modelo_rf)

translate(modelo_lineal)


# Ajustes -----------------------------------------------------------------


ml_ajuste <- modelo_lineal %>%
             fit(masa_corporal_g ~ ., data = entrenamiento_juice)

rf_ajuste <- modelo_rf %>%
             fit(masa_corporal_g ~ ., data = entrenamiento_juice)


# Prediccion de datos en testeo -------------------------------------------
lm_prediccion <- ml_ajuste %>%
                 predict(testeo_bake) %>%
                 bind_cols(testeo_bake) 

rf_prediccion <- rf_ajuste %>%
                 predict(testeo_bake) %>%
                 bind_cols(testeo_bake) 

# Metricas ----------------------------------------------------------------


lm_prediccion %>% metrics(truth = masa_corporal_g, estimate = .pred)

rf_prediccion %>% metrics(truth = masa_corporal_g, estimate = .pred)


# Comparacion dos modelos -------------------------------------------------


comparacion_prediccion <- lm_prediccion %>% 
                          full_join(., rf_prediccion,
                                    by = "masa_corporal_g") %>% 
                          select(.pred.x, .pred.y, masa_corporal_g) %>% 
                          rename(LM = .pred.x, RF = .pred.y) %>% 
                          pivot_longer(LM:RF, names_to = "modelo")

comparacion_prediccion %>%  ggplot(aes(x=masa_corporal_g, y=value, 
                               color=masa_corporal_g, shape=modelo)) +
                               geom_point(alpha=0.5) +
                               geom_abline() +
                               coord_equal() +
                               ylim(c(2000,6000)) +
                               xlim(c(2000,6000)) 
