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

pinguinos_db %>% 
  select(-especie,-sexo,-isla) %>% 
  corrr::correlate() %>% 
  rearrange() %>%  # ordena las correlaciones
  shave() %>%# limpia las correlaciones repetidas
  fashion()

pinguinos_db %>% 
  select(-especie,-sexo,-isla) %>% 
  corrr::correlate() %>% 
  network_plot()

# Dividimos el dataset en 80% entrenamiento y 20% testeo ------------------
set.seed(1234)
division      <-  initial_split(data = pinguinos_db, prop = .8, strata = sexo)
entrenamiento <-  training(division)
testeo        <-  testing(division)

entrenamiento %>% count(sexo) 
testeo        %>% count(sexo) 


# Receta ------------------------------------------------------------------

masa_recipe <-recipe(masa_corporal_g ~ ., data = entrenamiento) %>% 
              step_corr(all_numeric()) %>%
              step_dummy(all_nominal()) %>% 
              prep()

entrenamiento_juice <- masa_recipe %>%
                       juice()

testeo_bake         <- masa_recipe %>%
                       bake(testeo)

# Creamos nuestro modelo --------------------------------------------------

modelo_lineal <- linear_reg() %>% 
                 set_engine("lm") %>% 
                 set_mode("regression")

translate(modelo_lineal)

ml_ajuste <- modelo_lineal %>%
             fit(masa_corporal_g ~ ., data = entrenamiento_juice)

ml_ajuste



# Prediccion de datos en testeo -------------------------------------------
lm_prediccion <- ml_ajuste %>%
  predict(testeo_bake) %>%
  bind_cols(testeo_bake) 


lm_prediccion %>% metrics(truth = masa_corporal_g, estimate = .pred)

lm_prediccion %>%  ggplot(aes(x=masa_corporal_g, y=.pred,
                               color=masa_corporal_g)) +
                   geom_point(alpha=0.5) +
                   geom_abline() +
                   coord_equal() +
                   ylim(c(2000,6000)) +
                   xlim(c(2000,6000)) 
