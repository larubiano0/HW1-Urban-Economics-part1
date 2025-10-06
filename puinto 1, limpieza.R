library(dplyr)

df <- readRDS("C:/Users/pc/Downloads/proyecto urbaniaaa/dataTaller01_PriceIndeces.Rds")
# Note que el type de la base es grouped df, desagrupemosla:
df <- readRDS("C:/Users/pc/Downloads/proyecto urbaniaaa/dataTaller01_PriceIndeces.Rds")
df <- dplyr::ungroup(df)

# Verifiquemos
dplyr::is_grouped_df(df)  # debe devolver FALSE
dplyr::group_vars(df)     # debe devolver character(0)

n <- nrow(df); n

# NAs por variable
colSums(is.na(df))
str(df)   # tipos de datos

# Ahora, revisemos con base al diccionario las variables y ver si toca limpiar
# 1) pin (identificador de predio / propiedad)
#    Acá revisamos:
#    - NAs, unicidad, ventas repetidas por pin
#    - Duplicados exactos pin-year-price
sum(is.na(df$pin))
length(unique(df$pin))
# ventas por pin
df %>% count(pin, name = "n_sales") %>%
  arrange(desc(n_sales)) %>% head(20)
summary(df %>% count(pin, name = "n_sales") %>% pull(n_sales))

# Duplicados exactos de mismo pin-year-sale_price
df %>% count(pin, year, sale_price, name = "n") %>%
  filter(n > 1) %>% arrange(desc(n)) %>% head(20)


# 2) year (año de venta)
#    Acá revisamos:
#    - Rango, valores fuera de rango/0/NA
#    - Distribución simple por año

sum(is.na(df$year)); min(df$year, na.rm=TRUE); max(df$year, na.rm=TRUE)
df %>% count(year) %>% arrange(year) %>% head(30)
df %>% count(year) %>% arrange(desc(n)) %>% head(10)

# 3) sale_price (precio de venta)
#    Acá revisamos:
#    - NAs, ceros/negativos, colas (quantiles)
#    - Precios muy bajos (posibles no-arms-length)

sum(is.na(df$sale_price))
sum(df$sale_price <= 0, na.rm = TRUE)
quantile(df$sale_price, probs = c(0, .01, .05, .5, .95, .99, 1), na.rm = TRUE)

# Conteo de ventas por umbral bajo
sum(df$sale_price < 10000, na.rm = TRUE)
df %>% filter(sale_price < 10000) %>% select(pin, year, sale_price) %>% head(20)

# 4) township_code
#    Acá revisamos:
#    - NAs, cardinalidad, top frecuencias

sum(is.na(df$township_code))
length(unique(df$township_code))
df %>% count(township_code, sort = TRUE) %>% head(20)


# 5) class (clase de propiedad)
#    Acá revisamos:
#    - Categorías, limpieza potencial (pero aquí solo vemos)

sum(is.na(df$class))
df %>% count(class, sort = TRUE) %>% head(50)

# 6) type_of_residence
#    Acá revisamos:
#    - Categorías, relación con class

sum(is.na(df$type_of_residence))
df %>% count(type_of_residence, sort = TRUE) %>% head(50)

# Tabla cruzada resumida
df %>% count(class, type_of_residence, sort = TRUE) %>% head(50)

# 7) year_built (año de construcción)
#    Acá revisamos:
#    - Rango, NAs, inconsistencias year_built > year

sum(is.na(df$year_built))
min(df$year_built, na.rm=TRUE); max(df$year_built, na.rm=TRUE)
# Inconsistencias
df %>% filter(!is.na(year_built), !is.na(year), year_built > year) %>%
  select(pin, year_built, year, sale_price) %>% head(20)
nrow(df %>% filter(!is.na(year_built), !is.na(year), year_built > year))


# 8) building_sqft (área construida)
#    Acá revisamos:
#    - NAs, <=0, quantiles

sum(is.na(df$building_sqft))
sum(df$building_sqft <= 0, na.rm = TRUE)
quantile(df$building_sqft, probs = c(0, .01, .05, .5, .95, .99, 1), na.rm = TRUE)

# 9) land_sqft (área de lote)
#    Acá revisamos:
#    - NAs estructurales (condos), ceros, quantiles en no-NA
#    - Casos donde building_sqft > land_sqft (bandera para casas)

sum(is.na(df$land_sqft))
sum(df$land_sqft == 0, na.rm = TRUE)
quantile(df$land_sqft, probs = c(0, .01, .05, .5, .95, .99, 1), na.rm = TRUE)

# building > land (cuando land > 0): posible inconsistencia para casas
df %>%
  filter(!is.na(land_sqft), land_sqft > 0, !is.na(building_sqft), building_sqft > land_sqft) %>%
  select(pin, year, building_sqft, land_sqft, class, type_of_residence) %>% head(20)
nrow(df %>%
       filter(!is.na(land_sqft), land_sqft > 0, !is.na(building_sqft), building_sqft > land_sqft))

# 10) Dormitorios y cuartos (num_bedrooms, num_rooms)
#     Acá revisamos:
#     - NAs, rooms >= bedrooms, outliers simples

sum(is.na(df$num_bedrooms)); sum(is.na(df$num_rooms))
# Violaciones rooms < bedrooms
df %>% filter(!is.na(num_rooms), !is.na(num_bedrooms), num_rooms < num_bedrooms) %>%
  select(pin, year, num_rooms, num_bedrooms, class, type_of_residence) %>% head(20)
nrow(df %>% filter(!is.na(num_rooms), !is.na(num_bedrooms), num_rooms < num_bedrooms))

# Rangos
quantile(df$num_bedrooms, probs = c(0, .01, .5, .99, 1), na.rm = TRUE)
quantile(df$num_rooms,    probs = c(0, .01, .5, .99, 1), na.rm = TRUE)

# 11) Baños (num_full_baths, num_half_baths)
#     Acá revisamos:
#     - NAs, extremos, combinación básica

sum(is.na(df$num_full_baths)); sum(is.na(df$num_half_baths))
quantile(df$num_full_baths, probs = c(0, .01, .5, .99, 1), na.rm = TRUE)
quantile(df$num_half_baths, probs = c(0, .01, .5, .99, 1), na.rm = TRUE)


# 12) Chimeneas (num_fireplaces)
#     Acá revisamos:
#     - NAs, extremos
sum(is.na(df$num_fireplaces))
quantile(df$num_fireplaces, probs = c(0, .01, .5, .99, 1), na.rm = TRUE)

# 13) Categóricas de calidad, acabados y sistemas
#     Acá revisamos:
#     - Conteos de categorías más frecuentes

df %>% count(construction_quality, sort = TRUE) %>% head(30)
df %>% count(attic_finish,         sort = TRUE) %>% head(30)
df %>% count(garage_attached,      sort = TRUE) %>% head(30)
df %>% count(garage_area_included, sort = TRUE) %>% head(30)
df %>% count(garage_size,          sort = TRUE) %>% head(30)
df %>% count(garage_ext_wall_material, sort = TRUE) %>% head(30)
df %>% count(attic_type,           sort = TRUE) %>% head(30)
df %>% count(basement_type,        sort = TRUE) %>% head(30)
df %>% count(ext_wall_material,    sort = TRUE) %>% head(30)
df %>% count(central_heating,      sort = TRUE) %>% head(30)
df %>% count(basement_finish,      sort = TRUE) %>% head(30)
df %>% count(roof_material,        sort = TRUE) %>% head(30)
df %>% count(site_desirability,    sort = TRUE) %>% head(30)
df %>% count(renovation,           sort = TRUE) %>% head(30)
df %>% count(recent_renovation,    sort = TRUE) %>% head(30)
df %>% count(porch,                sort = TRUE) %>% head(30)
df %>% count(central_air,          sort = TRUE) %>% head(30)

# 14) Señales útiles para CASA vs APARTAMENTO
#     Acá revisamos:
#     - land_sqft = 0/NA (suele ser condo), patrones en type_of_residence y class

# Proporción con land_sqft NA o 0
mean(is.na(df$land_sqft))
mean(df$land_sqft == 0, na.rm = TRUE)

# Top categorías de type_of_residence y class ya listadas arriba.
# cruzar land_sqft con class para ver el patrón:
df %>% mutate(land_flag = ifelse(is.na(land_sqft) | land_sqft == 0, "sin_lote", "con_lote")) %>%
  count(class, land_flag, sort = TRUE) %>% head(50)

#Basado en lass salidas anteriores de 1 a 14, limpiemos la base, primero para Hedónico:

# LIMPIEZA PARA HEDÓNICO
rm(list=ls())
library(dplyr)
library(tidyr)

df <- readRDS("C:/Users/pc/Downloads/proyecto urbaniaaa/dataTaller01_PriceIndeces.Rds")
df <- dplyr::ungroup(df)

# 1) Nos quedamos con variables que usaremos en el hedónico
vars_hed <- c(
  "pin","year","sale_price",
  "building_sqft","num_bedrooms","num_rooms",
  "num_full_baths","num_half_baths","num_fireplaces",
  "type_of_residence","township_code","year_built",
  "land_sqft","central_air","renovation"
)
df_h <- df[, vars_hed]

# 2) Tratar "" como NA SOLO en variables que usaremos (evita NA encubiertos)
df_h <- df_h %>%
  mutate(
    type_of_residence = na_if(type_of_residence, ""),
    central_air       = na_if(central_air,       "")
  )

# 3) Eliminar filas con faltantes del bloque “interiores”
#    (coincide con ~1,060 filas: building_sqft/rooms/bedrooms/baths/fireplaces + estilo)
n0 <- nrow(df_h)
df_h <- df_h %>% drop_na(c(
  "building_sqft","num_bedrooms","num_rooms",
  "num_full_baths","num_half_baths","num_fireplaces",
  "type_of_residence"
))
n1 <- nrow(df_h)

# 4) Quitar inconsistencias simples (rooms < bedrooms)
df_h <- df_h %>% filter(num_rooms >= num_bedrooms)
n2 <- nrow(df_h)

# 5) Filtrar outliers SUAVES (basado en los quantiles observados)
#    - building_sqft: P99 ~ 3,582 ⇒ límite alto 5,000 y bajo 300
#    - num_rooms:     P99 ~ 10    ⇒ límite alto 20
#    - demás: límites razonables para evitar valores extremos raros
df_h <- df_h %>%
  filter(
    building_sqft >= 300, building_sqft <= 5000,
    num_rooms     >= 2,   num_rooms     <= 20,
    num_bedrooms  >= 1,   num_bedrooms  <= 10,
    num_full_baths>= 0,   num_full_baths<= 8,
    num_half_baths>= 0,   num_half_baths<= 6,
    num_fireplaces>= 0,   num_fireplaces<= 6
  )
n3 <- nrow(df_h)

# 6) Variables auxiliares
df_h <- df_h %>%
  mutate(
    sin_lote = as.integer(is.na(land_sqft) | land_sqft == 0),
    central_air_bin = case_when(
      central_air == "Central A/C"    ~ 1L,
      central_air == "No Central A/C" ~ 0L,
      TRUE                            ~ NA_integer_
    )
  )

# 7) Chequeos rápidos
list(
  filas_iniciales              = n0,
  tras_drop_faltantes_bloque   = n1,
  tras_reglas_consistencia     = n2,
  tras_outliers_suaves         = n3
)

colSums(is.na(df_h[, c(
  "sale_price","building_sqft","num_bedrooms","num_rooms",
  "num_full_baths","num_half_baths","num_fireplaces"
)]))

quantile(df_h$building_sqft, probs = c(0,.01,.5,.99,1), na.rm = TRUE)
quantile(df_h$num_rooms,     probs = c(0,.01,.5,.99,1), na.rm = TRUE)
quantile(df_h$sale_price,    probs = c(0,.01,.5,.99,1), na.rm = TRUE)

# Guardar muestra para hedónica
saveRDS(df_h, "C:/Users/pc/Downloads/proyecto urbaniaaa/hedonico_minimo.Rds")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Hedónico: índice anual (2000=100) con FE por township

rm(list = setdiff(ls(), c("df_h")))
suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(broom)
  library(ggplot2)
})

# Cargar la muestra limpia para hedónico
  df_h <- readRDS("C:/Users/pc/Downloads/proyecto urbaniaaa/hedonico_minimo.Rds")

# Variables de trabajo

# Año base fijo (el dataset va 2000-2020)
base_ref <- "2000"

df_h <- df_h %>%
  mutate(
    # dependiente
    log_sale_price = log(sale_price),
    # controles estructurales
    log_building   = log(building_sqft),
    baths_equiv    = num_full_baths + 0.5 * num_half_baths,
    # edad (y cuadrado) de la vivienda
    age            = pmax(year - year_built, 0),
    age2           = age^2,
    # factores
    year_f         = factor(year),
    year_f         = stats::relevel(year_f, ref = base_ref),
    tor_f          = factor(type_of_residence)
  )

# Especificaciones de modelo
# Mod1: (estructura + estilo) + FE ubicación + dummies de año
fml_m1 <- log_sale_price ~ year_f + log_building + num_rooms + num_bedrooms +
  baths_equiv + num_fireplaces + i(tor_f) | township_code

# Mod2: M1 + dummies limpias adicionales
fml_m2 <- log_sale_price ~ year_f + log_building + num_rooms + num_bedrooms +
  baths_equiv + num_fireplaces + i(tor_f) +
  sin_lote + central_air_bin | township_code

# Mod3: M2 + edad y edad^2
fml_m3 <- log_sale_price ~ year_f + log_building + num_rooms + num_bedrooms +
  baths_equiv + num_fireplaces + i(tor_f) +
  sin_lote + central_air_bin + age + age2 | township_code

# 3) Estimación (cluster por township)
m1 <- feols(fml_m1, data = df_h, vcov = ~ township_code)
m2 <- feols(fml_m2, data = df_h, vcov = ~ township_code)
m3 <- feols(fml_m3, data = df_h, vcov = ~ township_code)

# 4) Índices anuales (2000=100)
# Función simple para extraer dummies de año y armar índice
extrae_indice <- function(modelo, base_ref = "2000") {
  tt <- broom::tidy(modelo, conf.int = TRUE)
  # En fixest, los dummies salen como términos que empiezan por "year_f"
  idx <- tt %>%
    dplyr::filter(grepl("^year_f", term)) %>%
    dplyr::mutate(
      year  = gsub("^year_f", "", term),
      year  = as.integer(year),
      idx   = 100 * exp(estimate),          # índice relativo a base (log)
      idx_l = 100 * exp(conf.low),
      idx_u = 100 * exp(conf.high)
    ) %>%
    dplyr::select(year, idx, idx_l, idx_u) %>%
    dplyr::arrange(year)
  # Insertamos el año base = 100
  base_row <- data.frame(year = as.integer(base_ref), idx = 100, idx_l = 100, idx_u = 100)
  out <- dplyr::bind_rows(base_row, idx) %>% dplyr::arrange(year)
  return(out)
}

idx_m1 <- extrae_indice(m1, base_ref)
idx_m2 <- extrae_indice(m2, base_ref) 
idx_m3 <- extrae_indice(m3, base_ref)

# 5) Gráfico del índice (modelo recomendado M2
ggplot(idx_m2, aes(x = year, y = idx)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = idx_l, ymax = idx_u), alpha = 0.15) +
  labs(
    title = "Índice de precios hedónico (FE township) — Modelo M2",
    subtitle = paste0("Base = ", base_ref, " (", base_ref, "=100)"),
    x = "Año de venta", y = "Índice (2000=100)"
  ) +
  theme_minimal()

# 6) Tabla comparativa
etable(
  list(M1 = m1, M2 = m2, M3 = m3),
  fitstat = c("n","r2","ar2","aic","bic","rmse"),
  signifCode = c("***"=0.01,"**"=0.05,"*"=0.10,"."=0.15," "=1),
  title = "Hedónicos con FE por township (cluster = township_code)"
)

# 7) (Opcional) Guardar índices en CSV -----------------------------------------
# write.csv(idx_m2, "C:/Users/pc/Downloads/proyecto urbaniaaa/indice_hedonico_M2.csv", row.names = FALSE)

