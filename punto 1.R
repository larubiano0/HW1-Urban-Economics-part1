############################################################
# Taller 1 — Índices de Precios (Cook County, IL)
# Autor: Carlos Castillo
############################################################
# 0) Paquetes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, janitor, fixest, broom, modelsummary, readr, lubridate
)
# 1) Rutas + carga cruda
DATA_DIR <- "C:/Users/pc/Downloads/proyecto urbaniaaa"
OUT_DIR  <- file.path(DATA_DIR, "export")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Intentamos ambos nombres (con y sin guion bajo)
file_candidates <- c(
  file.path(DATA_DIR, "dataTaller01_PriceIndeces.Rds"),
  file.path(DATA_DIR, "dataTaller01 PriceIndeces.Rds")
)
DATA_FILE <- file_candidates[file.exists(file_candidates)][1]
if (is.na(DATA_FILE)) stop("No se encontró el archivo RDS en la ruta indicada: ", DATA_DIR)

raw <- readRDS(DATA_FILE)

# Para revisar la estructura y nombres
cat("N obs:", nrow(raw), "  N vars:", ncol(raw), "\n")
print(names(raw))

# guardo los nombres
writeLines(names(raw), file.path(OUT_DIR, "column_names.txt"))


# 2) LIMPIEZA Y DERIVADAS

library(dplyr); library(stringr)

df0 <- raw %>%
  dplyr::ungroup() %>%              # por si viene 'grouped_df'
  janitor::clean_names() %>%
  mutate(
    # ---- coerciones seguras ----
    sale_price     = readr::parse_number(as.character(sale_price)),
    year           = suppressWarnings(as.integer(year)),
    year_built     = suppressWarnings(as.integer(year_built)),
    building_sqft  = suppressWarnings(as.numeric(building_sqft)),
    land_sqft      = suppressWarnings(as.numeric(land_sqft)),
    num_bedrooms   = suppressWarnings(as.numeric(num_bedrooms)),
    num_rooms      = suppressWarnings(as.numeric(num_rooms)),
    num_full_baths = suppressWarnings(as.numeric(num_full_baths)),
    num_half_baths = suppressWarnings(as.numeric(num_half_baths)),
    num_fireplaces = suppressWarnings(as.numeric(num_fireplaces)),
    pin            = as.character(pin),
    township_code  = as.character(township_code),
    
    #normalización de central_air (texto tipo "Central A/C" vs "No Central A/C")
    central_air_chr = tolower(str_squish(as.character(central_air))),
    central_air_bin = case_when(
      # negativas explícitas primero
      str_detect(central_air_chr, "^no\\b") ~ 0L,
      str_detect(central_air_chr, "no central") ~ 0L,
      central_air_chr %in% c("n","no","0","false") ~ 0L,
      # positivas
      str_detect(central_air_chr, "central") ~ 1L,
      central_air_chr %in% c("y","yes","1","true","si","sí","s\u00ed") ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    # filtros básicos
    .keep = "all"
  ) %>%
  filter(!is.na(sale_price), sale_price > 1000, !is.na(year)) %>%
  mutate(
    # derivadas
    log_price  = log(sale_price),
    age        = ifelse(!is.na(year_built), pmax(year - year_built, 0), NA_real_),
    log_bldg   = ifelse(!is.na(building_sqft) & building_sqft > 0, log(building_sqft), NA_real_),
    log_land   = ifelse(!is.na(land_sqft)    & land_sqft > 0,    log(land_sqft),    NA_real_),
    full_baths = dplyr::coalesce(num_full_baths, 0),
    half_baths = dplyr::coalesce(num_half_baths, 0),
    firepl     = dplyr::coalesce(num_fireplaces, 0),
    
    # FE / clusters robustos
    township_fe = ifelse(is.na(township_code) | township_code == "", "UNK", township_code),
    prop_class  = dplyr::coalesce(as.character(class), as.character(type_of_residence), "UNK")
  )

# CHEQUEOS CLAVE
cat("\nFilas tras filtros básicos:", nrow(df0), "\n")

cand_ctrl <- c("log_bldg","log_land","num_bedrooms","num_rooms",
               "full_baths","half_baths","firepl","age","central_air_bin")

# Conteo de NO-NA por control
non_miss <- sapply(df0[cand_ctrl], function(x) sum(!is.na(x)))
print(non_miss)

# ¿Alguna variable con 100% NA?
all_na <- names(non_miss)[non_miss == 0]
if (length(all_na) > 0) {
  message("Variables con 100% NA (se excluirán de los modelos): ",
          paste(all_na, collapse = ", "))
} else {
  message("No hay controles con 100% NA.")
}

# Tabla rápida de central_air_bin
cat("\nTabla central_air_bin (0=no, 1=sí, NA=indeterminado):\n")
print(table(df0$central_air_bin, useNA = "ifany"))

# Rango de años y año base
years <- sort(unique(df0$year))
cat("\nRango de años en datos:", years[1], "—", years[length(years)], "\n")
base_year <- min(years)
cat("Año base:", base_year, "\n")

# 3) Selección de controles y muestra completa

N <- nrow(df0)

# Candidatos (de tu chequeo anterior todos tienen datos, pero ponemos umbral por seguridad)
cand_ctrl <- c("log_bldg","log_land","num_bedrooms","num_rooms",
               "full_baths","half_baths","firepl","age","central_air_bin")

min_cov <- 0.60  # cobertura mínima requerida (60%)
keep_ctrl <- names(which(colMeans(!is.na(df0[cand_ctrl])) >= min_cov))

cat("\nControles candidatos: ", paste(cand_ctrl, collapse=", "), "\n")
cat("Cobertura mínima requerida:", 100*min_cov, "%\n")
cat("Controles usados (cumplen cobertura): ", paste(keep_ctrl, collapse=", "), "\n")

# Muestra sin NA en variables necesarias
df_hedo <- df0 %>%
  dplyr::select(log_price, year, all_of(keep_ctrl), township_fe, prop_class) %>%
  tidyr::drop_na(log_price, year, all_of(keep_ctrl), township_fe, prop_class) %>%
  mutate(year_f = factor(year))  # factor para usar i(year_f, ref=...)

cat("Muestra hedónica (n):", nrow(df_hedo), "de", N, "obs.\n")
cat("Años presentes en la muestra hedónica:\n")
print(sort(table(df_hedo$year)))


# 4) Índice Hedónico con FE (township, clase) y clúster por township


# Fórmula: log_price ~ i(year_f, ref=base) + controles_seleccionados
rhs_ctrl <- paste(keep_ctrl, collapse = " + ")
fml_txt  <- paste0("log_price ~ i(year_f, ref = '", base_year, "') + ", rhs_ctrl)
cat("\nFórmula hedónica:\n", fml_txt, "\n")

fml_hedo <- as.formula(fml_txt)

# NOTA: paso los FE como vector de nombres (máxima compatibilidad de versiones)
model_hedo <- fixest::feols(
  fml = fml_hedo,
  data = df_hedo,
  fixef = c("township_fe","prop_class"),
  cluster = "township_fe"
)

# Extraer los coeficientes de tiempo y construir el índice (base = 100)
hedo_tbl <- broom::tidy(model_hedo) %>%
  dplyr::filter(grepl("^year_f::", term)) %>%
  mutate(
    year  = as.integer(sub("year_f::", "", term)),
    index = 100 * exp(estimate),
    lwr   = 100 * exp(estimate - 1.96 * std.error),
    upr   = 100 * exp(estimate + 1.96 * std.error),
    method = "Hedónico (FE township+class)"
  ) %>%
  dplyr::select(year, index, lwr, upr, method) %>%
  dplyr::bind_rows(tibble(year = base_year, index = 100, lwr = NA_real_, upr = NA_real_,
                          method = "Hedónico (FE township+class)")) %>%
  dplyr::arrange(year)

# Guarda resultado para revisar/usar luego
readr::write_csv(hedo_tbl, file.path(OUT_DIR, "index_hedonic.csv"))

# Resumen corto en consola
cat("\nÍndice hedónico (primeras filas):\n")
print(head(hedo_tbl, 5))
cat("\nÍndice hedónico (últimas filas):\n")
print(tail(hedo_tbl, 5))

# 5) Repeat Sales (3 etapas)

library(dplyr)
library(tidyr)

# a) preparar pares consecutivos por PIN (si hay múltiples ventas en el mismo año, nos quedamos con la última)
df_tx <- df0 %>%
  arrange(pin, year) %>%
  group_by(pin, year) %>%
  slice_tail(n = 1) %>%              # una obs por pin-año
  ungroup()

rs_pairs <- df_tx %>%
  arrange(pin, year) %>%
  group_by(pin) %>%
  mutate(p0 = lag(sale_price),
         t0 = lag(year)) %>%
  ungroup() %>%
  filter(!is.na(p0), !is.na(t0)) %>%
  transmute(
    pin,
    t1 = year, t0,
    p1 = sale_price, p0,
    dv = log(p1) - log(p0),
    dt = t1 - t0
  ) %>%
  filter(is.finite(dv), dt > 0)

cat("\nRepeat-sales: Nº de pares:", nrow(rs_pairs), " | Nº pins con al menos 2 ventas:", length(unique(rs_pairs$pin)), "\n")

# b) Matriz de tiempo (sin intercepto; base = primer año de rs_times)
rs_times <- sort(unique(c(rs_pairs$t0, rs_pairs$t1)))
D1 <- model.matrix(~ 0 + factor(rs_pairs$t1, levels = rs_times))
D0 <- model.matrix(~ 0 + factor(rs_pairs$t0, levels = rs_times))
D  <- D1 - D0
colnames(D) <- paste0("T", rs_times)
D  <- D[, -1, drop = FALSE]   # quitar base

# c) Etapa 1 (OLS)
fit1 <- lm(rs_pairs$dv ~ D + 0)
e2   <- resid(fit1)^2

# d) Etapa 2 (modelo de la varianza condicional en función del tiempo transcurrido)
vfit <- lm(e2 ~ rs_pairs$dt + I(rs_pairs$dt^2))
vhat <- pmax(fitted(vfit), 1e-10)
w    <- 1 / vhat

# e) Etapa 3 (WLS)
fit3 <- lm(rs_pairs$dv ~ D + 0, weights = w)

rs_beta <- coef(fit3)                # efectos vs. año base
rs_vcov <- vcov(fit3)
rs_var  <- diag(rs_vcov)

rs_tbl <- tibble(
  year  = rs_times,
  beta  = c(0, as.numeric(rs_beta)),
  vbeta = c(0, as.numeric(rs_var))
) %>%
  transmute(
    year,
    index  = 100 * exp(beta),
    lwr    = 100 * exp(beta - 1.96 * sqrt(pmax(vbeta, 0))),
    upr    = 100 * exp(beta + 1.96 * sqrt(pmax(vbeta, 0))),
    method = "Repeat Sales (3 etapas)"
  )

readr::write_csv(rs_tbl, file.path(OUT_DIR, "index_repeat_sales.csv"))

cat("\nRepeat-sales (primeras filas):\n"); print(head(rs_tbl, 5))
cat("\nRepeat-sales (últimas filas):\n");  print(tail(rs_tbl, 5))

# 6) Índice con efectos fijos por propiedad
#    log(price) ~ i(year, ref=base) | pin
#    cluster = pin

library(fixest)
df_fe <- df0 %>%
  select(log_price, year, pin) %>%
  drop_na(log_price, year, pin) %>%
  mutate(year_f = factor(year))

fml_fe <- as.formula(paste0("log_price ~ i(year_f, ref = '", base_year, "')"))
mod_fe_prop <- feols(
  fml = fml_fe,
  data = df_fe,
  fixef = "pin",
  cluster = "pin"
)

fe_tbl <- broom::tidy(mod_fe_prop) %>%
  dplyr::filter(grepl("^year_f::", term)) %>%
  mutate(
    year  = as.integer(sub("year_f::", "", term)),
    index = 100 * exp(estimate),
    lwr   = 100 * exp(estimate - 1.96 * std.error),
    upr   = 100 * exp(estimate + 1.96 * std.error),
    method = "FE propiedad (cluster=pin)"
  ) %>%
  select(year, index, lwr, upr, method) %>%
  bind_rows(tibble(year = base_year, index = 100, lwr = NA_real_, upr = NA_real_,
                   method = "FE propiedad (cluster=pin)")) %>%
  arrange(year)

readr::write_csv(fe_tbl, file.path(OUT_DIR, "index_fe_prop.csv"))

cat("\nFE propiedad (primeras filas):\n"); print(head(fe_tbl, 5))
cat("\nFE propiedad (últimas filas):\n");  print(tail(fe_tbl, 5))


# 7) Unir y graficar

library(ggplot2)

all_idx <- bind_rows(
  hedo_tbl,
  rs_tbl,
  fe_tbl
) %>%
  arrange(method, year)

readr::write_csv(all_idx, file.path(OUT_DIR, "indices_comparados.csv"))

p_idx <- ggplot(all_idx, aes(x = year, y = index, color = method, linetype = method)) +
  geom_line() +
  geom_point(size = 1) +
  labs(
    title = "Índices de precios de vivienda – Cook County",
    subtitle = "Hedónico (FE township+class), Repeat Sales (3 etapas), FE por propiedad (cluster=pin)",
    x = "Año", y = "Índice (base=100 en año base)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(filename = file.path(OUT_DIR, "indices_comparados.png"),
       plot = p_idx, width = 9, height = 5, dpi = 300)

cat("\nArchivos guardados en:\n", OUT_DIR, "\n",
    "- index_hedonic.csv\n- index_repeat_sales.csv\n- index_fe_prop.csv\n- indices_comparados.csv\n- indices_comparados.png\n", sep = "")

#En resumen:
# Comparabilidad de años y correlaciones entre índices
library(dplyr); library(tidyr); library(readr)

common_years <- Reduce(intersect, list(hedo_tbl$year, rs_tbl$year, fe_tbl$year))
cmp <- bind_rows(hedo_tbl, rs_tbl, fe_tbl) %>%
  filter(year %in% common_years) %>%
  select(year, method, index) %>%
  pivot_wider(names_from = method, values_from = index) %>%
  arrange(year)

# Correlaciones entre las tres series
cors <- cor(cmp %>% select(-year), use = "pairwise.complete.obs")
round(cors, 3)

# Ancho promedio de los IC (precsión
ciw <- bind_rows(hedo_tbl, rs_tbl, fe_tbl) %>%
  group_by(method) %>%
  summarise(mean_ci_width = mean(upr - lwr, na.rm = TRUE)) %>%
  arrange(mean_ci_width)
ciw

# Tamaños muestrales efectivamente usados por cada método

n_hedo_used <- stats::nobs(model_hedo)     # obs. usadas en hedónico
n_rs_pairs  <- nrow(rs_pairs)              # pares en repeat sales
n_fe_total  <- nrow(df_fe)                 # transacciones candidatas a FE
n_fe_used   <- stats::nobs(mod_fe_prop)    # obs. usadas en FE por propiedad
n_fe_drop   <- n_fe_total - n_fe_used      # singletons removidos

sample_tbl <- tibble::tibble(
  metodo = c(
    "Hedónico (obs. usadas)",
    "Repeat-sales (pares)",
    "FE por propiedad (obs. usadas)",
    "FE por propiedad (singletons removidos)"
  ),
  n = c(n_hedo_used, n_rs_pairs, n_fe_used, n_fe_drop)
)

print(sample_tbl)
readr::write_csv(sample_tbl, file.path(OUT_DIR, "indices_samples.csv"))

# Crecimientos acumulados y por subperíodos (útil para discutir desempeño)
grow_tbl <- bind_rows(hedo_tbl, rs_tbl, fe_tbl) %>%
  group_by(method) %>%
  summarise(
    base  = first(index[order(year)]),
    last  = last(index[order(year)]),
    growth_00_07 = (index[year==2007] / index[year==2000] - 1) * 100,
    drop_07_11   = (index[year==2011] / index[year==2007] - 1) * 100,
    recov_11_20  = (index[year==2020] / index[year==2011] - 1) * 100
  )
grow_tbl

# Guardar resúmenes
readr::write_csv(cmp,        file.path(OUT_DIR, "indices_common_years.csv"))
readr::write_csv(ciw,        file.path(OUT_DIR, "indices_ci_width.csv"))
readr::write_csv(sample_tbl, file.path(OUT_DIR, "indices_samples.csv"))
readr::write_csv(grow_tbl,   file.path(OUT_DIR, "indices_growth.csv"))
