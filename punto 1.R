############################################################
# Taller 1 — Índices de Precios (Cook County, IL)
# Autor: Carlos Castillo
############################################################
# 0) Paquetes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, janitor, fixest, broom, modelsummary, readr, lubridate
)
# 1) Rutas y carga
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
    sale_price     = readr::parse_number(as.character(sale_price)),
    year           = readr::parse_integer(as.character(year), na = c("", "NA")),
    year_built     = readr::parse_integer(as.character(year_built), na = c("", "NA")),
    building_sqft  = readr::parse_double(as.character(building_sqft), na = c("", "NA")),
    land_sqft      = readr::parse_double(as.character(land_sqft),    na = c("", "NA")),
    num_bedrooms   = readr::parse_double(as.character(num_bedrooms), na = c("", "NA")),
    num_rooms      = readr::parse_double(as.character(num_rooms),    na = c("", "NA")),
    num_full_baths = readr::parse_double(as.character(num_full_baths), na = c("", "NA")),
    num_half_baths = readr::parse_double(as.character(num_half_baths), na = c("", "NA")),
    num_fireplaces = readr::parse_double(as.character(num_fireplaces), na = c("", "NA")),
    pin            = as.character(pin),
    township_code  = as.character(township_code),
    
    # normalización de central_air (texto tipo "Central A/C" vs "No Central A/C")
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

# mini-log de NAs post-coerción
cand_num <- c("year","year_built","building_sqft","land_sqft",
               "num_bedrooms","num_rooms","num_full_baths","num_half_baths","num_fireplaces")
 coercion_na <- sapply(df0[cand_num], function(x) sum(is.na(x)))
 readr::write_csv(tibble::tibble(var = names(coercion_na), na_count = as.integer(coercion_na)),
                  file.path(OUT_DIR, "coercion_na_counts.csv"))

saveRDS(df0, file.path(OUT_DIR, "clean_df0.rds"))
 
# 3) Selección de controles y muestra completa

N <- nrow(df0)

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
  mutate(
    year_f      = factor(year),
    township_fe = factor(township_fe),
    prop_class  = factor(prop_class)
  )

cat("Muestra hedónica (n):", nrow(df_hedo), "de", N, "obs.\n")

# Conteo de años ordenado cronológicamente (no por frecuencia)
yr_counts <- as.data.frame(table(df_hedo$year)) %>%
  dplyr::mutate(year = as.integer(Var1), n = Freq) %>%
  dplyr::arrange(year) %>%
  dplyr::select(year, n)
print(yr_counts, row.names = FALSE)

# guardo para reproducibilidad y reporte
readr::write_csv(
  tibble::tibble(control = keep_ctrl),
  file.path(OUT_DIR, "controls_used_hedonic.csv")
)
saveRDS(df_hedo, file.path(OUT_DIR, "df_hedo.rds"))



# 4) Índice Hedónico con FE (township, clase) y clúster por township


# Fórmula: log_price ~ i(year_f, ref=base) + controles_seleccionados
rhs_ctrl <- paste(keep_ctrl, collapse = " + ")
fml_txt  <- paste0("log_price ~ i(year_f, ref = '", base_year, "') + ", rhs_ctrl)
cat("\nFórmula hedónica:\n", fml_txt, "\n")

fml_hedo <- as.formula(fml_txt)

# NOTA: paso los FE como vector de nombres
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

# Guard resultado para revisar
readr::write_csv(hedo_tbl, file.path(OUT_DIR, "index_hedonic.csv"))

# Resumen
cat("\nÍndice hedónico (primeras filas):\n")
print(head(hedo_tbl, 5))
cat("\nÍndice hedónico (últimas filas):\n")
print(tail(hedo_tbl, 5))

cl_sz <- df_hedo %>% count(township_fe, name = "n")
message("Clusters con n=1: ", sum(cl_sz$n == 1))

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


# 7) Unir, graficar y validar

library(ggplot2); library(dplyr); library(tidyr); library(readr)

all_idx <- bind_rows(hedo_tbl, rs_tbl, fe_tbl) %>%
  arrange(method, year)

write_csv(all_idx, file.path(OUT_DIR, "indices_comparados.csv"))

p_idx <- ggplot(all_idx, aes(x = year, y = index, color = method, linetype = method)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  labs(
    title = "Índices de precios de vivienda – Cook County",
    subtitle = "Hedónico (FE township+class), Repeat Sales (3 etapas), FE por propiedad (cluster=pin)",
    x = "Año",
    y = paste0("Índice (Base ", base_year, " = 100)")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(filename = file.path(OUT_DIR, "indices_comparados.png"),
       plot = p_idx, width = 9, height = 5, dpi = 300)

# Ahora,

# (1) Años comunes a los tres métodos
yrs_common <- all_idx %>%
  count(method, year) %>%
  pivot_wider(names_from = method, values_from = n) %>%
  filter(if_all(-year, ~ !is.na(.x))) %>%
  pull(year)

indices_common <- all_idx %>% filter(year %in% yrs_common)
write_csv(indices_common, file.path(OUT_DIR, "indices_common_years.csv"))

# (2) Correlaciones entre métodos en años comunes
wide_idx <- indices_common %>%
  select(year, method, index) %>%
  pivot_wider(names_from = method, values_from = index)

cors <- cor(wide_idx %>% select(-year), use = "pairwise.complete.obs")
write_csv(as.data.frame(as.table(cors)), file.path(OUT_DIR, "indices_correlations.csv"))
cat("\nCorrelaciones (años comunes):\n"); print(round(cors, 3))

# (3) Ancho promedio de IC por método
ci_width <- all_idx %>%
  mutate(width = upr - lwr) %>%
  group_by(method) %>%
  summarise(mean_ci_width = mean(width, na.rm = TRUE), .groups = "drop")
write_csv(ci_width, file.path(OUT_DIR, "indices_ci_width.csv"))
cat("\nAncho promedio de IC:\n"); print(ci_width)

# (4) Crecimientos por períodos
subperiods <- tibble::tribble(
  ~start,   ~end,
  base_year, 2007,
  2007,      2012,
  2012,      2020
)

growth_tbl <- crossing(subperiods, method = unique(all_idx$method)) %>%
  rowwise() %>%
  mutate(
    idx_start = all_idx$index[all_idx$method == method & all_idx$year == start][1],
    idx_end   = all_idx$index[all_idx$method == method & all_idx$year == end][1],
    growth_pct = ifelse(is.finite(idx_start) & is.finite(idx_end),
                        100 * (idx_end / idx_start - 1), NA_real_)
  ) %>%
  ungroup()

write_csv(growth_tbl, file.path(OUT_DIR, "indices_growth.csv"))

cat("\nArchivos guardados en:\n", OUT_DIR,
    "\n- indices_comparados.csv\n- indices_comparados.png",
    "\n- indices_common_years.csv\n- indices_correlations.csv\n- indices_ci_width.csv\n- indices_growth.csv\n", sep = "")

n_hedo_used <- stats::nobs(model_hedo)
n_rs_pairs  <- nrow(rs_pairs)
n_fe_total  <- nrow(df_fe)
n_fe_used   <- stats::nobs(mod_fe_prop)
n_fe_drop   <- n_fe_total - n_fe_used

sample_tbl <- tibble::tibble(
  metodo = c(
    "Hedónico (obs. usadas)",
    "Repeat-sales (pares)",
    "FE por propiedad (obs. usadas)",
    "FE por propiedad (singletons removidos)"
  ),
  n = c(n_hedo_used, n_rs_pairs, n_fe_used, n_fe_drop)
)
readr::write_csv(sample_tbl, file.path(OUT_DIR, "indices_samples.csv"))

