library(tidyr)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggrepel)
library(purrr)
library(scales) 
library(ggfx)

# Cargar datos y limpiar---------------------------

df <- readRDS(file.path("datasets", "dataTaller01_PriceIndeces.Rds")) # Leer el archivo
df <- dplyr::ungroup(df) # Desagrupar


print(class(df))

na_counts <- colSums(is.na(df)) # Revisar NAs
print(na_counts)

df <- df %>% drop_na(c("building_sqft", "num_bedrooms", "num_rooms", "num_full_baths", "num_half_baths", 
                       "num_fireplaces")) # Nos deshacemos de las "pocas" observaciones con NAs en 
                                          # en estas variables (1060)

na_counts <- colSums(is.na(df)) # Revisamos que hayamos hecho lo deseado
print(na_counts)

df$log_sale_price <- log(df$sale_price) # Creamos variable del ln del precio

# Características estructurales (H)
H_vars <- c("class", "year_built", "building_sqft", "land_sqft", "num_bedrooms",
            "num_rooms", "num_full_baths", "num_half_baths", "num_fireplaces",
            "type_of_residence", "construction_quality", "attic_finish", "garage_attached",
            "garage_area_included", "garage_size", "garage_ext_wall_material", "attic_type",
            "basement_type", "ext_wall_material", "central_heating", "basement_finish", "roof_material",
            "renovation", "recent_renovation", "porch", "central_air")

# Estructurales sin land_sqft (151929 N.As, casi el 40% de las observaciones)
H_vars2 <- c("class", "year_built", "building_sqft", "num_bedrooms",
             "num_rooms", "num_full_baths", "num_half_baths", "num_fireplaces",
             "type_of_residence", "construction_quality", "attic_finish", "garage_attached",
             "garage_area_included", "garage_size", "garage_ext_wall_material", "attic_type",
             "basement_type", "ext_wall_material", "central_heating", "basement_finish", "roof_material",
             "renovation", "recent_renovation", "porch", "central_air")
            

# Características de ubicación (N)
N_vars <- c("township_code", "site_desirability")


# Variables categoricas

cat_vars <- c(
  "class", "type_of_residence", "construction_quality", "attic_finish",
  "garage_attached", "garage_area_included", "garage_size", "garage_ext_wall_material",
  "attic_type", "basement_type", "ext_wall_material", "central_heating",
  "basement_finish", "roof_material", "renovation", "recent_renovation",
  "porch", "central_air", "township_code", "site_desirability"
)


# Volver factores las categóricas 
cat_vars_in_df <- intersect(cat_vars, names(df))
df <- df %>%
  mutate(across(all_of(cat_vars_in_df), ~ as.factor(.)))

df2 <- df %>% select(-land_sqft) # Quitamos la variable 
df1 <- df %>% drop_na(c("land_sqft")) # Quitamos las observaciones con NA

all_rhs_vars <- c(H_vars, N_vars)

all_rhs_vars2 <- c(H_vars2, N_vars) # Sin land_sqft

cont_vars <- setdiff(all_rhs_vars, cat_vars_in_df)
cont_vars2 <- setdiff(all_rhs_vars2, cat_vars_in_df)

# Modelos hedonicos---------------------------

rhs_parts <- c( # Lado derecho de la fórmula
  cont_vars,
  paste0("i(", cat_vars_in_df, ")"),
  "factor(year)"          
)

rhs_parts2 <- c( # Lado derecho de la fórmula
  cont_vars2,
  paste0("i(", cat_vars_in_df, ")"),
  "factor(year)"          
)

rhs <- paste(rhs_parts, collapse = " + ")
rhs2 <- paste(rhs_parts2, collapse = " + ")

fml <- as.formula(paste("log_sale_price ~", rhs)) # Creamos la fórmula
fml2 <- as.formula(paste("log_sale_price ~", rhs2)) # Creamos la fórmula


m_hedonic <- feols(   # Standard errors?
  fml,                  
  data = df1,
  cluster = ~township_code
)

m_hedonic2 <- feols(
  fml2,
  data = df2,
  cluster = ~township_code
)


# Comparación lado a lado ---------------------------

# Nombres bonitos de modelos
names_models <- c("Con land_sqft", "Sin land_sqft")


# Tabla comparativa 
etable(
  list(m_hedonic, m_hedonic2),
  headers = names_models,
  fitstat = c("n","r2","ar2","aic","bic","wald","rmse"),
  signifCode = c("***"=0.01,"**"=0.05,"*"=0.10,"."=0.15," "=1),
  title = "Hedónicos: comparación con y sin land_sqft"
)


# Ventas repetidas IPVU ---------------------------


df_IPVU <- df %>% # viviendas vendidas al menos dos veces
  group_by(pin) %>% #durante el periodo de estudio y que no hayan presentado modificaciones significativas en
  filter(n() > 1, recent_renovation == FALSE) %>% #su estructura física
  ungroup()


# Ordenar por pin y año
df_IPVU <- df_IPVU[ order(df_IPVU[["pin"]], df_IPVU[["year"]]) , ]

tab <- table(df_IPVU[["pin"]]) # Por pin 

t2 <- ave(df_IPVU[["year"]],        df_IPVU[["pin"]], FUN = function(x) c(x[-1], NA)) # año de la siguiente venta para cada fila de ese pin
p2 <- ave(df_IPVU[["sale_price"]],  df_IPVU[["pin"]], FUN = function(x) c(x[-1], NA)) # Lo mismo con el precio

pairs_ok <- !is.na(t2) & !is.na(p2) & (df_IPVU[["year"]] != t2)  # Condición de años distintos
t1  <- df_IPVU[["year"]][pairs_ok]
p1  <- df_IPVU[["sale_price"]][pairs_ok]
t2  <- t2[pairs_ok]
p2  <- p2[pairs_ok]



y   <- log(p2) - log(p1)                     # Etapa 1: variable dependiente
gap <- as.integer(t2 - t1)                   # tiempo de diferencia (años)

years_all <- sort(unique(c(t1, t2)))
base_year <- min(years_all)                  # fija el año base automáticamente

X_plus  <- model.matrix(~ factor(t2, levels = years_all) - 1) #marca los años de venta (con +1)
X_minus <- model.matrix(~ factor(t1, levels = years_all) - 1) #marca los años de compra (con −1)
X <- X_plus - X_minus # Matriz de diseño

# eliminar columna del año base 
base_idx <- match(base_year, years_all)
X_restricted <- X[, -base_idx, drop = FALSE]

fit_ols <- lm.fit(x = X_restricted, y = y) # MCO 1ra etapa
e1 <- fit_ols$residuals # Residuales


gap <- as.integer(t2 - t1)

var_lm <- lm(I(e1^2) ~ gap + I(gap^2)) # Modelo cuadrático etapa 2
var_hat <- fitted(var_lm)  

# Pesos correctos para generalized least squares (1/Var)
w <- 1 / var_hat

fit_wls <- lm.wfit(x = X_restricted, y = y, w = w)

# Reconstruir beta incluyendo el año base en 0
beta_restricted <- fit_wls$coefficients
beta <- numeric(length(years_all))
beta[-base_idx] <- beta_restricted
beta[ base_idx] <- 0
names(beta) <- years_all

# Errores estándar para beta 
XtW <- t(X_restricted * w)
vc  <- solve(XtW %*% X_restricted)
sigma2 <- sum((fit_wls$residuals * sqrt(w))^2) / (nrow(X_restricted) - ncol(X_restricted))
se_restricted <- sqrt(diag(vc) * sigma2)
se_beta[-base_idx] <- se_restricted
se_beta[ base_idx] <- 0

# base = 100 en base_year
index_geom <- 100 * exp(beta - beta[as.character(base_year)])

ipvu_df <- data.frame(
  year       = years_all,
  beta       = as.numeric(beta),
  se_beta    = as.numeric(se_beta),
  index_geom = as.numeric(index_geom),
  base_year  = base_year
)


# Efectos fijos con efectos agrupados ---------------------------

df_3 <- df %>% # viviendas vendidas al menos dos veces
  group_by(pin) %>% #durante el periodo de estudio 
  filter(n() > 1) %>%
  ungroup()


fe_vars <- c("building_sqft", "num_bedrooms", "num_rooms", "num_full_baths",
             "num_half_baths", "renovation", "recent_renovation", # Variables que creemos 
             "central_air", "central_heating", "garage_attached", # pudieron cambiar en una casa 
             "garage_size", "attic_finish", "basement_finish") # a través de los años


fml_fe <- as.formula(
  paste("log_sale_price ~", paste(fe_vars, collapse = " + "), "+ i(year, ref = min(year)) | pin") # Efectos fijos por fe_vars y pin
)

m_fe <- feols(
  fml_fe,
  data    = df_3,
  cluster = ~ pin 
)


etable(m_fe, se = "cluster", cluster = "pin",
       title = "FE de propiedad con dummies de año (cluster por propiedad)")


###Gráfico ---------------------------
  
base_year <- min(df$year, na.rm = TRUE)  # Año base común
zcrit <- qnorm(.975)

# Helper ROBUSTO: extrae serie de índices (y CI) desde un modelo fixest con dummies de año
index_from_fixest <- function(mod, model_label, base_year) {
  coefs <- coef(mod)
  vcv   <- vcov(mod)
  nm    <- names(coefs)
  
  # Detecta variantes en nombres de año: factor(year)YYYY, i(year,...)::YYYY, year::YYYY, etc.
  is_year_term <- grepl("factor\\(year\\)|^i\\(year|\\byear(::|:)", nm)
  if (!any(is_year_term)) is_year_term <- grepl("year.*\\d+$", nm, perl = TRUE)
  
  nm_yr <- nm[is_year_term]
  yrs   <- suppressWarnings(as.integer(sub(".*?(\\d+)$", "\\1", nm_yr, perl = TRUE)))
  keep  <- !is.na(yrs)
  
  beta_hat <- coefs[is_year_term][keep]
  se_hat   <- sqrt(diag(vcv))[is_year_term][keep]
  yrs      <- yrs[keep]
  
  # Inserta año base (coef=0, se=0)
  years_all <- sort(unique(c(yrs, base_year)))
  beta <- setNames(numeric(length(years_all)), years_all)
  se   <- beta
  beta[as.character(yrs)]       <- beta_hat
  se[as.character(yrs)]         <- se_hat
  beta[as.character(base_year)] <- 0
  se[as.character(base_year)]   <- 0
  
  tibble(
    model = model_label,
    year  = as.integer(names(beta)),
    beta  = as.numeric(beta),
    se    = as.numeric(se)
  ) |>
    mutate(
      index = 100 * exp(beta),                 # base_year → 100
      lwr   = 100 * exp(beta - zcrit * se),
      upr   = 100 * exp(beta + zcrit * se)
    )
}

# ── Series por modelo ─────────────────────────────────────────────────────────
df_h1  <- index_from_fixest(m_hedonic,  "Hedónico (con land_sqft)", base_year)
df_h2  <- index_from_fixest(m_hedonic2, "Hedónico (sin land_sqft)", base_year)

df_ipvu <- ipvu_df |>
  transmute(
    model = "Reventas (IPVU)",
    year  = as.integer(year),
    beta  = beta,
    se    = se_beta,
    index = 100 * exp(beta - beta[year == base_year]),
    lwr   = 100 * exp(beta - zcrit * se - beta[year == base_year]),
    upr   = 100 * exp(beta + zcrit * se - beta[year == base_year])
  )

df_fe  <- index_from_fixest(m_fe, "Efectos fijos (propiedad)", base_year)

plot_df <- bind_rows(df_h1, df_h2, df_ipvu, df_fe) |>
  distinct(model, year, .keep_all = TRUE) |>
  arrange(model, year)

# Paleta alto contraste (líneas) y versión aclarada para bandas

cols_line <- c(
  "Reventas (IPVU)"            = "#2CA02C",
  "Hedónico (con land_sqft)"   = "#D95F02",
  "Hedónico (sin land_sqft)"   = "#0033A0",
  "Efectos fijos (propiedad)"  = "#D62728"
)
cols_fill <- alpha(cols_line, 0.18)  # base para la banda (y halo)


ggplot(plot_df, aes(year, index)) +
  # 1) Banda "halo" difuminada (debajo)
  with_blur(
    geom_ribbon(
      aes(ymin = lwr, ymax = upr, fill = model),
      alpha = 0.18, color = NA, show.legend = FALSE
    ),
    sigma = 4, expand = 2
  ) +
  # 2) Banda nítida y más clara (encima)
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, fill = model),
    alpha = 0.12, color = NA, show.legend = FALSE
  ) +
  # 3) Línea y puntos
  geom_line(aes(color = model), linewidth = 1.2) +
  geom_point(aes(color = model), shape = 21, fill = "white",
             stroke = 0.6, size = 2) +
  # Escalas (mismos colores que definiste)
  scale_color_manual(values = cols_line, guide = "none") +
  scale_fill_manual(values = cols_fill, guide = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x = "Año", y = "Índice de precios (base = 100)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(8, 12, 8, 8)
  ) +
  facet_wrap(~ model, ncol = 2)




ggplot(plot_df, aes(x = year, y = index, color = model, fill = model)) +
  # HALO difuminado (debajo): hereda fill=model y usa la escala manual
  with_blur(
    geom_ribbon(aes(ymin = lwr, ymax = upr), color = NA, show.legend = FALSE),
    sigma = 4, expand = 2
  ) +
  # Banda nítida (encima)
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, color = NA, show.legend = FALSE) +
  # Líneas y puntos
  geom_line(linewidth = 1.25) +
  geom_point(shape = 21, fill = "white", stroke = 0.65, size = 2.1, show.legend = FALSE) +
  # Escalas
  scale_color_manual(values = cols_line, name = NULL) +
  scale_fill_manual(values = cols_fill, guide = "none") +
  scale_x_continuous(breaks = pretty_breaks(6),
                     expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = "Año", y = "Índice de precios (base = 100)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25),
    legend.position    = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background  = element_rect(fill = alpha("white", 0.9), color = NA),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(8, 16, 8, 8)
  )