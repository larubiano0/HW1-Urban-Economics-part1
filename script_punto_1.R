library(tidyr)
library(dplyr)
library(fixest)

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

# Estructurales sin land_sqft (151929 N.As, casi la mitad de las observaciones)
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
df <- df %>% drop_na(c("land_sqft")) # Quitamos las observaciones con NA

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


m_hedonic <- feols(
  fml,
  data = df
)

m_hedonic2 <- feols(
  fml2,
  data = df2
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


# Ventas repetidas ---------------------------











