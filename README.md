# Índices de Precios de Vivienda - Cook County, Illinois

## Descripción del Proyecto

Este proyecto implementa y compara cuatro metodologías diferentes para calcular índices de precios de vivienda en Cook County, Illinois, durante el período 2000-2020. El análisis forma parte del Taller 1 del curso de Economía Urbana (ECON 4683) y busca evaluar el desempeño de diferentes enfoques metodológicos en la medición de precios inmobiliarios.

### Metodologías Implementadas

1. **Índice Hedónico** (dos versiones: con y sin la variable `land_sqft`)
2. **Índice de Ventas Repetidas (IPVU)** - Método Case-Shiller
3. **Estimador de Efectos Fijos** con errores agrupados a nivel de propiedad

## Estructura del Repositorio

```
├── script_punto_1.R          # Código principal para el Ejercicio 1
├── datasets/
│   └── dataTaller01_PriceIndeces.Rds  # Datos de transacciones inmobiliarias
├── writeup/
│   └── parte1.tex            # Documento técnico con análisis y resultados
├── figures/                   # Gráficas generadas (no incluido en el repo)
└── README.md                  # Este archivo
```

## Requisitos y Dependencias

### Software Requerido
- R (versión 4.0 o superior)
- RStudio (recomendado)

### Paquetes R Necesarios
```r
library(tidyr)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggrepel)
library(purrr)
library(scales)
library(ggfx)
```

## Instrucciones de Replicación

### 1. Preparación del Entorno
```r
# Instalar paquetes necesarios (si no están instalados)
install.packages(c("tidyr", "dplyr", "fixest", "ggplot2", "ggrepel", 
                   "purrr", "scales", "ggfx"))
```

### 2. Ejecución del Análisis
```r
# Ejecutar el script completo
source("script_punto_1.R")
```

### 3. Flujo del Proceso
El script realiza automáticamente:
- **Carga y limpieza de datos**: Manejo de valores faltantes y creación de variables
- **Estimación de modelos**: Cuatro metodologías diferentes
- **Generación de gráficas**: Comparación visual de los índices
- **Análisis comparativo**: Evaluación del desempeño de cada método

## Resultados Principales

### Hallazgos Clave
- Las cuatro metodologías muestran patrones similares: auge (2000-2006), crisis (2008-2011) y recuperación (2013-2020)
- El modelo hedónico sin `land_sqft` presenta mejor ajuste (`R² = 0.623`) que con `land_sqft` (`R² = 0.617`)
- El modelo de efectos fijos tiene el mayor poder explicativo (`R² = 0.868`)
- Los índices son robustos a diferentes metodologías metodológicas

### Gráficas Generadas
- Comparación individual de los cuatro índices
- Gráfica conjunta con bandas de confianza
- Análisis de varianza para el método IPVU

## Metodologías Detalladas

### 1. Índice Hedónico
```r
log(P)_{it} = ∑δ_t D_{it} + ∑β_j H_{ij} + ∑β_k N_{ik} + μ_{it}
```
- **H**: Características estructurales (25 variables)
- **N**: Características de ubicación (2 variables)
- Errores agrupados por `township_code`

### 2. IPVU (Case-Shiller)
- Tres etapas: MCO inicial, modelamiento de varianza, MCP
- Filtra propiedades con múltiples ventas y sin renovaciones recientes
- Considera la heterocedasticidad en función del tiempo entre ventas

### 3. Efectos Fijos
```r
log(P)_{it} = ∑δ_t D_{it} + ∑β_j Z_{ij} + α_i + ε_{it}
```
- Efectos fijos por propiedad (`pin`)
- Variables que varían en el tiempo
- Errores agrupados a nivel de propiedad

## Contribuciones

### Miembros del Equipo
- **Luis Alejandro Rubiano Guerrero** 
- **Andres Felipe Rosas Castillo** 
- **Carlos Andrés Castillo Cabrera** 

### Principales Contribuciones
1. Implementación de los cuatro métodos de índices de precios
2. Desarrollo de funciones para extracción y comparación de índices
3. Análisis de robustez y comparación metodológica
4. Documentación técnica y writeup académico

## Referencias Técnicas

- Case, K. E., & Shiller, R. J. (1989). The Efficiency of the Market for Single-Family Homes
- Hill, R. J. (2013). Hedonic Price Indexes for Real Estate
- Wooldridge, J. M. (2010). Econometric Analysis of Cross Section and Panel Data

---
*Proyecto desarrollado para el curso de Economía Urbana (ECON 4683) - Universidad de los Andes, 2025-2*
