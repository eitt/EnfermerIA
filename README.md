# EnfermerIA: pipeline psicométrico en R y playground en Streamlit

Este proyecto analiza `ds_all_2026_07_22.xlsx` mediante un pipeline reproducible en R y una aplicación interactiva en Python/Streamlit. El pipeline genera tablas auditables, figuras en formato PNG y diagnósticos psicométricos para las escalas de actitudes hacia la IA y alfabetización en IA.

## Estructura de las escalas

- **GAAIS:** dos dimensiones correlacionadas: actitud positiva (12 ítems) y actitud negativa/preocupación (8 ítems). Las dimensiones se reportan por separado; no se recomienda interpretar una media global como factor confirmado.
- **Alfabetización en IA:** cuatro dimensiones teóricas: Awareness, Usage, Evaluation y Ethics, con 12 ítems en total. Los ítems almacenados con sufijo `_r` se transforman en columnas de análisis mediante inversión Likert cuando corresponde.
- **Cuadrantes:** la figura de actitudes utiliza cortes teóricos fijos en 3 para ambos ejes. No calcula los cortes a partir de la media o mediana de la muestra.

## Atención y muestras de análisis

La clave validada en `R/00_config.R` es:

```r
ATTENTION_CORRECT_RESPONSES <- c(
  ac_1 = 1,
  ac_2 = 1,
  ac_3 = 1,
  ac_4 = 1
)
```

La variable original `ac_total` no se sobrescribe. El pipeline calcula además `ac_total_recalculated` directamente desde `ac_1`--`ac_4`.

Las muestras se definen así:

| Muestra | Regla | Tamaño esperado | Uso |
|---|---|---:|---|
| `all_core` | Criterios de consentimiento/compromiso del núcleo | 1,211 | Análisis principal |
| `fail_at_most_2` | Al menos 2 attention checks correctos | 1,073 | Sensibilidad |
| `fail_at_most_1` | Al menos 3 correctos | 980 | Sensibilidad |
| `pass_all_4` | Los 4 correctos | 823 | Sensibilidad estricta |

La distribución esperada de `ac_total_recalculated` es:

```text
0 aciertos: 98
1 acierto: 40
2 aciertos: 93
3 aciertos: 157
4 aciertos: 823
```

Los attention checks no se utilizan para reemplazar automáticamente la muestra primaria. Las muestras de sensibilidad sirven para comprobar la estabilidad de confiabilidad, CFA, factorabilidad y decisiones psicométricas.

## Ejecutar el pipeline R

Desde la raíz del proyecto:

```powershell
Rscript 00_run_pipeline.R
```

Si `Rscript` no está en el `PATH` en Windows, usar la ruta de la instalación:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" 00_run_pipeline.R
```

También puede ejecutarse desde RStudio:

```r
source("00_run_pipeline.R")
```

El pipeline guarda los mensajes de consola y advertencias en `outputs/logs/`.

## Ejecutar el playground Streamlit

Instalar las dependencias:

```powershell
python -m pip install -r requirements.txt
```

Iniciar la aplicación:

```powershell
streamlit run EnfermerIA_playground_main.py
```

La app permite cargar el Excel o CSV y contiene estas páginas:

1. guía de datos y constructos;
2. playground de filtros;
3. diagnósticos descriptivos;
4. explorador de correlaciones;
5. playground CFA;
6. cuadrantes de actitud positiva y preocupación negativa;
7. exportación y auditoría;
8. guía del pipeline y sensibilidad.

## Cómo funcionan los filtros del playground

- **Consentimiento = 1:** reproduce el criterio de inclusión principal del pipeline R.
- **Compromiso = 1:** está desactivado por defecto porque `REQUIRE_COMMITMENT = FALSE` en la configuración R. Activarlo cambia la población objetivo y debe justificarse antes del análisis.
- **Attention checks:** utiliza la clave recalculada y las cuatro muestras compatibles con R. La columna original `ac_total` se conserva para auditoría.
- **Valores Likert fuera de 1--5:** se convierten en valores perdidos; no se elimina automáticamente toda la fila.
- **Filas con respuestas insuficientes:** es un filtro exploratorio del playground y no equivale a la gestión de casos completos que realiza cada CFA.
- **Inversión de ítems:** se crean columnas `__analysis` para preservar los valores originales y evitar inversiones silenciosas o dobles.

Cada cambio de filtro produce una auditoría con las filas incluidas, excluidas y el motivo de exclusión. La app también exporta la validación de la clave y la distribución recalculada de `ac_total`.

## Playground y análisis de sensibilidad

Las comparaciones recomendadas son:

1. `all_core` frente a los tres subsamples de attention checks;
2. estimación continua frente a estimación ordinal/robusta;
3. modelos CFA preespecificados de cuatro, tres y un factor para competencia;
4. estabilidad de cargas, confiabilidad, correlaciones latentes y admisibilidad;
5. diagnóstico de observaciones influyentes mediante residuos, distancia robusta, eliminación temporal y medidas tipo Cook.

Una observación influyente no es automáticamente errónea. No se deben eliminar filas únicamente porque CFI aumente o RMSEA disminuya. La exclusión requiere evidencia independiente: duplicación, valores imposibles, incumplimiento de consentimiento, respuesta fuera de rango o una regla de calidad definida antes del análisis. Las decisiones influidas por el ajuste deben replicarse en otra muestra o en una partición confirmatoria.

## CFA en R y playground en Python

Los modelos finales se estiman y validan en R/lavaan. El playground Python usa `semopy` para exploración interactiva y no sustituye los resultados WLSMV/robustos del pipeline R.

La app extrae los índices de `semopy` directamente desde las columnas devueltas por `calc_stats()`. CFI, TLI, RMSEA, GFI, AGFI, NFI y `chi2 p-value` deben interpretarse junto con convergencia, cargas, residuos y admisibilidad. Si una matriz de covarianzas latentes no es positiva definida, el modelo no debe interpretarse como confirmado aunque el optimizador reporte convergencia.

## Outputs principales

### Tablas

- `outputs/tables/sample_and_missingness_audit.xlsx`
- `outputs/tables/attention_check_recalculation.xlsx`
- `outputs/tables/psychometric_diagnostics.xlsx`
- `outputs/tables/cfa_results_all_attention_subsamples.xlsx`
- `outputs/tables/robustness_and_decisions.xlsx`
- `outputs/tables/bivariate_positive_negative_attitudes.xlsx`
- `outputs/analysis_data_scored.xlsx`

### Figuras

Las figuras se guardan en `outputs/figures/` con resolución de 300 dpi. Incluyen distribuciones, correlaciones, análisis paralelo, cargas CFA, confiabilidad, admisibilidad, cuadrantes y comparaciones entre muestras.

### Informe LaTeX

El informe standalone se encuentra en:

```text
factorial_psychometric_report_standalone.tex
```

Utiliza las figuras de `figuras/` cuando se copia al proyecto de Overleaf. Incluye interpretación, guía de lectura, diccionario de variables, resultados actualizados y una sección sobre influencia de observaciones y análisis de sensibilidad.

## Regla de interpretación psicométrica

La factorización se evalúa con correlaciones apropiadas, KMO, Bartlett, variación de los ítems, tamaño muestral y proporción participante/ítem. La convergencia numérica no garantiza admisibilidad. Las matrices latentes no positivas definidas, correlaciones latentes superiores a uno, cargas muy bajas y confiabilidades débiles deben reportarse y auditarse, no ocultarse mediante eliminación post hoc.
