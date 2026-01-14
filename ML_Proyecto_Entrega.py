#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan  3 08:43:53 2026

@author: danielgp
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import chi2


import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import NearestNeighbors
from sklearn.cluster import DBSCAN
from sklearn.metrics import silhouette_score
import itertools


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.cluster.hierarchy import dendrogram, linkage, fcluster
from sklearn.cluster import AgglomerativeClustering
from sklearn.metrics import calinski_harabasz_score

from sklearn.cluster import AgglomerativeClustering
import seaborn as sns
import matplotlib.pyplot as plt

# ==========================================
# 1. Carga y Preprocesamiento de Tiempo
# ==========================================
url_github = 'rutadatos_github'
df = pd.read_csv(url_github, sep=';')

# Procesamiento de la hora (Snippet proporcionado)
# Limpiamos strings, quitamos milisegundos si existen y convertimos a datetime
df['Time_Obj'] = pd.to_datetime(
    df['Time_M'].astype(str).str.split('.').str[0].str.strip(), 
    format='%H:%M:%S', 
    errors='coerce'
)
df['hour'] = df['Time_Obj'].dt.hour

# Eliminamos filas donde no se pudo determinar la hora
df = df.dropna(subset=['hour'])

# ==========================================
# 2. Definición de Features y Limpieza de Tipos
# ==========================================
features_acusticas = [
    'Height_mean', 'Depth_mean', 'Corrected_length', 'Corrected_thickness', 'NASC',
    'Corrected_perimeter', 'Corrected_area', 'Image_compactness',
    'Corrected_MVBS', 'Coefficient_of_variation',
    'X3D_school_area', 'X3D_school_volume'
]

# Asegurar que todas las features sean numéricas (coercing errors to NaN)
for col in features_acusticas + ['Lat_M', 'Lon_M']:
    df[col] = pd.to_numeric(df[col], errors='coerce')

# ==========================================
# 3. Filtros Físicos y Temporales
# ==========================================
# Lógica de exclusión: Lat/Lon positivos o nulos, MVBS nulo o positivo
cond_eliminar = (
    ((df['Lat_M'] > 0) | (df['Lat_M'] == -999)) |
    ((df['Lon_M'] > 0) | (df['Lon_M'] == -999)) |
    (df['Corrected_MVBS'] == -999) | (df['Corrected_MVBS'] > 0)
)

# Aplicar filtro físico (~) y hacer copia
df_clean = df[~cond_eliminar].copy()

# Aplicar filtro horario (Día: 06:00 - 18:00)
df_clean = df_clean[(df_clean['hour'] >= 6) & (df_clean['hour'] <= 18)]

print(f"Dimensiones del dataset limpio: {df_clean.shape}")

# ==========================================
# 4. Análisis Exploratorio (EDA) - Histogramas
# ==========================================
# Configuración del grid de gráficos
n_cols = 3
n_rows = (len(features_acusticas) + n_cols - 1) // n_cols

plt.figure(figsize=(15, 4 * n_rows))

for i, col in enumerate(features_acusticas):
    ax = plt.subplot(n_rows, n_cols, i + 1)
    
    # .values es clave para la velocidad: pasamos un array de numpy, no una serie
    data_vector = df_clean[col].dropna().values
    
    ax.hist(data_vector, bins=50, color='teal', alpha=0.7)
    ax.set_title(f'Distribución: {col}')
    ax.set_xlabel(col)

plt.tight_layout()
#plt.show()


nombre_archivo = 'histogramas_variables_acusticas.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()



# ==========================================
# 5. Detección de Outliers Multivariados (Mahalanobis)
# ==========================================
# Paso A: Crear subconjunto solo con las features de interés y sin NaNs
# Mahalanobis falla si hay NaNs en la matriz
df_mahal = df_clean[features_acusticas].dropna()

# Paso B: Calcular Matriz de Covarianza y su Inversa
cov_matrix = np.cov(df_mahal.values.T)
inv_cov_matrix = np.linalg.inv(cov_matrix)
means = df_mahal.mean().values

# Paso C: Calcular distancia para cada punto
def calculate_mahalanobis(row, mean_vec, inv_cov):
    diff = row - mean_vec
    return np.sqrt(diff.dot(inv_cov).dot(diff.T))

df_mahal['mahalanobis_dist'] = df_mahal.apply(
    lambda row: calculate_mahalanobis(row.values, means, inv_cov_matrix), 
    axis=1
)

# Paso D: Establecer umbral (Chi-cuadrado)
# Grados de libertad = número de variables
k = len(features_acusticas)
p_value = 0.9 # Criterio muy estricto (0.1% superior)
threshold = chi2.ppf(p_value, df=k)

df_mahal['is_outlier'] = df_mahal['mahalanobis_dist'] > threshold

# Unir los resultados al dataframe original (left join por índice)
df_clean = df_clean.join(df_mahal[['mahalanobis_dist', 'is_outlier']])

print(f"\n--- Resultados Mahalanobis ---")
print(f"Variables utilizadas: {k}")
print(f"Umbral Chi2 (p={p_value}): {threshold:.4f}")
print(f"Outliers detectados: {df_clean['is_outlier'].sum()}")

# Compara los promedios de los datos normales vs. los outliers detectados
print(df_clean.groupby('is_outlier')[['Corrected_MVBS', 'X3D_school_volume', 'Depth_mean']].mean())

# Visualización de outliers vs inliers en dos variables clave (ej. Area vs NASC)
plt.figure(figsize=(10, 6))
sns.scatterplot(
    data=df_clean, 
    x='X3D_school_area', 
    y='NASC', 
    hue='is_outlier', 
    palette={False: 'blue', True: 'red'},
    alpha=0.6
)
plt.title('Detección de Outliers: Área vs NASC')
plt.xscale('log') # Escala logarítmica ayuda a ver mejor estas variables
plt.yscale('log')
#plt.show()


nombre_archivo = 'Mahalanobis_Distance.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()


import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Asumimos que df_clean y features_acusticas ya están definidos del paso anterior

# ==========================================
# 6. Transformación Logarítmica y Visualización
# ==========================================
cols_log_transformed = []

# 1. Aplicar Logaritmo Natural
# ------------------------------------------
for col in features_acusticas:
    # Verificamos valores válidos (no nulos y estrictamente positivos)
    series_valid = df_clean[col].dropna()
    
    if len(series_valid) > 0 and (series_valid > 0).all():
        col_new = f'log_{col}'
        df_clean[col_new] = np.log(df_clean[col])
        cols_log_transformed.append(col_new)
    else:
        # Informamos qué variables no se tocan (ej. MVBS que es negativo)
        print(f"Skipping '{col}': contiene valores <= 0 o NaNs.")

print(f"\nVariables transformadas ({len(cols_log_transformed)}): {cols_log_transformed}")

# 2. Plotting Eficiente (Pandas Vectorizado)
# ------------------------------------------
if cols_log_transformed:
    # Calculamos filas necesarias para el grid
    n_cols = 3
    n_rows = (len(cols_log_transformed) + n_cols - 1) // n_cols

    # Usamos .hist() sobre el subset de columnas transformadas
    ax = df_clean[cols_log_transformed].hist(
        bins=50,
        figsize=(15, 4 * n_rows),
        color='purple',       # Color distintivo para datos transformados
        grid=False,           # Estilo limpio
        layout=(n_rows, n_cols),
        edgecolor='black',
        linewidth=0.5,
        xlabelsize=10,
        ylabelsize=10
    )

    plt.suptitle("Distribución de Variables Transformadas (Log-Scale)", fontsize=16, y=1.02)
    plt.tight_layout()

    # 3. Guardar Figura
    # ------------------------------------------
    filename = 'histogramas_log_features.png'
    print(f"Guardando gráfico en: {filename}")
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.show()
else:
    print("No se generaron columnas logarítmicas para graficar.")


# ==============================================================================
# 1. Preparación de Features y Matriz de Correlación
# ==============================================================================
# Seleccionamos las mejores features disponibles:
# Usamos las transformadas a Log si existen (distribución más normal), si no, las originales.
features_model = []
for col in features_acusticas:
    col_log = f'log_{col}'
    if col_log in df_clean.columns:
        features_model.append(col_log)
    else:
        features_model.append(col)

print(f"Features seleccionadas para el modelo: {features_model}")

# Matriz de datos sin nulos
X = df_clean[features_model].dropna()

# Gráfico de Correlaciones
plt.figure(figsize=(12, 10))
corr_matrix = X.corr()
mask = np.triu(np.ones_like(corr_matrix, dtype=bool)) # Máscara para ver solo la mitad inferior
sns.heatmap(corr_matrix, mask=mask, annot=True, fmt=".2f", cmap='coolwarm', vmin=-1, vmax=1)
plt.title('Matriz de Correlación de Pearson (Features Transformadas)')
#plt.show()


nombre_archivo = 'MatrizCorrelacion.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()


features_acusticaslog = [
    'log_Height_mean', 'log_Depth_mean', 'log_Corrected_length',
           'log_Corrected_thickness', 'log_NASC', 'Corrected_perimeter',
           'log_Corrected_area', 'Image_compactness', 'Corrected_MVBS',
           'log_Coefficient_of_variation'
]

X = X[features_acusticaslog]
# ==============================================================================
# 2. Estandarización de Datos (Z-Score)
# ==============================================================================
# DBSCAN es sensible a la escala. Transformamos a media 0 y varianza 1.
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# ==============================================================================
# 3. Gráfico de K-Distance (Para estimar eps)
# ==============================================================================
# Heurística: k = min_samples. Una regla de dedo es k = 2 * dimensiones
k = 2 * X.shape[1] 

# Calculamos los vecinos más cercanos
neighbors = NearestNeighbors(n_neighbors=k)
neighbors_fit = neighbors.fit(X_scaled)
distances, indices = neighbors_fit.kneighbors(X_scaled)

# Ordenamos las distancias al k-ésimo vecino
distances = np.sort(distances[:, k-1], axis=0)

plt.figure(figsize=(10, 6))
plt.plot(distances)
plt.title(f'Gráfico de K-Distance (k={k})')
plt.ylabel(f'Distancia eps al {k}-ésimo vecino')
plt.xlabel('Puntos ordenados por distancia')
# --- AGREGA ESTO ---
plt.ylim(0, 3.0)  # Cambia 3.0 por el valor máximo que qui
plt.grid(True)
#plt.show()

nombre_archivo = 'K_Distance.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()


print("Interpreta el gráfico anterior: El valor óptimo de 'eps' suele estar en el 'codo' (donde la curvatura es máxima).")

# ==============================================================================
# 4. DBSCAN con Grid Search (Búsqueda de Hiperparámetros)
# ==============================================================================
# DBSCAN no tiene un método predict() estándar, por lo que GridSearch de sklearn 
# es difícil de aplicar directamente. Hacemos un loop manual.

# Definimos rangos basados en lo que solemos ver en datos estandarizados
# eps: suele estar entre 0.1 y 2.0 en datos escalados
# min_samples: suele estar alrededor de k o un poco más bajo
eps_values = np.arange(0.4, 1.6, 0.2) 
min_samples_values = range(5, 50, 10)

best_score = -1
best_params = {}
results = []

print("\nIniciando Grid Search para DBSCAN (esto puede tardar)...")

for eps, min_samples in itertools.product(eps_values, min_samples_values):
    # Instanciar y ajustar DBSCAN
    db = DBSCAN(eps=eps, min_samples=min_samples)
    labels = db.fit_predict(X_scaled)
    
    # Métricas de validación
    # Silhouette requiere al menos 2 clusters y ruido, o mas de 1 cluster
    n_clusters = len(set(labels)) - (1 if -1 in labels else 0)
    n_noise = list(labels).count(-1)
    
    if n_clusters > 1:
        # Silhouette score mide qué tan separados están los clusters
        # Nota: Es computacionalmente costoso si tienes > 10k datos. 
        # Si tarda mucho, usa una muestra: sample_size=1000 en silhouette_score
        score = silhouette_score(X_scaled, labels)
        
        results.append([eps, min_samples, n_clusters, n_noise, score])
        
        if score > best_score:
            best_score = score
            best_params = {'eps': eps, 'min_samples': min_samples}
            best_labels = labels
    else:
        results.append([eps, min_samples, n_clusters, n_noise, -1])

# Convertir resultados a DataFrame para visualizar
res_df = pd.DataFrame(results, columns=['eps', 'min_samples', 'n_clusters', 'n_noise', 'silhouette'])

print(f"\nMejor Silhouette Score: {best_score:.4f}")
print(f"Mejores Parámetros: {best_params}")

# ==============================================================================
# 5. Visualización del Resultado del Grid Search
# ==============================================================================
# Pivotar para heatmap
pivot_res = res_df.pivot(index='min_samples', columns='eps', values='silhouette')

# Generas una lista de etiquetas formateadas
etiquetas_x = ["{:.1f}".format(x) for x in pivot_res.columns]

# Se las pasas al gráfico
sns.heatmap(pivot_res, annot=True, xticklabels=etiquetas_x)

plt.figure(figsize=(10, 6))
etiquetas_x = ["{:.1f}".format(x) for x in pivot_res.columns]

# Se las pasas al gráfico
sns.heatmap(pivot_res, annot=True, xticklabels=etiquetas_x)

plt.title('Grid Search: Silhouette Score por parámetros DBSCAN')
#plt.show()
nombre_archivo = 'MatrizParametros.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()

# Aplicar el mejor modelo a los datos originales para análisis posterior
if best_score != -1:
    X['cluster_dbscan'] = best_labels
    print("\nConteo de clusters ( -1 es ruido ):")
    print(X['cluster_dbscan'].value_counts())
else:
    print("No se encontraron parámetros que generaran más de 1 cluster válido.")



# Asumimos que X_scaled ya existe del paso anterior
# X_scaled = scaler.fit_transform(X)

# ==============================================================================
# 1. Generación de la Matriz de Enlace (Linkage Matrix)
# ==============================================================================
# Usamos el método 'ward' y métrica euclidiana
print("Calculando matriz de enlace (esto puede tardar con muchos datos)...")
Z = linkage(X_scaled, method='ward')

# ==============================================================================
# 2. Visualización del Dendograma
# ==============================================================================
plt.figure(figsize=(12, 7))
plt.title('Dendograma de Clustering Jerárquico (Método Ward)')
plt.xlabel('Índices de las Muestras (o tamaño del cluster fusionado)')
plt.ylabel('Distancia (Varianza intra-cluster incremental)')

# Usamos truncate_mode para que el gráfico sea legible si hay muchos puntos
dendrogram(
    Z,
    truncate_mode='lastp',  # Mostrar solo los últimos p clusters fusionados
    p=50,                   # Mostrar las últimas 50 fusiones
    leaf_rotation=90.,
    leaf_font_size=10.,
    show_contracted=True    # Muestra la distribución en ramas contraídas
)
plt.axhline(y=0, color='black', linewidth=1) # Línea base
#plt.show()

nombre_archivo = 'Dendograma.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()

print("Interpretación: La altura vertical de las líneas indica la varianza ganada al unir dos clusters. Corta donde el salto vertical sea más grande.")

# ==============================================================================
# 3. Análisis de Varianza (Criterio del Codo y Calinski-Harabasz)
# ==============================================================================
# Para elegir K rigurosamente basándonos en varianza intra (WSS) y entre (BSS)

wss_values = [] # Within-Cluster Sum of Squares (Inercia)
bss_values = [] # Between-Cluster Sum of Squares
calinski_values = [] # Ratio BSS / WSS (aproximado)

# Calculamos la Varianza Total (TSS) una sola vez
# TSS es la suma de distancias al cuadrado de todos los puntos al centroide global
grand_mean = np.mean(X_scaled, axis=0)
tss = np.sum((X_scaled - grand_mean) ** 2)

k_range = range(2, 15) # Probamos de 2 a 14 clusters

print("\nEvaluando varianza para diferentes k...")

for k in k_range:
    # Ajustamos el modelo para k clusters
    model = AgglomerativeClustering(n_clusters=k, linkage='ward')
    labels = model.fit_predict(X_scaled)
    
    # Cálculo manual de WSS (Inercia) para Agglomerative
    wss_k = 0
    for i in range(k):
        cluster_points = X_scaled[labels == i]
        if len(cluster_points) > 0:
            centroid = np.mean(cluster_points, axis=0)
            wss_k += np.sum((cluster_points - centroid) ** 2)
            
    wss_values.append(wss_k)
    
    # BSS = TSS - WSS
    bss_values.append(tss - wss_k)
    
    # Índice Calinski-Harabasz (Métrica estándar de varianza entre/intra)
    ch_score = calinski_harabasz_score(X_scaled, labels)
    calinski_values.append(ch_score)

# ==============================================================================
# 4. Visualización de Métricas para selección de K
# ==============================================================================
fig, ax1 = plt.subplots(figsize=(12, 6))

# Eje izquierdo: WSS (Codo)
color = 'tab:blue'
ax1.set_xlabel('Número de Clusters (k)')
ax1.set_ylabel('Varianza Intra-Cluster (WSS)', color=color)
ax1.plot(k_range, wss_values, marker='o', color=color, label='WSS (Codo)')
ax1.tick_params(axis='y', labelcolor=color)
ax1.grid(True, alpha=0.3)

# Eje derecho: Calinski-Harabasz (Varianza Entre / Varianza Intra)
ax2 = ax1.twinx()  
color = 'tab:red'
ax2.set_ylabel('Índice Calinski-Harabasz (Mayor es mejor)', color=color)
ax2.plot(k_range, calinski_values, marker='s', linestyle='--', color=color, label='Calinski-Harabasz')
ax2.tick_params(axis='y', labelcolor=color)

plt.title('Evaluación de Clusters: Inercia vs Separación')
#plt.show()

nombre_archivo = 'AglomerativeCriterio.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()


# Resumen numérico
results_var = pd.DataFrame({
    'k': k_range,
    'WSS (Compactación)': wss_values,
    'BSS (Separación)': bss_values,
    'Explicación Varianza (%)': np.array(bss_values) / tss * 100
})

print("\n--- Tabla de Varianza Explicada ---")
print(results_var.round(2))



# 1. Aplicar el Modelo Final con k=6
# ==========================================================
k_selected = 6
model_hc = AgglomerativeClustering(n_clusters=k_selected, linkage='ward')

# Ajustamos sobre los datos escalados (X_scaled viene de tu paso anterior)
labels_hc = model_hc.fit_predict(X_scaled)

# Guardamos los clusters en el DataFrame original limpio
df_clean['cluster_hc'] = labels_hc

# 2. Conteo de muestras por cluster
# ==========================================================
print("Distribución de cardúmenes por cluster:")
print(df_clean['cluster_hc'].value_counts().sort_index())

# 3. Visualización de Perfiles (Interpretación Biológica)
# ==========================================================
# Elegimos variables clave para interpretar los grupos
# Sugiero ver: Profundidad, Energía (MVBS/NASC), Tamaño (Area) y Morfología (Compactness)
vars_interpretar = ['Depth_mean', 'Corrected_MVBS', 'Corrected_perimeter', 'Corrected_area', 'Image_compactness', 'NASC']

# Filtramos las que existan en tu df
vars_plot = [v for v in vars_interpretar if v in df_clean.columns]

# Configuración del gráfico
#plt.figure(figsize=(15, 10))

# Lista de palabras clave que SUGIEREN escala logarítmica
# (NASC, Área y Volumen suelen tener distribución exponencial)
log_keywords = ['nasc', 'area', 'perimeter' ,'volume', 'compactness']

for i, col in enumerate(vars_plot):
    plt.subplot(2, 3, i+1)
    
    # 1. Crear el boxplot sin outliers (showfliers=False)
    sns.boxplot(x='cluster_hc', y=col, data=df_clean, palette='viridis', showfliers=False)
    
    plt.title(f'{col}')
    plt.xlabel('Cluster ID')
    
    # 2. Lógica para aplicar escala Logarítmica
    col_lower = col.lower()
    
    # Aplicamos log si está en las palabras clave, PERO...
    # Excluimos 'MVBS' explícitamente porque suele ser negativo (dB) y daría error en log
    if any(k in col_lower for k in log_keywords) and 'mvbs' not in col_lower:
        plt.yscale('log')
        plt.ylabel(f'{col}')
    else:
        plt.ylabel(col)

plt.tight_layout()
#plt.show()

nombre_archivo = 'GruposAnalisis.png'

print(f"Guardando figura como '{nombre_archivo}'...")

plt.savefig(
    nombre_archivo,         # Nombre y extensión
    dpi=300,                # Resolución: 300 es estándar para papers/informes (72 es web)
    bbox_inches='tight',    # CRÍTICO: Elimina espacios blancos extra y evita que se corten textos
    facecolor='white'       # Asegura fondo blanco (útil si tu IDE usa modo oscuro)
)

# 3. Mostrar (Limpia el canvas, así que guardar va antes)
plt.show()

# 4. Cerrar la figura (Buena práctica para liberar memoria RAM en loops)
plt.close()

# 4. Tabla Resumen de Promedios (Centroides reales)
# ==========================================================
# Agrupamos por cluster y sacamos la media de las variables originales (no escaladas)
perfil_clusters = df_clean.groupby('cluster_hc')[vars_plot].mean()
print("\n--- Caracterización Promedio de los Clusters ---")
print(perfil_clusters)


## comparación

# ==============================================================================
# 7. Comparación Justa de Métricas (Silhouette Score)
# ==============================================================================
print("\n==========================================")
print("COMPARATIVA FINAL: DBSCAN vs JERÁRQUICO")
print("==========================================")

from sklearn import metrics

# Aseguramos tener las etiquetas listas
# Nota: labels_dbscan debe venir de tu mejor modelo (best_labels o df_clean['cluster_dbscan'])
if 'cluster_dbscan' in df_clean.columns:
    labels_db = df_clean['cluster_dbscan'].values
else:
    print("Advertencia: No se encontró columna 'cluster_dbscan', usando 'best_labels' del grid search.")
    labels_db = best_labels

labels_hc = df_clean['cluster_hc'].values

# --- 1. Cálculo para DBSCAN (Ignorando Ruido) ---
# El ruido (-1) disperso penaliza injustamente el score si se incluye como un cluster.
mask_validos = labels_db != -1  # Filtro: Solo datos que NO son ruido

if np.sum(mask_validos) > 2: # Necesitamos al menos 2 puntos y >1 cluster para calcular
    # Importante: Usamos X_scaled[mask] para que coincida con las etiquetas filtradas
    score_db = metrics.silhouette_score(X_scaled[mask_validos], labels_db[mask_validos])
    num_clusters_db = len(set(labels_db[mask_validos]))
    pct_ruido = (len(labels_db) - np.sum(mask_validos)) / len(labels_db) * 100
    
    print(f"DBSCAN Silhouette (Sin Ruido):  {score_db:.4f}")
    print(f" -> Clusters detectados: {num_clusters_db}")
    print(f" -> Datos descartados (Ruido): {pct_ruido:.2f}%")
else:
    score_db = -1
    print("DBSCAN: No se pudo calcular (Solo encontró ruido o un único cluster).")

# --- 2. Cálculo para Agglomerative (Ward) ---
# Ward fuerza a clasificar todos los puntos, así que usamos el dataset completo.
score_hc = metrics.silhouette_score(X_scaled, labels_hc)
num_clusters_hc = len(set(labels_hc))


# ==============================================================================
# BLOQUE NUEVO: K-Means Clustering
# ==============================================================================
from sklearn.cluster import KMeans

print("\n==========================================")
print("INICIANDO ANÁLISIS K-MEANS")
print("==========================================")

# 1. Búsqueda del K óptimo (Codo y Silueta)
# ----------------------------------------------------------
inertia = []
silhouette_km = []
k_range_km = range(2, 15)

print("Calculando métricas para K-Means (k=2 a 14)...")

for k in k_range_km:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
    labels = kmeans.fit_predict(X_scaled)
    
    inertia.append(kmeans.inertia_)
    silhouette_km.append(silhouette_score(X_scaled, labels))

# 2. Visualización de Métricas K-Means
# ----------------------------------------------------------
fig, ax1 = plt.subplots(figsize=(12, 6))

# Eje Izquierdo: Inercia (Método del Codo)
color = 'tab:blue'
ax1.set_xlabel('Número de Clusters (k)')
ax1.set_ylabel('Inercia (Suma errores al cuadrado)', color=color)
ax1.plot(k_range_km, inertia, marker='o', color=color, label='Inercia')
ax1.tick_params(axis='y', labelcolor=color)

# Eje Derecho: Silhouette Score
ax2 = ax1.twinx()
color = 'tab:orange'
ax2.set_ylabel('Silhouette Score (Promedio)', color=color)
ax2.plot(k_range_km, silhouette_km, marker='s', linestyle='--', color=color, label='Silhouette')
ax2.tick_params(axis='y', labelcolor=color)

plt.title('Evaluación K-Means: Codo vs Silueta')
plt.grid(True, alpha=0.3)
plt.tight_layout()

nombre_archivo_km = 'KMeans_Evaluacion.png'
print(f"Guardando gráfico: {nombre_archivo_km}")
plt.savefig(nombre_archivo_km, dpi=300, bbox_inches='tight')
plt.show()
plt.close()

# 3. Ajuste del Modelo K-Means Final
# ----------------------------------------------------------
# Seleccionamos el k con mejor Silhouette (o puedes fijarlo manualmente ej: k=6)
best_k_index = np.argmax(silhouette_km)
best_k = k_range_km[best_k_index]

print(f"\nMejor K sugerido por Silueta: {best_k}")
print(f"Ajustando K-Means con k={best_k}...")

kmeans_final = KMeans(n_clusters=best_k, random_state=42, n_init=10)
labels_km = kmeans_final.fit_predict(X_scaled)

# Guardar en DataFrame
df_clean['cluster_kmeans'] = labels_km

print("Conteo de clusters K-Means:")
print(df_clean['cluster_kmeans'].value_counts().sort_index())




import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics import calinski_harabasz_score
from sklearn.datasets import make_blobs

# --- 1. PREPARACIÓN DE DATOS ---
# Reemplaza esta línea con la carga de tus propios datos
# X = tus_datos_normalizados
X = X_scaled#make_blobs(n_samples=500, centers=4, cluster_std=0.7, random_state=42)

# --- 2. CÁLCULO DE MÉTRICAS ---
k_range = range(2, 11)  # Empezamos en 2 porque Calinski-Harabasz necesita al menos 2 grupos
wss_values = []
calinski_values = []

for k in k_range:
    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
    labels = kmeans.fit_predict(X)
    
    # Inercia (WSS)
    wss_values.append(kmeans.inertia_)
    # Índice Calinski-Harabasz
    calinski_values.append(calinski_harabasz_score(X, labels))

# --- 3. GRAFICACIÓN CON DOBLE EJE (Tu código) ---
fig, ax1 = plt.subplots(figsize=(10, 6))

# Eje izquierdo: WSS (Codo)
color = 'tab:blue'
ax1.set_xlabel('Número de Clusters (k)', fontsize=12)
ax1.set_ylabel('Varianza Intra-Cluster (WSS)', color=color, fontsize=12)
ax1.plot(k_range, wss_values, marker='o', color=color, linewidth=2, label='WSS (Codo)')
ax1.tick_params(axis='y', labelcolor=color)
ax1.grid(True, alpha=0.3)

# Eje derecho: Calinski-Harabasz
ax2 = ax1.twinx() 
color = 'tab:red'
ax2.set_ylabel('Índice Calinski-Harabasz (Mayor es mejor)', color=color, fontsize=12)
ax2.plot(k_range, calinski_values, marker='s', linestyle='--', color=color, linewidth=2, label='Calinski-Harabasz')
ax2.tick_params(axis='y', labelcolor=color)

plt.title('Evaluación de Clusters: Inercia vs Separación', fontsize=14)
fig.tight_layout()  # Ajusta márgenes para que no se corten los textos
plt.show()






# ==============================================================================
# SECCIÓN FINAL CORREGIDA: K-MEANS CLUSTERING (Exploración de 4 Clústeres)
# ==============================================================================
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

print("\n==========================================")
print("EJECUTANDO K-MEANS FINAL (k=4)")
print("==========================================")

# 1. Ajuste del Modelo
# ----------------------------------------------------------
k = 4
kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
# Ajustamos sobre los datos estandarizados
labels_km = kmeans.fit_predict(X_scaled)
centroids = kmeans.cluster_centers_


# ==============================================================================
# 2. Preparación de Datos para Análisis (CORRECCIÓN MANUAL ROBUSTA)
# ==============================================================================
# Definimos manualmente las 10 variables exactas que componen X (modelo log-transformado)
# Esto evita que el código tome por error la lista de 12 variables originales.
nombres_modelo_10 = [
    'log_Height_mean', 'log_Depth_mean', 'log_Corrected_length',
    'log_Corrected_thickness', 'log_NASC', 'Corrected_perimeter',
    'log_Corrected_area', 'Image_compactness', 'Corrected_MVBS',
    'log_Coefficient_of_variation'
]

# Verificación de seguridad antes de crear el DataFrame
if isinstance(X, np.ndarray):
    if X.shape[1] == len(nombres_modelo_10):
        # Si coinciden (10 vs 10), usamos los nombres bonitos
        cols = nombres_modelo_10
    else:
        # Si por alguna razón X tiene otro tamaño, usamos nombres genéricos para que no falle
        print(f"Advertencia: X tiene {X.shape[1]} columnas, pero esperábamos 10. Usando nombres genéricos.")
        cols = [f'Var_{i}' for i in range(X.shape[1])]
    
    df_analisis = pd.DataFrame(X, columns=cols)
else:
    # Si X ya es DataFrame, solo copiamos
    df_analisis = X.copy()

# Asignamos los clusters (Reseteamos índice para asegurar alineación)
df_analisis.reset_index(drop=True, inplace=True)
df_analisis['Cluster_ID'] = labels_km

print("DataFrame de análisis creado correctamente.")
print(f"Dimensiones: {df_analisis.shape}")

# 3. Estadísticas e Interpretación
# ----------------------------------------------------------
# Calculamos el perfil promedio (Centroides reales interpretables)
perfil_promedio = df_analisis.groupby('Cluster_ID').mean().round(3)
conteo_muestras = df_analisis['Cluster_ID'].value_counts().sort_index()

print("\n--- Cantidad de Cardúmenes por Cluster ---")
print(conteo_muestras)

print("\n--- Perfil Promedio de cada Cluster ---")
print(perfil_promedio)

# 4. Visualización con PCA
# ----------------------------------------------------------
pca = PCA(n_components=2)
# Usamos X_scaled para el PCA (es lo correcto matemáticamente)
X_pca = pca.fit_transform(X_scaled)
centroids_pca = pca.transform(centroids)

plt.figure(figsize=(10, 7))
colors = ['#FF9999', '#66B2FF', '#99FF99', '#FFCC99']

for i in range(k):
    # Filtramos usando los índices reseteados o alineados
    mask = labels_km == i
    plt.scatter(
        X_pca[mask, 0], X_pca[mask, 1], 
        c=colors[i], 
        label=f'Cluster {i} (n={conteo_muestras[i]})', 
        alpha=0.6, 
        edgecolors='none', 
        s=30
    )

# Centroides
plt.scatter(
    centroids_pca[:, 0], centroids_pca[:, 1], 
    s=250, c='red', marker='X', 
    label='Centroides', edgecolors='black', linewidth=1.5
)

plt.title(f'Segmentación K-Means (k={k}) - Proyección PCA', fontsize=14)
plt.xlabel('PC1 (Dimensión principal)')
plt.ylabel('PC2 (Dimensión secundaria)')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()

nombre_archivo_km = 'KMeans_4Clusters_Final_Corregido.png'
plt.savefig(nombre_archivo_km, dpi=300, bbox_inches='tight')
print(f"\nGráfica guardada como: {nombre_archivo_km}")
plt.show()


