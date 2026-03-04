# Plan de implementación - ACOTAR PILARES v2

## Problema actual
El script v1 dibuja líneas y textos manuales. El resultado correcto usa:
- **DIMENSION ALIGNED** de AutoCAD con estilo "cota100"
- Un **rombo interior** más pequeño dentro del original
- Capa **z_ACOTADO**

## Geometría del rombo
- 4 LINE entities, lado ~24 unidades, ángulos diagonales (~40°/130°)
- Capa: DIBUJO_DE_ELEMENTOS
- Vértices: N (arriba), E (derecha), S (abajo), W (izquierda)
- Centro = promedio de 4 vértices

## Escala
- Factor = valor_PHC / lado_rombo (ej: 120/24 = 5)
- El estilo "cota100" ya tiene este factor en DIMLFAC

## Lo que hay que dibujar (para PHC 120.6):

### 1. Rombo interior (4 LINEs)
- Offset cada vértice hacia el centro por: top_value / scale_factor (6/5 = 1.2 uds)
- Color rojo, capa z_ACOTADO

### 2. Cotas alineadas (3 DIMENSION ALIGNED)
- **Cota izquierda**: del vértice S al vértice W → muestra "120"
- **Cota derecha**: del vértice S al vértice E → muestra "120"
- **Cota superior**: del vértice N exterior al vértice N interior → muestra "6"
- Estilo: cota100, capa: z_ACOTADO

## Para PHR 16.12.6 (asimétrico):
- Los lados izquierdo y derecho tienen longitudes DIFERENTES
- Se dibuja geometría nueva (no coincide con el rombo original)
- Izq: 160/scale, Der: 120/scale desde el centro
- Se resuelve después de que PHC funcione correctamente

## Pasos de implementación:

### Paso 1: Encontrar el rombo
- Filtrar LINE por longitud (15-40) Y ángulo diagonal (30°-60° o 120°-150° normalizado 0-180°)
- Agrupar por proximidad → extraer 4 vértices únicos
- Identificar N/E/S/W por coordenadas

### Paso 2: Calcular escala y offset
- side_length = longitud media de las 4 líneas del rombo
- scale = left_value / side_length
- offset = top_value / scale (en unidades de dibujo)

### Paso 3: Dibujar rombo interior
- Para cada vértice: mover hacia el centro por offset
- 4 LINEs en capa z_ACOTADO, color rojo

### Paso 4: Crear cotas DIMALIGNED
- Establecer estilo "cota100"
- (command "_.DIMALIGNED" ptS ptW dim_line_pt) → cota izquierda
- (command "_.DIMALIGNED" ptS ptE dim_line_pt) → cota derecha
- (command "_.DIMALIGNED" ptN_ext ptN_int dim_line_pt) → cota superior
- Restaurar estilo original

### Paso 5: Ajustes para PHR
- Calcular geometría asimétrica
- Dibujar nuevo rombo exterior si los lados difieren del original
