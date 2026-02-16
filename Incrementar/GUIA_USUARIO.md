# Guía de Usuario: Incremento Numeración v2.0

Esta herramienta para AutoCAD/LusoCAD permite automatizar la numeración e incremento de textos y bloques con atributos de forma rápida y flexible.

---

## 1. Instalación y Carga

1. Abre AutoCAD o LusoCAD.
2. Escribe el comando `APPLOAD`.
3. Busca y selecciona el archivo **`Incrementar.lsp`**.
4. Haz clic en "Cargar" (Load).
   * Verás el mensaje: `Incremento numeracion cargado.`

---

## 2. Comando Principal: `INCREMENTAR`

Escribe **`INCREMENTAR`** en la línea de comandos para abrir el panel principal.

### Panel de Parámetros (Izquierda)
Este panel se comparte entre todas las pestañas:
* **Tipo de valor**: Elige entre números, letras mayúsculas o minúsculas.
* **Valor inicial**: El número o letra por el que quieres empezar (ej: `1` o `01`). 
  * *Tip: Si escribes `01` con el cero delante, el programa mantendrá ese formato (01, 02...).*
* **Incremento**: Cuánto se suma en cada paso (ej: `1`, `2`, `10`).
* **Separador, Prefijo y Sufijo**: Añade texto antes o después del número automáticamente.

---

## 3. Pestañas de Funcionamiento

### 📑 ATRIBUTO (Insertar Bloques)
Sirve para ir **insertando nuevos bloques** que ya tienen atributos.
1. Haz clic en **Examinar...** y pincha un bloque que ya tengas en el dibujo.
2. Elige en la lista el **Identificador** (Tag) que quieres que se numere.
3. Ajusta la escala y rotación si es necesario.
4. Pulsa **OK** y haz clic en la pantalla donde quieras ir colocando los nuevos bloques numerados.

### 📑 TEXTO (Insertar Textos nuevos)
Crea y coloca **textos nuevos** punto por punto.
1. Configura el estilo, altura y rotación del texto.
2. Pulsa **OK** y haz clic en el dibujo para ir colocando los números uno a uno.

### 📑 SELECCIÓN (Modificar existentes)
Para numerar cosas que **ya están dibujadas** haciendo clic sobre ellas.
1. Marca qué quieres numerar (Texto, MTexto o Bloque).
2. Si es un bloque, escribe el nombre del atributo (ej: `NM`).
3. Elige la **Acción**: Sustituir el texto, o añadirlo como prefijo/sufijo.
4. Pulsa **OK** y ve pinchando los elementos en el orden que quieras que se numeren.

### 📑 AUTO (Renumeración masiva)
Numeración automática de muchos elementos a la vez **por su posición**.
1. Configura los filtros y la acción (igual que en Selección).
2. Elige el orden de clasificación (ej: de izquierda a derecha por X Ascendente).
3. Pulsa **OK** y haz una ventana de selección para pillar todos los elementos a la vez. El programa hará el resto.

---

## 4. Notas y Consejos
* **Bloques Dinámicos**: El programa es compatible con bloques dinámicos y anónimos.
* **Padding (Ceros)**: 
  * En modo "Auto", el programa pone los ceros automáticamente según el total.
  * En modos manuales, pon los ceros en el "Valor inicial" (ej: `001`).
* **Atajos**: Si no quieres pasar por el menú, puedes usar `NUMERAR`, `INSERTARTEXTO`, `RENUMERAR` directamente para funciones rápidas. Además tienes `INCREMENTARSUFIJO`, `ANADIRVALOR` e `INCREMENTARSELECCION`.

---

*Desarrollado para LusoCAD - 2026*
