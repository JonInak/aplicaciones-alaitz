;;=============================================================================
;; Generador de Perfiles Topográficos (Multi-PK)
;; Comando principal: GRAFICAPTOP
;;=============================================================================
;; Este script genera gráficas de perfil de terreno dadas una o más líneas
;; de sección (P.K.) y un conjunto de curvas de nivel seleccionadas.
;; Las gráficas se disponen en filas de 4.
;;=============================================================================

(vl-load-com)

;;-----------------------------------------------------------------------------
;; Funciones de Utilidad Básicas
;;-----------------------------------------------------------------------------

;; Convierte un punto a formato 3D (Z=0.0 si falta)
(defun top:as-3d (pt)
  (cond
    ((and pt (= (length pt) 2)) (list (car pt) (cadr pt) 0.0))
    ((and pt (= (length pt) 3)) pt)
    (T '(0.0 0.0 0.0))
  )
)

;; Extrae una lista de puntos 3D desde un VARIANT o SAFEARRAY
(defun top:variant->points (v / raw lst pts)
  (setq raw
         (cond
           ((= (type v) 'VARIANT) (vlax-variant-value v))
           ((= (type v) 'SAFEARRAY) v)
           (T nil)
         ))
  (if (= (type raw) 'SAFEARRAY)
    (progn
      (setq lst (vlax-safearray->list raw))
      (while (>= (length lst) 3)
        (setq pts (cons (list (car lst) (cadr lst) (caddr lst)) pts)
              lst (cdddr lst))
      )
      (reverse pts)
    )
  )
)

;; Calcula la cota (Z) efectiva de un objeto curva en un punto dado
(defun top:get-elevation (obj pt / ent elev pt_on_curve)
  (setq ent (vlax-vla-object->ename obj))
  ;; Intento 1: A través de los datos DXF (grupo 38 = Elevación para LWPOLYLINE)
  (setq elev (cdr (assoc 38 (entget ent))))
  
  ;; Intento 2: Propiedad COM
  (if (not elev)
    (if (vlax-property-available-p obj 'Elevation)
      (setq elev (vl-catch-all-apply 'vla-get-Elevation (list obj)))
    )
  )
  (if (vl-catch-all-error-p elev) (setq elev nil))
  
  ;; Intento 3: Si todo falla, proyectamos el punto (X,Y) a la curva original para sacar su Z real
  (if (not elev)
    (progn
      (setq pt_on_curve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj pt T)))
      (if (and pt_on_curve (not (vl-catch-all-error-p pt_on_curve)))
        (setq elev (caddr pt_on_curve))
        (setq elev 0.0)
      )
    )
  )
  ;; Convertir a flotante por seguridad
  (if (and elev (numberp elev)) (float elev) 0.0)
)

;; Intersección en 2D ignorando la altura Z (Apparent Intersection)
(defun top:intersect-2d (obj1 obj2 / copy1 copy2 flatten-obj ints pt)
  (defun flatten-obj (obj / typ start end)
    (setq typ (vla-get-ObjectName obj))
    (cond
      ((or (= typ "AcDbPolyline") (= typ "AcDb2dPolyline") (= typ "AcDbCircle") (= typ "AcDbArc"))
       (vl-catch-all-apply 'vla-put-Elevation (list obj 0.0)))
      ((= typ "AcDbLine")
       (setq start (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
       (setq end (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
       (vla-put-StartPoint obj (vlax-3d-point (list (car start) (cadr start) 0.0)))
       (vla-put-EndPoint obj (vlax-3d-point (list (car end) (cadr end) 0.0))))
    )
    obj
  )
  (setq copy1 (vla-Copy obj1))
  (setq copy2 (vla-Copy obj2))
  (flatten-obj copy1) (flatten-obj copy2)
  (setq ints (vl-catch-all-apply 'vlax-invoke (list copy1 'IntersectWith copy2 0)))
  (vla-Delete copy1) (vla-Delete copy2)
  (if (or (vl-catch-all-error-p ints) (not ints)) nil ints)
)

;; Obtiene el inicio y el fin de la curva y determina la dirección origen
(defun top:get-curve-origin (obj pt_click / start_pt end_pt d_start d_end total_dist)
  (setq start_pt (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))
  (setq end_pt (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
  (setq total_dist (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
  
  (if (or (vl-catch-all-error-p start_pt) (vl-catch-all-error-p end_pt))
    nil
    (progn
      (setq d_start (distance pt_click start_pt))
      (setq d_end (distance pt_click end_pt))
      ;; Devuelve una lista: Punto Origen, Flag Invertido, Longitud Total
      (if (< d_end d_start)
        (list end_pt T total_dist)   ;; Clicó cerca del final -> el origen es el END
        (list start_pt nil total_dist) ;; Clicó cerca de inicio -> origen es START
      )
    )
  )
)

;; Busca el texto "P.K.=XXX" más cercano a una línea P.K.
;; Usa distancia PERPENDICULAR a la línea (no al punto medio) para mayor precisión
;; Devuelve el string del texto encontrado o nil
(defun top:find-pk-label (obj_pk / start_pt end_pt mid_pt radius
                          ss_txt i ent_txt obj_txt txt_str txt_pt closest_pt
                          best_str best_dist cur_dist p1 p2)
  (setq start_pt (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj_pk)))
  (setq end_pt (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj_pk)))
  
  (if (or (vl-catch-all-error-p start_pt) (vl-catch-all-error-p end_pt))
    nil
    (progn
      ;; Punto medio para definir la zona de búsqueda
      (setq mid_pt (list (/ (+ (car start_pt) (car end_pt)) 2.0)
                         (/ (+ (cadr start_pt) (cadr end_pt)) 2.0)
                         0.0))
      ;; Radio de búsqueda generoso (la longitud completa de la línea)
      (setq radius (distance start_pt end_pt))
      
      ;; Definir caja de búsqueda alrededor del punto medio
      (setq p1 (list (- (car mid_pt) radius) (- (cadr mid_pt) radius)))
      (setq p2 (list (+ (car mid_pt) radius) (+ (cadr mid_pt) radius)))
      
      ;; Buscar TEXT y MTEXT en la zona
      (setq ss_txt (ssget "C" p1 p2 '((0 . "TEXT,MTEXT"))))
      
      (if ss_txt
        (progn
          (setq best_str nil best_dist 1e10 i 0)
          (while (< i (sslength ss_txt))
            (setq ent_txt (ssname ss_txt i))
            (setq obj_txt (vlax-ename->vla-object ent_txt))
            (setq txt_str (vla-get-TextString obj_txt))
            
            ;; Filtrar: debe contener "P.K" o "PK"
            (if (or (vl-string-search "P.K" (strcase txt_str))
                    (vl-string-search "PK" (strcase txt_str)))
              (progn
                ;; Obtener punto de inserción del texto
                (setq txt_pt (vlax-safearray->list 
                               (vlax-variant-value (vla-get-InsertionPoint obj_txt))))
                ;; Distancia PERPENDICULAR: punto más cercano sobre la línea P.K. al texto
                (setq closest_pt (vl-catch-all-apply 'vlax-curve-getClosestPointTo 
                                   (list obj_pk txt_pt)))
                (if (not (vl-catch-all-error-p closest_pt))
                  (setq cur_dist (distance closest_pt txt_pt))
                  (setq cur_dist (distance mid_pt txt_pt)) ;; Fallback
                )
                (if (< cur_dist best_dist)
                  (setq best_dist cur_dist
                        best_str txt_str)
                )
              )
            )
            (setq i (1+ i))
          )
          best_str
        )
      )
    )
  )
)

;;-----------------------------------------------------------------------------
;; Funciones de Cálculo y Dibujo (extraídas para Multi-PK)
;;-----------------------------------------------------------------------------

;; Calcula las intersecciones de UN P.K. con las curvas de nivel
;; Devuelve una lista ordenada de (distancia z)
;; También dibuja marcas magenta en el plano topográfico
(defun top:calc-intersections (ms obj_pk invert_dist total_length ss
                               / i obj_curva ints_raw pts pt pt_on_pk d z lst temp_raw
                                 obj_mark mark_str)
  (setq lst nil)
  (setq i 0)
  
  (while (< i (sslength ss))
    (setq obj_curva (vlax-ename->vla-object (ssname ss i)))
    (setq ints_raw (top:intersect-2d obj_pk obj_curva))
    
    (if (and (not (vl-catch-all-error-p ints_raw)) ints_raw)
      (progn
        (setq pts nil)
        (setq temp_raw ints_raw)
        (while (>= (length temp_raw) 3)
          (setq pts (cons (list (car temp_raw) (cadr temp_raw) (caddr temp_raw)) pts))
          (setq temp_raw (cdddr temp_raw))
        )
        (setq pts (reverse pts))

        (foreach pt pts
          (setq pt_on_pk (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj_pk pt)))
          (if (not (vl-catch-all-error-p pt_on_pk))
            (progn
              (setq d (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list obj_pk pt_on_pk)))
              (if (not (vl-catch-all-error-p d))
                (progn
                  (if invert_dist (setq d (- total_length d)))
                  (setq z (top:get-elevation obj_curva pt))
                  
                  ;; FILTRO: Solo incluir puntos con elevación real (Z > 0)
                  (if (and d z (> z 0.0))
                    (progn
                      (setq lst (cons (list d z) lst))
                      ;; MARCA en el plano topográfico
                      (setq mark_str (rtos z 2 1))
                      (setq obj_mark (vl-catch-all-apply 'vla-AddCircle
                                       (list ms (vlax-3d-point pt) 0.3)))
                      (if (not (vl-catch-all-error-p obj_mark))
                        (vla-put-Color obj_mark 6)
                      )
                      (setq obj_mark (vl-catch-all-apply 'vla-AddText
                                       (list ms mark_str (vlax-3d-point (list (+ (car pt) 0.4) (+ (cadr pt) 0.2) 0.0)) 0.5)))
                      (if (not (vl-catch-all-error-p obj_mark))
                        (vla-put-Color obj_mark 6)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; Devolver lista ordenada por distancia
  (if (>= (length lst) 2)
    (vl-sort lst '(lambda (a b) (< (car a) (car b))))
    nil
  )
)

;; Calcula d_ref para un P.K. dado y una línea de referencia
(defun top:calc-ref-dist (obj_pk obj_ref invert_dist total_length
                          / ref_ints ref_pt pt_on_pk_ref d_ref)
  (setq d_ref 0.0)
  (setq ref_ints (top:intersect-2d obj_pk obj_ref))
  (if (and ref_ints (not (vl-catch-all-error-p ref_ints)) (>= (length ref_ints) 3))
    (progn
      (setq ref_pt (list (car ref_ints) (cadr ref_ints) (caddr ref_ints)))
      (setq pt_on_pk_ref (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj_pk ref_pt)))
      (if (not (vl-catch-all-error-p pt_on_pk_ref))
        (progn
          (setq d_ref (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list obj_pk pt_on_pk_ref)))
          (if (vl-catch-all-error-p d_ref) (setq d_ref 0.0))
          (if invert_dist (setq d_ref (- total_length d_ref)))
        )
      )
    )
  )
  d_ref
)

;; Dibuja UNA gráfica de perfil completa en la posición p_base
(defun top:draw-one-profile (ms lst_sorted p_base scaleX scaleY datum z_max d_ref total_length pk_label
                             / offset text_h p_base_end obj_base p_eje_x p_eje_top obj_eje obj_txt
                               pl_pts i d z px py obj_pl obj_mark mark_str label_h)
  
  (setq offset (/ (- z_max (+ datum 0.0)) 10.0))
  (if (< offset 1.0) (setq offset 1.0))
  (setq text_h (* offset 0.5))
  
  ;; Línea base horizontal (Blanca)
  (setq p_base_end (list (- (car p_base) (* total_length scaleX)) (cadr p_base) 0.0))
  (setq obj_base (vl-catch-all-apply 'vla-AddLine (list ms (vlax-3d-point p_base) (vlax-3d-point p_base_end))))
  (if (not (vl-catch-all-error-p obj_base)) (vla-put-Color obj_base 7))
  
  ;; Línea vertical de Referencia (Naranja) — posicionada en d_ref
  (setq p_eje_x (- (car p_base) (* d_ref scaleX)))
  (setq p_eje_top (list p_eje_x (+ (cadr p_base) (* (- z_max datum) scaleY) offset) 0.0))
  (setq obj_eje (vl-catch-all-apply 'vla-AddLine 
                 (list ms (vlax-3d-point (list p_eje_x (cadr p_base) 0.0)) (vlax-3d-point p_eje_top))))
  (if (not (vl-catch-all-error-p obj_eje)) (vla-put-Color obj_eje 30))
  
  ;; Texto de referencia en el eje
  (setq obj_txt (vl-catch-all-apply 'vla-AddText 
                 (list ms 
                       (if (> d_ref 0.0) "REF" "P.K. 0")
                       (vlax-3d-point (list (+ p_eje_x (* text_h 0.2)) (+ (cadr p_base) text_h) 0.0)) 
                       text_h)))
  (if (not (vl-catch-all-error-p obj_txt)) (vla-put-Color obj_txt 30))
  
  ;; Etiqueta del P.K. (debajo de la línea base, centrada bajo la gráfica)
  (if pk_label
    (progn
      (setq obj_txt (vl-catch-all-apply 'vla-AddText
                     (list ms pk_label
                           (vlax-3d-point (list (- (car p_base) (/ (* total_length scaleX) 2.0))
                                               (- (cadr p_base) (* text_h 2.5)) 0.0))
                           (* text_h 1.2))))
      (if (not (vl-catch-all-error-p obj_txt)) (vla-put-Color obj_txt 7)) ;; Blanco
    )
  )
  
  ;; Polilínea de Terreno (Verde)
  (setq pl_pts (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length lst_sorted))))))
  (setq i 0)
  (foreach obj lst_sorted
    (setq d (car obj)
          z (cadr obj))
    (setq py (+ (cadr p_base) (* (- z datum) scaleY)))
    (setq px (- (car p_base) (* d scaleX)))
    
    (vlax-safearray-put-element pl_pts i px)
    (vlax-safearray-put-element pl_pts (1+ i) py)
    (setq i (+ i 2))
  )
  
  (setq obj_pl (vl-catch-all-apply 'vla-AddLightWeightPolyline (list ms pl_pts)))
  (if (not (vl-catch-all-error-p obj_pl))
    (progn
      (vla-put-Color obj_pl 3)
      (vla-put-ConstantWidth obj_pl (* text_h 0.1))
    )
    (princ (strcat "\nError dibujando perfil. Intersecciones válidas: " (itoa (length lst_sorted))))
  )
  
  ;; Etiquetas de elevación (Cyan, vertical)
  (setq label_h (* text_h 0.4))
  (foreach obj lst_sorted
    (setq d (car obj)
          z (cadr obj))
    (setq py (+ (cadr p_base) (* (- z datum) scaleY)))
    (setq px (- (car p_base) (* d scaleX)))
    (setq mark_str (rtos z 2 1))
    (setq obj_mark (vl-catch-all-apply 'vla-AddText
                     (list ms mark_str (vlax-3d-point (list px (+ py label_h) 0.0)) label_h)))
    (if (not (vl-catch-all-error-p obj_mark))
      (progn
        (vla-put-Color obj_mark 4)
        (vla-put-Rotation obj_mark (/ pi 2))
      )
    )
  )
)

;;-----------------------------------------------------------------------------
;; Comando Principal (Multi-PK)
;;-----------------------------------------------------------------------------

(defun c:GRAFICAPTOP (/ ms pk_list sel_pk ent_pk pt_pk obj_pk origin_info invert_dist total_length
                        sel_ref ent_ref obj_ref has_ref detected_label user_input
                        ss pk_data all_results result lst_sorted d_ref
                        z_min z_max z_min_g z_max_g offset datum
                        p_base scaleX scaleY max_width max_height gap
                        n col row p_cur pk_label num_pk)
                       
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  
  ;; 1. Selección MÚLTIPLE de líneas de sección (P.K.)
  (princ "\n=============================================")
  (princ "\nSelecciona las LINEAS P.K. una por una.")
  (princ "\nClica cerca de la parte ALTA (Distancia 0) de cada P.K.")
  (princ "\nPulsa ENTER cuando hayas terminado de seleccionar.")
  (princ "\n=============================================")
  
  (setq pk_list nil)
  (setq sel_pk T) ;; Valor inicial para entrar al loop
  
  (while sel_pk
    (setq sel_pk (entsel (strcat "\nSelecciona P.K. #" (itoa (1+ (length pk_list))) " [Enter=terminar]: ")))
    (if sel_pk
      (progn
        (setq ent_pk (car sel_pk)
              pt_pk (top:as-3d (cadr sel_pk))
              obj_pk (vlax-ename->vla-object ent_pk))
        (setq origin_info (top:get-curve-origin obj_pk pt_pk))
        (if origin_info
          (progn
            ;; Intentar detectar etiqueta P.K. automáticamente
            (setq detected_label (top:find-pk-label obj_pk))
            (if detected_label
              (progn
                (princ (strcat "\n  -> Detectado: " detected_label))
                (setq user_input (getstring (strcat "\n     Confirmar [Enter=ok / escribir otro]: ")))
                (if (= user_input "")
                  (setq pk_label detected_label)
                  (setq pk_label user_input)
                )
              )
              (progn
                (setq user_input (getstring "\n  -> No se detectó etiqueta. Escribe nombre del P.K.: "))
                (if (= user_input "")
                  (setq pk_label (strcat "P.K. #" (itoa (1+ (length pk_list)))))
                  (setq pk_label user_input)
                )
              )
            )
            
            (setq pk_list (cons (list obj_pk
                                     (cadr origin_info)    ;; invert_dist
                                     (caddr origin_info)   ;; total_length
                                     pk_label)             ;; etiqueta
                                pk_list))
            (princ (strcat "\n  -> " pk_label " añadido (Long: " (rtos (caddr origin_info) 2 1) ")"))
          )
          (princ "\n  -> Entidad no válida, ignorada.")
        )
      )
    )
  )
  
  (setq pk_list (reverse pk_list))
  (setq num_pk (length pk_list))
  
  (if (< num_pk 1)
    (progn (princ "\nNo se seleccionó ninguna línea P.K.") (exit))
  )
  (princ (strcat "\n" (itoa num_pk) " línea(s) P.K. seleccionada(s)."))
  
  ;; 2. Selección de la línea de referencia (opcional, compartida)
  (princ "\n=============================================")
  (princ "\nSelecciona la LINEA DE REFERENCIA (compartida para todos los P.K.)")
  (princ "\nPulsa ENTER para omitir.")
  (princ "\n=============================================")
  (setq sel_ref (entsel "\nSelecciona la linea de referencia [Enter=omitir]: "))
  (setq has_ref nil)
  
  (if sel_ref
    (progn
      (setq ent_ref (car sel_ref)
            obj_ref (vlax-ename->vla-object ent_ref))
      (setq has_ref T)
      (princ "\n  -> Línea de referencia seleccionada.")
    )
    (princ "\nSin línea de referencia. El eje se dibujará en Distancia 0.")
  )
  
  ;; 3. Selección de curvas de nivel (compartida)
  (princ "\n=============================================")
  (princ "\nAHORA SELECCIONA LAS CURVAS DE NIVEL (Terreno).")
  (princ "\nHaz una ventana grande que englobe TODAS las líneas P.K.")
  (princ "\n=============================================")
  (setq ss (ssget '((0 . "*POLYLINE,LINE,SPLINE,ARC"))))
  (if (not ss)
    (progn (princ "\nNo se seleccionaron curvas de nivel.") (exit))
  )
  
  ;; 4. Calcular intersecciones para CADA P.K.
  (princ "\n\nCalculando intersecciones...")
  (setq all_results nil)
  (setq z_min_g 1e10)  ;; Mínimo global
  (setq z_max_g -1e10) ;; Máximo global
  (setq max_width 0.0) ;; Ancho máximo de gráfica
  (setq n 0)
  
  (foreach pk_data pk_list
    (setq obj_pk (car pk_data)
          invert_dist (cadr pk_data)
          total_length (caddr pk_data)
          pk_label (cadddr pk_data))
    
    ;; Calcular intersecciones
    (setq lst_sorted (top:calc-intersections ms obj_pk invert_dist total_length ss))
    
    ;; Calcular d_ref para este P.K.
    (if has_ref
      (setq d_ref (top:calc-ref-dist obj_pk obj_ref invert_dist total_length))
      (setq d_ref 0.0)
    )
    
    (if lst_sorted
      (progn
        ;; Actualizar mínimos/máximos globales
        (setq z_min (apply 'min (mapcar 'cadr lst_sorted)))
        (setq z_max (apply 'max (mapcar 'cadr lst_sorted)))
        (if (< z_min z_min_g) (setq z_min_g z_min))
        (if (> z_max z_max_g) (setq z_max_g z_max))
        (if (> total_length max_width) (setq max_width total_length))
        
        ;; Guardar resultado: (lst_sorted d_ref total_length pk_label)
        (setq all_results (cons (list lst_sorted d_ref total_length pk_label) all_results))
        (princ (strcat "\n  " pk_label ": " (itoa (length lst_sorted)) " puntos (Z: " (rtos z_min 2 1) "-" (rtos z_max 2 1) ")"))
      )
      (princ (strcat "\n  " pk_label ": Sin intersecciones válidas, omitido."))
    )
    (setq n (1+ n))
  )
  
  (setq all_results (reverse all_results))
  
  (if (< (length all_results) 1)
    (progn (princ "\nNingún P.K. produjo intersecciones válidas.") (exit))
  )
  
  ;; 5. Inputs Gráficos del Usuario
  (princ (strcat "\n\nRango de elevaciones GLOBAL: Min=" (rtos z_min_g 2 2) " Max=" (rtos z_max_g 2 2)))
  (princ (strcat "\nGráficas a dibujar: " (itoa (length all_results))))
  (setq p_base (getpoint "\nSelecciona el punto de inserción (INFERIOR DERECHA de la primera gráfica): "))
  (if (not p_base) (exit))
  
  (setq scaleX (getreal "\nFactor de Escala Horizontal (X) <1.0>: "))
  (if (not scaleX) (setq scaleX 1.0))
  (setq scaleY (getreal "\nFactor de Escala Vertical (Y) <1.0>: "))
  (if (not scaleY) (setq scaleY 1.0))
  
  ;; 6. Calcular Datum global y dimensiones de layout
  (setq offset (/ (- z_max_g z_min_g) 10.0))
  (if (< offset 1.0) (setq offset 1.0))
  (setq datum (- z_min_g offset))
  
  (setq max_width (* max_width scaleX))
  (setq max_height (+ (* (- z_max_g datum) scaleY) (* offset 2)))
  (setq gap (* max_width 0.15)) ;; 15% de separación entre gráficas
  
  ;; 7. Dibujar gráficas en filas de 4
  (setq n 0)
  (foreach result all_results
    (setq lst_sorted (car result)
          d_ref (cadr result)
          total_length (caddr result))
    (setq pk_label (cadddr result))
    
    ;; Calcular posición en el grid (filas de 4)
    (setq col (rem n 5))
    (setq row (/ n 5))
    
    ;; p_cur = posición inferior-derecha de esta gráfica
    (setq p_cur (list
                  (+ (car p_base) (* col (+ max_width gap)))
                  (- (cadr p_base) (* row (+ max_height gap)))
                  0.0))
    
    ;; Dibujar la gráfica
    (top:draw-one-profile ms lst_sorted p_cur scaleX scaleY datum z_max_g d_ref total_length pk_label)
    
    (setq n (1+ n))
  )
  
  (princ (strcat "\n\nGeneración Completa. " (itoa (length all_results)) " gráfica(s) dibujada(s)."))
  (princ)
)

(princ "\n=============================================")
(princ "\nAplicacion Topografica (Multi-PK) Cargada con Exito.")
(princ "\nEscribe GRAFICAPTOP para crear graficas de perfiles.")
(princ "\n=============================================")
(princ)
