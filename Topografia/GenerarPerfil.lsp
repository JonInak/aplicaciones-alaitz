;;=============================================================================
;; Generador de Perfiles Topográficos
;; Comando principal: GRAFICAPTOP
;;=============================================================================
;; Este script genera una gráfica perfil de terreno dadas una línea de sección
;; (P.K.) y un conjunto de curvas de nivel seleccionadas.
;;=============================================================================

(vl-load-com)

;;-----------------------------------------------------------------------------
;; Funciones de Utilidad
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

;;-----------------------------------------------------------------------------
;; Comando Principal
;;-----------------------------------------------------------------------------

(defun c:GRAFICAPTOP (/ ms sel_pk ent_pk pt_pk obj_pk origin_info p_origen invert_dist total_length
                        sel_ref ent_ref obj_ref ref_ints ref_pt d_ref pt_on_pk_ref
                        ss i obj_curva ints_raw pts pt pt_on_pk d z lst lst_sorted temp_raw
                        p_base scaleX scaleY z_min z_max offset datum pl_pts
                        p_base_end obj_base p_eje_x p_eje_top obj_eje obj_pl text_h obj_txt)
                       
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  
  ;; 1. Seleccion de la línea de sección (P.K.)
  (setq sel_pk (entsel "\nSelecciona la linea de sección (P.K) cerca del lado que representará la COTA ALTA / DISTANCIA 0: "))
  (if (not sel_pk)
    (progn (princ "\nNo se seleccionó ninguna entidad.") (exit))
  )
  
  (setq ent_pk (car sel_pk)
        pt_pk (top:as-3d (cadr sel_pk))
        obj_pk (vlax-ename->vla-object ent_pk))
        
  (setq origin_info (top:get-curve-origin obj_pk pt_pk))
  (if (not origin_info)
    (progn (princ "\nLa entidad seleccionada no es una curva geométrica válida (Linea, Polilinea, Arco...).") (exit))
  )
  
  (setq p_origen (car origin_info)
        invert_dist (cadr origin_info)
        total_length (caddr origin_info))

  ;; 1.5. Selección de la línea de referencia (opcional)
  (princ "\n=============================================")
  (princ "\nSelecciona la LINEA DE REFERENCIA (eje de carretera, muro, etc.)")
  (princ "\nPulsa ENTER para omitir (el eje se dibujará en Distancia 0).")
  (princ "\n=============================================")
  (setq sel_ref (entsel "\nSelecciona la linea de referencia [Enter=omitir]: "))
  (setq d_ref 0.0) ;; Valor por defecto
  
  (if sel_ref
    (progn
      (setq ent_ref (car sel_ref)
            obj_ref (vlax-ename->vla-object ent_ref))
      ;; Calcular donde la referencia interseca la línea P.K.
      (setq ref_ints (top:intersect-2d obj_pk obj_ref))
      (if (and ref_ints (not (vl-catch-all-error-p ref_ints)) (>= (length ref_ints) 3))
        (progn
          ;; Tomar el primer punto de intersección
          (setq ref_pt (list (car ref_ints) (cadr ref_ints) (caddr ref_ints)))
          ;; Calcular la distancia a lo largo del P.K.
          (setq pt_on_pk_ref (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj_pk ref_pt)))
          (if (not (vl-catch-all-error-p pt_on_pk_ref))
            (progn
              (setq d_ref (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list obj_pk pt_on_pk_ref)))
              (if (vl-catch-all-error-p d_ref) (setq d_ref 0.0))
              ;; Invertir si el origen es el end
              (if invert_dist (setq d_ref (- total_length d_ref)))
              (princ (strcat "\nReferencia detectada a distancia: " (rtos d_ref 2 2) " del origen P.K."))
            )
          )
        )
        (princ "\nNo se encontró intersección entre la referencia y el P.K. Se usará Distancia 0.")
      )
    )
    (princ "\nSin línea de referencia. El eje se dibujará en Distancia 0.")
  )

  ;; 2. Selección de curvas de nivel
  (princ "\n=============================================")
  (princ "\nAHORA SELECCIONA LAS CURVAS DE NIVEL (Terreno).")
  (princ "\nPuedes hacer una ventana que cruce la linea morada para seleccionar")
  (princ "\ntodas las líneas marrones/verdes del terreno topográfico.")
  (princ "\n=============================================")
  (setq ss (ssget '((0 . "*POLYLINE,LINE,SPLINE,ARC"))))
  (if (not ss)
    (progn (princ "\nNo se seleccionaron curvas de nivel.") (exit))
  )
  
  ;; 3. Calculo de Intersecciones (Distancia, Elevacion Z)
  (setq lst nil) ;; Lista global de pares (distancia z)
  (setq i 0)
  
  (while (< i (sslength ss))
    (setq obj_curva (vlax-ename->vla-object (ssname ss i)))
    ;; Intersección en 2D para que funcione aunque el P.K. esté a Z=0 y la curva a Z=100
    (setq ints_raw (top:intersect-2d obj_pk obj_curva))
    
    (if (and (not (vl-catch-all-error-p ints_raw)) ints_raw)
      (progn
        ;; ints_raw devuelto por vlax-invoke es directamente una lista plana '(x y z x y z...)
        (setq pts nil)
        (setq temp_raw ints_raw)
        (while (>= (length temp_raw) 3)
          (setq pts (cons (list (car temp_raw) (cadr temp_raw) (caddr temp_raw)) pts))
          (setq temp_raw (cdddr temp_raw))
        )
        (setq pts (reverse pts))

        (foreach pt pts
          ;; Obtenemos la D desde el inicio de la línea de PK
          (setq pt_on_pk (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj_pk pt)))
          (if (not (vl-catch-all-error-p pt_on_pk))
            (progn
              (setq d (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list obj_pk pt_on_pk)))
              (if (not (vl-catch-all-error-p d))
                (progn
                  ;; Si el inicio seleccionado resulta ser el end, entonces dist real es Longitud - dist actual
                  (if invert_dist (setq d (- total_length d)))
                  
                  ;; Obtenemos la cota (Z) de la curva
                  (setq z (top:get-elevation obj_curva pt))
                  
                  ;; Añadir a nuestra lista validando los valores
                  ;; FILTRO: Solo incluir puntos con elevación real (Z > 0)
                  (if (and d z (> z 0.0))
                    (setq lst (cons (list d z) lst))
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
  
  (if (< (length lst) 2)
    (progn (princ "\nNo se encontraron suficientes cortes (intersecciones) para dibujar gráfica.") (exit))
  )
  
  ;; 4. Ordenar y Acotar
  ;; Ordenamos por distancia de menor a mayor
  (setq lst_sorted (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
  
  (setq z_min (apply 'min (mapcar 'cadr lst_sorted)))
  (setq z_max (apply 'max (mapcar 'cadr lst_sorted)))

  ;; 5. Inputs Gráficos del Usuario
  (princ (strcat "\nRango de elevaciones encontrado: Min=" (rtos z_min 2 2) " Max=" (rtos z_max 2 2)))
  (setq p_base (getpoint "\nSelecciona el punto de inserción para la parte INFERIOR DERECHA (Punto 0): "))
  (if (not p_base) (exit))
  
  ;; Preguntar escalas si el dibujante necesita exagerar, por defecto 1 a 1.
  (setq scaleX (getreal "\nFactor de Escala Horizontal (X) <1.0>: "))
  (if (not scaleX) (setq scaleX 1.0))
  (setq scaleY (getreal "\nFactor de Escala Vertical (Y) <1.0>: "))
  (if (not scaleY) (setq scaleY 1.0))
  
  ;; 6. Dibujar Ejes Base
  ;; El origen de coordenadas local es p_base, con X=0 siendo el valor máximo de origen P.K.
  ;; Como el plan indica que se desplaza hacia la IZQUIERDA:
  ;; X = X_base - (Distancia * ScaleX)
  
  ;; Definir un Datum (Base Y) ligeramente por debajo de la mínima Z para margen gráfico.
  (setq offset (/ (- z_max z_min) 10.0)) ; dejamos un 10% de margen
  (if (< offset 1.0) (setq offset 1.0))
  (setq datum (- z_min offset))
  
  ;; Línea base horizontal (Blanca, simulando tierra nivel 0 grafico)
  (setq p_base_end (list (- (car p_base) (* total_length scaleX)) (cadr p_base) 0.0))
  (setq obj_base (vl-catch-all-apply 'vla-AddLine (list ms (vlax-3d-point p_base) (vlax-3d-point p_base_end))))
  (if (not (vl-catch-all-error-p obj_base)) (vla-put-Color obj_base 7))
  
  ;; Línea vertical de Referencia (Naranja) — posicionada en d_ref
  (setq p_eje_x (- (car p_base) (* d_ref scaleX)))
  (setq p_eje_top (list p_eje_x (+ (cadr p_base) (* (- z_max datum) scaleY) offset) 0.0))
  (setq obj_eje (vl-catch-all-apply 'vla-AddLine 
                 (list ms (vlax-3d-point (list p_eje_x (cadr p_base) 0.0)) (vlax-3d-point p_eje_top))))
  (if (not (vl-catch-all-error-p obj_eje)) (vla-put-Color obj_eje 30))
  
  ;; Insertar texto de referencia en el eje
  (setq text_h (* offset 0.5)) ; altura texto
  (setq obj_txt (vl-catch-all-apply 'vla-AddText 
                 (list ms 
                       (if (> d_ref 0.0) "REF" "P.K. 0")
                       (vlax-3d-point (list (+ p_eje_x (* text_h 0.2)) (+ (cadr p_base) text_h) 0.0)) 
                       text_h)))
  (if (not (vl-catch-all-error-p obj_txt)) (vla-put-Color obj_txt 30))

  ;; 7. Dibujar Polilínea de Terreno (Verde)
  (setq pl_pts (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length lst_sorted))))))
  (setq i 0)
  (foreach obj lst_sorted
    ;; obj = (distancia altitud_z)
    (setq d (car obj)
          z (cadr obj))
    (setq py (+ (cadr p_base) (* (- z datum) scaleY)))
    (setq px (- (car p_base) (* d scaleX))) ;; resta porque avanzamos a la izquierda desde el inicio
    
    (vlax-safearray-put-element pl_pts i px)
    (vlax-safearray-put-element pl_pts (1+ i) py)
    (setq i (+ i 2))
  )
  
  (setq obj_pl (vl-catch-all-apply 'vla-AddLightWeightPolyline (list ms pl_pts)))
  (if (not (vl-catch-all-error-p obj_pl))
    (progn
      (vla-put-Color obj_pl 3) ;; Color 3 = VERDE
      (vla-put-ConstantWidth obj_pl (* text_h 0.1)) ;; dar un poco de grosor
    )
    (princ (strcat "\nError dibujando perfil. Intersecciones válidas: " (itoa (length lst_sorted))))
  )

  (princ "\nGeneración Completa. Puntos analizados: ")
  (princ (length lst_sorted))
  (princ)
)

(princ "\n=============================================")
(princ "\nAplicacion Topografica Cargada con Exito.")
(princ "\nEscribe GRAFICAPTOP para crear graficas de perfiles.")
(princ "\n=============================================")
(princ)
