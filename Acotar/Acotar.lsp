;|
  ACOTAR PILARES v2 para LusoCad
  ================================
  Acota automaticamente pilares (PHC/PHR) en cuadros de pilotes.
  Usa DIMENSION ALIGNED con estilo "cota100" y dibuja rombo interior.

  Formato soportado:
    PHC 120.6     -> Circular:    izq=120, der=120, sup=6
    PHR 16.12.6   -> Rectangular: izq=160, der=120, sup=6

  Modo de uso:
    1. Cargar con APPLOAD
    2. Ejecutar comando ACOTAR
    3. Seleccionar los elementos del cuadro (textos PHC/PHR + lineas del rombo)

  Revision: 04/03/2026
|;

(vl-load-com)

;;=================== CONFIGURACION ===================;;

(setq acot:*side-min* 10.0)       ;; Long. minima arista rombo
(setq acot:*side-max* 65.0)       ;; Long. maxima arista rombo
(setq acot:*cluster-radius* 100.0) ;; Radio agrupacion lineas del mismo rombo
(setq acot:*layer* "z_ACOTADO")    ;; Capa para anotaciones
(setq acot:*src-layer* "DIBUJO_DE_ELEMENTOS") ;; Capa de los rombos originales
(setq acot:*dimstyle* "cota100")  ;; Estilo de cota
(setq acot:*color* 1)             ;; Color rojo
(setq acot:*dim-off-ratio* 1.2)   ;; Distancia linea de cota al rombo = ratio * lado_rombo
(setq acot:*logfile* "C:/Users/Jon/Desktop/Jon/PRUEBA/AplicacionesAlaitz/Acotar/acotar_log.txt")

;;=================== LOG ===================;;

(defun acot:log-open ()
  (setq acot:*logfp* (open acot:*logfile* "w"))
  (if acot:*logfp*
    (princ "=== ACOTAR LOG ===\n" acot:*logfp*)
  )
)

(defun acot:log (msg)
  (if acot:*logfp*
    (princ (strcat msg "\n") acot:*logfp*)
  )
)

(defun acot:log-close ()
  (if acot:*logfp*
    (progn (close acot:*logfp*) (setq acot:*logfp* nil))
  )
)

;;=================== UTILIDADES MATEMATICAS ===================;;

(defun acot:dist2d (a b)
  (distance (list (car a) (cadr a)) (list (car b) (cadr b)))
)

(defun acot:midpt (a b)
  (list (/ (+ (car a) (car b)) 2.0)
        (/ (+ (cadr a) (cadr b)) 2.0))
)

;;; Normaliza un vector 2D
(defun acot:normalize (v / len)
  (setq len (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))
  (if (> len 1e-6)
    (list (/ (car v) len) (/ (cadr v) len))
    (list 0.0 0.0)
  )
)

;;; Desplaza un punto en la direccion de un vector por una distancia
(defun acot:pt-offset (pt dir dist)
  (list (+ (car pt) (* (car dir) dist))
        (+ (cadr pt) (* (cadr dir) dist)))
)

;;; Normal interior (perpendicular al lado, apuntando hacia el centro)
;;; Dado un lado A->B y un centro, devuelve la normal unitaria hacia dentro
(defun acot:inward-normal (a b center / dx dy nx ny mp dot)
  (setq dx (- (car b) (car a))
        dy (- (cadr b) (cadr a)))
  ;; Normal perpendicular (dos opciones: (-dy,dx) o (dy,-dx))
  (setq nx (- dy)  ny dx)
  ;; Elegir la que apunta hacia el centro
  (setq mp (acot:midpt a b))
  (setq dot (+ (* nx (- (car center) (car mp)))
               (* ny (- (cadr center) (cadr mp)))))
  (if (< dot 0)
    (setq nx (- nx)  ny (- ny))
  )
  (acot:normalize (list nx ny))
)

;;; Interseccion de dos lineas definidas por punto+direccion
;;; L1: p1 + t*d1, L2: p2 + t*d2
;;; Devuelve punto de interseccion o nil
(defun acot:line-intersect (p1 d1 p2 d2 / cross tp)
  (setq cross (- (* (car d1) (cadr d2)) (* (cadr d1) (car d2))))
  (if (> (abs cross) 1e-10)
    (progn
      (setq tp (/ (- (* (car d2) (- (cadr p1) (cadr p2)))
                     (* (cadr d2) (- (car p1) (car p2))))
                  cross))
      (list (+ (car p1) (* tp (car d1)))
            (+ (cadr p1) (* tp (cadr d1))))
    )
  )
)

;;=================== PARSEO ===================;;

(defun acot:normalize-num (s / p)
  (while (setq p (vl-string-search "," s))
    (setq s (strcat (substr s 1 p) "." (substr s (+ p 2)))))
  s
)

(defun acot:extract-ph-line (txt / u pos sub endp)
  (setq u (strcase txt))
  (if (or (wcmatch u "PHC *") (wcmatch u "PHR *"))
    txt
    (progn
      (setq pos (vl-string-search "PH" u))
      (if pos
        (progn
          (setq sub (substr txt (1+ pos)))
          (setq endp (vl-string-search "\\P" sub))
          (if endp (substr sub 1 endp) sub)
        )
      )
    )
  )
)

(defun acot:parse-phc (txt / s p)
  (setq s (acot:normalize-num (vl-string-trim " " (substr txt 5)))
        p (vl-string-search "." s))
  (if p
    (list (atof (substr s 1 p))
          (atof (substr s 1 p))
          (atof (substr s (+ p 2))))
  )
)

(defun acot:parse-phr (txt / s p1 p2)
  (setq s  (acot:normalize-num (vl-string-trim " " (substr txt 5)))
        p1 (vl-string-search "." s))
  (if p1
    (progn
      (setq p2 (vl-string-search "." s (1+ p1)))
      (if p2
        (list (* 10.0 (atof (substr s 1 p1)))
              (* 10.0 (atof (substr s (+ p1 2) (- p2 p1 1))))
              (atof (substr s (+ p2 2))))
        (list (atof (substr s 1 p1))
              (atof (substr s 1 p1))
              (atof (substr s (+ p1 2))))
      )
    )
  )
)

(defun acot:parse (txt / ph u)
  (setq ph (acot:extract-ph-line txt))
  (if ph
    (progn
      (setq u (strcase ph))
      (cond
        ((wcmatch u "PHC *") (acot:parse-phc ph))
        ((wcmatch u "PHR *") (acot:parse-phr ph))
      )
    )
  )
)

;;=================== DETECCION DEL ROMBO ===================;;

;;; Comprueba si una LINE es arista del rombo (solo por longitud)
;;; El filtro de capa se aplica en el bucle principal
(defun acot:is-diamond-edge (ent / ed p1 p2 len)
  (setq ed  (entget ent)
        p1  (cdr (assoc 10 ed))
        p2  (cdr (assoc 11 ed))
        len (acot:dist2d p1 p2))
  (and (> len acot:*side-min*) (< len acot:*side-max*))
)

;;; Obtiene los 2 endpoints 2D de una LINE
(defun acot:line-pts (ent / ed p1 p2)
  (setq ed (entget ent)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (list (car p1) (cadr p1))
        (list (car p2) (cadr p2)))
)

;;; Busca las 4 lineas del rombo mas cercano al punto dado
;;; Estrategia: encontrar linea mas cercana, estimar centro del rombo
;;; desde sus endpoints, luego agrupar las demas lineas alrededor del centro
(defun acot:find-diamond-lines (pt diamond-lines /
  best-d best-ent ent mp d pts center result)
  ;; Paso 1: encontrar la linea mas cercana al texto
  (setq best-d 1e99)
  (foreach ent diamond-lines
    (setq mp (apply 'acot:midpt (acot:line-pts ent))
          d  (acot:dist2d pt mp))
    (if (< d best-d)
      (setq best-d d  best-ent ent)
    )
  )
  (if best-ent
    (progn
      ;; Paso 2: estimar centro del rombo desde los endpoints de la linea mas cercana
      ;; El centro del rombo esta aprox en el midpoint de la linea
      ;; Pero mejor: recoger TODAS las lineas cercanas iterativamente
      ;; Primero, agrupar con radio generoso desde el midpoint de la mas cercana
      (setq center (apply 'acot:midpt (acot:line-pts best-ent)))
      (setq result nil)
      (foreach ent diamond-lines
        (setq mp (apply 'acot:midpt (acot:line-pts ent)))
        (if (< (acot:dist2d center mp) acot:*cluster-radius*)
          (setq result (cons ent result))
        )
      )
      ;; Si encontramos >= 4, recalcular centro real y filtrar mas fino
      (if (>= (length result) 4)
        (progn
          ;; Calcular centro real: promedio de TODOS los endpoints
          (setq pts nil)
          (foreach ent result
            (setq pts (append (acot:line-pts ent) pts)))
          (setq center (list
            (/ (apply '+ (mapcar 'car pts)) (float (length pts)))
            (/ (apply '+ (mapcar 'cadr pts)) (float (length pts)))))
          ;; Re-agrupar con el centro real
          (setq result nil)
          (foreach ent diamond-lines
            (setq mp (apply 'acot:midpt (acot:line-pts ent)))
            (if (< (acot:dist2d center mp) acot:*cluster-radius*)
              (setq result (cons ent result))
            )
          )
        )
      )
      (acot:log (strcat "  Lineas agrupadas: " (itoa (length result))
                        " (centro=" (rtos (car center) 2 1) "," (rtos (cadr center) 2 1) ")"))
      (if (= (length result) 4) result nil)
    )
  )
)

;;; Extrae 4 vertices unicos de las 4 lineas
(defun acot:extract-vertices (lines / all-pts unique tol pt found)
  (setq tol 0.5  all-pts nil)
  (foreach ent lines
    (setq all-pts (append (acot:line-pts ent) all-pts))
  )
  (setq unique nil)
  (foreach pt all-pts
    (setq found nil)
    (foreach u unique
      (if (< (acot:dist2d pt u) tol) (setq found T))
    )
    (if (not found) (setq unique (cons pt unique)))
  )
  unique
)

;;; Identifica N/E/S/W por coordenadas
;;; Ordena vertices por angulo desde el centro (CCW) para garantizar
;;; que N->E->S->W sigue el perimetro (sin cruces diagonales)
(defun acot:identify-nesw (verts / center sorted n-idx i max-y)
  ;; Centro geometrico
  (setq center (list
    (/ (apply '+ (mapcar 'car verts)) 4.0)
    (/ (apply '+ (mapcar 'cadr verts)) 4.0)))
  ;; Ordenar CCW por angulo desde el centro
  (setq sorted (vl-sort verts
    '(lambda (a b)
      (< (angle center a) (angle center b)))))
  ;; Buscar vertice con mayor Y = Norte
  (setq max-y -1e99  n-idx 0  i 0)
  (foreach v sorted
    (if (> (cadr v) max-y)
      (setq max-y (cadr v)  n-idx i))
    (setq i (1+ i)))
  ;; Desde N en sentido CW: N, E, S, W
  ;; CW = retroceder en CCW: E = (n-1), S = (n-2), W = (n+1)
  (list
    (cons "N" (nth n-idx sorted))
    (cons "E" (nth (rem (+ n-idx 3) 4) sorted))
    (cons "S" (nth (rem (+ n-idx 2) 4) sorted))
    (cons "W" (nth (rem (+ n-idx 1) 4) sorted)))
)

;;=================== DIBUJO ===================;;

(defun acot:mk-line (a b ly cl)
  (entmake
    (list '(0 . "LINE") (cons 8 ly) (cons 62 cl)
      (cons 10 (list (car a) (cadr a) 0.0))
      (cons 11 (list (car b) (cadr b) 0.0))))
)

;;; Crea cota DIMALIGNED con texto override y estilo forzado
;;; Si txt-override es nil, usa la medida real
;;; txt-offset: vector 2D (dx dy) para desplazar el texto, o nil
(defun acot:make-dim (pt1 pt2 dim-pt txt-override txt-offset / ent ed tp70 old-tp new-tp)
  (command "_.DIMALIGNED"
    (list (car pt1) (cadr pt1) 0.0)
    (list (car pt2) (cadr pt2) 0.0)
    (list (car dim-pt) (cadr dim-pt) 0.0))
  ;; Modificar la cota recien creada
  (setq ent (entlast))
  (if ent
    (progn
      (setq ed (entget ent))
      ;; Forzar estilo de cota (DXF group 3) + texto override (group 1)
      (if (tblsearch "DIMSTYLE" acot:*dimstyle*)
        (if (assoc 3 ed)
          (setq ed (subst (cons 3 acot:*dimstyle*) (assoc 3 ed) ed))
          (setq ed (append ed (list (cons 3 acot:*dimstyle*))))
        )
      )
      (if txt-override
        (if (assoc 1 ed)
          (setq ed (subst (cons 1 txt-override) (assoc 1 ed) ed))
          (setq ed (append ed (list (cons 1 txt-override))))
        )
      )
      ;; Desplazar texto si se pide offset
      (if txt-offset
        (progn
          (setq old-tp (cdr (assoc 11 ed)))
          (setq new-tp (list (+ (car old-tp) (car txt-offset))
                             (+ (cadr old-tp) (cadr txt-offset))
                             (if (caddr old-tp) (caddr old-tp) 0.0)))
          (setq ed (subst (cons 11 new-tp) (assoc 11 ed) ed))
          ;; Activar flag "texto posicionado por usuario" (bit 128 del group 70)
          (setq tp70 (cdr (assoc 70 ed)))
          (if (and tp70 (= (logand tp70 128) 0))
            (setq ed (subst (cons 70 (+ tp70 128)) (assoc 70 ed) ed))
          )
        )
      )
      (entmod ed)
      (entupd ent)
    )
  )
)

;;=================== ACOTACION PRINCIPAL ===================;;

(defun acot:annotate-pile (diamond-lines lv rv tv /
  verts nesw center side-len scale
  n e s w
  perp-offset
  n-ne n-es n-sw n-wn
  ne-off es-off sw-off wn-off
  d-ne d-es d-sw d-wn
  n-in e-in s-in w-in
  mid-ne-out mid-ne-in
  dim-off mid-sw mid-se
  lv-txt rv-txt tv-txt)

  (setq verts (acot:extract-vertices diamond-lines))
  (acot:log (strcat "  Vertices: " (itoa (length verts))))
  (if (/= (length verts) 4)
    (progn (princ "\nError: no se encontraron 4 vertices.") nil)
    (progn
      (setq nesw (acot:identify-nesw verts))
      (setq n (cdr (assoc "N" nesw))
            e (cdr (assoc "E" nesw))
            s (cdr (assoc "S" nesw))
            w (cdr (assoc "W" nesw)))
      (acot:log (strcat "  N=(" (rtos (car n) 2 1) "," (rtos (cadr n) 2 1) ")"
                        " E=(" (rtos (car e) 2 1) "," (rtos (cadr e) 2 1) ")"
                        " S=(" (rtos (car s) 2 1) "," (rtos (cadr s) 2 1) ")"
                        " W=(" (rtos (car w) 2 1) "," (rtos (cadr w) 2 1) ")"))

      ;; Centro
      (setq center (list
        (/ (+ (car n) (car e) (car s) (car w)) 4.0)
        (/ (+ (cadr n) (cadr e) (cadr s) (cadr w)) 4.0)))

      ;; Lado medio del rombo
      (setq side-len (/ (+ (acot:dist2d n e) (acot:dist2d e s)
                           (acot:dist2d s w) (acot:dist2d w n)) 4.0))

      ;; Escala: media de lv y rv / lado medio
      (setq scale (/ (+ lv rv) (* 2.0 side-len)))

      ;; Offset perpendicular UNIFORME a todos los lados
      (setq perp-offset (/ tv scale))

      ;; --- ROMBO INTERIOR (inset por lados) ---
      ;; Calcular normal interior de cada lado
      (setq n-ne (acot:inward-normal n e center))  ;; lado NE
      (setq n-es (acot:inward-normal e s center))  ;; lado ES
      (setq n-sw (acot:inward-normal s w center))  ;; lado SW
      (setq n-wn (acot:inward-normal w n center))  ;; lado WN

      ;; Desplazar cada lado perpendicular por perp-offset
      ;; Lado NE desplazado: pasa por (N + normal*offset)
      (setq ne-off (acot:pt-offset n n-ne perp-offset))
      (setq es-off (acot:pt-offset e n-es perp-offset))
      (setq sw-off (acot:pt-offset s n-sw perp-offset))
      (setq wn-off (acot:pt-offset w n-wn perp-offset))

      ;; Direccion de cada lado
      (setq d-ne (list (- (car e) (car n)) (- (cadr e) (cadr n))))
      (setq d-es (list (- (car s) (car e)) (- (cadr s) (cadr e))))
      (setq d-sw (list (- (car w) (car s)) (- (cadr w) (cadr s))))
      (setq d-wn (list (- (car n) (car w)) (- (cadr n) (cadr w))))

      ;; Vertices interiores = interseccion de lados desplazados adyacentes
      (setq n-in (acot:line-intersect wn-off d-wn ne-off d-ne))  ;; WN ∩ NE
      (setq e-in (acot:line-intersect ne-off d-ne es-off d-es))  ;; NE ∩ ES
      (setq s-in (acot:line-intersect es-off d-es sw-off d-sw))  ;; ES ∩ SW
      (setq w-in (acot:line-intersect sw-off d-sw wn-off d-wn))  ;; SW ∩ WN

      ;; Verificar que todas las intersecciones existen
      (if (not (and n-in e-in s-in w-in))
        (progn (princ "\nError: no se pudo calcular rombo interior.") nil)
        (progn

      ;; Dibujar rombo interior (4 lados)
      (acot:mk-line n-in e-in acot:*layer* acot:*color*)
      (acot:mk-line e-in s-in acot:*layer* acot:*color*)
      (acot:mk-line s-in w-in acot:*layer* acot:*color*)
      (acot:mk-line w-in n-in acot:*layer* acot:*color*)

      ;; --- COTAS ALINEADAS (con texto override) ---
      (setq dim-off (* side-len acot:*dim-off-ratio*))

      ;; Formatear valores para texto: entero si no tiene decimales
      (setq lv-txt (if (= lv (fix lv)) (itoa (fix lv)) (rtos lv 2 1)))
      (setq rv-txt (if (= rv (fix rv)) (itoa (fix rv)) (rtos rv 2 1)))
      (setq tv-txt (if (= tv (fix tv)) (itoa (fix tv)) (rtos tv 2 1)))

      ;; Cota izquierda: S -> W (muestra left_value, ej: "120")
      (setq mid-sw (acot:midpt s w))
      (acot:make-dim s w
        (acot:pt-offset mid-sw
          (acot:normalize (list (- (car center) (car mid-sw))
                                (- (cadr center) (cadr mid-sw))))
          (- dim-off))
        lv-txt
        nil)

      ;; Cota derecha: S -> E (muestra right_value, ej: "120")
      (setq mid-se (acot:midpt s e))
      (acot:make-dim s e
        (acot:pt-offset mid-se
          (acot:normalize (list (- (car center) (car mid-se))
                                (- (cadr center) (cadr mid-se))))
          (- dim-off))
        rv-txt
        nil)

      ;; Cota superior: entre midpoints de lado NE exterior e interior
      (setq mid-ne-out (acot:midpt n e))
      (setq mid-ne-in  (acot:midpt n-in e-in))
      (acot:make-dim mid-ne-out mid-ne-in
        (acot:pt-offset (acot:midpt mid-ne-out mid-ne-in)
          (acot:normalize (list (- (car center) (car mid-ne-out))
                                (- (cadr center) (cadr mid-ne-out))))
          (- dim-off))
        tv-txt
        nil)

      T
      )) ;; cierra if interseccion + progn
    )
  )
)

;;=================== COMANDO PRINCIPAL ===================;;

(defun c:ACOTAR (/ ss i n ent ed tp tx vals
                   tlst dlst tpt dlines result
                   old-dimstyle old-clayer old-osm old-cmdecho cnt)

  (princ "\nACOTAR PILARES - Seleccione textos PHC/PHR y lineas del rombo: ")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nNada seleccionado.")
    (progn
      ;; Guardar estado
      (setq old-osm     (getvar "OSMODE")
            old-cmdecho (getvar "CMDECHO")
            old-clayer  (getvar "CLAYER")
            old-dimstyle (getvar "DIMSTYLE"))
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (vla-StartUndoMark
        (vla-get-ActiveDocument (vlax-get-acad-object)))

      ;; Crear capa si no existe
      (if (not (tblsearch "LAYER" acot:*layer*))
        (entmake (list '(0 . "LAYER") (cons 2 acot:*layer*) '(70 . 0) '(62 . 1)))
      )

      ;; Restaurar estilo de cota si existe
      (if (tblsearch "DIMSTYLE" acot:*dimstyle*)
        (command "_.DIMSTYLE" "_Restore" acot:*dimstyle*)
        (princ (strcat "\nAVISO: Estilo '" acot:*dimstyle* "' no encontrado. Copie una cota con ese estilo desde el plano de referencia."))
      )
      (setvar "CLAYER" acot:*layer*)

      ;; Abrir log
      (acot:log-open)
      (acot:log (strcat "Seleccion: " (itoa (sslength ss)) " entidades"))

      ;; Clasificar entidades
      (setq tlst nil  dlst nil  n (sslength ss)  i 0)
      (repeat n
        (setq ent (ssname ss i)
              ed  (entget ent)
              tp  (cdr (assoc 0 ed)))
        (cond
          ((and (member tp '("TEXT" "MTEXT"))
                (setq tx (cdr (assoc 1 ed)))
                (or (vl-string-search "PHC" (strcase tx))
                    (vl-string-search "PHR" (strcase tx))))
           (setq tlst (cons ent tlst))
           (acot:log (strcat "  TEXT ok: \"" tx "\" pos=("
             (rtos (car (cdr (assoc 10 ed))) 2 1) ","
             (rtos (cadr (cdr (assoc 10 ed))) 2 1) ")")))
          ((and (= tp "LINE")
                (= (strcase (cdr (assoc 8 ed))) (strcase acot:*src-layer*))
                (acot:is-diamond-edge ent))
           (setq dlst (cons ent dlst))
           (acot:log (strcat "  LINE ok: long="
             (rtos (acot:dist2d (cdr (assoc 10 ed)) (cdr (assoc 11 ed))) 2 1)
             " ang="
             (rtos (rem (* (/ (angle
               (list (car (cdr (assoc 10 ed))) (cadr (cdr (assoc 10 ed))))
               (list (car (cdr (assoc 11 ed))) (cadr (cdr (assoc 11 ed)))))
               pi) 180.0) 180.0) 2 1)
             " mid=("
             (rtos (/ (+ (car (cdr (assoc 10 ed))) (car (cdr (assoc 11 ed)))) 2.0) 2 1)
             ","
             (rtos (/ (+ (cadr (cdr (assoc 10 ed))) (cadr (cdr (assoc 11 ed)))) 2.0) 2 1)
             ")")))
          ((= tp "LINE")
           (setq ed (entget ent))
           (acot:log (strcat "  LINE rechazada: long="
             (rtos (acot:dist2d (cdr (assoc 10 ed)) (cdr (assoc 11 ed))) 2 1)
             " ang="
             (rtos (rem (* (/ (angle
               (list (car (cdr (assoc 10 ed))) (cadr (cdr (assoc 10 ed))))
               (list (car (cdr (assoc 11 ed))) (cadr (cdr (assoc 11 ed)))))
               pi) 180.0) 180.0) 2 1)
             " capa=" (cdr (assoc 8 ed)))))
          (T
           (acot:log (strcat "  Ignorada: tipo=" tp)))
        )
        (setq i (1+ i))
      )
      (acot:log (strcat "Resumen: " (itoa (length tlst)) " textos, "
                        (itoa (length dlst)) " lineas rombo"))

      (cond
        ((null tlst)
         (princ "\nNo se encontraron textos PHC/PHR."))
        ((null dlst)
         (princ "\nNo se encontraron lineas diagonales del rombo."))
        (T
         (setq cnt 0)
         (foreach tent tlst
           (setq ed   (entget tent)
                 tx   (cdr (assoc 1 ed))
                 vals (acot:parse tx))
           (acot:log (strcat "\n--- Procesando: \"" tx "\" ---"))
           (if vals
             (progn
               (acot:log (strcat "  Parseado: lv=" (rtos (car vals) 2 1)
                                 " rv=" (rtos (cadr vals) 2 1)
                                 " tv=" (rtos (caddr vals) 2 1)))
               (setq tpt (cdr (assoc 10 ed)))
               (acot:log (strcat "  Punto texto: ("
                 (rtos (car tpt) 2 1) "," (rtos (cadr tpt) 2 1) ")"))
               (setq dlines (acot:find-diamond-lines tpt dlst))
               (if dlines
                 (progn
                   (acot:log "  Rombo encontrado -> acotando")
                   (if (acot:annotate-pile dlines
                         (car vals) (cadr vals) (caddr vals))
                     (progn
                       (setq cnt (1+ cnt))
                       (acot:log "  OK acotado"))
                     (acot:log "  ERROR al acotar")
                   )
                 )
                 (progn
                   (acot:log "  FALLO: no se encontro rombo (4 lineas)")
                   (princ (strcat "\nNo se encontro rombo para: " tx))
                 )
               )
             )
             (progn
               (acot:log (strcat "  FALLO: formato no reconocido"))
               (princ (strcat "\nFormato no reconocido: " tx))
             )
           )
         )
         (acot:log (strcat "\nResultado: " (itoa cnt) " pilar(es) acotado(s)"))
         (princ (strcat "\n" (itoa cnt) " pilar(es) acotado(s)."))
        )
      )

      ;; Cerrar log
      (acot:log-close)
      (princ (strcat "\nLog guardado en: " acot:*logfile*))

      ;; Restaurar estado
      (setvar "CLAYER" old-clayer)
      (if (tblsearch "DIMSTYLE" old-dimstyle)
        (command "_.DIMSTYLE" "_Restore" old-dimstyle)
      )
      (vla-EndUndoMark
        (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setvar "CMDECHO" old-cmdecho)
      (setvar "OSMODE" old-osm)
    )
  )
  (princ)
)

(princ "\nACOTAR PILARES v2 cargado. Escriba ACOTAR para ejecutar.")
(princ)
