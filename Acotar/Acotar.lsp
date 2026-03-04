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
    4. El script detecta el rombo, calcula escala y dibuja cotas + rombo interior

  Revision: 04/03/2026
|;

(vl-load-com)

;;=================== CONFIGURACION ===================;;

;; Longitud min/max de las aristas del rombo (en unidades de dibujo)
(setq acot:*side-min* 10.0)
(setq acot:*side-max* 60.0)

;; Radio para agrupar lineas del mismo rombo
(setq acot:*cluster-radius* 40.0)

;; Capa para las anotaciones
(setq acot:*layer* "z_ACOTADO")

;; Estilo de cota a usar
(setq acot:*dimstyle* "cota100")

;; Color rojo para el rombo interior
(setq acot:*color* 1)

;;=================== PARSEO ===================;;

;;; Reemplaza comas por puntos (notacion europea)
(defun acot:normalize-num (s / p)
  (while (setq p (vl-string-search "," s))
    (setq s (strcat (substr s 1 p) "." (substr s (+ p 2)))))
  s
)

;;; Extrae la linea PHC/PHR de un texto (soporta MTEXT con \P)
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

;;; Parsea "PHC 120.6" -> (120.0 120.0 6.0)
(defun acot:parse-phc (txt / s p)
  (setq s (acot:normalize-num (vl-string-trim " " (substr txt 5)))
        p (vl-string-search "." s))
  (if p
    (list
      (atof (substr s 1 p))
      (atof (substr s 1 p))
      (atof (substr s (+ p 2)))
    )
  )
)

;;; Parsea "PHR 16.12.6" -> (160.0 120.0 6.0)
(defun acot:parse-phr (txt / s p1 p2)
  (setq s  (acot:normalize-num (vl-string-trim " " (substr txt 5)))
        p1 (vl-string-search "." s))
  (if p1
    (progn
      (setq p2 (vl-string-search "." s (1+ p1)))
      (if p2
        (list
          (* 10.0 (atof (substr s 1 p1)))
          (* 10.0 (atof (substr s (+ p1 2) (- p2 p1 1))))
          (atof (substr s (+ p2 2)))
        )
        (list
          (atof (substr s 1 p1))
          (atof (substr s 1 p1))
          (atof (substr s (+ p1 2)))
        )
      )
    )
  )
)

;;; Parsea cualquier texto PH*
(defun acot:parse (txt / ph u)
  (setq ph (acot:extract-ph-line txt))
  (if ph
    (progn
      (setq u (strcase ph))
      (cond
        ((wcmatch u "PHC *") (acot:parse-phc ph))
        ((wcmatch u "PHR *") (acot:parse-phr ph))
        (T nil)
      )
    )
  )
)

;;=================== DETECCION DEL ROMBO ===================;;

;;; Comprueba si una LINE es una arista diagonal del rombo
;;; Criterios: longitud entre min/max Y angulo diagonal (~40-50 o ~130-140 grados)
(defun acot:is-diamond-edge (ent / ed p1 p2 len ang norm-ang)
  (setq ed  (entget ent)
        p1  (cdr (assoc 10 ed))
        p2  (cdr (assoc 11 ed))
        len (distance (list (car p1) (cadr p1))
                      (list (car p2) (cadr p2))))
  ;; Filtro por longitud
  (if (and (> len acot:*side-min*) (< len acot:*side-max*))
    (progn
      ;; Calcular angulo y normalizar a 0-180
      (setq ang (angle (list (car p1) (cadr p1))
                       (list (car p2) (cadr p2))))
      (setq ang (* (/ ang pi) 180.0))  ;; radianes a grados
      ;; Normalizar a 0-180
      (if (>= ang 180.0) (setq ang (- ang 180.0)))
      ;; Aceptar angulos diagonales: 20-70 o 110-160 grados
      ;; (evita horizontales 0/180 y verticales 90)
      (or (and (> ang 20.0) (< ang 70.0))
          (and (> ang 110.0) (< ang 160.0)))
    )
  )
)

;;; Calcula el punto medio de una LINE
(defun acot:line-midpoint (ent / ed p1 p2)
  (setq ed (entget ent)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (/ (+ (car p1) (car p2)) 2.0)
        (/ (+ (cadr p1) (cadr p2)) 2.0))
)

;;; Obtiene los 2 endpoints de una LINE como lista 2D
(defun acot:line-endpoints (ent / ed p1 p2)
  (setq ed (entget ent)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (list (car p1) (cadr p1))
        (list (car p2) (cadr p2)))
)

;;; Busca las 4 lineas del rombo mas cercano a un punto
;;; Devuelve lista de 4 entidades LINE o nil
(defun acot:find-diamond-lines (pt diamond-lines /
  best-d best-mp ent mp d result)

  ;; Paso 1: Encontrar la linea diagonal mas cercana al texto
  (setq best-d 1e99)
  (foreach ent diamond-lines
    (setq mp (acot:line-midpoint ent)
          d  (distance (list (car pt) (cadr pt)) mp))
    (if (< d best-d)
      (setq best-d d
            best-mp mp)
    )
  )

  (if (null best-mp)
    nil
    (progn
      ;; Paso 2: Recoger todas las lineas cercanas (mismo rombo)
      (setq result nil)
      (foreach ent diamond-lines
        (setq mp (acot:line-midpoint ent))
        (if (< (distance best-mp mp) acot:*cluster-radius*)
          (setq result (cons ent result))
        )
      )
      ;; Devolver solo si tenemos 4 lineas (un rombo completo)
      (if (= (length result) 4)
        result
        nil
      )
    )
  )
)

;;; Extrae los 4 vertices unicos de 4 lineas del rombo
;;; Devuelve lista de 4 puntos 2D
(defun acot:extract-vertices (lines / all-pts unique-pts pt found tol)
  (setq tol 0.5)  ;; tolerancia para considerar puntos iguales
  ;; Recoger todos los endpoints (8 puntos)
  (setq all-pts nil)
  (foreach ent lines
    (setq all-pts (append (acot:line-endpoints ent) all-pts))
  )
  ;; Eliminar duplicados (cada vertice aparece 2 veces)
  (setq unique-pts nil)
  (foreach pt all-pts
    (setq found nil)
    (foreach upt unique-pts
      (if (< (distance pt upt) tol)
        (setq found T)
      )
    )
    (if (not found)
      (setq unique-pts (cons pt unique-pts))
    )
  )
  unique-pts
)

;;; Identifica vertices N/E/S/W de un rombo (por coordenadas)
;;; Devuelve lista asociativa: ((N . pt) (E . pt) (S . pt) (W . pt))
(defun acot:identify-nesw (vertices / sorted-y sorted-x n s e w)
  ;; N = mayor Y, S = menor Y
  (setq sorted-y (vl-sort vertices
    '(lambda (a b) (> (cadr a) (cadr b)))))
  (setq n (car sorted-y)
        s (last sorted-y))
  ;; E = mayor X, W = menor X (entre los 2 restantes)
  (setq sorted-x (vl-sort vertices
    '(lambda (a b) (> (car a) (car b)))))
  (setq e (car sorted-x)
        w (last sorted-x))
  (list (cons "N" n) (cons "E" e) (cons "S" s) (cons "W" w))
)

;;=================== DIBUJO ===================;;

;;; Crea una linea
(defun acot:mk-line (a b ly cl)
  (entmake
    (list
      '(0 . "LINE")
      (cons 8 ly)
      (cons 62 cl)
      (cons 10 (list (car a) (cadr a) 0.0))
      (cons 11 (list (car b) (cadr b) 0.0))
    )
  )
)

;;; Mueve un punto hacia el centro por una distancia dada
(defun acot:offset-toward-center (pt center offset / dx dy dist nx ny)
  (setq dx (- (car center) (car pt))
        dy (- (cadr center) (cadr pt))
        dist (sqrt (+ (* dx dx) (* dy dy))))
  (if (> dist 0.001)
    (list
      (+ (car pt) (* (/ dx dist) offset))
      (+ (cadr pt) (* (/ dy dist) offset)))
    pt
  )
)

;;; Dibuja el rombo interior (4 lineas)
(defun acot:draw-inner-diamond (nesw center offset ly cl /
  n-in e-in s-in w-in)
  (setq n-in (acot:offset-toward-center (cdr (assoc "N" nesw)) center offset)
        e-in (acot:offset-toward-center (cdr (assoc "E" nesw)) center offset)
        s-in (acot:offset-toward-center (cdr (assoc "S" nesw)) center offset)
        w-in (acot:offset-toward-center (cdr (assoc "W" nesw)) center offset))
  ;; Dibujar 4 lados del rombo interior
  (acot:mk-line n-in e-in ly cl)
  (acot:mk-line e-in s-in ly cl)
  (acot:mk-line s-in w-in ly cl)
  (acot:mk-line w-in n-in ly cl)
  ;; Devolver vertices interiores
  (list (cons "N" n-in) (cons "E" e-in) (cons "S" s-in) (cons "W" w-in))
)

;;; Crea una cota DIMALIGNED entre dos puntos
;;; dim-offset: distancia de la linea de cota respecto a la linea medida
(defun acot:make-dim-aligned (pt1 pt2 dim-offset-pt)
  (command "_.DIMALIGNED"
    (list (car pt1) (cadr pt1) 0.0)
    (list (car pt2) (cadr pt2) 0.0)
    (list (car dim-offset-pt) (cadr dim-offset-pt) 0.0))
)

;;=================== ACOTACION PRINCIPAL ===================;;

;;; Acota un pilar PHC/PHR
;;; diamond-lines: las 4 LINE del rombo
;;; lv, rv, tv: left value, right value, top value (del parseo)
(defun acot:annotate-pile (diamond-lines lv rv tv /
  vertices nesw center side-len scale offset
  n e s w n-in inner-nesw
  dim-offset mid-sw mid-se)

  ;; Extraer vertices y geometria
  (setq vertices (acot:extract-vertices diamond-lines))
  (if (/= (length vertices) 4)
    (progn (princ "\nError: no se encontraron 4 vertices del rombo.") nil)
    (progn
      (setq nesw (acot:identify-nesw vertices))
      (setq n (cdr (assoc "N" nesw))
            e (cdr (assoc "E" nesw))
            s (cdr (assoc "S" nesw))
            w (cdr (assoc "W" nesw)))

      ;; Centro = promedio de 4 vertices
      (setq center (list
        (/ (+ (car n) (car e) (car s) (car w)) 4.0)
        (/ (+ (cadr n) (cadr e) (cadr s) (cadr w)) 4.0)))

      ;; Longitud del lado (promedio)
      (setq side-len (/ (+ (distance s w) (distance s e)
                           (distance n w) (distance n e)) 4.0))

      ;; Factor de escala: valor_cota / lado_dibujo
      (setq scale (/ lv side-len))

      ;; Offset para el rombo interior (en unidades de dibujo)
      (setq offset (/ tv scale))

      ;; --- Dibujar rombo interior ---
      (setq inner-nesw
        (acot:draw-inner-diamond nesw center offset acot:*layer* acot:*color*))

      ;; --- Crear cotas alineadas ---
      ;; Offset de la linea de cota respecto a la arista medida
      (setq dim-offset (* side-len 0.15))

      ;; Cota izquierda: S -> W (arista inferior-izquierda)
      ;; La linea de cota va por fuera (offset hacia abajo-izquierda)
      (setq mid-sw (list
        (/ (+ (car s) (car w)) 2.0)
        (/ (+ (cadr s) (cadr w)) 2.0)))
      (acot:make-dim-aligned s w
        (list (- (car mid-sw) (* dim-offset (cos (/ pi 4.0))))
              (- (cadr mid-sw) (* dim-offset (sin (/ pi 4.0))))))

      ;; Cota derecha: S -> E (arista inferior-derecha)
      (setq mid-se (list
        (/ (+ (car s) (car e)) 2.0)
        (/ (+ (cadr s) (cadr e)) 2.0)))
      (acot:make-dim-aligned s e
        (list (+ (car mid-se) (* dim-offset (cos (/ pi 4.0))))
              (- (cadr mid-se) (* dim-offset (sin (/ pi 4.0))))))

      ;; Cota superior: N exterior -> N interior (gap = top_value)
      (setq n-in (cdr (assoc "N" inner-nesw)))
      (acot:make-dim-aligned n n-in
        (list (- (car n) (* dim-offset (cos (/ pi 4.0))))
              (+ (cadr n) (* dim-offset (sin (/ pi 4.0))))))

      T  ;; exito
    )
  )
)

;;=================== COMANDO PRINCIPAL ===================;;

(defun c:ACOTAR (/ ss i n ent ed tp tx vals
                   tlst dlst tpt
                   dlines result
                   old-dimstyle old-clayer old-osm old-cmdecho
                   cnt)

  (princ "\nACOTAR PILARES v2 - Seleccione textos PHC/PHR y lineas del rombo: ")
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

      ;; Crear capa z_ACOTADO si no existe
      (if (not (tblsearch "LAYER" acot:*layer*))
        (entmake (list '(0 . "LAYER") (cons 2 acot:*layer*) '(70 . 0) '(62 . 7)))
      )

      ;; Establecer estilo de cota y capa
      (if (tblsearch "DIMSTYLE" acot:*dimstyle*)
        (command "_.DIMSTYLE" "_Restore" acot:*dimstyle*)
        (princ (strcat "\nAVISO: Estilo de cota '" acot:*dimstyle* "' no encontrado. Usando el actual."))
      )
      (setvar "CLAYER" acot:*layer*)

      ;; Clasificar entidades
      (setq tlst nil   ;; textos PHC/PHR
            dlst nil   ;; lineas diagonales del rombo
            n    (sslength ss)
            i    0)

      (repeat n
        (setq ent (ssname ss i)
              ed  (entget ent)
              tp  (cdr (assoc 0 ed)))
        (cond
          ;; Texto que contiene PHC o PHR
          ((and (member tp '("TEXT" "MTEXT"))
                (setq tx (cdr (assoc 1 ed)))
                (or (vl-string-search "PHC" (strcase tx))
                    (vl-string-search "PHR" (strcase tx))))
           (setq tlst (cons ent tlst))
          )
          ;; LINE: comprobar si es arista diagonal del rombo
          ((and (= tp "LINE") (acot:is-diamond-edge ent))
           (setq dlst (cons ent dlst))
          )
        )
        (setq i (1+ i))
      )

      (cond
        ((null tlst)
         (princ "\nNo se encontraron textos PHC/PHR en la seleccion."))
        ((null dlst)
         (princ "\nNo se encontraron lineas diagonales del rombo en la seleccion."))
        (T
         (setq cnt 0)

         (foreach tent tlst
           (setq ed   (entget tent)
                 tx   (cdr (assoc 1 ed))
                 vals (acot:parse tx))

           (if vals
             (progn
               (setq tpt (cdr (assoc 10 ed)))

               ;; Buscar las 4 lineas del rombo mas cercano
               (setq dlines (acot:find-diamond-lines tpt dlst))

               (if dlines
                 (progn
                   (setq result
                     (acot:annotate-pile
                       dlines
                       (car vals)     ;; left
                       (cadr vals)    ;; right
                       (caddr vals))) ;; top
                   (if result
                     (setq cnt (1+ cnt))
                   )
                 )
                 (princ (strcat "\nNo se encontro rombo (4 lineas) para: " tx))
               )
             )
             (princ (strcat "\nFormato no reconocido: " tx))
           )
         )

         (princ (strcat "\n" (itoa cnt) " pilar(es) acotado(s)."))
        )
      )

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
