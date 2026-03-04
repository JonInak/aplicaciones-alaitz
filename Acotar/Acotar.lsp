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
(setq acot:*side-max* 60.0)       ;; Long. maxima arista rombo
(setq acot:*cluster-radius* 40.0) ;; Radio agrupacion lineas del mismo rombo
(setq acot:*layer* "z_ACOTADO")   ;; Capa para anotaciones
(setq acot:*dimstyle* "cota100")  ;; Estilo de cota
(setq acot:*color* 1)             ;; Color rojo

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

;;; Comprueba si una LINE es arista diagonal del rombo
(defun acot:is-diamond-edge (ent / ed p1 p2 len ang)
  (setq ed  (entget ent)
        p1  (cdr (assoc 10 ed))
        p2  (cdr (assoc 11 ed))
        len (acot:dist2d p1 p2))
  (if (and (> len acot:*side-min*) (< len acot:*side-max*))
    (progn
      (setq ang (angle (list (car p1) (cadr p1))
                       (list (car p2) (cadr p2))))
      (setq ang (rem (* (/ ang pi) 180.0) 180.0))
      (if (< ang 0) (setq ang (+ ang 180.0)))
      ;; Solo angulos diagonales: 20-70 o 110-160 grados
      (or (and (> ang 20.0) (< ang 70.0))
          (and (> ang 110.0) (< ang 160.0)))
    )
  )
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
(defun acot:find-diamond-lines (pt diamond-lines /
  best-d best-mp ent mp d result)
  (setq best-d 1e99)
  (foreach ent diamond-lines
    (setq mp (apply 'acot:midpt (acot:line-pts ent))
          d  (acot:dist2d pt mp))
    (if (< d best-d)
      (setq best-d d  best-mp mp)
    )
  )
  (if best-mp
    (progn
      (setq result nil)
      (foreach ent diamond-lines
        (setq mp (apply 'acot:midpt (acot:line-pts ent)))
        (if (< (acot:dist2d best-mp mp) acot:*cluster-radius*)
          (setq result (cons ent result))
        )
      )
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
(defun acot:identify-nesw (verts / sy sx)
  (setq sy (vl-sort verts '(lambda (a b) (> (cadr a) (cadr b)))))
  (setq sx (vl-sort verts '(lambda (a b) (> (car a) (car b)))))
  (list (cons "N" (car sy))
        (cons "S" (last sy))
        (cons "E" (car sx))
        (cons "W" (last sx)))
)

;;=================== DIBUJO ===================;;

(defun acot:mk-line (a b ly cl)
  (entmake
    (list '(0 . "LINE") (cons 8 ly) (cons 62 cl)
      (cons 10 (list (car a) (cadr a) 0.0))
      (cons 11 (list (car b) (cadr b) 0.0))))
)

(defun acot:make-dim (pt1 pt2 dim-pt)
  (command "_.DIMALIGNED"
    (list (car pt1) (cadr pt1) 0.0)
    (list (car pt2) (cadr pt2) 0.0)
    (list (car dim-pt) (cadr dim-pt) 0.0))
)

;;=================== ACOTACION PRINCIPAL ===================;;

(defun acot:annotate-pile (diamond-lines lv rv tv /
  verts nesw center side-len scale
  n e s w
  perp-offset vertex-offset dir-to-cen
  n-in e-in s-in w-in
  mid-ne-out mid-ne-in
  dim-off mid-sw mid-se)

  (setq verts (acot:extract-vertices diamond-lines))
  (if (/= (length verts) 4)
    (progn (princ "\nError: no se encontraron 4 vertices.") nil)
    (progn
      (setq nesw (acot:identify-nesw verts))
      (setq n (cdr (assoc "N" nesw))
            e (cdr (assoc "E" nesw))
            s (cdr (assoc "S" nesw))
            w (cdr (assoc "W" nesw)))

      ;; Centro
      (setq center (list
        (/ (+ (car n) (car e) (car s) (car w)) 4.0)
        (/ (+ (cadr n) (cadr e) (cadr s) (cadr w)) 4.0)))

      ;; Lado medio del rombo
      (setq side-len (/ (+ (acot:dist2d n e) (acot:dist2d e s)
                           (acot:dist2d s w) (acot:dist2d w n)) 4.0))

      ;; Factor de escala: cota100 muestra (distancia_dibujo * scale) = valor_real
      (setq scale (/ lv side-len))

      ;; Offset perpendicular a los lados (en unidades de dibujo)
      (setq perp-offset (/ tv scale))

      ;; Para mover vertices: offset_vertice = offset_perpendicular * sqrt(2)
      ;; (geometria de un cuadrado: inset perpendicular d -> vertices se mueven d*sqrt(2))
      (setq vertex-offset (* perp-offset (sqrt 2.0)))

      ;; --- ROMBO INTERIOR ---
      ;; Mover cada vertice hacia el centro por vertex-offset
      (setq dir-to-cen (acot:normalize (list (- (car center) (car n))
                                              (- (cadr center) (cadr n)))))
      (setq n-in (acot:pt-offset n dir-to-cen vertex-offset))

      (setq dir-to-cen (acot:normalize (list (- (car center) (car e))
                                              (- (cadr center) (cadr e)))))
      (setq e-in (acot:pt-offset e dir-to-cen vertex-offset))

      (setq dir-to-cen (acot:normalize (list (- (car center) (car s))
                                              (- (cadr center) (cadr s)))))
      (setq s-in (acot:pt-offset s dir-to-cen vertex-offset))

      (setq dir-to-cen (acot:normalize (list (- (car center) (car w))
                                              (- (cadr center) (cadr w)))))
      (setq w-in (acot:pt-offset w dir-to-cen vertex-offset))

      ;; Dibujar rombo interior (4 lados)
      (acot:mk-line n-in e-in acot:*layer* acot:*color*)
      (acot:mk-line e-in s-in acot:*layer* acot:*color*)
      (acot:mk-line s-in w-in acot:*layer* acot:*color*)
      (acot:mk-line w-in n-in acot:*layer* acot:*color*)

      ;; --- COTAS ALINEADAS ---
      (setq dim-off (* side-len 0.15))

      ;; Cota izquierda: S -> W (muestra left_value, ej: "120")
      (setq mid-sw (acot:midpt s w))
      (acot:make-dim s w
        (acot:pt-offset mid-sw
          (acot:normalize (list (- (car center) (car mid-sw))
                                (- (cadr center) (cadr mid-sw))))
          (- dim-off)))  ;; hacia fuera

      ;; Cota derecha: S -> E (muestra right_value, ej: "120")
      (setq mid-se (acot:midpt s e))
      (acot:make-dim s e
        (acot:pt-offset mid-se
          (acot:normalize (list (- (car center) (car mid-se))
                                (- (cadr center) (cadr mid-se))))
          (- dim-off)))  ;; hacia fuera

      ;; Cota superior: distancia perpendicular entre lado NE exterior e interior
      ;; Medir desde midpoint del lado NE exterior al midpoint del lado NE interior
      ;; Esto da exactamente perp-offset en dibujo -> muestra tv con cota100
      (setq mid-ne-out (acot:midpt n e))
      (setq mid-ne-in  (acot:midpt n-in e-in))
      (acot:make-dim mid-ne-out mid-ne-in
        (acot:pt-offset (acot:midpt mid-ne-out mid-ne-in)
          (acot:normalize (list (- (car center) (car mid-ne-out))
                                (- (cadr center) (cadr mid-ne-out))))
          (- dim-off)))  ;; hacia fuera

      T
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

      ;; Establecer estilo de cota
      (if (tblsearch "DIMSTYLE" acot:*dimstyle*)
        (command "_.DIMSTYLE" "_Restore" acot:*dimstyle*)
        (princ (strcat "\nAVISO: Estilo '" acot:*dimstyle* "' no encontrado."))
      )
      (setvar "CLAYER" acot:*layer*)

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
           (setq tlst (cons ent tlst)))
          ((and (= tp "LINE") (acot:is-diamond-edge ent))
           (setq dlst (cons ent dlst)))
        )
        (setq i (1+ i))
      )

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
           (if vals
             (progn
               (setq tpt (cdr (assoc 10 ed)))
               (setq dlines (acot:find-diamond-lines tpt dlst))
               (if dlines
                 (if (acot:annotate-pile dlines
                       (car vals) (cadr vals) (caddr vals))
                   (setq cnt (1+ cnt))
                 )
                 (princ (strcat "\nNo se encontro rombo para: " tx))
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
