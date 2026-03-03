;|
  ACOTAR PILARES para LusoCad
  ============================
  Acota automaticamente pilares (PHC/PHR) en cuadros de pilotes.
  Dibuja lineas diagonales, cruces en extremos y textos de cota en ROJO.

  Formato soportado:
    PHC 120.6     -> Circular:    izq=120, der=120, sup=6
    PHC 170.8     -> Circular:    izq=170, der=170, sup=8
    PHR 16.12.6   -> Rectangular: izq=160, der=120, sup=6
    PHR 25.15.8   -> Rectangular: izq=250, der=150, sup=8

  Modo de uso:
    1. Cargar con APPLOAD o (load "Acotar.lsp")
    2. Ejecutar comando ACOTAR
    3. Seleccionar los elementos del cuadro (textos PHC/PHR + rombo del pilar)
    4. El script detecta los textos, busca el rombo mas cercano y dibuja

  El rombo del pilar puede ser: LINE (4 lineas), LWPOLYLINE o INSERT.

  Revision: 03/03/2026
|;

(vl-load-com)

;;=================== CONFIGURACION ===================;;

;; Ratio del tamano de las cruces (+) respecto a la altura de texto
(setq acot:*cross-ratio* 0.4)

;; Offset del texto respecto a la linea diagonal (multiplicador de altura)
(setq acot:*text-offset* 1.0)

;; Longitud maxima de LINE para considerarla arista del rombo
(setq acot:*line-max-len* 100.0)

;; Radio para agrupar lineas del mismo rombo
(setq acot:*cluster-radius* 50.0)

;; Color de las anotaciones (1 = rojo)
(setq acot:*color* 1)

;;=================== PARSEO ===================;;

;;; Reemplaza comas por puntos (notacion europea)
(defun acot:normalize-num (s / p)
  (while (setq p (vl-string-search "," s))
    (setq s (strcat (substr s 1 p) "." (substr s (+ p 2)))))
  s
)

;;; Extrae la linea PHC/PHR de un texto (soporta MTEXT con varias lineas)
(defun acot:extract-ph-line (txt / u pos sub endp)
  (setq u (strcase txt))
  ;; Si el texto empieza directamente con PHC/PHR
  (if (or (wcmatch u "PHC *") (wcmatch u "PHR *"))
    txt
    ;; Si no, buscar dentro del texto (MTEXT con \P)
    (progn
      (setq pos (vl-string-search "PH" u))
      (if pos
        (progn
          (setq sub (substr txt (1+ pos)))
          (setq endp (vl-string-search "\\P" sub))
          (if endp
            (substr sub 1 endp)
            sub
          )
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
        ;; PHR A.B.C -> (A*10, B*10, C)
        (list
          (* 10.0 (atof (substr s 1 p1)))
          (* 10.0 (atof (substr s (+ p1 2) (- p2 p1 1))))
          (atof (substr s (+ p2 2)))
        )
        ;; PHR A.B -> simetrico (A, A, B)
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
(defun acot:parse (txt / u ph)
  ;; Primero extraer la linea PHC/PHR (por si es MTEXT)
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

;;=================== BUSQUEDA DEL CENTRO DEL PILAR ===================;;

;;; Calcula el punto medio de una LINE
(defun acot:line-midpoint (ent / ed p1 p2)
  (setq ed (entget ent)
        p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (/ (+ (car p1) (car p2)) 2.0)
        (/ (+ (cadr p1) (cadr p2)) 2.0))
)

;;; Calcula el centro de una LWPOLYLINE (promedio de vertices)
(defun acot:lwpoly-center (ent / ed pts cx cy n)
  (setq ed  (entget ent)
        pts (vl-remove-if-not '(lambda (p) (= (car p) 10)) ed)
        cx  0.0
        cy  0.0
        n   0)
  (foreach p pts
    (setq cx (+ cx (cadr p))
          cy (+ cy (caddr p))
          n  (1+ n)))
  (if (> n 0)
    (list (/ cx n) (/ cy n))
  )
)

;;; Busca el centro del rombo mas cercano a un punto dado.
;;; Usa las lineas cortas (aristas del rombo): encuentra la mas cercana,
;;; luego agrupa todas las lineas del mismo rombo y promedia sus extremos.
(defun acot:find-diamond-center (pt short-lines /
  best-d best-mp ent mp d ed p1 p2 cx cy n)

  ;; Paso 1: Encontrar el midpoint de linea corta mas cercano al texto
  (setq best-d 1e99
        best-mp nil)
  (foreach ent short-lines
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
      ;; Paso 2: Agrupar todas las lineas cercanas al mismo rombo
      ;; y promediar TODOS sus endpoints para obtener el centro exacto
      (setq cx 0.0  cy 0.0  n 0)
      (foreach ent short-lines
        (setq mp (acot:line-midpoint ent))
        (if (< (distance best-mp mp) acot:*cluster-radius*)
          (progn
            (setq ed (entget ent)
                  p1 (cdr (assoc 10 ed))
                  p2 (cdr (assoc 11 ed)))
            (setq cx (+ cx (car p1) (car p2))
                  cy (+ cy (cadr p1) (cadr p2))
                  n  (+ n 2))
          )
        )
      )

      (if (> n 0)
        (list (/ cx n) (/ cy n))
        best-mp
      )
    )
  )
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

;;; Crea una marca de cruz (+) en un punto
(defun acot:mk-cross (pt sz ly cl / h x y)
  (setq h (/ sz 2.0)
        x (car pt)
        y (cadr pt))
  ;; Linea horizontal
  (acot:mk-line (list (- x h) y) (list (+ x h) y) ly cl)
  ;; Linea vertical
  (acot:mk-line (list x (- y h)) (list x (+ y h)) ly cl)
)

;;; Crea un texto centrado y rotado
(defun acot:mk-text (pt s ht rot ly cl)
  (entmake
    (list
      '(0 . "TEXT")
      (cons 8 ly)
      (cons 62 cl)
      (cons 10 (list (car pt) (cadr pt) 0.0))
      (cons 11 (list (car pt) (cadr pt) 0.0))
      (cons 40 ht)
      (cons 50 rot)
      (cons 1 s)
      (cons 72 1)    ;; Centrado horizontal
      (cons 73 2)    ;; Centrado vertical (Middle)
    )
  )
)

;;; Formatea un valor numerico: entero si no tiene decimales
(defun acot:fmt (v)
  (if (equal (- v (fix v)) 0.0 1e-6)
    (itoa (fix v))
    (rtos v 2 1)
  )
)

;;=================== ACOTACION ===================;;

;;; Dibuja la acotacion completa alrededor del centro del pilar.
;;;
;;; Geometria (angulos desde el centro):
;;;   225 (SW) = brazo inferior-izquierdo, longitud = lv (left value)
;;;   315 (SE) = brazo inferior-derecho,   longitud = rv (right value)
;;;    45 (NE) = brazo superior-derecho,   longitud = tv (top value)
;;;   135 (NW) = brazo superior-izquierdo, longitud = tv (top value)
;;;
;;; Se dibujan 2 lineas diagonales cruzando el centro:
;;;   Linea 1: punto SW <-> punto NE
;;;   Linea 2: punto SE <-> punto NW
;;;
;;; Cruces (+) en los 4 extremos.
;;; Textos de cota en el punto medio de cada brazo, con offset perpendicular.

(defun acot:annotate (cen lv rv tv th ly cl
  / ang c45 s45 csz off
    pt-sw pt-se pt-ne pt-nw
    mid-l mid-r mid-t)

  (setq ang (/ pi 4.0)
        c45 (cos ang)
        s45 (sin ang)
        csz (* th acot:*cross-ratio*)
        off (* th acot:*text-offset*))

  ;; Calcular los 4 extremos del diamante
  ;; SW (225): brazo izquierdo
  (setq pt-sw (list
    (- (car cen) (* lv c45))
    (- (cadr cen) (* lv s45))))

  ;; SE (315): brazo derecho
  (setq pt-se (list
    (+ (car cen) (* rv c45))
    (- (cadr cen) (* rv s45))))

  ;; NE (45): brazo superior-derecho
  (setq pt-ne (list
    (+ (car cen) (* tv c45))
    (+ (cadr cen) (* tv s45))))

  ;; NW (135): brazo superior-izquierdo
  (setq pt-nw (list
    (- (car cen) (* tv c45))
    (+ (cadr cen) (* tv s45))))

  ;; --- Dibujar lineas diagonales ---
  (acot:mk-line pt-sw pt-ne ly cl)  ;; SW <-> NE
  (acot:mk-line pt-se pt-nw ly cl)  ;; SE <-> NW

  ;; --- Dibujar cruces en los 4 extremos ---
  (acot:mk-cross pt-sw csz ly cl)
  (acot:mk-cross pt-se csz ly cl)
  (acot:mk-cross pt-ne csz ly cl)
  (acot:mk-cross pt-nw csz ly cl)

  ;; --- Textos de acotacion ---
  ;; Punto medio de cada brazo
  (setq mid-l (list
    (/ (+ (car cen) (car pt-sw)) 2.0)
    (/ (+ (cadr cen) (cadr pt-sw)) 2.0)))

  (setq mid-r (list
    (/ (+ (car cen) (car pt-se)) 2.0)
    (/ (+ (cadr cen) (cadr pt-se)) 2.0)))

  (setq mid-t (list
    (/ (+ (car cen) (car pt-ne)) 2.0)
    (/ (+ (cadr cen) (cadr pt-ne)) 2.0)))

  ;; Texto IZQUIERDO: offset perpendicular hacia arriba-izquierda (135)
  ;; Rotacion 45 para que se lea a lo largo de la diagonal
  (acot:mk-text
    (list
      (- (car mid-l) (* off c45))
      (+ (cadr mid-l) (* off s45)))
    (acot:fmt lv)
    th
    ang    ;; 45 = pi/4
    ly cl)

  ;; Texto DERECHO: offset perpendicular hacia arriba-derecha (45)
  ;; Rotacion -45 (315) para que se lea a lo largo de la diagonal
  (acot:mk-text
    (list
      (+ (car mid-r) (* off c45))
      (+ (cadr mid-r) (* off s45)))
    (acot:fmt rv)
    th
    (- (* 2.0 pi) ang)  ;; 315 = 7*pi/4
    ly cl)

  ;; Texto SUPERIOR: offset perpendicular hacia arriba-izquierda (135)
  ;; Rotacion 45
  (acot:mk-text
    (list
      (- (car mid-t) (* off c45))
      (+ (cadr mid-t) (* off s45)))
    (acot:fmt tv)
    th
    ang    ;; 45 = pi/4
    ly cl)
)

;;=================== COMANDO PRINCIPAL ===================;;

(defun c:ACOTAR (/ ss i n ent ed tp tx vals
                   tlst slst plst tpt cen th
                   ly cl cnt osm p1 p2 len)

  (princ "\nACOTAR PILARES - Seleccione textos PHC/PHR y rombo del pilar: ")
  (setq ss (ssget))

  (if (null ss)
    (princ "\nNada seleccionado.")
    (progn
      ;; Guardar estado
      (setq osm (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      (vla-StartUndoMark
        (vla-get-ActiveDocument (vlax-get-acad-object)))

      ;; Clasificar entidades:
      ;;   tlst = textos PHC/PHR
      ;;   slst = lineas cortas (aristas del rombo)
      ;;   plst = polilíneas/inserts (alternativas para el rombo)
      (setq tlst nil
            slst nil
            plst nil
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
          ;; LINE: si es corta, es candidata a arista del rombo
          ((= tp "LINE")
           (setq p1  (cdr (assoc 10 ed))
                 p2  (cdr (assoc 11 ed))
                 len (distance (list (car p1) (cadr p1))
                               (list (car p2) (cadr p2))))
           (if (< len acot:*line-max-len*)
             (setq slst (cons ent slst))
           )
          )
          ;; LWPOLYLINE: candidata si es pequena
          ((= tp "LWPOLYLINE")
           (setq plst (cons ent plst))
          )
          ;; INSERT: candidata
          ((= tp "INSERT")
           (setq plst (cons ent plst))
          )
        )
        (setq i (1+ i))
      )

      (cond
        ((null tlst)
         (princ "\nNo se encontraron textos PHC/PHR en la seleccion."))
        ((and (null slst) (null plst))
         (princ "\nNo se encontraron rombos de pilar (LINE/LWPOLYLINE/INSERT) en la seleccion."))
        (T
         (setq cnt 0
               cl  acot:*color*)  ;; Rojo

         (foreach tent tlst
           (setq ed   (entget tent)
                 tx   (cdr (assoc 1 ed))
                 vals (acot:parse tx))

           (if vals
             (progn
               ;; Usar la capa del texto original para las anotaciones
               (setq ly  (cdr (assoc 8 ed))
                     tpt (cdr (assoc 10 ed))
                     th  (cdr (assoc 40 ed)))

               ;; Buscar centro del rombo
               (setq cen nil)

               ;; Primero buscar en lineas cortas (LINE)
               (if slst
                 (setq cen (acot:find-diamond-center tpt slst))
               )

               ;; Si no se encontro, buscar en LWPOLYLINE/INSERT
               (if (and (null cen) plst)
                 (progn
                   (setq best-d 1e99)
                   (foreach pent plst
                     (setq ed2 (entget pent)
                           tp2 (cdr (assoc 0 ed2)))
                     (cond
                       ((= tp2 "LWPOLYLINE")
                        (setq ip (acot:lwpoly-center pent)))
                       ((= tp2 "INSERT")
                        (setq ip (cdr (assoc 10 ed2))))
                       (T (setq ip nil))
                     )
                     (if ip
                       (progn
                         (setq d (distance
                                   (list (car tpt) (cadr tpt))
                                   (list (car ip) (cadr ip))))
                         (if (< d best-d)
                           (setq best-d d
                                 cen    ip)
                         )
                       )
                     )
                   )
                 )
               )

               (if cen
                 (progn
                   (acot:annotate
                     cen
                     (car vals)    ;; left
                     (cadr vals)   ;; right
                     (caddr vals)  ;; top
                     th ly cl)
                   (setq cnt (1+ cnt))
                 )
                 (princ (strcat "\nSin rombo cercano para: " tx))
               )
             )
             (princ (strcat "\nFormato no reconocido: " tx))
           )
         )

         (princ (strcat "\n" (itoa cnt) " pilar(es) acotado(s)."))
        )
      )

      ;; Restaurar estado
      (vla-EndUndoMark
        (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setvar "OSMODE" osm)
    )
  )
  (princ)
)

(princ "\nACOTAR PILARES cargado. Escriba ACOTAR para ejecutar.")
(princ)
