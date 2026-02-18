;; ALINEAR / ESTIRAR columnas sobre referencias
;; - ESTIRARCOL: estira cada INSERT hasta tocar referencia por arriba.
;; - ALINEARCOL: mueve la insercion Y del INSERT hasta la referencia.
(vl-load-com)

;; ------------------------------------------------------------
;; Utilidades base
;; ------------------------------------------------------------
(setq ali:*xdata-app* "ALICOL")

(defun ali:ensure-regapp (app)
  (if (and app (not (tblsearch "APPID" app)))
    (entmake
      (list
        (cons 0 "APPID")
        (cons 2 app)
        (cons 70 0)))
  )
)

(defun ali:set-stretched-flag (ent / app ed rec)
  (setq app ali:*xdata-app*)
  (if (and ent app)
    (progn
      (ali:ensure-regapp app)
      (setq ed (entget ent '("*")))
      ;; Quita xdata previa de esta app para no duplicar marcas.
      (setq ed
             (vl-remove-if
               '(lambda (it)
                  (and (= (car it) -3)
                       (listp (cdr it))
                       (listp (car (cdr it)))
                       (= (car (car (cdr it))) app)))
               ed))
      (setq rec (list -3 (list app (cons 1000 "STRETCHED"))))
      (if (entmod (append ed (list rec)))
        (entupd ent))
    )
  )
)

(defun ali:is-stretched-flag (ent / app xd pack)
  (setq app ali:*xdata-app*)
  (if (and ent app)
    (progn
      (setq xd (assoc -3 (entget ent (list app))))
      (if (and xd
               (listp (cdr xd))
               (listp (car (cdr xd))))
        (progn
          (setq pack (car (cdr xd))) ; ("APP" (1000 . "..."))
          (if (and (listp pack)
                   (= (car pack) app)
                   (= (cdr (assoc 1000 (cdr pack))) "STRETCHED"))
            T))
      )
    )
  )
)

(defun ali:as-3d (pt)
  (cond
    ((and pt (= (length pt) 2)) (list (car pt) (cadr pt) 0.0))
    ((and pt (= (length pt) 3)) pt)
    (T nil)
  )
)

(defun ali:point-p (pt)
  (and (listp pt)
       (>= (length pt) 2)
       (numberp (car pt))
       (numberp (cadr pt)))
)

(defun ali:to-number (v / raw)
  (cond
    ((numberp v) (float v))
    ((= (type v) 'VARIANT)
     (setq raw (vlax-variant-value v))
     (if (numberp raw) (float raw))
    )
  )
)

(defun ali:str-contains-ci (txt needle / ut un)
  (if (and txt needle)
    (progn
      (setq ut (strcase txt)
            un (strcase needle))
      (not (null (vl-string-search un ut)))
    )
  )
)

(defun ali:get-bbox (ent / obj minp maxp r)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-method-applicable-p obj 'GetBoundingBox)
    (progn
      (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minp 'maxp)))
      (if (not (vl-catch-all-error-p r))
        (list
          (vlax-safearray->list (vlax-variant-value minp))
          (vlax-safearray->list (vlax-variant-value maxp))
        )
      )
    )
  )
)

(defun ali:get-top-y (ent / bb)
  (setq bb (ali:get-bbox ent))
  (if bb (cadr (cadr bb)))
)

(defun ali:get-bot-y (ent / bb)
  (setq bb (ali:get-bbox ent))
  (if bb (cadr (car bb)))
)

(defun ali:variant->points (v / raw lst pts)
  (setq raw
         (cond
           ((= (type v) 'VARIANT) (vlax-variant-value v))
           ((= (type v) 'SAFEARRAY) v)
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

;; Lista de Y por interseccion de una vertical X con el INSERT.
(defun ali:get-insert-ys-at-x (ent x / obj bb minp maxp ms ln p1 p2 ip pts ys)
  (setq obj (vlax-ename->vla-object ent))
  (setq bb (ali:get-bbox ent))
  (if bb
    (progn
      (setq minp (car bb)
            maxp (cadr bb))
      (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq p1 (list x (- (cadr minp) 1000.0) 0.0)
            p2 (list x (+ (cadr maxp) 1000.0) 0.0))
      (setq ln
             (vl-catch-all-apply
               'vla-AddLine
               (list ms (vlax-3d-point p1) (vlax-3d-point p2))))
      (if (vl-catch-all-error-p ln)
        nil
        (progn
          (setq ip (vl-catch-all-apply 'vla-IntersectWith (list ln obj 0)))
          (vl-catch-all-apply 'vla-Delete (list ln))
          (if (vl-catch-all-error-p ip)
            nil
            (progn
              (setq pts (ali:variant->points ip))
              (foreach p pts
                (setq ys (cons (cadr p) ys))
              )
              ys
            )
          )
        )
      )
    )
  )
)

;; Rango Y (min max) por interseccion de una vertical con el INSERT.
(defun ali:get-insert-y-range-at-x (ent x / bb ys)
  (setq bb (ali:get-bbox ent))
  (if bb
    (progn
      (setq ys (ali:get-insert-ys-at-x ent x))
      (if ys
        (list (apply 'min ys) (apply 'max ys))
        (list (cadr (car bb)) (cadr (cadr bb)))
      )
    )
  )
)

;; Rango Y util tomando la interseccion mas cercana por arriba/abajo del punto de insercion.
;; Evita coger geometria "parasita" de bloques dinamicos.
(defun ali:get-insert-core-y-range-at-x (ent x yIns / ys up dn tol top bot)
  (setq ys (ali:get-insert-ys-at-x ent x)
        tol 1e-6)
  (if ys
    (progn
      (foreach y ys
        (if (> y (+ yIns tol)) (setq up (cons y up)))
        (if (< y (- yIns tol)) (setq dn (cons y dn)))
      )
      (setq top (if up (apply 'min up) (apply 'max ys))
            bot (if dn (apply 'max dn) (apply 'min ys)))
      (if (> top bot)
        (list bot top)
      )
    )
  )
)

(defun ali:get-insert-top-at-x (ent x / yr)
  (setq yr (ali:get-insert-y-range-at-x ent x))
  (if yr (cadr yr))
)

(defun ali:get-insert-bot-at-x (ent x / yr)
  (setq yr (ali:get-insert-y-range-at-x ent x))
  (if yr (car yr))
)

(defun ali:sort-nums (vals)
  (vl-sort vals '<)
)

(defun ali:median-nums (vals / s n)
  (if vals
    (progn
      (setq s (ali:sort-nums vals)
            n (length s))
      (if (= (rem n 2) 1)
        (nth (/ n 2) s)
        (/ (+ (nth (1- (/ n 2)) s)
              (nth (/ n 2) s))
           2.0)
      )
    )
  )
)

;; Rango Y "efectivo" del INSERT robusto frente a geometria oculta:
;; usa la vecindad del punto de insercion y medianas.
(defun ali:get-effective-y-range (ent / bb ed ins minp maxp minx maxx w x0 y0 xs x yr bots tops fallback)
  (setq bb (ali:get-bbox ent))
  (if bb
    (progn
      (setq ed (entget ent)
            ins (ali:as-3d (cdr (assoc 10 ed))))
      (setq minp (car bb)
            maxp (cadr bb)
            minx (car minp)
            maxx (car maxp)
            w (- maxx minx))
      (setq x0 (if ins (car ins) (/ (+ minx maxx) 2.0))
            y0 (if ins (cadr ins) (/ (+ (cadr minp) (cadr maxp)) 2.0)))
      (setq xs
             (if (> w 1e-6)
               (list (- x0 (* w 0.10))
                     x0
                     (+ x0 (* w 0.10)))
               (list x0)))
      (foreach x xs
        (setq yr (ali:get-insert-core-y-range-at-x ent x y0))
        (if yr
          (progn
            (setq bots (cons (car yr) bots))
            (setq tops (cons (cadr yr) tops))
          )
        )
      )
      (setq fallback (ali:get-insert-y-range-at-x ent x0))
      (if (and bots tops)
        (list (ali:median-nums bots) (ali:median-nums tops))
        (if fallback
          fallback
          (list (cadr minp) (cadr maxp))
        )
      )
    )
  )
)

;; Rango (bot top) de la "pieza vertical principal" respecto a insercion/objetivo.
;; - top: interseccion mas alta por debajo del objetivo (evita geometria por encima).
;; - bot: interseccion mas cercana por debajo de la insercion.
(defun ali:get-insert-range-for-target (ent x yIns yTarget / ys tol belowTarget belowIns top bot)
  (setq ys (ali:get-insert-ys-at-x ent x)
        tol 1e-6)
  (if ys
    (progn
      (foreach y ys
        (if (<= y (+ yTarget tol))
          (setq belowTarget (cons y belowTarget)))
        (if (< y (- yIns tol))
          (setq belowIns (cons y belowIns)))
      )
      (setq top (if belowTarget (apply 'max belowTarget) (apply 'max ys))
            bot (if belowIns (apply 'max belowIns) (apply 'min ys)))
      (if (> top bot)
        (list bot top)
      )
    )
  )
)

;; Altura "real" de bloque segun propiedades (dinamica u objeto).
(defun ali:get-height-value-for-ent (ent / obj d o v)
  (setq obj (vlax-ename->vla-object ent))
  (setq d (ali:get-dyn-height-prop obj))
  (setq v (if d (cadr d)))
  (if (or (not v) (<= v 1e-8))
    (progn
      (setq o (ali:get-obj-height-prop ent))
      (setq v (if o (cadr o)))
    )
  )
  (if (and v (> v 1e-8)) v)
)

(defun ali:move-by (ent vec / obj r)
  (setq obj (vlax-ename->vla-object ent))
  (setq r
         (vl-catch-all-apply
           'vla-Move
           (list obj (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point vec))
         ))
  (not (vl-catch-all-error-p r))
)

(defun ali:get-insert-y (ent / ed ins)
  (setq ed (entget ent)
        ins (cdr (assoc 10 ed)))
  (if ins (cadr ins))
)

(defun ali:lock-insert-y (ent yRef / yNow dy)
  (setq yNow (ali:get-insert-y ent))
  (if (and yNow yRef)
    (progn
      (setq dy (- yRef yNow))
      (if (> (abs dy) 1e-9)
        (ali:move-by ent (list 0.0 dy 0.0))
      )
    )
  )
)

(defun ali:lock-bottom (ent botY / bb newBot dy)
  (setq bb (ali:get-bbox ent))
  (if bb
    (progn
      (setq newBot (cadr (car bb))
            dy (- botY newBot))
      (if (> (abs dy) 1e-9)
        (ali:move-by ent (list 0.0 dy 0.0))
      )
    )
  )
)

;; Rango geometrico (bot/top) consistente con la logica de debug.
(defun ali:get-stretch-range (ent x yIns targetY / yr bb)
  (setq yr (if targetY
             (ali:get-insert-range-for-target ent x yIns targetY)
             nil))
  (if (not yr)
    (setq yr (ali:get-effective-y-range ent)))
  (if yr
    yr
    (progn
      (setq bb (ali:get-bbox ent))
      (if bb
        (list (cadr (car bb)) (cadr (cadr bb)))
      )
    )
  )
)

;; Fija base tras estirado usando rango geometrico (no bbox global).
(defun ali:lock-bottom-stretch (ent x yIns targetY botY / yr newBot dy)
  (setq yr (ali:get-stretch-range ent x yIns nil))
  (setq newBot (if yr (car yr) nil))
  (if (not newBot)
    (setq newBot (ali:get-bot-y ent)))
  (if newBot
    (progn
      (setq dy (- botY newBot))
      (if (> (abs dy) 1e-9)
        (ali:move-by ent (list 0.0 dy 0.0))
      )
    )
  )
)

(defun ali:get-top-stretch (ent x yIns targetY / yr)
  (setq yr (ali:get-stretch-range ent x yIns nil))
  (if yr (cadr yr))
)

;; ------------------------------------------------------------
;; Referencia: Y objetivo por interseccion vertical en X
;; ------------------------------------------------------------
(defun ali:get-nearest-on-curve (curve pt / obj r)
  (setq obj (if (= (type curve) 'ENAME) (vlax-ename->vla-object curve) curve))
  (setq r (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj pt)))
  (if (vl-catch-all-error-p r)
    nil
    (cond
      ((listp r) (ali:as-3d r))
      ((= (type r) 'VARIANT) (ali:as-3d (vlax-safearray->list (vlax-variant-value r))))
      ((= (type r) 'SAFEARRAY) (ali:as-3d (vlax-safearray->list r)))
      (T nil)
    )
  )
)

(defun ali:segment-y-candidates (x p1 p2 / x1 y1 x2 y2 tval tol)
  (setq x1 (car p1)
        y1 (cadr p1)
        x2 (car p2)
        y2 (cadr p2)
        tol 1e-9)
  (cond
    ((< (abs (- x2 x1)) tol)
     (if (< (abs (- x x1)) 1e-6) (list y1 y2))
    )
    ((and (<= (- (min x1 x2) 1e-6) x) (<= x (+ (max x1 x2) 1e-6)))
     (setq tval (/ (- x x1) (- x2 x1)))
     (list (+ y1 (* tval (- y2 y1))))
    )
  )
)

(defun ali:get-lwpoly-verts (el / verts)
  (foreach d el
    (if (= (car d) 10)
      (setq verts (append verts (list (ali:as-3d (cdr d)))))
    )
  )
  verts
)

(defun ali:get-poly-verts (ent / nx ed verts)
  (setq nx (entnext ent))
  (while nx
    (setq ed (entget nx))
    (cond
      ((= (cdr (assoc 0 ed)) "VERTEX")
       (setq verts (append verts (list (ali:as-3d (cdr (assoc 10 ed))))))
       (setq nx (entnext nx))
      )
      ((= (cdr (assoc 0 ed)) "SEQEND")
       (setq nx nil)
      )
      (T
       (setq nx (entnext nx))
      )
    )
  )
  verts
)

(defun ali:ref-ys-at-x-one (ref x / el typ verts closed ys i p1 p2)
  (setq el (entget ref)
        typ (cdr (assoc 0 el)))
  (cond
    ((= typ "LINE")
     (ali:segment-y-candidates
       x
       (ali:as-3d (cdr (assoc 10 el)))
       (ali:as-3d (cdr (assoc 11 el)))
     )
    )
    ((= typ "LWPOLYLINE")
     (setq verts (ali:get-lwpoly-verts el)
           closed (not (zerop (logand 1 (cdr (assoc 70 el)))))
           i 0)
     (repeat (max 0 (1- (length verts)))
       (setq p1 (nth i verts)
             p2 (nth (1+ i) verts)
             ys (append ys (ali:segment-y-candidates x p1 p2))
             i (1+ i))
     )
     (if (and closed (> (length verts) 2))
       (setq ys
              (append ys
                      (ali:segment-y-candidates x (car (last verts)) (car verts))))
     )
     ys
    )
    ((= typ "POLYLINE")
     (setq verts (ali:get-poly-verts ref)
           closed (not (zerop (logand 1 (cdr (assoc 70 el)))))
           i 0)
     (repeat (max 0 (1- (length verts)))
       (setq p1 (nth i verts)
             p2 (nth (1+ i) verts)
             ys (append ys (ali:segment-y-candidates x p1 p2))
             i (1+ i))
     )
     (if (and closed (> (length verts) 2))
       (setq ys
              (append ys
                      (ali:segment-y-candidates x (car (last verts)) (car verts))))
     )
     ys
    )
  )
)

(defun ali:pick-y (ys hintY onlyAbove / tol best bestD d)
  (setq tol 1e-6)
  (if onlyAbove
    (progn
      (foreach y ys
        (if (>= y (- hintY tol))
          (if (or (not best) (< y best))
            (setq best y)
          )
        )
      )
      best
    )
    (progn
      (foreach y ys
        (setq d (abs (- y hintY)))
        (if (or (not bestD) (< d bestD))
          (setq best y
                bestD d)
        )
      )
      best
    )
  )
)

(defun ali:get-target-y (refs x hintY onlyAbove / ys i ref typ cand p)
  (if refs
    (progn
      (setq i 0)
      (repeat (sslength refs)
        (setq ref (ssname refs i)
              typ (cdr (assoc 0 (entget ref)))
              cand (ali:ref-ys-at-x-one ref x))
        (if cand
          (setq ys (append cand ys))
          (if (or (= typ "ARC") (= typ "SPLINE"))
            (progn
              (setq p (ali:get-nearest-on-curve ref (list x hintY 0.0)))
              (if (ali:point-p p)
                (setq ys (cons (cadr p) ys))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if ys
        (ali:pick-y ys hintY onlyAbove)
      )
    )
  )
)

;; ------------------------------------------------------------
;; Diagnostico geometrico (paso 1)
;; ------------------------------------------------------------
(defun ali:pt->str (p)
  (strcat
    "("
    (rtos (car p) 2 3) ", "
    (rtos (cadr p) 2 3) ", "
    (rtos (caddr (ali:as-3d p)) 2 3)
    ")"
  )
)

(defun ali:print-ref-coords-one (ref / el typ p1 p2 verts closed i)
  (setq el (entget ref)
        typ (cdr (assoc 0 el)))
  (princ (strcat "\n--- Referencia " typ " ---"))
  (cond
    ((= typ "LINE")
     (setq p1 (ali:as-3d (cdr (assoc 10 el)))
           p2 (ali:as-3d (cdr (assoc 11 el))))
     (princ (strcat "\n  P1: " (ali:pt->str p1)))
     (princ (strcat "\n  P2: " (ali:pt->str p2)))
    )
    ((= typ "LWPOLYLINE")
     (setq verts (ali:get-lwpoly-verts el)
           closed (not (zerop (logand 1 (cdr (assoc 70 el)))))
           i 0)
     (foreach v verts
       (princ (strcat "\n  V" (itoa i) ": " (ali:pt->str v)))
       (setq i (1+ i))
     )
     (princ
       (strcat
         "\n  Segmentos: "
         (itoa (if closed (length verts) (max 0 (1- (length verts)))))
       )
     )
    )
    ((= typ "POLYLINE")
     (setq verts (ali:get-poly-verts ref)
           closed (not (zerop (logand 1 (cdr (assoc 70 el)))))
           i 0)
     (foreach v verts
       (princ (strcat "\n  V" (itoa i) ": " (ali:pt->str v)))
       (setq i (1+ i))
     )
     (princ
       (strcat
         "\n  Segmentos: "
         (itoa (if closed (length verts) (max 0 (1- (length verts)))))
       )
     )
    )
    ((or (= typ "ARC") (= typ "SPLINE"))
     (setq p1 (vl-catch-all-apply 'vlax-curve-getStartPoint (list ref)))
     (setq p2 (vl-catch-all-apply 'vlax-curve-getEndPoint (list ref)))
     (if (not (vl-catch-all-error-p p1))
       (princ (strcat "\n  Start: " (ali:pt->str p1)))
     )
     (if (not (vl-catch-all-error-p p2))
       (princ (strcat "\n  End:   " (ali:pt->str p2)))
     )
    )
  )
)

(defun ali:add-debug-line (p1 p2 colorIndex / ms ln)
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq ln
         (vl-catch-all-apply
           'vla-AddLine
           (list ms (vlax-3d-point (ali:as-3d p1)) (vlax-3d-point (ali:as-3d p2)))))
  (if (vl-catch-all-error-p ln)
    nil
    (progn
      (if (vlax-property-available-p ln 'Color T)
        (vl-catch-all-apply 'vla-put-Color (list ln (if colorIndex colorIndex 3)))
      )
      T
    )
  )
)

(defun ali:get-target-y-nearest-on-refs (refs probe / i ref p d bestY bestD)
  (if refs
    (progn
      (setq i 0)
      (repeat (sslength refs)
        (setq ref (ssname refs i)
              p (ali:get-nearest-on-curve ref probe))
        (if (ali:point-p p)
          (progn
            (setq d (abs (- (cadr p) (cadr probe))))
            (if (or (not bestD) (< d bestD))
              (setq bestD d
                    bestY (cadr p))
            )
          )
        )
        (setq i (1+ i))
      )
      bestY
    )
  )
)

(defun ali:draw-dbg-ray-one (refs ent / ed typ bb minp maxp yr botY topY hVal xMid yMid ins xProbe yIns targetUp targetAny targetNear targetY col pFrom pTar)
  (setq ed (entget ent)
        typ (cdr (assoc 0 ed)))
  (if (/= typ "INSERT")
    (progn
      (setq ali:*last-reason* "no-es-insert")
      nil
    )
    (progn
      (setq bb (ali:get-bbox ent))
      (if (not bb)
        (progn
          (setq ali:*last-reason* "sin-bbox")
          nil
        )
        (progn
          (setq minp (car bb)
                maxp (cadr bb)
                xMid (/ (+ (car minp) (car maxp)) 2.0)
                yMid (/ (+ (cadr minp) (cadr maxp)) 2.0)
                ins (ali:as-3d (cdr (assoc 10 ed)))
                xProbe (if ins (car ins) xMid)
                yIns (if ins (cadr ins) yMid))
          (setq targetUp (ali:get-target-y refs xProbe yIns T))
          (setq targetAny (if targetUp nil (ali:get-target-y refs xProbe yIns nil)))
          (setq targetNear (if (or targetUp targetAny) nil
                             (ali:get-target-y-nearest-on-refs refs (list xProbe yIns 0.0))))
          (setq targetY (cond (targetUp) (targetAny) (targetNear)))
          (setq col (cond (targetUp 3) (targetAny 1) (targetNear 2)))
          (setq yr (if targetY
                     (ali:get-insert-range-for-target ent xProbe yIns targetY)
                     nil))
          (if (not yr) (setq yr (ali:get-effective-y-range ent)))
          (setq botY (if yr (car yr) (cadr minp)))
          (setq hVal (ali:get-height-value-for-ent ent))
          (setq topY (if hVal
                       (+ botY hVal)
                       (if yr (cadr yr) (cadr maxp))))
          (if (not targetY)
            (progn
              (setq ali:*last-reason*
                     (strcat
                       "sin-target"
                       " x=" (rtos xProbe 2 3)
                       " yIns=" (rtos yIns 2 3)))
              nil
            )
            (progn
              ;; Debug desde el punto de insercion real del bloque.
              (setq pFrom (list xProbe yIns 0.0)
                    pTar (list xProbe targetY 0.0))
              (if (ali:add-debug-line pFrom pTar col)
                (progn
                  (setq ali:*last-reason*
                         (strcat
                           "x=" (rtos xProbe 2 3)
                           " xMid=" (rtos xMid 2 3)
                           " yIns=" (rtos yIns 2 3)
                           " bot=" (rtos botY 2 3)
                           " top=" (rtos topY 2 3)
                           (if hVal (strcat " hVal=" (rtos hVal 2 3)) "")
                           " target=" (rtos targetY 2 3)
                           (cond
                             (targetUp " [up]")
                             (targetAny " [down]")
                             (targetNear " [nearest]"))))
                  T
                )
                (progn
                  (setq ali:*last-reason* "error-dibujando-linea")
                  nil
                )
              )
            )
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; Metodos para modificar altura
;; ------------------------------------------------------------
(defun ali:get-dynprops-list (obj / r raw)
  (setq r (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p r)
    nil
    (cond
      ((listp r) r)
      ((= (type r) 'VARIANT)
       (setq raw (vlax-variant-value r))
       (if (= (type raw) 'SAFEARRAY) (vlax-safearray->list raw))
      )
      ((= (type r) 'SAFEARRAY) (vlax-safearray->list r))
    )
  )
)

(defun ali:get-dyn-height-prop (obj / props p pn pv nv exact partial any)
  (setq props (ali:get-dynprops-list obj))
  (if (not props)
    nil
    (progn
      (foreach p props
        (setq pn (vl-catch-all-apply 'vla-get-PropertyName (list p)))
        (if (not (vl-catch-all-error-p pn))
          (progn
            (setq pv (vl-catch-all-apply 'vlax-get (list p 'Value)))
            (setq nv (if (vl-catch-all-error-p pv) nil (ali:to-number pv)))
            (if nv
              (cond
                ((or (= (strcase pn) "ALTURA")
                     (= (strcase pn) "HEIGHT")
                     (= (strcase pn) "DISTANCE")
                     (= (strcase pn) "DISTANCE1"))
                 (if (not exact)
                   (setq exact (list p nv pn))
                 ))
                ((or (ali:str-contains-ci pn "ALTURA")
                     (ali:str-contains-ci pn "HEIGHT")
                     (ali:str-contains-ci pn "DISTANCE"))
                 (if (not partial)
                   (setq partial (list p nv pn))
                 ))
                (T
                 (if (not any)
                   (setq any (list p nv pn))
                 ))
              )
            )
          )
        )
      )
      (cond (exact) (partial) (any))
    )
  )
)

(defun ali:set-dyn-prop-number (prop val / ro cur typ r)
  (if prop
    (progn
      (setq ro (vl-catch-all-apply 'vlax-get (list prop 'ReadOnly)))
      (if (and (not (vl-catch-all-error-p ro)) (= ro :vlax-true))
        nil
        (progn
          (setq cur (vl-catch-all-apply 'vla-get-Value (list prop)))
          (if (vl-catch-all-error-p cur)
            nil
            (progn
              (setq typ (if (= (type cur) 'VARIANT)
                          (vlax-variant-type cur)
                          (vlax-variant-type (vlax-make-variant cur))))
              (setq r
                     (vl-catch-all-apply
                       'vla-put-Value
                       (list prop (vlax-make-variant val typ))))
              (not (vl-catch-all-error-p r))
            )
          )
        )
      )
    )
  )
)

(defun ali:get-obj-height-prop (ent / names nm v n out)
  (setq names '("Altura" "ALTURA" "Height" "HEIGHT" "Distance" "DISTANCE" "Distance1" "DISTANCE1"))
  (if (fboundp 'getpropertyvalue)
    (while (and names (not out))
      (setq nm (car names))
      (setq v (vl-catch-all-apply 'getpropertyvalue (list ent nm)))
      (setq n (if (vl-catch-all-error-p v) nil (ali:to-number v)))
      (if n
        (setq out (list nm n))
      )
      (setq names (cdr names))
    )
  )
  out
)

(defun ali:set-obj-prop-number (ent pname val / r)
  (if (fboundp 'setpropertyvalue)
    (progn
      (setq r (vl-catch-all-apply 'setpropertyvalue (list ent pname val)))
      (not (vl-catch-all-error-p r))
    )
  )
)

(defun ali:set-yscale (obj val / r)
  (setq r (vl-catch-all-apply 'vla-put-yscalefactor (list obj val)))
  (not (vl-catch-all-error-p r))
)

(defun ali:verify-top (ent targetY tol / top)
  (setq top (ali:get-top-y ent))
  (if (and top (<= (abs (- top targetY)) tol))
    T
  )
)

(defun ali:verify-top-geo (ent x yIns targetY tol / topNow)
  (setq topNow (ali:get-top-stretch ent x yIns targetY))
  (if (and topNow (<= (abs (- topNow targetY)) tol))
    T
  )
)

(defun ali:try-dyn-height (ent obj x yIns delta botY targetY / c prop v0 v1 topNow)
  (setq c (ali:get-dyn-height-prop obj))
  (if c
    (progn
      (setq prop (car c)
            v0 (cadr c))
      (setq v1 (+ v0 delta))
      (if (> v1 1e-6)
        (if (ali:set-dyn-prop-number prop v1)
          (progn
            (ali:lock-insert-y ent yIns)
            (if (not (ali:verify-top-geo ent x yIns targetY 0.05))
              (progn
                ;; segunda pasada correctiva
                (setq topNow (ali:get-top-stretch ent x yIns targetY))
                (if topNow
                  (progn
                    (setq v1 (+ v1 (- targetY topNow)))
                    (if (> v1 1e-6)
                      (if (ali:set-dyn-prop-number prop v1)
                        (ali:lock-insert-y ent yIns)
                      )
                    )
                  )
                )
              )
            )
            (if (ali:verify-top-geo ent x yIns targetY 0.10)
              (progn
                (setq ali:*last-reason* (strcat "ok-dynprop:" (caddr c)))
                T
              )
              (progn
                (setq ali:*last-reason* (strcat "dynprop-no-ajusta:" (caddr c)))
                nil
              )
            )
          )
        )
      )
    )
  )
)

(defun ali:try-obj-height (ent x yIns delta botY targetY / c pname v0 v1 topNow)
  (setq c (ali:get-obj-height-prop ent))
  (if c
    (progn
      (setq pname (car c)
            v0 (cadr c))
      (setq v1 (+ v0 delta))
      (if (> v1 1e-6)
        (if (ali:set-obj-prop-number ent pname v1)
          (progn
            (ali:lock-insert-y ent yIns)
            (if (not (ali:verify-top-geo ent x yIns targetY 0.05))
              (progn
                ;; segunda pasada correctiva
                (setq topNow (ali:get-top-stretch ent x yIns targetY))
                (if topNow
                  (progn
                    (setq v1 (+ v1 (- targetY topNow)))
                    (if (> v1 1e-6)
                      (if (ali:set-obj-prop-number ent pname v1)
                        (ali:lock-insert-y ent yIns)
                      )
                    )
                  )
                )
              )
            )
            (if (ali:verify-top-geo ent x yIns targetY 0.10)
              (progn
                (setq ali:*last-reason* (strcat "ok-objprop:" pname))
                T
              )
              (progn
                (setq ali:*last-reason* (strcat "objprop-no-ajusta:" pname))
                nil
              )
            )
          )
        )
      )
    )
  )
)

(defun ali:try-yscale (ent obj x yIns botY topY targetY / ys sign absS curSpan goalSpan newScale topNow curSpanNow ratio i)
  (setq curSpan (- topY yIns)
        goalSpan (- targetY yIns))
  (if (and (> curSpan 1e-8)
           (> goalSpan (+ curSpan 1e-6))
           (vlax-property-available-p obj 'YScaleFactor T))
    (progn
      (setq ys (vla-get-yscalefactor obj)
            sign (if (< ys 0.0) -1.0 1.0)
            absS (max 1e-6 (abs ys))
            newScale (* sign (* absS (/ goalSpan curSpan))))
      (if (ali:set-yscale obj newScale)
        (progn
          (ali:lock-insert-y ent yIns)
          ;; Pasadas correctivas sobre la misma medida verde (insercion->top).
          (setq i 0)
          (while (< i 3)
            (setq topNow (ali:get-top-stretch ent x yIns targetY))
            (if (or (not topNow) (<= (abs (- targetY topNow)) 0.05))
              (setq i 3)
              (progn
                (setq curSpanNow (- topNow yIns))
                (if (> curSpanNow 1e-8)
                  (progn
                    (setq ys (vla-get-yscalefactor obj)
                          sign (if (< ys 0.0) -1.0 1.0)
                          absS (max 1e-6 (abs ys))
                          ratio (/ goalSpan curSpanNow)
                          newScale (* sign absS ratio))
                    (if (ali:set-yscale obj newScale)
                      (ali:lock-insert-y ent yIns)
                      (setq i 3)
                    )
                  )
                  (setq i 3)
                )
              )
            )
            (setq i (1+ i))
          )
          (if (ali:verify-top-geo ent x yIns targetY 0.08)
            (progn
              (setq ali:*last-reason* "ok-yscale")
              T
            )
            (progn
              (setq topNow (ali:get-top-stretch ent x yIns targetY))
              (setq ali:*last-reason*
                     (if topNow
                       (strcat "yscale-residual=" (rtos (- targetY topNow) 2 3))
                       "yscale-no-ajusta"))
              nil
            )
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; ESTIRARCOL
;; ------------------------------------------------------------
(defun ali:stretch-one (refs ent / ed typ ins x yIns bb yr yrEff botCandidates topCandidates botY topY curSpan goalSpan hVal targetY targetAny delta obj ok minp maxp xMid yMid)
  (setq ali:*last-reason* "sin-detalle")
  (setq ed (entget ent)
        typ (cdr (assoc 0 ed)))
  (if (/= typ "INSERT")
    (progn
      (setq ali:*last-reason* "no-es-insert")
      nil
    )
    (progn
      (setq ins (ali:as-3d (cdr (assoc 10 ed))))
      (setq bb (ali:get-bbox ent))
      (if (not (and (ali:point-p ins) bb))
        (progn
          (setq ali:*last-reason* "sin-ins-o-bbox")
          nil
        )
        (progn
          (setq x (car ins))
          (setq minp (car bb)
                maxp (cadr bb)
                xMid (/ (+ (car minp) (car maxp)) 2.0)
                yMid (/ (+ (cadr minp) (cadr maxp)) 2.0)
                yIns (if ins (cadr ins) yMid))
          (setq targetY (ali:get-target-y refs x yIns T))
          (if (not targetY)
            (progn
              (setq targetAny (ali:get-target-y refs x yIns nil))
              (setq ali:*last-reason*
                     (strcat
                       "sin-target-por-encima"
                       " x=" (rtos x 2 3)
                       " xMid=" (rtos xMid 2 3)
                       " yIns=" (rtos yIns 2 3)
                       (if targetAny
                         (strcat " nearest=" (rtos targetAny 2 3))
                         " nearest=nil")))
              nil
            )
            (progn
              (setq yr (ali:get-insert-range-for-target ent x yIns targetY))
              (setq yrEff (ali:get-effective-y-range ent))
              (if yr
                (progn
                  (setq botCandidates (cons (car yr) botCandidates))
                  (setq topCandidates (cons (cadr yr) topCandidates))
                )
              )
              (if yrEff
                (progn
                  (setq botCandidates (cons (car yrEff) botCandidates))
                  (setq topCandidates (cons (cadr yrEff) topCandidates))
                )
              )
              ;; Bot robusto: el mas alto de candidatos (evita colas por debajo).
              (setq botY (if botCandidates (apply 'max botCandidates) (cadr minp)))
              ;; Top conservador: el mas bajo de candidatos (evita geometria parasita por arriba).
              (setq topY (if topCandidates (apply 'min topCandidates) (cadr maxp)))
              ;; Si existe propiedad de altura fiable, usarla como tope superior de seguridad.
              (setq hVal (ali:get-height-value-for-ent ent))
              (if (and hVal (> hVal 1e-8))
                (setq topY (min topY (+ botY hVal))))
              (if (<= topY botY)
                (setq topY (if yr (cadr yr) (cadr maxp))))
              (setq curSpan (- topY yIns))
              (setq goalSpan (- targetY yIns))
              (if (<= curSpan 1e-8)
                (progn
                  (setq ali:*last-reason* "altura-inicial-invalida")
                  nil
                )
                (progn
              (setq delta (- targetY topY))
              (if (<= goalSpan (+ curSpan 1e-4))
                (progn
                  (setq ali:*last-reason*
                         (strcat
                           "target-no-supera-span"
                           " spanCur=" (rtos curSpan 2 3)
                           " spanGoal=" (rtos goalSpan 2 3)))
                  nil
                )
                (progn
                  (setq obj (vlax-ename->vla-object ent))
                  (setq ok nil)
                  ;; Mantener logica principal por propiedad de altura.
                  ;; YScale solo como ultimo recurso.
                  (if (ali:try-dyn-height ent obj x yIns delta botY targetY)
                    (setq ok T)
                    (if (ali:try-obj-height ent x yIns delta botY targetY)
                      (setq ok T)
                      (if (ali:try-yscale ent obj x yIns botY topY targetY)
                        (setq ok T)
                        (setq ali:*last-reason* "no-se-pudo-ajustar")
                      )
                    )
                  )
                  (if ok
                    (ali:set-stretched-flag ent))
                  ok
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
)

(defun ali:run-stretch (/ refs ss i ent ok skip tryres)
  (princ "\nSeleccione linea(s)/curva(s) de referencia y pulse ENTER: ")
  (setq refs (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if (not refs)
    (progn
      (princ "\nCancelado.")
      (princ)
    )
    (progn
      (princ "\nSeleccione columnas/objetos a estirar y pulse ENTER: ")
      (setq ss (ssget))
      (if (not ss)
        (progn
          (princ "\nNo se seleccionaron objetos.")
          (princ)
        )
        (progn
          (command "_.UNDO" "_BEGIN")
          (setq i 0 ok 0 skip 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq ali:*last-reason* "sin-detalle")
            (setq tryres (vl-catch-all-apply 'ali:stretch-one (list refs ent)))
            (if (or (vl-catch-all-error-p tryres) (not tryres))
              (progn
                (setq skip (1+ skip))
                (princ
                  (strcat
                    "\n  [SKIP "
                    (itoa (1+ i))
                    "] "
                    (if (vl-catch-all-error-p tryres)
                      (strcat "ERR: " (vl-catch-all-error-message tryres))
                      ali:*last-reason*)))
              )
              (progn
                (setq ok (1+ ok))
                (princ
                  (strcat
                    "\n  [OK   "
                    (itoa (1+ i))
                    "] "
                    ali:*last-reason*)))
            )
            (setq i (1+ i))
          )
          (command "_.UNDO" "_END")
          (command "_.REGEN")
          (princ
            (strcat
              "\nEstirado completado. Estirados: "
              (itoa ok)
              " | Sin cambios/no validos: "
              (itoa skip)
              "."))
          (princ)
        )
      )
    )
  )
)

;; Mantiene el top en la misma cota despues de alinear, corrigiendo solo altura.
;; No toca la logica de ESTIRARCOL: se usa solo como ajuste post-movimiento.
(defun ali:preserve-top-after-align (ent yIns topKeep / obj topNow delta c prop pname v i ok)
  (if (and topKeep yIns)
    (progn
      (setq topNow (ali:get-top-y ent))
      (if (and topNow (> (abs (- topKeep topNow)) 1e-6))
        (progn
          (setq obj (vlax-ename->vla-object ent)
                ok nil)
          ;; 1) Propiedad dinamica de altura
          (setq c (ali:get-dyn-height-prop obj))
          (if c
            (progn
              (setq prop (car c)
                    v (cadr c)
                    i 0)
              (while (and (< i 3) (not ok))
                (setq topNow (ali:get-top-y ent)
                      delta (if topNow (- topKeep topNow) 0.0))
                (if (<= (abs delta) 0.10)
                  (setq ok T)
                  (progn
                    (setq v (+ v delta))
                    (if (and (> v 1e-6) (ali:set-dyn-prop-number prop v))
                      (ali:lock-insert-y ent yIns)
                      (setq i 3))
                  )
                )
                (setq i (1+ i))
              )
            )
          )
          ;; 2) Propiedad de objeto (Altura/Height/Distance)
          (if (not ok)
            (progn
              (setq c (ali:get-obj-height-prop ent))
              (if c
                (progn
                  (setq pname (car c)
                        v (cadr c)
                        i 0)
                  (while (and (< i 3) (not ok))
                    (setq topNow (ali:get-top-y ent)
                          delta (if topNow (- topKeep topNow) 0.0))
                    (if (<= (abs delta) 0.10)
                      (setq ok T)
                      (progn
                        (setq v (+ v delta))
                        (if (and (> v 1e-6) (ali:set-obj-prop-number ent pname v))
                          (ali:lock-insert-y ent yIns)
                          (setq i 3))
                      )
                    )
                    (setq i (1+ i))
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

;; ------------------------------------------------------------
;; ALINEARCOL
;; ------------------------------------------------------------
(defun ali:align-one (refs ent / ed typ ins x y targetY dy topBefore)
  (setq ed (entget ent)
        typ (cdr (assoc 0 ed)))
  (if (/= typ "INSERT")
    nil
    (progn
      (setq ins (ali:as-3d (cdr (assoc 10 ed))))
      (if (not (ali:point-p ins))
        nil
        (progn
          (setq x (car ins)
                y (cadr ins))
          (setq topBefore (ali:get-top-y ent))
          ;; para alinear, usamos el punto mas cercano en Y.
          (setq targetY (ali:get-target-y refs x y nil))
          (if targetY
            (progn
              (setq dy (- targetY y))
              (if (> (abs dy) 1e-6)
                (if (ali:move-by ent (list 0.0 dy 0.0))
                  (progn
                    (ali:lock-insert-y ent targetY)
                    (if (ali:is-stretched-flag ent)
                      (ali:preserve-top-after-align ent targetY topBefore))
                    T
                  )
                )
                T
              )
            )
          )
        )
      )
    )
  )
)

(defun ali:run-align (/ refs ss i ent ok skip tryres)
  (princ "\nSeleccione linea(s)/curva(s) de referencia y pulse ENTER: ")
  (setq refs (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if (not refs)
    (progn
      (princ "\nCancelado.")
      (princ)
    )
    (progn
      (princ "\nSeleccione columnas/objetos a alinear y pulse ENTER: ")
      (setq ss (ssget))
      (if (not ss)
        (progn
          (princ "\nNo se seleccionaron objetos.")
          (princ)
        )
        (progn
          (command "_.UNDO" "_BEGIN")
          (setq i 0 ok 0 skip 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq tryres (vl-catch-all-apply 'ali:align-one (list refs ent)))
            (if (or (vl-catch-all-error-p tryres) (not tryres))
              (setq skip (1+ skip))
              (setq ok (1+ ok))
            )
            (setq i (1+ i))
          )
          (command "_.UNDO" "_END")
          (command "_.REGEN")
          (princ
            (strcat
              "\nAlineacion completada. Movidos: "
              (itoa ok)
              " | Sin cambios/no validos: "
              (itoa skip)
              "."))
          (princ)
        )
      )
    )
  )
)

(defun ali:run-refcoords (/ refs i ref)
  (princ "\nSeleccione linea(s)/curva(s) de referencia y pulse ENTER: ")
  (setq refs (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if (not refs)
    (princ "\nCancelado.")
    (progn
      (setq i 0)
      (repeat (sslength refs)
        (setq ref (ssname refs i))
        (ali:print-ref-coords-one ref)
        (setq i (1+ i))
      )
      (princ
        (strcat
          "\nTotal referencias listadas: "
          (itoa (sslength refs))
          "."))
    )
  )
  (princ)
)

(defun ali:run-dbg-rays (/ refs ss i ent ok skip tryres)
  (princ "\nSeleccione linea(s)/curva(s) de referencia y pulse ENTER: ")
  (setq refs (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if (not refs)
    (progn
      (princ "\nCancelado.")
      (princ)
    )
    (progn
      (princ "\nSeleccione columnas/objetos para rayas debug y pulse ENTER: ")
      (setq ss (ssget))
      (if (not ss)
        (progn
          (princ "\nNo se seleccionaron objetos.")
          (princ)
        )
        (progn
          (command "_.UNDO" "_BEGIN")
          (setq i 0 ok 0 skip 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq ali:*last-reason* "sin-detalle")
            (setq tryres (vl-catch-all-apply 'ali:draw-dbg-ray-one (list refs ent)))
            (if (or (vl-catch-all-error-p tryres) (not tryres))
              (progn
                (setq skip (1+ skip))
                (princ
                  (strcat
                    "\n  [SKIP "
                    (itoa (1+ i))
                    "] "
                    (if (vl-catch-all-error-p tryres)
                      (strcat "ERR: " (vl-catch-all-error-message tryres))
                      ali:*last-reason*)))
              )
              (progn
                (setq ok (1+ ok))
                (princ
                  (strcat
                    "\n  [OK   "
                    (itoa (1+ i))
                    "] "
                    ali:*last-reason*)))
            )
            (setq i (1+ i))
          )
          (command "_.UNDO" "_END")
          (command "_.REGEN")
          (princ
            (strcat
              "\nDebug completado. Rayas dibujadas: "
              (itoa ok)
              " | Sin objetivo/error: "
              (itoa skip)
              "."))
          (princ)
        )
      )
    )
  )
)

(defun c:ESTIRARCOL () (ali:run-stretch))
(defun c:ALINEARCOL () (ali:run-align))

(princ "\nALINEAR cargado. Comandos: ALINEARCOL, ESTIRARCOL")
(princ)
