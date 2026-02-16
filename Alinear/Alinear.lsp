;|
  ALINEAR para LusoCAD/AutoCAD
  ----------------------------
  Alinea el punto base/insercion de objetos seleccionados sobre una linea/curva.

  Flujo:
    1) Seleccionar linea o curva de referencia.
    2) Seleccionar columnas/objetos a alinear.
|;

(vl-load-com)

;; Convierte un punto 2D/3D a una lista 3D.
(defun ali:as-3d (pt)
  (cond
    ((and pt (= 2 (length pt))) (list (car pt) (cadr pt) 0.0))
    ((and pt (= 3 (length pt))) pt)
    (T nil)
  )
)

;; Punto medio entre dos puntos 3D.
(defun ali:midpoint (a b)
  (mapcar '(lambda (x y) (/ (+ x y) 2.0)) a b)
)

;; Valida tipos permitidos como referencia.
(defun ali:is-valid-ref-p (ent / typ)
  (setq typ (cdr (assoc 0 (entget ent))))
  (member typ '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE"))
)

;; Verifica formato de punto numerico 2D/3D.
(defun ali:point-p (pt)
  (and (listp pt)
       (>= (length pt) 2)
       (numberp (car pt))
       (numberp (cadr pt))
       (or (= (length pt) 2)
           (numberp (caddr pt)))
  )
)

;; Magnitud de vector 3D.
(defun ali:vec-length (v)
  (sqrt (+ (* (car v) (car v))
           (* (cadr v) (cadr v))
           (* (caddr v) (caddr v))
        )
  )
)

;; Seleccion unica de referencia con filtro de entidades validas.
(defun ali:select-ref (/ pick ent)
  (while (and (not ent)
              (setq pick (entsel "\nLinea/curva de referencia: ")))
    (if (ali:is-valid-ref-p (car pick))
      (setq ent (car pick))
      (princ "\nEntidad no valida. Seleccione LINE/POLYLINE/ARC/SPLINE.")
    )
  )
  ent
)

;; Obtiene el punto base para alinear:
;; - Si existe grupo 10, usa ese punto.
;; - Si no, usa el centro de la caja envolvente como fallback.
(defun ali:get-base-point (ent / elst obj minp maxp)
  (setq elst (entget ent))
  (cond
    ((assoc 10 elst)
     (ali:as-3d (cdr (assoc 10 elst)))
    )
    (T
     (setq obj (vlax-ename->vla-object ent))
     (if (vlax-method-applicable-p obj 'GetBoundingBox)
       (progn
         (vla-GetBoundingBox obj 'minp 'maxp)
         (setq minp (vlax-safearray->list (vlax-variant-value minp))
               maxp (vlax-safearray->list (vlax-variant-value maxp))
         )
         (ali:midpoint minp maxp)
       )
     )
    )
  )
)

;; Proyecta un punto sobre una curva de referencia.
(defun ali:get-nearest-on-curve (curve pt / obj res)
  (setq obj (if (= (type curve) 'ENAME) (vlax-ename->vla-object curve) curve))
  (setq res (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list obj pt)))
  (if (vl-catch-all-error-p res)
    nil
    (cond
      ((listp res) (ali:as-3d res))
      ((= (type res) 'VARIANT) (ali:as-3d (vlax-safearray->list (vlax-variant-value res))))
      ((= (type res) 'SAFEARRAY) (ali:as-3d (vlax-safearray->list res)))
      (T nil)
    )
  )
)

;; Mueve una entidad por un vector. Retorna T si pudo mover.
(defun ali:move-entity-by (ent vec / obj res)
  (setq obj (vlax-ename->vla-object ent))
  (setq res
         (vl-catch-all-apply
           'vla-Move
           (list obj (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point vec))
         )
  )
  (not (vl-catch-all-error-p res))
)

;; Alinea una entidad individual. Retorna T si se mueve.
(defun ali:align-one (ref ent / base target vec)
  (setq base (ali:get-base-point ent)
        target (if base (ali:get-nearest-on-curve ref base))
  )
  (if (and (ali:point-p base) (ali:point-p target))
    (progn
      (setq vec (mapcar '- target base))
      (if (> (ali:vec-length vec) 1e-9)
        (ali:move-entity-by ent vec)
      )
    )
  )
)

(defun ali:run (/ ref ss i ent moved skipped tryres)
  (princ "\nSeleccione linea/curva de referencia.")
  (setq ref (ali:select-ref))

  (if (not ref)
    (progn
      (princ "\nCancelado.")
      (princ)
    )
    (progn
      (princ "\nSeleccione columnas/objetos a alinear y pulse ENTER: ")
      (if (setq ss (ssget))
        (progn
          ;; Nunca mover la linea de referencia aunque se incluya por error.
          (if (ssmemb ref ss)
            (setq ss (ssdel ref ss))
          )

          (command "_.UNDO" "_BEGIN")
          (setq i 0
                moved 0
                skipped 0
          )
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (eq ent ref)
              (setq skipped (1+ skipped))
              (progn
                (setq tryres (vl-catch-all-apply 'ali:align-one (list ref ent)))
                (if (or (vl-catch-all-error-p tryres) (not tryres))
                  (setq skipped (1+ skipped))
                  (setq moved (1+ moved))
                )
              )
            )
            (setq i (1+ i))
          )
          (command "_.UNDO" "_END")
          (princ
            (strcat
              "\nAlineacion completada. Movidos: "
              (itoa moved)
              " | Sin cambios/no validos: "
              (itoa skipped)
              "."
            )
          )
        )
        (princ "\nNo se seleccionaron objetos.")
      )
      (princ)
    )
  )
)

(defun c:ALINEARCOL () (ali:run))
(defun c:ALINEARPIL () (ali:run))

(princ "\nALINEAR cargado. Comandos: ALINEARCOL, ALINEARPIL")
(princ)
