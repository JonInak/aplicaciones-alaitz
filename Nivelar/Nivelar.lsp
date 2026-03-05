;|
  NIVELAR - Herramienta de Nivelacion para LusoCad
  =================================================
  Actualiza automaticamente cotas de nivel al mover marcadores
  en base a una escala de referencia.

  Modo de uso:
    1. Cargar con APPLOAD
    2. Ejecutar comando NIVELAR
    3. Primera vez: calibrar haciendo clic en dos lineas de nivel conocidas
    4. Seleccionar marcadores de nivel, moverlos, el valor se recalcula

  Comandos:
    NIVELAR  - Comando principal (calibrar + mover marcadores)
    NIVRESET - Resetear escala para recalibrar

  Revision: 05/03/2026
|;

(vl-load-com)

;;=================== CONFIGURACION ===================;;

(setq niv:*mark-layer* "z_COTAS NIVELES")  ;; Capa de los marcadores
(setq niv:*decimals* 2)                     ;; Decimales en la cota

;;=================== UTILIDADES ===================;;

;;; Parsea texto de nivel "+49.98" o "+49,98" -> 49.98
(defun niv:parse-level (txt / clean num neg)
  (if (or (null txt) (< (strlen txt) 2))
    nil
    (progn
      (setq neg (= (substr txt 1 1) "-"))
      (setq clean (vl-string-translate "," "." txt))
      (if (or (= (substr clean 1 1) "+") (= (substr clean 1 1) "-"))
        (setq clean (substr clean 2))
      )
      (setq num (atof clean))
      (if (or (/= num 0.0) (wcmatch clean "0*"))
        (if neg (- num) num)
        nil
      )
    )
  )
)

;;; Formatea nivel con signo, respetando separador decimal del original
(defun niv:format-level (val orig-txt / result)
  (setq result
    (if (>= val 0.0)
      (strcat "+" (rtos val 2 niv:*decimals*))
      (rtos val 2 niv:*decimals*)
    )
  )
  ;; Si el original usaba coma, convertir punto a coma
  (if (and orig-txt (vl-string-search "," orig-txt))
    (vl-string-translate "." "," result)
    result
  )
)

;;; Comprueba si un texto es un valor de nivel ([+-]digitos[.,]digitos)
(defun niv:level-text-p (txt)
  (and txt
       (> (strlen txt) 3)
       (or (= (substr txt 1 1) "+") (= (substr txt 1 1) "-"))
       (wcmatch (substr txt 2 1) "#")
       (niv:parse-level txt)
  )
)

;;; Obtiene cadena de texto de una entidad TEXT o MTEXT
(defun niv:get-text-str (ent / ed etype)
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (if (or (= etype "TEXT") (= etype "MTEXT"))
    (cdr (assoc 1 ed))
    nil
  )
)

;;; Busca el atributo "COTA" en un bloque INSERT y devuelve (attrib-ename . valor)
(defun niv:get-block-cota (ent / ed etype sub sub-ed tag val result)
  (setq ed (entget ent))
  (setq etype (cdr (assoc 0 ed)))
  (if (and (= etype "INSERT") (= (cdr (assoc 66 ed)) 1)) ;; bloque con atributos
    (progn
      (setq sub (entnext ent))
      (while sub
        (setq sub-ed (entget sub))
        (if (= (cdr (assoc 0 sub-ed)) "ATTRIB")
          (progn
            (setq tag (cdr (assoc 2 sub-ed)))
            (setq val (cdr (assoc 1 sub-ed)))
            (if (= (strcase tag) "COTA")
              (setq result (cons sub val))
            )
          )
          (setq sub nil) ;; fin de atributos (SEQEND)
        )
        (if sub (setq sub (entnext sub)))
      )
      result
    )
    nil
  )
)

;;; Separa MTEXT por saltos de linea (\P)
(defun niv:split-mtext (txt / result pos)
  (setq result '())
  (while (setq pos (vl-string-search "\\P" txt))
    (setq result (append result (list (substr txt 1 pos))))
    (setq txt (substr txt (+ pos 3)))
  )
  (setq result (append result (list txt)))
  result
)

;;; Busca valor de nivel dentro de un MTEXT (devuelve la subcadena o nil)
(defun niv:extract-mtext-level (txt / lines found)
  (setq lines (niv:split-mtext txt))
  (setq found nil)
  (foreach line lines
    (if (niv:level-text-p (vl-string-trim " " line))
      (setq found (vl-string-trim " " line))
    )
  )
  found
)

;;; Reemplaza una subcadena en un string
(defun niv:str-replace (txt old-str new-str / pos)
  (setq pos (vl-string-search old-str txt))
  (if pos
    (strcat (substr txt 1 pos)
            new-str
            (substr txt (+ pos (strlen old-str) 1)))
    txt
  )
)

;;=================== ESCALA ===================;;

;;; Calibra la escala pidiendo al usuario que haga clic en dos lineas de nivel
(defun niv:build-scale (/ pt1 elev1 pt2 elev2 dy)
  (princ "\n--- CALIBRACION DE ESCALA ---")
  (princ "\nHaga clic en dos lineas de nivel conocidas de la escala.")

  ;; Primer punto
  (setq pt1 (getpoint "\nClic en la linea del primer nivel (ej: sobre la raya del +43): "))
  (if (null pt1) (progn (princ "\nCancelado.") nil)
    (progn
      (setq elev1 (getreal "\nValor de ese nivel (ej: 43): "))
      (if (null elev1) (progn (princ "\nCancelado.") nil)
        (progn
          ;; Segundo punto
          (setq pt2 (getpoint "\nClic en la linea del segundo nivel (ej: sobre la raya del +44): "))
          (if (null pt2) (progn (princ "\nCancelado.") nil)
            (progn
              (setq elev2 (getreal "\nValor de ese nivel (ej: 44): "))
              (if (null elev2) (progn (princ "\nCancelado.") nil)
                (progn
                  (setq dy (- (cadr pt2) (cadr pt1)))
                  (if (< (abs dy) 1e-6)
                    (progn (princ "\nError: Los dos puntos tienen la misma Y.") nil)
                    (progn
                      (setq niv:*elev-per-y* (/ (- elev2 elev1) dy))
                      (setq niv:*scale*
                        (list (cons elev1 (cadr pt1))
                              (cons elev2 (cadr pt2))))
                      (princ (strcat "\nEscala calibrada: "
                                     (niv:format-level elev1 nil) " a "
                                     (niv:format-level elev2 nil)
                                     " | Factor: " (rtos niv:*elev-per-y* 2 6) " m/u"))
                      T
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

;;; Calcula elevacion desde coordenada Y usando la escala calibrada
(defun niv:y-to-elev (y / p1)
  (if (and niv:*scale* niv:*elev-per-y*)
    (progn
      (setq p1 (car niv:*scale*))
      (+ (car p1) (* (- y (cdr p1)) niv:*elev-per-y*))
    )
    nil
  )
)

;;=================== COMANDO PRINCIPAL ===================;;

(defun c:NIVELAR (/ ss i ent ed txt etype block-cota
                    level-ent level-type old-level-str
                    base-pt dest-pt new-elev new-txt
                    continue)

  ;; Fase 1: Calibrar escala si no existe
  (if (or (null niv:*scale*) (< (length niv:*scale*) 2))
    (if (null (niv:build-scale))
      (progn (princ "\nAbortado.") (exit))
    )
    (princ (strcat "\nEscala activa: "
                   (niv:format-level (caar niv:*scale*) nil) " a "
                   (niv:format-level (car (last niv:*scale*)) nil)))
  )

  ;; Marca UNDO para poder deshacer todo de golpe
  (command "_.UNDO" "_Begin")

  ;; Fase 2: Bucle interactivo de movimiento
  (setq continue T)
  (while continue
    (princ "\nSeleccione marcador a mover [Enter=salir]: ")
    (setq ss (ssget))

    (if (null ss)
      (setq continue nil)
      (progn
        ;; Buscar texto de nivel en la seleccion (TEXT, MTEXT o atributo de bloque)
        (setq level-ent nil)
        (setq level-type nil)
        (setq old-level-str nil)
        (setq i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq ed (entget ent))
          (setq etype (cdr (assoc 0 ed)))
          (cond
            ;; TEXT simple con valor de nivel
            ((and (= etype "TEXT")
                  (niv:level-text-p (cdr (assoc 1 ed))))
             (setq level-ent ent)
             (setq level-type "TEXT")
             (setq old-level-str (cdr (assoc 1 ed)))
            )
            ;; MTEXT que contiene valor de nivel
            ((and (= etype "MTEXT")
                  (niv:extract-mtext-level (cdr (assoc 1 ed))))
             (setq level-ent ent)
             (setq level-type "MTEXT")
             (setq old-level-str (niv:extract-mtext-level (cdr (assoc 1 ed))))
            )
            ;; Bloque INSERT con atributo COTA
            ((= etype "INSERT")
             (setq block-cota (niv:get-block-cota ent))
             (if (and block-cota (niv:level-text-p (cdr block-cota)))
               (progn
                 (setq level-ent (car block-cota))  ;; ename del ATTRIB
                 (setq level-type "ATTRIB")
                 (setq old-level-str (cdr block-cota))
               )
             )
            )
          )
          (setq i (1+ i))
        )

        ;; Pedir puntos de movimiento
        (setq base-pt (getpoint "\nPunto base (linea de la punta del triangulo): "))
        (if base-pt
          (progn
            (setq dest-pt (getpoint base-pt "\nNuevo punto (donde va el nivel): "))
            (if dest-pt
              (progn
                ;; Mover entidades
                (command "_.MOVE" ss "" base-pt dest-pt)

                ;; Calcular nivel desde posicion Y absoluta del destino
                (if level-ent
                  (progn
                    (setq new-elev (niv:y-to-elev (cadr dest-pt)))
                    (setq new-txt (niv:format-level new-elev old-level-str))

                    ;; Actualizar la entidad
                    (setq ed (entget level-ent))
                    (cond
                      ((= level-type "TEXT")
                       (setq ed (subst (cons 1 new-txt) (assoc 1 ed) ed))
                      )
                      ((= level-type "MTEXT")
                       (setq ed (subst
                         (cons 1 (niv:str-replace (cdr (assoc 1 ed))
                                                  old-level-str
                                                  new-txt))
                         (assoc 1 ed) ed))
                      )
                      ((= level-type "ATTRIB")
                       (setq ed (subst (cons 1 new-txt) (assoc 1 ed) ed))
                      )
                    )
                    (entmod ed)
                    (entupd level-ent)

                    (princ (strcat "\n  Nivel: " old-level-str " -> " new-txt))
                  )
                  (princ "\nAviso: No se encontro texto de nivel en la seleccion.")
                )
              )
            )
          )
        )
      )
    )
  )

  (command "_.UNDO" "_End")
  (princ "\nNivelar finalizado.")
  (princ)
)

;;=================== NIVELAR TODOS ===================;;

;;; Comando NIVTODOS: actualiza el nivel de todos los bloques seleccionados
;;; segun su posicion Y actual (sin moverlos)
(defun c:NIVTODOS (/ ss i ent ed etype block-cota
                     attrib-ent old-txt old-elev new-elev new-txt
                     ins-pt count)

  ;; Verificar escala
  (if (or (null niv:*scale*) (< (length niv:*scale*) 2))
    (progn
      (princ "\nNo hay escala calibrada. Ejecute NIVELAR primero para calibrar.")
      (exit)
    )
  )

  (princ "\nSeleccione los bloques de cota a actualizar [Enter=salir]: ")
  (setq ss (ssget '((0 . "INSERT") (2 . "COTA alzado"))))
  (if (null ss)
    (progn (princ "\nNo se selecciono nada.") (exit))
  )

  (command "_.UNDO" "_Begin")

  (setq count 0)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq block-cota (niv:get-block-cota ent))

    (if (and block-cota (niv:level-text-p (cdr block-cota)))
      (progn
        (setq attrib-ent (car block-cota))
        (setq old-txt (cdr block-cota))

        ;; Usar la posicion Y del punto de insercion del bloque
        (setq ed (entget ent))
        (setq ins-pt (cdr (assoc 10 ed)))
        (setq new-elev (niv:y-to-elev (cadr ins-pt)))

        (if new-elev
          (progn
            (setq new-txt (niv:format-level new-elev old-txt))
            (setq ed (entget attrib-ent))
            (setq ed (subst (cons 1 new-txt) (assoc 1 ed) ed))
            (entmod ed)
            (entupd attrib-ent)
            (setq count (1+ count))
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (command "_.UNDO" "_End")
  (princ (strcat "\n" (itoa count) " marcadores actualizados."))
  (princ)
)

;;=================== RESET ===================;;

(defun c:NIVRESET ()
  (setq niv:*scale* nil)
  (setq niv:*elev-per-y* nil)
  (princ "\nEscala reseteada. Ejecute NIVELAR para recalibrar.")
  (princ)
)

;;=================== CARGA ===================;;

(princ "\nNivelar v1.1 cargado. Comandos: NIVELAR, NIVTODOS, NIVRESET")
(princ)
