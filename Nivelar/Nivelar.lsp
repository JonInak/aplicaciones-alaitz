;|
  NIVELAR - Herramienta de Nivelacion para LusoCad
  =================================================
  Actualiza automaticamente cotas de nivel al mover marcadores
  en base a una escala de referencia.

  Modo de uso:
    1. Cargar con APPLOAD
    2. Ejecutar comando NIVELAR
    3. Primera vez: seleccionar bloque de referencia (se explota y calibra la escala)
    4. Seleccionar marcadores de nivel, moverlos, el valor se recalcula

  Comandos:
    NIVELAR  - Comando principal (calibrar + mover marcadores)
    NIVRESET - Resetear escala para recalibrar

  Revision: 05/03/2026
|;

(vl-load-com)

;;=================== CONFIGURACION ===================;;

(setq niv:*ref-layer* "z_PL_FORJADOS")     ;; Capa del bloque de referencia
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

;;; Explota bloque de referencia y construye la escala
(defun niv:build-scale (/ ss last-ent ent ed etype txt elev y pts p1 p2)
  (princ "\nSeleccione el bloque de referencia de niveles: ")
  (setq ss (ssget ":S" (list (cons 8 niv:*ref-layer*))))
  (if (null ss)
    (progn (princ "\nNo se selecciono nada.") nil)
    (progn
      ;; Guardar ultimo entity name antes de explotar
      (setq last-ent (entlast))

      ;; Explotar bloque
      (command "_.EXPLODE" ss "")

      ;; Buscar textos de nivel entre las entidades nuevas
      (setq pts '())
      (setq ent (entnext last-ent))
      (while ent
        (setq ed (entget ent))
        (setq etype (cdr (assoc 0 ed)))
        (if (or (= etype "TEXT") (= etype "MTEXT"))
          (progn
            (setq txt (cdr (assoc 1 ed)))
            (setq elev (if (niv:level-text-p txt) (niv:parse-level txt) nil))
            (if elev
              (progn
                (setq y (cadr (cdr (assoc 10 ed))))
                (setq pts (cons (cons elev y) pts))
              )
            )
          )
        )
        (setq ent (entnext ent))
      )

      ;; Ordenar por elevacion
      (setq niv:*scale*
        (vl-sort pts '(lambda (a b) (< (car a) (car b))))
      )

      ;; Calcular factor de escala (elevacion por unidad Y)
      (if (>= (length niv:*scale*) 2)
        (progn
          (setq p1 (car niv:*scale*))
          (setq p2 (last niv:*scale*))
          (setq niv:*elev-per-y*
            (/ (- (car p2) (car p1))
               (- (cdr p2) (cdr p1)))
          )
          (princ (strcat "\nEscala calibrada: "
                         (itoa (length niv:*scale*)) " niveles ("
                         (niv:format-level (car p1) nil) " a "
                         (niv:format-level (car p2) nil) ")"
                         " | Factor: " (rtos niv:*elev-per-y* 2 6) " m/u"))
          T
        )
        (progn
          (princ "\nError: No se detectaron textos de nivel tras explotar.")
          (princ "\nSi hay sub-bloques, explote manualmente primero y reintente.")
          (setq niv:*scale* nil)
          nil
        )
      )
    )
  )
)

;;=================== COMANDO PRINCIPAL ===================;;

(defun c:NIVELAR (/ ss i ent ed txt etype block-cota
                    level-ent level-type old-level-str
                    base-pt dest-pt delta-y old-elev new-elev new-txt
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
        (setq base-pt (getpoint "\nPunto base (punta de flecha): "))
        (if base-pt
          (progn
            (setq dest-pt (getpoint base-pt "\nNuevo punto: "))
            (if dest-pt
              (progn
                ;; Mover entidades
                (command "_.MOVE" ss "" base-pt dest-pt)

                ;; Calcular y actualizar nivel
                (if level-ent
                  (progn
                    (setq old-elev (niv:parse-level old-level-str))
                    (setq delta-y (- (cadr dest-pt) (cadr base-pt)))
                    (setq new-elev (+ old-elev (* delta-y niv:*elev-per-y*)))
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

;;=================== RESET ===================;;

(defun c:NIVRESET ()
  (setq niv:*scale* nil)
  (setq niv:*elev-per-y* nil)
  (princ "\nEscala reseteada. Ejecute NIVELAR para recalibrar.")
  (princ)
)

;;=================== CARGA ===================;;

(princ "\nNivelar v1.0 cargado. Comandos: NIVELAR, NIVRESET")
(princ)
