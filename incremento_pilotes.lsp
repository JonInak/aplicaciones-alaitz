;|
  INCREMENTO DE PILOTES para LusoCAD
  ===================================
  Script para numerar e incrementar atributos "NM" en bloques de pilotes.
  Basado en las rutinas de Gile (Gilles Chanteau), adaptado para pilotes.

  Comandos disponibles:
    INCPIL    - Menu principal (elegir operacion)
    NUMPIL    - Numerar pilotes secuencialmente
    RENUMPIL  - Renumerar pilotes existentes por posicion
    INCPILTXT - Insertar textos con valor incremental
    INCPILSUF - Incrementar sufijo de atributos NM
    INCPILADD - Anadir valor incremental a atributos NM
    INCPILSEL - Incrementar valor en atributos NM seleccionados

  Revision: 12/02/2026
|;

;;=================== CONFIGURACION ===================;;

;; Tag del atributo a buscar en los bloques de pilotes
(setq *PIL:TAG* "NM")

;;=================== SUB-RUTINAS ===================;;

;; INCSUFF (adaptada de gile) - Incrementa el sufijo de una cadena
;; str: cadena, inc: entero positivo, alpha: codigo binario (1=nums, 2=MAY, 4=min)
(defun pil:incsuff (str inc alpha / lst crt pas ind val quo ret)
  (setq lst (reverse (vl-string->list str)))
  (while
    (and
      (setq crt (car lst))
      (cond
        ((and (< 47 crt 58) (= 1 (logand 1 alpha)))
         (setq pas 10 ind 48)
        )
        ((and (< 64 crt 91) (= 2 (logand 2 alpha)))
         (setq pas 26 ind 65)
        )
        ((and (< 96 crt 123) (= 4 (logand 4 alpha)))
         (setq pas 26 ind 97)
        )
        ((< 0 quo)
         (setq crt (if (= 10 pas) ind (1- ind))
               lst (cons (car lst) lst)
         )
        )
      )
    )
    (setq val (- crt ind)
          quo (/ (+ val inc) pas)
          ret (cons (+ ind (rem (+ val inc) pas)) ret)
    )
    (if (zerop quo)
      (setq ret (append (reverse (cdr lst)) ret)
            lst nil
      )
      (if (cdr lst)
        (setq lst (cdr lst)
              inc quo
        )
        (setq lst (list ind)
              inc (if (= 10 pas) quo (1- quo))
        )
      )
    )
  )
  (if ret
    (vl-list->string ret)
  )
)

;; pil:get-padding - Determina el numero de digitos segun la cantidad total
;; count: cantidad total de pilotes
;; Retorna: 2 si count < 100, 3 si >= 100
(defun pil:get-padding (count)
  (if (< count 100) 2 3)
)

;; pil:pad-number - Formatea un numero con ceros a la izquierda
;; num: numero entero
;; digits: cantidad de digitos deseada
;; Retorna: cadena con ceros (ej: (pil:pad-number 5 3) -> "005")
(defun pil:pad-number (num digits / str)
  (setq str (itoa num))
  (while (< (strlen str) digits)
    (setq str (strcat "0" str))
  )
  str
)

;; pil:get-nm-attrib - Busca la entidad del atributo "NM" dentro de un INSERT
;; ent: entidad INSERT
;; Retorna: la entidad del atributo NM o nil
(defun pil:get-nm-attrib (ent / enx sub found)
  (setq enx (entget ent))
  (if (and (= (cdr (assoc 0 enx)) "INSERT")
           (= (cdr (assoc 66 enx)) 1)
      )
    (progn
      (setq sub (entnext ent))
      (while (and sub (not found))
        (setq enx (entget sub))
        (cond
          ((= (cdr (assoc 0 enx)) "SEQEND")
           (setq sub nil)
          )
          ((and (= (cdr (assoc 0 enx)) "ATTRIB")
                (= (strcase (cdr (assoc 2 enx))) (strcase *PIL:TAG*))
           )
           (setq found sub)
          )
          (T (setq sub (entnext sub)))
        )
      )
    )
  )
  found
)

;; pil:set-nm-value - Establece el valor del atributo NM en un bloque
;; ent: entidad INSERT del bloque
;; val: nuevo valor (string)
;; Retorna: T si tuvo exito, nil si no
(defun pil:set-nm-value (ent val / att elst)
  (if (setq att (pil:get-nm-attrib ent))
    (progn
      (setq elst (entget att))
      (entmod (subst (cons 1 val) (assoc 1 elst) elst))
      (entupd ent)
      T
    )
  )
)

;; pil:get-nm-value - Lee el valor actual del atributo NM
;; ent: entidad INSERT del bloque
;; Retorna: cadena con el valor o nil
(defun pil:get-nm-value (ent / att)
  (if (setq att (pil:get-nm-attrib ent))
    (cdr (assoc 1 (entget att)))
  )
)

;; pil:get-insert-pos - Obtiene la posicion de insercion de un bloque
;; ent: entidad INSERT
;; Retorna: punto (x y z)
(defun pil:get-insert-pos (ent)
  (cdr (assoc 10 (entget ent)))
)

;; pil:sort-ents-by-x - Ordena lista de entidades por posicion X ascendente
;; ents: lista de entidades INSERT
;; Retorna: lista ordenada
(defun pil:sort-ents-by-x (ents)
  (vl-sort ents
    (function
      (lambda (a b)
        (< (car (pil:get-insert-pos a))
           (car (pil:get-insert-pos b))
        )
      )
    )
  )
)

;; pil:sort-ents-by-y - Ordena lista de entidades por posicion Y ascendente
(defun pil:sort-ents-by-y (ents)
  (vl-sort ents
    (function
      (lambda (a b)
        (< (cadr (pil:get-insert-pos a))
           (cadr (pil:get-insert-pos b))
        )
      )
    )
  )
)

;; pil:sort-ents-xy - Ordena por X primario, Y secundario
(defun pil:sort-ents-xy (ents)
  (vl-sort ents
    (function
      (lambda (a b / pa pb)
        (setq pa (pil:get-insert-pos a)
              pb (pil:get-insert-pos b)
        )
        (if (equal (car pa) (car pb) 0.1)
          (< (cadr pa) (cadr pb))
          (< (car pa) (car pb))
        )
      )
    )
  )
)

;; pil:select-pilotes - Solicita seleccion de pilotes y filtra por atributo NM
;; msg: mensaje para el usuario
;; Retorna: lista de entidades INSERT que tienen atributo NM
(defun pil:select-pilotes (msg / ss i ent lst)
  (princ (strcat "\n" msg))
  (if (setq ss (ssget '((0 . "INSERT") (66 . 1))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (pil:get-nm-attrib ent)
          (setq lst (cons ent lst))
        )
        (setq i (1+ i))
      )
      lst
    )
  )
)

;; pil:incvalue - Bucle de seleccion e incremento (adaptado de gile)
;; pref: prefijo, val: valor, suff: sufijo, inc: incremento
;; bin: tipo (1=num, 2=MAY, 4=min), save: lista undo
(defun pil:incvalue (pref val suff inc bin save / ent elst att newval)
  (while (or (initget 1 "Deshacer")
             (setq ent (nentsel
                         (strcat "\nSeleccione el siguiente pilote"
                                 (if save " o [Deshacer]: " ": ")
                         )
                       )
             )
         )
    (if (= ent "Deshacer")
      (if save
        (progn
          (setq elst (car save))
          (entmod elst)
          (entupd (cdr (assoc 330 elst)))
          (setq val  (pil:incsuff val (- inc) bin)
                save (cdr save)
          )
        )
        (princ "\nNada que deshacer.")
      )
      (if (and (setq elst (entget (car ent)))
               (= (cdr (assoc 0 elst)) "ATTRIB")
               (= (strcase (cdr (assoc 2 elst))) (strcase *PIL:TAG*))
          )
        (progn
          (setq save (cons elst save))
          (setq val (pil:incsuff val inc bin))
          (entmod (subst (cons 1 (strcat pref val suff)) (assoc 1 elst) elst))
          (entupd (cdr (assoc 330 elst)))
        )
        (princ "\nEntidad no valida. Seleccione un atributo NM de un pilote.")
      )
    )
  )
)

;;=================== COMANDO NUMPIL ===================;;
;; Numera pilotes secuencialmente con auto zero-padding

(defun c:NUMPIL (/ temp file dcl_id what_next val inc pref suff
                   pilotes count pad i ent)
  (setq temp (vl-filename-mktemp "PilNum.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilNum:dialog{label=\"Numerar Pilotes\";"
      ":boxed_column{label=\"Parametros\";"
      ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=8;allow_accept=true;}"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=8;allow_accept=true;}"
      ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=16;allow_accept=true;}}"
      ":boxed_column{label=\"Tipo de valor\";"
      ":radio_button{label=\"Numerico [0-9]\";key=\"num\";value=\"1\";}"
      ":radio_button{label=\"Alfanumerico [A-Z]\";key=\"alfa\";}}"
      "spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq val  "1"
        inc  1
        pref ""
        suff ""
        dcl_id (load_dialog temp)
        what_next 2
  )
  (while (>= what_next 2)
    (if (not (new_dialog "PilNum" dcl_id))
      (exit)
    )
    (set_tile "val" val)
    (set_tile "inc" (itoa inc))
    (set_tile "pref" pref)
    (set_tile "suff" suff)
    (action_tile "val" "(setq val $value)")
    (action_tile "pref" "(setq pref $value)")
    (action_tile "suff" "(setq suff $value)")
    (action_tile
      "inc"
      "(if (and (numberp (read $value)) (< 0 (read $value)))
         (setq inc (atoi $value))
         (progn
           (alert \"El incremento debe ser un entero positivo.\")
           (set_tile \"inc\" (itoa inc))
           (mode_tile \"inc\" 2)))"
    )
    (action_tile "accept" "(done_dialog 1)")
    (action_tile "cancel" "(setq val nil) (done_dialog 0)")
    (setq what_next (start_dialog))
  )
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if val
    (progn
      (setq pilotes (pil:select-pilotes "Seleccione los pilotes a numerar:"))
      (if pilotes
        (progn
          (setq count (length pilotes)
                pad   (pil:get-padding count)
                i     (atoi val)
          )
          (foreach ent pilotes
            (pil:set-nm-value ent (strcat pref (pil:pad-number i pad) suff))
            (setq i (+ i inc))
          )
          (princ (strcat "\n" (itoa count) " pilotes numerados correctamente."))
        )
        (princ "\nNo se seleccionaron pilotes con atributo NM.")
      )
    )
  )
  (princ)
)

;;=================== COMANDO RENUMPIL ===================;;
;; Renumera pilotes existentes por posicion X/Y

(defun c:RENUMPIL (/ temp file dcl_id what_next val inc pref suff
                     sortmode pilotes count pad i ent)
  (setq temp (vl-filename-mktemp "PilRenum.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilRenum:dialog{label=\"Renumerar Pilotes\";"
      ":boxed_column{label=\"Parametros\";"
      ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=8;allow_accept=true;}"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=8;allow_accept=true;}"
      ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=16;allow_accept=true;}}"
      ":boxed_column{label=\"Ordenar por\";"
      ":radio_button{label=\"X Ascendente\";key=\"sx\";value=\"1\";}"
      ":radio_button{label=\"Y Ascendente\";key=\"sy\";}"
      ":radio_button{label=\"X primario, Y secundario\";key=\"sxy\";}}"
      "spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq val  "1"
        inc  1
        pref ""
        suff ""
        sortmode "x"
        dcl_id (load_dialog temp)
        what_next 2
  )
  (while (>= what_next 2)
    (if (not (new_dialog "PilRenum" dcl_id))
      (exit)
    )
    (set_tile "val" val)
    (set_tile "inc" (itoa inc))
    (set_tile "pref" pref)
    (set_tile "suff" suff)
    (action_tile "val" "(setq val $value)")
    (action_tile "pref" "(setq pref $value)")
    (action_tile "suff" "(setq suff $value)")
    (action_tile "sx" "(setq sortmode \"x\")")
    (action_tile "sy" "(setq sortmode \"y\")")
    (action_tile "sxy" "(setq sortmode \"xy\")")
    (action_tile
      "inc"
      "(if (and (numberp (read $value)) (< 0 (read $value)))
         (setq inc (atoi $value))
         (progn
           (alert \"El incremento debe ser un entero positivo.\")
           (set_tile \"inc\" (itoa inc))
           (mode_tile \"inc\" 2)))"
    )
    (action_tile "accept" "(done_dialog 1)")
    (action_tile "cancel" "(setq val nil) (done_dialog 0)")
    (setq what_next (start_dialog))
  )
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if val
    (progn
      (setq pilotes (pil:select-pilotes "Seleccione los pilotes a renumerar:"))
      (if pilotes
        (progn
          ;; Ordenar segun el modo seleccionado
          (setq pilotes (cond
                          ((= sortmode "x")  (pil:sort-ents-by-x pilotes))
                          ((= sortmode "y")  (pil:sort-ents-by-y pilotes))
                          ((= sortmode "xy") (pil:sort-ents-xy pilotes))
                          (T pilotes)
                        )
          )
          (setq count (length pilotes)
                pad   (pil:get-padding count)
                i     (atoi val)
          )
          (foreach ent pilotes
            (pil:set-nm-value ent (strcat pref (pil:pad-number i pad) suff))
            (setq i (+ i inc))
          )
          (princ (strcat "\n" (itoa count) " pilotes renumerados correctamente."))
        )
        (princ "\nNo se seleccionaron pilotes con atributo NM.")
      )
    )
  )
  (princ)
)

;;=================== COMANDO INCPILTXT ===================;;
;; Inserta textos con valor incremental punto por punto

(defun c:INCPILTXT (/ temp file dcl_id slst st jlst ju ht ro val inc pref suff
                      hor vert nor pt pad)
  (setq temp (vl-filename-mktemp "PilTxt.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilTxt:dialog{label=\"Insertar Texto Incremental\";"
      ":boxed_column{label=\"Formato\";"
      ":row{"
      ":column{"
      ":popup_list{label=\"Estilo\";key=\"st\";edit_width=16;}"
      ":popup_list{label=\"Justificacion\";key=\"ju\";edit_width=16;}}"
      ":column{"
      ":edit_box{label=\"Altura\";key=\"ht\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Rotacion\";key=\"ro\";edit_width=6;allow_accept=true;}}}}"
      ":boxed_column{label=\"Valor\";"
      ":row{"
      ":column{"
      ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=6;allow_accept=true;}}"
      ":column{"
      ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=16;allow_accept=true;}}}}"
      "ok_cancel;}"
    )
    file
  )
  (close file)
  (setq dcl_id (load_dialog temp))
  (if (not (new_dialog "PilTxt" dcl_id))
    (exit)
  )
  ;; Recopilar estilos
  (while (setq st (tblnext "STYLE" (not st)))
    (if (/= (cdr (assoc 2 st)) "")
      (setq slst (cons (cdr (assoc 2 st)) slst))
    )
  )
  (setq slst (reverse slst))
  (start_list "st")
  (mapcar 'add_list slst)
  (end_list)
  ;; Justificaciones
  (setq jlst '("Izquierdo"    "Centro"          "Derecha"
               "Medio"        "Arriba Izquierda" "Arriba Centro"
               "Arriba Derecha" "Medio Izquierda" "Medio Centro"
               "Medio Derecha" "Abajo Izquierda"  "Abajo Centro"
               "Abajo Derecha"
              )
  )
  (start_list "ju")
  (mapcar 'add_list jlst)
  (end_list)
  ;; Valores por defecto
  (or st (setq st (getvar "TEXTSTYLE")))
  (or ju (setq ju "Izquierdo"))
  (or ht (setq ht (getvar "TEXTSIZE")))
  (or ro (setq ro 0.0))
  (or val (setq val "01"))
  (or inc (setq inc 1))
  (or pref (setq pref ""))
  (or suff (setq suff ""))
  (set_tile "st" (itoa (if (vl-position st slst) (vl-position st slst) 0)))
  (set_tile "ju" (itoa (if (vl-position ju jlst) (vl-position ju jlst) 0)))
  (set_tile "ht" (rtos ht))
  (set_tile "ro" (angtos ro))
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (set_tile "pref" pref)
  (set_tile "suff" suff)
  (action_tile "st" "(setq st (nth (atoi $value) slst))")
  (action_tile "ju" "(setq ju (nth (atoi $value) jlst))")
  (action_tile
    "ht"
    "(if (and (numberp (distof $value)) (< 0 (distof $value)))
       (setq ht (distof $value))
       (progn
         (alert \"Necesita un numero real positivo\")
         (set_tile \"ht\" (rtos ht))
         (mode_tile \"ht\" 2)))"
  )
  (action_tile
    "ro"
    "(if (numberp (angtof $value))
       (setq ro (angtof $value))
       (progn
         (alert \"Necesita un valor de angulo valido\")
         (set_tile \"ro\" (angtos ro))
         (mode_tile \"ro\" 2)))"
  )
  (action_tile
    "inc"
    "(if (and (numberp (read $value)) (<= 0 (read $value)))
       (setq inc (atoi $value))
       (progn
         (alert \"Necesita un entero positivo\")
         (set_tile \"inc\" (itoa inc))
         (mode_tile \"inc\" 2)))"
  )
  (action_tile "val" "(setq val $value)")
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(setq ju nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if ju
    (progn
      (setq hor  (cond
                   ((wcmatch ju "*Izquierda,Izquierdo") 0)
                   ((wcmatch ju "*Centro") 1)
                   ((wcmatch ju "*Derecha") 2)
                   (T 4)
                 )
            vert (cond
                   ((wcmatch ju "Arriba *") 3)
                   ((wcmatch ju "Medio *") 2)
                   ((wcmatch ju "Abajo *") 1)
                   (T 0)
                 )
            nor  (trans '(0 0 1) 1 0 T)
      )
      (while (setq pt (getpoint "\nEspecifique el punto de insercion: "))
        (setq pt (trans pt 1 nor))
        (entmake
          (list
            '(0 . "TEXT")
            (cons 10 pt)
            (cons 40 ht)
            (cons 50
                  (+ ro (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 nor)))
            )
            (cons 7 st)
            (cons 11 pt)
            (cons 72 hor)
            (cons 73 vert)
            (cons 1 (strcat pref val suff))
            (cons 210 nor)
          )
        )
        (setq val (pil:incsuff val inc 7))
      )
    )
  )
  (princ)
)

;;=================== COMANDO INCPILSUF ===================;;
;; Incrementa el sufijo de los atributos NM seleccionados

(defun c:INCPILSUF (/ temp file dcl_id what_next typ inc ent elst val save)

  (defun pil:ValidSel (elst / v)
    (setq v (cdr (assoc 1 elst))
          v (ascii (substr v (strlen v)))
    )
    (or
      (and (= 1 (logand 1 *pil:suffbin*)) (< 47 v 58))
      (and (= 2 (logand 2 *pil:suffbin*)) (< 64 v 91))
      (and (= 4 (logand 4 *pil:suffbin*)) (< 96 v 123))
    )
  )

  (or *pil:suffbin* (setq *pil:suffbin* 1))
  (or *pil:incrval* (setq *pil:incrval* 1))
  (setq typ  *pil:suffbin*
        inc  *pil:incrval*
        temp (vl-filename-mktemp "PilSuf.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilSuf:dialog{label=\"Incrementar Sufijo\";"
      ":boxed_column{label=\"Tipo(s) de caracter del sufijo\";"
      ":toggle{label=\"Numeros [0-9]\";key=\"num\";}"
      ":toggle{label=\"Mayusculas [A-Z]\";key=\"maj\";}"
      ":toggle{label=\"Minusculas [a-z]\";key=\"min\";}}"
      "spacer;"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=6;allow_accept=true;}"
      "spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq dcl_id    (load_dialog temp)
        what_next 2
  )
  (while (>= what_next 2)
    (if (not (new_dialog "PilSuf" dcl_id))
      (exit)
    )
    (if (= 1 (logand 1 typ)) (set_tile "num" "1"))
    (if (= 2 (logand 2 typ)) (set_tile "maj" "1"))
    (if (= 4 (logand 4 typ)) (set_tile "min" "1"))
    (set_tile "inc" (itoa inc))
    (action_tile
      "accept"
      "(setq typ (+
         (if (= (get_tile \"num\") \"1\") 1 0)
         (if (= (get_tile \"maj\") \"1\") 2 0)
         (if (= (get_tile \"min\") \"1\") 4 0))
         inc (read (get_tile \"inc\")))
         (cond
           ((zerop typ)
            (alert \"Al menos un tipo de caracter debe estar marcado.\")
            (setq typ *pil:suffbin*) (done_dialog 2))
           ((or (/= (type inc) 'INT) (< inc 0))
            (alert \"El incremento debe ser un entero positivo.\")
            (setq inc *pil:incrval*) (done_dialog 2))
           (T (done_dialog 1)))"
    )
    (action_tile "cancel" "(setq typ nil) (done_dialog 0)")
    (setq what_next (start_dialog))
  )
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if typ
    (progn
      (setq *pil:suffbin* typ
            *pil:incrval* inc
      )
      (while
        (not
          (and
            (setq ent (nentsel "\nSeleccione el atributo NM de partida: "))
            (setq elst (entget (car ent)))
            (= (cdr (assoc 0 elst)) "ATTRIB")
            (= (strcase (cdr (assoc 2 elst))) (strcase *PIL:TAG*))
            (setq val (cdr (assoc 1 elst)))
          )
        )
      )
      (if (pil:ValidSel elst)
        (pil:incvalue "" val "" *pil:incrval* *pil:suffbin* save)
        (princ "\nParametro de sufijo incorrecto.")
      )
    )
  )
  (princ)
)

;;=================== COMANDO INCPILADD ===================;;
;; Anade un valor incrementado al inicio o al final del atributo NM

(defun c:INCPILADD (/ temp file dcl_id val inc pref suff pos ent elst str
                      save att)
  (setq temp (vl-filename-mktemp "PilAdd.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilAdd:dialog{label=\"Anadir Valor Incremental\";"
      ":boxed_radio_column{label=\"Posicion\";key=\"pos\";"
      ":radio_button{label=\"Al inicio\";key=\"start\";}"
      ":radio_button{label=\"Al final\";key=\"end\";value=\"1\";}}"
      ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=16;allow_accept=true;}"
      "spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq val    "1"
        inc    1
        pref   ""
        suff   ""
        dcl_id (load_dialog temp)
  )
  (if (not (new_dialog "PilAdd" dcl_id))
    (exit)
  )
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (action_tile
    "inc"
    "(if (and (numberp (read $value)) (<= 0 (read $value)))
       (setq inc (atoi $value))
       (progn
         (alert \"Necesita un entero positivo\")
         (set_tile \"inc\" (itoa inc))
         (mode_tile \"inc\" 2)))"
  )
  (action_tile "val" "(setq val $value)")
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "accept" "(setq pos (get_tile \"pos\")) (done_dialog)")
  (action_tile "cancel" "(setq inc nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if inc
    (while (or (initget 1 "Deshacer")
               (setq ent (nentsel
                           (strcat "\nSeleccione un atributo NM"
                                   (if save " o [Deshacer]: " ": ")
                           )
                         )
               )
           )
      (if (= ent "Deshacer")
        (if save
          (progn
            (setq elst (car save))
            (entmod elst)
            (entupd (cdr (assoc 330 elst)))
            (setq val  (pil:incsuff val (- inc) 7)
                  save (cdr save)
            )
          )
          (princ "\nNada que deshacer.")
        )
        (if (and (setq elst (entget (car ent)))
                 (= (cdr (assoc 0 elst)) "ATTRIB")
                 (= (strcase (cdr (assoc 2 elst))) (strcase *PIL:TAG*))
            )
          (progn
            (setq save (cons elst save)
                  str  (cdr (assoc 1 elst))
            )
            (entmod
              (subst (cons 1
                           (if (= "start" pos)
                             (strcat pref val suff str)
                             (strcat str pref val suff)
                           )
                     )
                     (assoc 1 elst)
                     elst
              )
            )
            (entupd (cdr (assoc 330 elst)))
            (setq val (pil:incsuff val inc 7))
          )
          (princ "\nEntidad no valida. Seleccione un atributo NM.")
        )
      )
    )
  )
  (princ)
)

;;=================== COMANDO INCPILSEL ===================;;
;; Incrementa un valor en los atributos NM seleccionados uno a uno

(defun c:INCPILSEL (/ temp file dcl_id val inc pref suff ent elst typ save)
  (setq temp (vl-filename-mktemp "PilSel.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "PilSel:dialog{label=\"Incrementar por Seleccion\";"
      ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=6;allow_accept=true;}"
      ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=16;allow_accept=true;}"
      ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=16;allow_accept=true;}"
      "spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq val    "01"
        inc    1
        pref   ""
        suff   ""
        dcl_id (load_dialog temp)
  )
  (if (not (new_dialog "PilSel" dcl_id))
    (exit)
  )
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (action_tile
    "inc"
    "(if (and (numberp (read $value)) (<= 0 (read $value)))
       (setq inc (atoi $value))
       (progn
         (alert \"Necesita un entero positivo\")
         (set_tile \"inc\" (itoa inc))
         (mode_tile \"inc\" 2)))"
  )
  (action_tile "val" "(setq val $value)")
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(setq inc nil)")
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (if inc
    (progn
      ;; Seleccionar atributo NM de partida
      (while
        (not
          (and
            (setq ent (nentsel "\nSeleccione el atributo NM de partida: "))
            (setq elst (entget (car ent)))
            (= (cdr (assoc 0 elst)) "ATTRIB")
            (= (strcase (cdr (assoc 2 elst))) (strcase *PIL:TAG*))
          )
        )
      )
      ;; Establecer primer valor
      (setq save (cons elst save))
      (entmod
        (subst (cons 1 (strcat pref val suff)) (assoc 1 elst) elst)
      )
      (entupd (cdr (assoc 330 elst)))
      ;; Continuar con el bucle de incremento
      (pil:incvalue pref val suff inc 7 save)
    )
  )
  (princ)
)

;;=================== COMANDO INCPIL ===================;;
;; Menu principal - Permite elegir la operacion

(defun c:INCPIL (/ temp file dcl_id fun)
  (setq temp (vl-filename-mktemp "IncPil.dcl")
        file (open temp "w")
  )
  (write-line
    (strcat
      "IncPil:dialog{label=\"Incremento de Pilotes\";"
      ":radio_column{key=\"fun\";"
      ":radio_button{"
      "label=\"NUMPIL - Numerar pilotes secuencialmente\";key=\"c:NUMPIL\";}"
      ":radio_button{"
      "label=\"RENUMPIL - Renumerar por posicion\";key=\"c:RENUMPIL\";}"
      ":radio_button{"
      "label=\"INCPILTXT - Insertar textos incrementados\";key=\"c:INCPILTXT\";}"
      ":radio_button{"
      "label=\"INCPILSEL - Incrementar por seleccion\";key=\"c:INCPILSEL\";}"
      ":radio_button{"
      "label=\"INCPILSUF - Incrementar sufijo\";key=\"c:INCPILSUF\";}"
      ":radio_button{"
      "label=\"INCPILADD - Anadir valor incremental\";key=\"c:INCPILADD\";}}"
      "ok_cancel;}"
    )
    file
  )
  (close file)
  (setq dcl_id (load_dialog temp))
  (if (not (new_dialog "IncPil" dcl_id))
    (exit)
  )
  (set_tile "c:NUMPIL" "1")
  (action_tile
    "accept"
    "(setq fun (get_tile \"fun\")) (done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete temp)
  (and fun (apply (read fun) nil))
  (princ)
)

;;=================== CARGA ===================;;

(princ "\n======================================")
(princ "\n  INCREMENTO DE PILOTES cargado.")
(princ "\n  Comandos: INCPIL, NUMPIL, RENUMPIL,")
(princ "\n  INCPILTXT, INCPILSUF, INCPILADD, INCPILSEL")
(princ "\n======================================")
(princ)
