;|
  INCREMETAR NUMERACION para LusoCAD
  ===================================
  Script para numerar e incrementar atributos y textos.
  Basado en las rutinas de Gile (Gilles Chanteau).

  Comandos disponibles:
    INCREMENTAR          - Menu principal (elegir operacion)
    NUMERAR              - Numerar secuencialmente
    RENUMERAR            - Renumerar existentes por posicion
    INSERTARTEXTO        - Insertar textos con valor incremental
    INCREMENTARSUFIJO    - Incrementar sufijo de atributos NM
    ANADIRVALOR          - Anadir valor incremental a atributos NM
    INCREMENTARSELECCION - Incrementar valor en atributos NM seleccionados

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

;; pil:escape-block-name - Escapa nombres anonimos (*U###) para usar en -INSERT
(defun pil:escape-block-name (name)
  (if (and name (> (strlen name) 0) (= "*" (substr name 1 1)))
    (strcat "`" name)
    name
  )
)

;; pil:get-insert-block-name - Devuelve nombre valido para insertar un bloque
;; Para bloques dinamicos anonimos (*U###), intenta usar EffectiveName.
(defun pil:get-insert-block-name (ent / enx obj name)
  (setq enx (entget ent))
  (if (= (cdr (assoc 0 enx)) "INSERT")
    (progn
      (setq name (cdr (assoc 2 enx)))
      (if (and name (> (strlen name) 0) (= "*" (substr name 1 1)))
        (progn
          (vl-load-com)
          (setq obj (vlax-ename->vla-object ent))
          (if (vlax-property-available-p obj 'EffectiveName)
            (setq name (vla-get-EffectiveName obj))
          )
        )
      )
      name
    )
  )
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

;; pil:get-all-attrib-tags - Returns list of attribute tags in a block
(defun pil:get-all-attrib-tags (ent / enx sub lst)
  (setq enx (entget ent))
  (if (and (= (cdr (assoc 0 enx)) "INSERT")
           (= (cdr (assoc 66 enx)) 1))
    (progn
      (setq sub (entnext ent))
      (while sub
        (setq enx (entget sub))
        (if (= (cdr (assoc 0 enx)) "ATTRIB")
          (setq lst (cons (cdr (assoc 2 enx)) lst))
        )
        (if (= (cdr (assoc 0 enx)) "SEQEND")
          (setq sub nil)
          (setq sub (entnext sub))
        )
      )
      (reverse lst)
    )
  )
)

;; pil:find-tag-attrib - Busca un atributo por tag en cualquier entidad
;; ent: entidad (puede ser INSERT, ATTRIB, o cualquier otra)
;; tagname: nombre del tag a buscar (string)
;; Retorna: ename del atributo encontrado, o nil
(defun pil:find-tag-attrib (ent tagname / enx sub found)
  (setq enx (entget ent))
  (cond
    ;; Si es un INSERT (bloque), buscar entre sus atributos
    ((and (= (cdr (assoc 0 enx)) "INSERT")
          (= (cdr (assoc 66 enx)) 1))
     (setq sub (entnext ent))
     (while (and sub (not found))
       (setq enx (entget sub))
       (cond
         ((= (cdr (assoc 0 enx)) "SEQEND") (setq sub nil))
         ((and (= (cdr (assoc 0 enx)) "ATTRIB")
               (= (strcase (cdr (assoc 2 enx))) (strcase tagname)))
          (setq found sub)
         )
         (T (setq sub (entnext sub)))
       )
     )
     found
    )
    ;; Si es un ATTRIB directamente y coincide el tag
    ((and (= (cdr (assoc 0 enx)) "ATTRIB")
          (= (strcase (cdr (assoc 2 enx))) (strcase tagname)))
     ent
    )
    ;; Cualquier otra cosa: nil
    (T nil)
  )
)

;; pil:sel-pick - Selecciona una entidad y devuelve su entget segun tipos permitidos
;; entname: nombre de entidad (ename)
;; etypes: mascara de bits (1=TEXT, 2=MTEXT, 4=BLOCK)
;; tag: etiqueta de atributo (para bloques)
;; Retorna: entget del objetivo (TEXT, MTEXT, o ATTRIB dentro de bloque), o nil
(defun pil:sel-pick (entname etypes tag / enx typ)
  (setq enx (entget entname)
        typ (cdr (assoc 0 enx)))
  (cond
    ((and (= typ "TEXT") (= 1 (logand 1 etypes))) enx)
    ((and (= typ "MTEXT") (= 2 (logand 2 etypes))) enx)
    ((and (= typ "INSERT") (= 4 (logand 4 etypes)))
     (pil:find-tag-attrib entname tag))
    (T nil)
  )
)

;; pil:apply-action - Aplica la accion al texto existente
;; existing: texto actual de la entidad (string)
;; newval: nuevo valor compuesto (string)
;; action: "sub" (sustituir), "pre" (anadir como prefijo), "suf" (anadir como sufijo)
;; Retorna: texto final (string)
(defun pil:apply-action (existing newval action)
  (cond
    ((= action "sub") newval)
    ((= action "pre") (strcat newval existing))
    ((= action "suf") (strcat existing newval))
    (T newval)
  )
)

;; pil:entupd-smart - Actualiza entidad en pantalla (ATTRIB vs TEXT/MTEXT)
(defun pil:entupd-smart (elst)
  (if (= (cdr (assoc 0 elst)) "ATTRIB")
    (entupd (cdr (assoc 330 elst)))
    (entupd (cdr (assoc -1 elst)))
  )
)

;; pil:set-nm-value - Establece el valor del atributo NM en un bloque
;; ent: entidad INSERT del bloque
;; val: nuevo valor (string)
;; Retorna: T si tuvo exito, nil si no
;; pil:update-ent-val - Actualiza el valor de una entidad (Bloque/Text/MText)
;; ent: entidad
;; val: nuevo valor (string)
;; tag: etiqueta (para bloques)
;; etypes: mascara tipos (para validacion extra, opcional)
;; Retorna: T si tuvo exito
(defun pil:update-ent-val (ent val tag / typ elst att)
  (setq elst (entget ent)
        typ  (cdr (assoc 0 elst)))
  (cond
    ;; Bloque: buscar atributo por tag
    ((= typ "INSERT")
     (if (setq att (pil:find-tag-attrib ent tag))
       (progn
         (setq elst (entget att))
         (entmod (subst (cons 1 val) (assoc 1 elst) elst))
         (entupd ent)
         T
       )
     )
    )
    ;; Texto o MTexto: actualizar grupo 1
    ((or (= typ "TEXT") (= typ "MTEXT"))
     (entmod (subst (cons 1 val) (assoc 1 elst) elst))
     (entupd ent)
     T
    )
    (T nil)
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
;; pil:select-entities - Solicita seleccion y filtra por tipos
;; msg: mensaje
;; etypes: mascara (1=TEXT, 2=MTEXT, 4=INSERT)
;; tag: etiqueta para filtrar bloques
(defun pil:select-entities (msg etypes tag / ss i ent lst filter typ)
  (princ (strcat "\n" msg))
  (setq filter "")
  (if (= 1 (logand 1 etypes)) (setq filter (strcat filter "TEXT,")))
  (if (= 2 (logand 2 etypes)) (setq filter (strcat filter "MTEXT,")))
  (if (= 4 (logand 4 etypes)) (setq filter (strcat filter "INSERT,")))
  (if (> (strlen filter) 0)
    (setq filter (substr filter 1 (1- (strlen filter))))
  )
  (if (setq ss (ssget (list (cons 0 filter))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq typ (cdr (assoc 0 (entget ent))))
        (cond
          ((= typ "INSERT")
           (if (pil:find-tag-attrib ent tag) (setq lst (cons ent lst)))
          )
          (T (setq lst (cons ent lst)))
        )
        (setq i (1+ i))
      )
      lst
    )
  )
)

;; pil:incvalue - Bucle de seleccion e incremento (adaptado de gile)
;; pref: prefijo, val: valor, sep: separador, suff: sufijo, inc: incremento
;; bin: tipo (1=num, 2=MAY, 4=min), tag: etiqueta atributo
;; etypes: mascara tipos entidad (1=TEXT, 2=MTEXT, 4=BLOCK)
;; action: "sub"/"pre"/"suf", save: lista undo
(defun pil:incvalue (pref val sep suff inc bin tag etypes action save
                     / ent elst newval)
  (while (or (initget 1 "Deshacer")
             (setq ent (entsel
                         (strcat "\nSeleccione el siguiente elemento"
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
          (pil:entupd-smart elst)
          (setq val  (pil:incsuff val (- inc) bin)
                save (cdr save)
          )
        )
        (princ "\nNada que deshacer.")
      )
      (if (setq elst (pil:sel-pick (car ent) etypes tag))
        (progn
          (setq save (cons elst save))
          (setq val (pil:incsuff val inc bin))
          (setq newval (pil:apply-action
                         (cdr (assoc 1 elst))
                         (strcat pref val sep suff)
                         action))
          (entmod (subst (cons 1 newval) (assoc 1 elst) elst))
          (pil:entupd-smart elst)
        )
        (princ "\nEntidad no valida o tipo no permitido.")
      )
    )
  )
)

;;=================== COMANDO NUMPIL ===================;;
;; c:NUMERAR - Numerar pilotes secuencialmente en el dibujo
(defun c:NUMERAR (/ temp file dcl_id what_next val inc pref suff
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
      (setq pilotes (pil:select-entities "Seleccione los pilotes a numerar:" 4 *PIL:TAG*))
      (if pilotes
        (progn
          (setq count (length pilotes)
                pad   (pil:get-padding count)
                i     (atoi val)
          )
          (foreach ent pilotes
            (pil:update-ent-val ent (strcat pref (pil:pad-number i pad) suff) *PIL:TAG*)
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
;; c:RENUMERAR - Renumerar pilotes existentes por posicion
(defun c:RENUMERAR (/ temp file dcl_id what_next val inc pref suff
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
      (setq pilotes (pil:select-entities "Seleccione los pilotes a renumerar:" 4 *PIL:TAG*))
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
            (pil:update-ent-val ent (strcat pref (pil:pad-number i pad) suff) *PIL:TAG*)
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
;; c:INSERTARTEXTO - Insertar textos con valor incremental
(defun c:INSERTARTEXTO (/ temp file dcl_id slst st jlst ju ht ro val inc pref suff
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
;; c:INCREMENTARSUFIJO - Incrementar sufijo de atributos NM
(defun c:INCREMENTARSUFIJO (/ temp file dcl_id what_next typ inc ent elst val save)

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
        (pil:incvalue "" val "" "" *pil:incrval* *pil:suffbin* *PIL:TAG* 7 "sub" save)
        (princ "\nParametro de sufijo incorrecto.")
      )
    )
  )
  (princ)
)

;;=================== COMANDO INCPILADD ===================;;
;; c:ANADIRVALOR - Anadir valor incremental a atributos NM
(defun c:ANADIRVALOR (/ temp file dcl_id val inc pref suff pos ent elst str
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
;; c:INCREMENTARSELECCION - Incrementar valor en atributos NM seleccionados
(defun c:INCREMENTARSELECCION (/ temp file dcl_id val inc pref suff ent elst typ save)
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
      (pil:incvalue pref val "" suff inc 7 *PIL:TAG* 7 "sub" save)
    )
  )
  (princ)
)

;;=================== COMANDO INCPIL (DIALOGO UNIFICADO CON PESTANAS) ===================;;

;; pil:dcl-left-panel - Genera la cadena DCL del panel izquierdo (compartido)
(defun pil:dcl-left-panel ()
  (strcat
    ":column{fixed_width=true;width=26;"
    ":boxed_column{label=\"Tipo de valor\";"
    ":toggle{label=\"Numeros [0-9]\";key=\"num\";}"
    ":toggle{label=\"Mayusculas [A-Z]\";key=\"maj\";}"
    ":toggle{label=\"Minusculas [a-z]\";key=\"min\";}"
    "spacer;}"
    ":boxed_column{label=\"Parametros\";"
    ":edit_box{label=\"Valor inicial\";key=\"val\";edit_width=8;}"
    ":edit_box{label=\"Incremento\";key=\"inc\";edit_width=8;}"
    ":edit_box{label=\"Separador\";key=\"sep\";edit_width=8;}"
    ":edit_box{label=\"Prefijo\";key=\"pref\";edit_width=8;}"
    ":edit_box{label=\"Sufijo\";key=\"suff\";edit_width=8;}"
    "}}"
  )
)

;; pil:dcl-tab-buttons - Genera la cadena DCL de los botones de pestana
(defun pil:dcl-tab-buttons ()
  (strcat
    ":column{"
    ":row{"
    ":button{label=\"Atributo\";key=\"tab1\";width=10;fixed_width=true;}"
    ":button{label=\"Texto\";key=\"tab2\";width=10;fixed_width=true;}"
    ":button{label=\"Seleccion\";key=\"tab3\";width=10;fixed_width=true;}"
    ":button{label=\"Auto\";key=\"tab4\";width=10;fixed_width=true;}"
    "}"
    ":text{key=\"tabinfo\";label=\"Pestana actual: Seleccion\";}"
    "}"
  )
)

;; pil:dcl-tab-atributo - Contenido de la pestana Atributo
(defun pil:dcl-tab-atributo ()
  (strcat
    ":boxed_column{label=\"Bloque\";"
    ":edit_box{label=\"Nombre del bloque\";key=\"blkname\";edit_width=16;}"
    ":button{label=\"Examinar...\";key=\"browse\";alignment=right;fixed_width=true;}"
    "spacer;"
    ":edit_box{label=\"Escala global\";key=\"scl\";edit_width=8;}"
    ":edit_box{label=\"Rotacion\";key=\"rot\";edit_width=8;}"
    "}"
    ":boxed_column{label=\"Atributo\";"
    ":popup_list{label=\"Identificador\";key=\"attag\";edit_width=16;}"
    "}"
    ":boxed_column{label=\"Insercion\";"
    ":radio_button{label=\"Punto por punto\";key=\"ins_pt\";value=\"1\";}"
    "}"
  )
)

;; pil:dcl-tab-texto - Contenido de la pestana Texto
(defun pil:dcl-tab-texto ()
  (strcat
    ":boxed_column{label=\"Propiedades del texto\";"
    ":popup_list{label=\"Estilo\";key=\"st\";edit_width=16;}"
    ":popup_list{label=\"Justificacion\";key=\"ju\";edit_width=16;}"
    ":edit_box{label=\"Altura\";key=\"ht\";edit_width=8;}"
    ":edit_box{label=\"Rotacion\";key=\"tro\";edit_width=8;}"
    "}"
    ":boxed_column{label=\"Insercion\";"
    ":radio_button{label=\"Punto por punto\";key=\"ins_pt\";value=\"1\";}"
    "}"
  )
)

;; pil:dcl-tab-seleccion - Contenido de la pestana Seleccion
(defun pil:dcl-tab-seleccion ()
  (strcat
    ":boxed_column{label=\"Tipo de entidad\";"
    ":toggle{label=\"Texto\";key=\"et_txt\";}"
    ":toggle{label=\"Texto lineas multiples\";key=\"et_mtxt\";}"
    ":toggle{label=\"Bloque\";key=\"et_blk\";}"
    "}"
    ":edit_box{label=\"Identificador\";key=\"tag\";edit_width=16;}"
    ":boxed_column{label=\"Accion\";"
    ":radio_button{label=\"Anadir como prefijo\";key=\"act_pre\";}"
    ":radio_button{label=\"Anadir como sufijo\";key=\"act_suf\";}"
    ":radio_button{label=\"Sustituir\";key=\"act_sub\";value=\"1\";}"
    "}"
  )
)

;; pil:dcl-tab-auto - Contenido de la pestana Auto
(defun pil:dcl-tab-auto ()
  (strcat
    ":boxed_column{label=\"Tipo de entidad\";"
    ":radio_button{label=\"Texto\";key=\"aet_txt\";}"
    ":radio_button{label=\"Texto lineas multiples\";key=\"aet_mtxt\";}"
    ":radio_button{label=\"Bloque\";key=\"aet_blk\";value=\"1\";}"
    "}"
    ":edit_box{label=\"Identificador\";key=\"atag\";edit_width=16;}"
    ":boxed_column{label=\"Accion\";"
    ":radio_button{label=\"Anadir como prefijo\";key=\"aact_pre\";}"
    ":radio_button{label=\"Anadir como sufijo\";key=\"aact_suf\";}"
    ":radio_button{label=\"Sustituir\";key=\"aact_sub\";value=\"1\";}"
    "}"
    ":boxed_column{label=\"Clasificacion\";"
    ":popup_list{label=\"Ordenar por\";key=\"sort1\";edit_width=14;}"
    ":popup_list{label=\"Entonces por\";key=\"sort2\";edit_width=14;}"
    "}"
  )
)

;; pil:write-all-dcl - Escribe las 4 definiciones de dialogo en el archivo DCL
(defun pil:write-all-dcl (file)
  ;; Tab 1: Atributo
  (write-line
    (strcat
      "PilTab1:dialog{label=\"Incremetar numeracion\";"
      ":row{"
      (pil:dcl-left-panel)
      ":column{"
      (pil:dcl-tab-buttons)
      (pil:dcl-tab-atributo)
      "}}"
      "spacer;ok_cancel;}"
    )
    file
  )
  ;; Tab 2: Texto
  (write-line
    (strcat
      "PilTab2:dialog{label=\"Incremetar numeracion\";"
      ":row{"
      (pil:dcl-left-panel)
      ":column{"
      (pil:dcl-tab-buttons)
      (pil:dcl-tab-texto)
      "}}"
      "spacer;ok_cancel;}"
    )
    file
  )
  ;; Tab 3: Seleccion
  (write-line
    (strcat
      "PilTab3:dialog{label=\"Incremetar numeracion\";"
      ":row{"
      (pil:dcl-left-panel)
      ":column{"
      (pil:dcl-tab-buttons)
      (pil:dcl-tab-seleccion)
      "}}"
      "spacer;ok_cancel;}"
    )
    file
  )
  ;; Tab 4: Auto
  (write-line
    (strcat
      "PilTab4:dialog{label=\"Incremetar numeracion\";"
      ":row{"
      (pil:dcl-left-panel)
      ":column{"
      (pil:dcl-tab-buttons)
      (pil:dcl-tab-auto)
      "}}"
      "spacer;ok_cancel;}"
    )
    file
  )
)

;; pil:setup-left-panel - Configura los tiles del panel izquierdo (compartido)
(defun pil:setup-left-panel (typ val inc sep pref suff)
  (if (= 1 (logand 1 typ)) (set_tile "num" "1"))
  (if (= 2 (logand 2 typ)) (set_tile "maj" "1"))
  (if (= 4 (logand 4 typ)) (set_tile "min" "1"))
  (set_tile "val" val)
  (set_tile "inc" (itoa inc))
  (set_tile "sep" sep)
  (set_tile "pref" pref)
  (set_tile "suff" suff)
)

;; pil:setup-left-actions - Configura las acciones del panel izquierdo
(defun pil:setup-left-actions ()
  (action_tile "val" "(setq val $value)")
  (action_tile "pref" "(setq pref $value)")
  (action_tile "suff" "(setq suff $value)")
  (action_tile "sep" "(setq sep $value)")
  (action_tile
    "inc"
    "(if (and (numberp (read $value)) (< 0 (read $value)))
       (setq inc (atoi $value))
       (progn
         (alert \"El incremento debe ser un entero positivo.\")
         (set_tile \"inc\" (itoa inc))
         (mode_tile \"inc\" 2)))"
  )
)

;; pil:setup-tab-actions - Configura las acciones de los botones de pestana
(defun pil:setup-tab-actions ()
  (action_tile "tab1"
    "(setq typ (+
       (if (= (get_tile \"num\") \"1\") 1 0)
       (if (= (get_tile \"maj\") \"1\") 2 0)
       (if (= (get_tile \"min\") \"1\") 4 0)))
     (setq dlgpt (done_dialog 11))"
  )
  (action_tile "tab2"
    "(setq typ (+
       (if (= (get_tile \"num\") \"1\") 1 0)
       (if (= (get_tile \"maj\") \"1\") 2 0)
       (if (= (get_tile \"min\") \"1\") 4 0)))
     (setq dlgpt (done_dialog 12))"
  )
  (action_tile "tab3"
    "(setq typ (+
       (if (= (get_tile \"num\") \"1\") 1 0)
       (if (= (get_tile \"maj\") \"1\") 2 0)
       (if (= (get_tile \"min\") \"1\") 4 0)))
     (setq dlgpt (done_dialog 13))"
  )
  (action_tile "tab4"
    "(setq typ (+
       (if (= (get_tile \"num\") \"1\") 1 0)
       (if (= (get_tile \"maj\") \"1\") 2 0)
       (if (= (get_tile \"min\") \"1\") 4 0)))
     (setq dlgpt (done_dialog 14))"
  )
)

;; pil:get-tab-name - Nombre legible de la pestana actual
(defun pil:get-tab-name (curtab)
  (cond
    ((= curtab 1) "Atributo")
    ((= curtab 2) "Texto")
    ((= curtab 3) "Seleccion")
    ((= curtab 4) "Auto")
    (T "Desconocida")
  )
)

;; pil:setup-tab-visual - Marca visualmente la pestana activa
(defun pil:setup-tab-visual (curtab)
  (set_tile "tabinfo" (strcat "Pestana actual: " (pil:get-tab-name curtab)))
  (mode_tile "tab1" (if (= curtab 1) 1 0))
  (mode_tile "tab2" (if (= curtab 2) 1 0))
  (mode_tile "tab3" (if (= curtab 3) 1 0))
  (mode_tile "tab4" (if (= curtab 4) 1 0))
)

;; c:INCREMENTAR - Comando principal con dialogo unificado y pestanas
(defun c:INCREMENTAR (/ temp file dcl_id curtab what_next accepted
                   typ val inc sep pref suff
                   slst st jlst ju ht tro
                   sel_action sel_etypes tag
                   auto_sort1 auto_sort2 sortlst
                   blkname scl rot attlst atidx
                   pilotes count pad i ent elst pt hor vert nor
                   newval save old_attreq dlgpt)

  ;; Valores por defecto
  (setq typ   1            ;; Numeros
        val   "1"
        inc   1
        sep   ""
        pref  ""
        suff  ""
        curtab 3           ;; Pestana Seleccion por defecto
        accepted nil
  )

  ;; Recopilar estilos de texto
  (setq slst nil)
  (while (setq st (tblnext "STYLE" (not st)))
    (if (/= (cdr (assoc 2 st)) "")
      (setq slst (cons (cdr (assoc 2 st)) slst))
    )
  )
  (setq slst (reverse slst))
  (setq st (if slst (car slst) "Standard"))
  (setq jlst '("Izquierdo"     "Centro"           "Derecha"
               "Medio"         "Arriba Izquierda"  "Arriba Centro"
               "Arriba Derecha" "Medio Izquierda"  "Medio Centro"
               "Medio Derecha" "Abajo Izquierda"   "Abajo Centro"
               "Abajo Derecha")
  )
  (setq ju "Izquierdo")
  (setq ht (getvar "TEXTSIZE"))
  (setq tro 0.0)
  (setq sortlst '("X Ascendente" "X Descendente" "Y Ascendente" "Y Descendente"))
  (setq auto_sort1 0 auto_sort2 2)
  (setq sel_action "sub" sel_etypes 4 tag *PIL:TAG*)
  (setq blkname "" scl 1.0 rot 0.0 atidx 0)

  ;; Generar archivo DCL con las 4 pestanas
  (setq temp (vl-filename-mktemp "IncPil.dcl")
        file (open temp "w")
  )
  (pil:write-all-dcl file)
  (close file)
  (setq dcl_id (load_dialog temp))
  (setq what_next curtab)

  ;; === BUCLE PRINCIPAL DE PESTANAS ===
  (while (> what_next 0)
    (if (not (new_dialog
               (strcat "PilTab" (itoa curtab))
               dcl_id
               ""
               dlgpt))
      (progn (alert "Error cargando el dialogo.") (setq what_next 0))
    )
    (if (> what_next 0)
      (progn
        ;; Configurar panel izquierdo (compartido)
        (pil:setup-left-panel typ val inc sep pref suff)
        (pil:setup-left-actions)
        (pil:setup-tab-actions)
        (pil:setup-tab-visual curtab)

        ;; Configurar contenido segun la pestana actual
        (cond
          ;; ---- PESTANA 1: ATRIBUTO ----
          ((= curtab 1)
           (set_tile "blkname" blkname)
           (set_tile "scl" (rtos scl))
           (set_tile "rot" (rtos rot))
           (if attlst
             (progn
               (start_list "attag")
               (mapcar 'add_list attlst)
               (end_list)
               (set_tile "attag" (itoa atidx))
             )
           )
           (action_tile "blkname" "(setq blkname $value)")
           (action_tile "scl"
             "(if (and (distof $value) (< 0 (distof $value)))
                (setq scl (distof $value))
                (progn (alert \"Requiere un numero positivo\")
                       (set_tile \"scl\" (rtos scl))
                       (mode_tile \"scl\" 2)))"
           )
           (action_tile "rot"
             "(if (numberp (angtof $value))
                (setq rot (angtof $value))
                (progn (alert \"Requiere un angulo valido\")
                       (set_tile \"rot\" (rtos rot))
                       (mode_tile \"rot\" 2)))"
           )
           (action_tile "attag" "(setq atidx (atoi $value))")
           (action_tile "browse" "(setq dlgpt (done_dialog 20))")
          )

          ;; ---- PESTANA 2: TEXTO ----
          ((= curtab 2)
           (if slst
             (progn
               (start_list "st")
               (mapcar 'add_list slst)
               (end_list)
               (set_tile "st" (itoa (if (vl-position st slst)
                                      (vl-position st slst) 0)))
             )
           )
           (start_list "ju")
           (mapcar 'add_list jlst)
           (end_list)
           (set_tile "ju" (itoa (if (vl-position ju jlst)
                                  (vl-position ju jlst) 0)))
           (set_tile "ht" (rtos ht))
           (set_tile "tro" (angtos tro))
           (action_tile "st" "(setq st (nth (atoi $value) slst))")
           (action_tile "ju" "(setq ju (nth (atoi $value) jlst))")
           (action_tile "ht"
             "(if (and (numberp (distof $value)) (< 0 (distof $value)))
                (setq ht (distof $value))
                (progn (alert \"Requiere un numero positivo\")
                       (set_tile \"ht\" (rtos ht))
                       (mode_tile \"ht\" 2)))"
           )
           (action_tile "tro"
             "(if (numberp (angtof $value))
                (setq tro (angtof $value))
                (progn (alert \"Requiere un angulo valido\")
                       (set_tile \"tro\" (angtos tro))
                       (mode_tile \"tro\" 2)))"
           )
          )

          ;; ---- PESTANA 3: SELECCION ----
          ((= curtab 3)
           (if (= 1 (logand 1 sel_etypes)) (set_tile "et_txt" "1"))
           (if (= 2 (logand 2 sel_etypes)) (set_tile "et_mtxt" "1"))
           (if (= 4 (logand 4 sel_etypes)) (set_tile "et_blk" "1"))
           (set_tile "tag" tag)
            (mode_tile "tag" (if (= 4 (logand 4 sel_etypes)) 0 1))
           (cond
             ((= sel_action "pre") (set_tile "act_pre" "1"))
             ((= sel_action "suf") (set_tile "act_suf" "1"))
             (T (set_tile "act_sub" "1"))
           )
           (action_tile "tag" "(setq tag $value)")
           (action_tile "et_txt"
             "(setq sel_etypes (if (= $value \"1\") (logior sel_etypes 1) (logand sel_etypes 6)))")
           (action_tile "et_mtxt"
             "(setq sel_etypes (if (= $value \"1\") (logior sel_etypes 2) (logand sel_etypes 5)))")
           (action_tile "et_blk"
             "(setq sel_etypes (if (= $value \"1\") (logior sel_etypes 4) (logand sel_etypes 3)))
              (mode_tile \"tag\" (if (= $value \"1\") 0 1))")
           (action_tile "act_pre" "(setq sel_action \"pre\")")
           (action_tile "act_suf" "(setq sel_action \"suf\")")
           (action_tile "act_sub" "(setq sel_action \"sub\")")
          )

          ;; ---- PESTANA 4: AUTO ----
          ((= curtab 4)
           (set_tile "atag" tag)
           (mode_tile "atag" (if (= 4 (logand 4 sel_etypes)) 0 1))
           ;; Inicializar radio buttons de tipo de entidad
           (cond
             ((= sel_etypes 1) (set_tile "aet_txt" "1"))
             ((= sel_etypes 2) (set_tile "aet_mtxt" "1"))
             (T (set_tile "aet_blk" "1"))
           )
           (start_list "sort1")
           (mapcar 'add_list sortlst)
           (end_list)
           (start_list "sort2")
           (mapcar 'add_list sortlst)
           (end_list)
           (set_tile "sort1" (itoa auto_sort1))
           (set_tile "sort2" (itoa auto_sort2))
           ;; Inicializar radio buttons de accion
           (cond
             ((= sel_action "pre") (set_tile "aact_pre" "1"))
             ((= sel_action "suf") (set_tile "aact_suf" "1"))
             (T (set_tile "aact_sub" "1"))
           )
           ;; Action tiles
           (action_tile "atag" "(setq tag $value)")
           (action_tile "aet_txt"  "(setq sel_etypes 1) (mode_tile \"atag\" 1)")
           (action_tile "aet_mtxt" "(setq sel_etypes 2) (mode_tile \"atag\" 1)")
           (action_tile "aet_blk"  "(setq sel_etypes 4) (mode_tile \"atag\" 0)")
           (action_tile "sort1" "(setq auto_sort1 (atoi $value))")
           (action_tile "sort2" "(setq auto_sort2 (atoi $value))")
           (action_tile "aact_pre" "(setq sel_action \"pre\")")
           (action_tile "aact_suf" "(setq sel_action \"suf\")")
           (action_tile "aact_sub" "(setq sel_action \"sub\")")
          )
        ) ;; end cond

        ;; Accion OK: guardar estado del panel izquierdo
        (action_tile "accept"
          "(setq typ (+
             (if (= (get_tile \"num\") \"1\") 1 0)
             (if (= (get_tile \"maj\") \"1\") 2 0)
             (if (= (get_tile \"min\") \"1\") 4 0)))
           (if (zerop typ)
             (progn (alert \"Seleccione al menos un tipo de valor.\")
                    (setq typ 1) (setq dlgpt (done_dialog 2)))
             (setq dlgpt (done_dialog 1)))"
        )
        (action_tile "cancel" "(setq dlgpt (done_dialog 0))")

        ;; Iniciar dialogo
        (setq what_next (start_dialog))

        ;; Procesar resultado
        (cond
          ((= what_next 0) (setq what_next 0))       ;; Cancelar - sale del bucle
          ((= what_next 1)                            ;; OK - marcar aceptado y salir
           (setq accepted T what_next 0)
          )
          ((= what_next 2) (setq what_next curtab))   ;; Reabrir mismo tab
          ((= what_next 20)                           ;; Examinar bloque
           (if (setq ent (car (entsel "\nSeleccione un bloque de referencia: ")))
             (if (= (cdr (assoc 0 (entget ent))) "INSERT")
               (if (setq attlst (pil:get-all-attrib-tags ent))
                 (setq blkname (pil:get-insert-block-name ent)
                       atidx   0
                       scl     (cdr (assoc 41 (entget ent)))
                       rot     (cdr (assoc 50 (entget ent)))
                 )
                 (alert "El bloque seleccionado no tiene atributos.")
               )
               (alert "La entidad seleccionada no es un bloque.")
             )
           ) 
           (setq what_next curtab)
          )
          ((and (>= what_next 11) (<= what_next 14))  ;; Cambiar de pestana
           (setq curtab (- what_next 10)
                 what_next curtab
           )
          )
        )
      )
    )
  ) ;; end while

  (unload_dialog dcl_id)
  (vl-file-delete temp)

  ;; === EJECUCION SEGUN PESTANA ACTIVA ===
  (if accepted
    (cond
      ;; ---- EJECUTAR ATRIBUTO (Tab 1) ----
      ((= curtab 1)
       (if (and blkname (/= blkname "") attlst)
         (progn
           (setq old_attreq (getvar "ATTREQ"))
           (setvar "ATTREQ" 0)
           (setq tag (nth atidx attlst))
           ;; Bucle de insercion
           (while (setq pt (getpoint "\nEspecifique punto de insercion: "))
             (command "_.-insert" (pil:escape-block-name blkname) pt scl scl (angtos rot))
             (setq ent (entlast))
             (pil:update-ent-val ent (strcat pref val sep suff) tag)
             (setq val (pil:incsuff val inc typ))
           )
           (setvar "ATTREQ" old_attreq)
         )
         (alert "Configure primero el bloque y el atributo.")
       )
      )

      ;; ---- EJECUTAR TEXTO (Tab 2) ----
      ((= curtab 2)
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
       (while (setq pt (getpoint "\nEspecifique punto de insercion: "))
         (setq pt (trans pt 1 nor))
         (entmake
           (list
             '(0 . "TEXT")
             (cons 10 pt)
             (cons 40 ht)
             (cons 50 (+ tro (angle '(0 0 0)
                         (trans (getvar "UCSXDIR") 0 nor))))
             (cons 7 st)
             (cons 11 pt)
             (cons 72 hor)
             (cons 73 vert)
             (cons 1 (strcat pref val sep suff))
             (cons 210 nor)
           )
         )
         (setq val (pil:incsuff val inc typ))
       )
      )

      ;; ---- EJECUTAR SELECCION (Tab 3) ----
      ((= curtab 3)
       ;; Leer checkboxes de tipo de entidad del dialogo
       ;; sel_etypes ya contiene la mascara de bits
       ;; Seleccionar la primera entidad
       (while
         (not
           (and
             (setq ent (entsel "\nSeleccione el elemento de partida: "))
             (setq elst (pil:sel-pick (car ent) sel_etypes tag))
           )
         )
         (if ent (princ "\nEntidad no valida o tipo no permitido. Intente de nuevo."))
       )
       ;; Aplicar accion al primer elemento
       (setq save (cons elst save))
       (setq newval (pil:apply-action
                      (cdr (assoc 1 elst))
                      (strcat pref val sep suff)
                      sel_action))
       (entmod (subst (cons 1 newval) (assoc 1 elst) elst))
       (pil:entupd-smart elst)
       ;; Continuar con el bucle de incremento
       (pil:incvalue pref val sep suff inc typ tag sel_etypes sel_action save)
      )

      ;; ---- EJECUTAR AUTO (Tab 4) ----
      ((= curtab 4)
       (setq pilotes (pil:select-entities
                       "Seleccione los elementos a renumerar:" sel_etypes tag))
       (if pilotes
         (progn
           ;; Ordenar segun sort1
           (setq pilotes
             (cond
               ((= auto_sort1 0) (pil:sort-ents-by-x pilotes))  ;; X asc
               ((= auto_sort1 1)                                ;; X desc
                (reverse (pil:sort-ents-by-x pilotes)))
               ((= auto_sort1 2) (pil:sort-ents-by-y pilotes))  ;; Y asc
               ((= auto_sort1 3)                                ;; Y desc
                (reverse (pil:sort-ents-by-y pilotes)))
               (T pilotes)
             )
           )
           (setq count (length pilotes)
                 pad   (pil:get-padding count)
                 i     (atoi val)
           )
           (foreach ent pilotes
             (setq newval (pil:apply-action
                            (if (= (cdr (assoc 0 (entget ent))) "INSERT")
                                (cdr (assoc 1 (entget (pil:find-tag-attrib ent tag))))
                                (cdr (assoc 1 (entget ent)))
                            )
                            (strcat pref sep (pil:pad-number i pad) suff)
                            sel_action))
             (pil:update-ent-val ent newval tag)
             (setq i (+ i inc))
           )
           (princ (strcat "\n" (itoa count) " pilotes renumerados."))
         )
         (princ "\nNo se seleccionaron pilotes con atributo NM.")
       )
      )
    ) ;; end cond
  ) ;; end if

  (princ)
)

;;=================== CARGA ===================;;

(princ "\n==============================================")
(princ "\n  Incremento numeracion cargado.")
(princ "\n  Comando principal: INCREMENTAR")
(princ "\n  Atajos: NUMERAR, RENUMERAR, INSERTARTEXTO,")
(princ "\n          INCREMENTARSUFIJO, ANADIRVALOR, INCREMENTARSELECCION")
(princ "\n==============================================")(princ)
