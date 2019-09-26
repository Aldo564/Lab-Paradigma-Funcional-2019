#!r6rs

;Aqui se importa r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6))
        (rnrs r5rs (6)))


;################################################################
;TDA Scene

;el equipo del jugador (n jugadores)
;el equipo de la CPU (n jugadores)
;los obstaculos
;los proyectiles

;################################################################

;################################################################
;TDA Objeto

;coordenada X
;coordenada Y
;angulo
;direccion (izq o drch)
;pasos
;caracter

;################################################################

;################################################################
;TDA Proyectil

;coordenada X
;coordenada Y
;tiempo (0 y 1 suben, 2 mantiene, +3 baja)
;angulo

;################################################################

;################################################################
;Formula tf (parabola)

;angulo puede ser de 120 a -120 donde -90 es derecha 

;120 90 60 45 30 0 -30 -45 -60 -90 -120

;si angulo es distinto de 120 o -120
;Y = t + a , con a en [2,-2]

;(0,0)
;suba 2, mantenga 1, baje 2 

;(angulo 120)
; -> (-2,-1) -> (-4,-2) -> (-4,-3) ...
;-2 en X, -1 en Y

;(angulo 90)
; -> (1,0) -> (0,0) ....
;1 en X, 0 en Y

;(angulo 60)
; -> (2,1) -> (4,2) -> (6,2) -> (8,1) -> (10,0) ...
;2 en X, 1 en Y

;(angulo 45)
; -> (1,1) -> (2,2) -> (3,2) -> (4,1) -> (5,0) ...
;1 en X, 1 en Y

;(angulo 30)
; -> (1,2) -> (2,4) -> (3,4) -> (4,2) -> (5,0) ...
;1 en X, 2 en Y

;(angulo 0)
; -> (0,1) -> (0,2) -> (0,1) ...
;0 en X, 1 en Y

;X = X0 + entregaX(-angulo)
;Y = Y0 + entregaY(-angulo)
;angulo variara en cada llamado

;################################################################

;########################################################################################
;Bloque Random

;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random toma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.


;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cuantos" indica el largo de la lista a generar.
;* "xActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximo" Los números generados van desde 0 hasta maximo-1
(define getListaRandom
  (lambda (cuantos xActual maximo)
    (if (= 0 cuantos)
        '()
        (let ((xNvo (myRandom xActual)))
          (cons (remainder xNvo maximo)
              (getListaRandom (- cuantos 1) xNvo maximo)
          )
        )
    )
  )
)

;Ejemplos getListaRandom
;(display (getListaRandom 6 5 8))
;(display (getListaRandom 7 2 3))
;(display (getListaRandom 8 2 9))

;Fin Random
;########################################################################################

;################################################################

;Funcion que dado un angulo entrega el movimiento en X del proyectil
;Entrada: Angulo (entero)
;Salida: Desplazamiento en X (entero)

(define (entregaX angulo)
  (cond
    ((= angulo 120) -2)
    ((= angulo 90) 1)
    ((= angulo 60) 2)
    ((= angulo 30) 1)
    ((= angulo 0) 0)
    ((= angulo -30) 1)
    ((= angulo -60) 2)
    ((= angulo -90) 1)
    ((= angulo -120) -2)
    ((= angulo -1) 0)
    )
  )

;Funcion que dado un angulo entrega el movimiento en Y del proyectil
;Entrada: Angulo (entero)
;Salida: Desplazamiento en Y (entero)
	
(define (entregaY angulo)
  (cond
    ((= angulo 120) -1)
    ((= angulo 90) 0)
    ((= angulo 60) 1)
    ((= angulo 30) 2)
    ((= angulo 0) 1)
    ((= angulo -30) 2)
    ((= angulo -60) 1)
    ((= angulo -90) 0)
    ((= angulo -120) -1)
    ((= angulo -1) -1)
    )
  )	

;################################################################

;################################################################

;Funcion que cambia el angulo en funcion del tiempo que tiene el proyectil
;Entrada: Tiempo de vida del proyectil (entero) y angulo (entero)
;Salida: Angulo (entero)

(define (cambiarAngulo tiempo angulo)
  (if (or (= tiempo 0) (= tiempo 1))
      angulo
      (if (< tiempo 5)
          (cond
            ((= angulo 120) -1 )
            ((= angulo 90) 120 )
            ((= angulo 60) 90 )
            ((= angulo 45) 60 )
            ((= angulo 30) 45 )
            ((= angulo 0) -1 )
            ((= angulo -30) -45 )
            ((= angulo -45) -60 )
            ((= angulo -60) -90 )
            ((= angulo -90) -120 )
            ((= angulo -120) -1 )
            ((= angulo -1) -1 )
            )
          (cond
            ((= angulo 120) -1 )
            ((= angulo 90) -1 )
            ((= angulo 60) -1 )
            ((= angulo 45) -1 )
            ((= angulo 30) -1 )
            ((= angulo 0) -1 )
            ((= angulo -30) -1 )
            ((= angulo -45) -1 )
            ((= angulo -60) -1 )
            ((= angulo -90) -1 )
            ((= angulo -120) -1 )
            ((= angulo -1) -1 )
            )
          )
      )
  )

;################################################################
;se debe usar recursion lineal
;n columnas
;m filas 
;e cant enemigos inciales
;seed semilla
;d dificultad del escenario (cantidad y posicion de obstaculos en el escenario)
;El jugador debe partir en -90 grados (derecha)

;Funcion que crea el escenario.
;Entrada: N (entero), M (entero), e (entero), d (entero) y Seed (entero).
;Salida: TDA Scene con datos validos.

(define (createScene n m e d seed)
  ;(list (consJugadores n m -90 1 0 seed 0) (consEnemigos n m e 90 0 0 seed (list))) ;agregar obstaculos al escenario 
  (if (and (number? n) (number? m) (number? e) (number? d) (number? seed))
      (if (and (> n 0) (> m 0) (>= e 0))
          (list (consObjeto  n m (* e 2) 0 seed (list) 0) (consObstaculos 1 (- m 1) seed (list)) e n m)
          #f
          )
      #f
      )
  )

;(createScene 9 10 3 0 1561)
;(createScene 10 11 8 0 125)
;(createScene 12 15 1 0 963)


(define (consObjeto n m e pasos seed out id)
  (if (= e 0)
      out
      (consObjeto n m (- e 1) pasos (* seed 2) (cons (crearObjeto n m e pasos seed id) out) (+ id 1))
      )
  )


;angulo inicial -90 a la derecha (1) y pasos 0
(define (crearObjeto n m e pasos seed id)
  (if (< id e)
      (list "Jugador" id (+ 2 (car (getListaRandom 1 (* seed id) (- 2 m)))) (+ 2 (car (getListaRandom 1 (* seed (+ id 1)) (- 2 n)))) -90 1 pasos "1")
      (list "Enemigo" id (+ 2 (car (getListaRandom 1 (* seed id) (- 2 m)))) (+ 2 (car (getListaRandom 1 (* seed (+ id 1)) (- 2 n)))) 90 0 pasos "0")
      )
  )

;Funcion que crea el obstaculo
(define (consObs n m seed out)
  (if (= m -1)
      out
      (consObs n (- m 1) seed (cons (list "Obstaculo" -1 m n  0 0 0 "#") out))
      )
  )

;Funcion que une la lista de obstaculos
(define (consObstaculos n m seed out)
  (if (= n -1)
      out
      (consObstaculos (- n 1) m seed (append (consObs n m seed (list)) out))
      )
  )

;################################################################

;################################################################
;Selectores TDA Scene

(define (getObjetos scene)
  (car scene)
  )

(define (getCantidadObjetos scene)
  (caddr scene)
  )

(define (getN scene)
  (cadddr scene)
  )

(define (getM scene)
  (cadddr scene)
  )

(define (getObstaculos scene)
  (cadr  scene)
  )

(define (getJugadores scene)
  (auxiliarJ (car scene) (list) (getCantidadObjetos scene) 0)
  )

(define (auxiliarJ objetos out e cont)
  (if (= cont (* e 2))
      out
      (if (< cont e)
          (auxiliarJ (cdr objetos) out e (+ cont 1))
          (auxiliarJ (cdr objetos) (cons (car objetos) out) e (+ cont 1))
          )
      )
  )

(define (getEnemigos scene)
  (auxiliarE (car scene) (list) (getCantidadObjetos scene) 0)
  )

(define (auxiliarE objetos out e cont)
  (if (= cont (* e 2))
      out
      (if (< cont e)
          (auxiliarE (cdr objetos) (cons (car objetos) out) e (+ cont 1))
          (auxiliarE (cdr objetos) out e (+ cont 1))
          )
      )
  )

;################################################################

;################################################################
;Selectores TDA Objeto

(define (getNombre objeto)
  (car objeto)
  )

(define (getId objeto)
  (cadr objeto)
  )

(define (getX objeto)
  (caddr objeto)
  )

(define (getY objeto)
  (cadddr objeto)
  )

(define (getAngulo objeto)
  (cadr (cdddr objeto))
  )

(define (getDir objeto)
  (caddr (cdddr objeto))
  )

(define (getPasos objeto)
  (cadddr (cdddr objeto))
  )

(define (getCaracter objeto)
  (cadr (cdddr (cdddr objeto)))
  )

;################################################################

;################################################################
;Modificadores TDA Objeto

(define (setNombre objeto nombre)
  (setNombre2 objeto nombre 0 (list))
  )

(define (setNombre2 objeto nombre cont cabeza)
  (if (= cont 0)
      (if (string? nombre)
          (cons nombre (cdr objeto))
          #f
          )
      #f
      )
  )

(define (setId objeto id)
  (setId2 objeto id 0 (list))
  )

(define (setId2 objeto id cont cabeza)
  (if (= cont 1)
      (if (number? id)
          (append (append cabeza (list id)) (cdr objeto))
          #f
          )
      (setId2 (cdr objeto) id (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setM objeto m)
  (setM2 objeto m 0 (list))
  )

(define (setM2 objeto m cont cabeza)
  (if (= cont 2)
      (if (number? m)
          (append (append cabeza (list m)) (cdr objeto))
          #f
          )
      (setM2 (cdr objeto) m (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setN objeto n)
  (setN2 objeto n 0 (list))
  )

(define (setN2 objeto n cont cabeza)
  (if (= cont 3)
      (if (number? n)
          (append (append cabeza (list n)) (cdr objeto))
          #f
          )
      (setN2 (cdr objeto) n (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setAngulo objeto angulo)
  (setAngulo2 objeto angulo 0 (list))
  )

(define (setAngulo2 objeto angulo cont cabeza)
  (if (= cont 4)
      (if (number? angulo)
          (append (append cabeza (list angulo)) (cdr objeto))
          #f
          )
      (setAngulo2 (cdr objeto) angulo (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setDir objeto dir)
  (setDir2 objeto dir 0 (list))
  )

(define (setDir2 objeto dir cont cabeza)
  (if (= cont 5)
      (if (number? dir)
          (append (append cabeza (list dir)) (cdr objeto))
          #f
          )
      (setDir2 (cdr objeto) dir (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setPasos objeto pasos)
  (setPasos2 objeto pasos 0 (list))
  )

(define (setPasos2 objeto pasos cont cabeza)
  (if (= cont 6)
      (if (number? pasos)
          (append (append cabeza (list pasos)) (cdr objeto))
          #f
          )
      (setPasos2 (cdr objeto) pasos (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

(define (setCaracter objeto caracter)
  (setCaracter2 objeto caracter 0 (list))
  )

(define (setCaracter2 objeto caracter cont cabeza)
  (if (= cont 7)
      (if (string? caracter)
          (append (append cabeza (list caracter)) (cdr objeto))
          #f
          )
      (setCaracter2 (cdr objeto) caracter (+ cont 1) (append cabeza (list (car objeto))))
      )
  )

;################################################################

;################################################################
;se debe usar recursion contraria a createScene (cola)

(define (check scene e)
  (if (and (= (length (getJugadores scene)) e) (= (length (getEnemigos scene)) e))
      #t
      #f
      )
  )

(define (checkScene scene)
  (check scene (getCantidadObjetos scene))
  )

;(checkScene (createScene 12 15 5 0 789))
;(checkScene (createScene 10 10 5 0 457))
;(checkScene (createScene 10 8 5 0 112))

;################################################################

;################################################################
;funcion debe estar currificada

;member desplazamiento del personaje (numero del personaje que se controlara)
;move izquiera o derecha (cantidad de pasos)
;angle angulo del disparo
;tf funcion de trayectoria (parabolica, aceleracion, teledirigido, etc)

;orden: member -> move -> angle -> tf -> enemigo

;la funcion debe actualizar el mapa del juego y el estado del juego (Victory, Defeat, Draw)


(define (play scene)
  (lambda (member)
    (lambda (move)
      (lambda (tf)
        (lambda (angle)
          (lambda (seed)
            #t
            )
          )
        )	
      )
    )
  )
	

;################################################################

;################################################################
;funcion no debe estar currificada

;member desplazamiento del personaje (numero del personaje que se controlara)
;move izquiera o derecha (cantidad de pasos)
;angle angulo del disparo
;tf funcion de trayectoria (parabolica, aceleracion, teledirigido, etc)
;t tiempo transcurrido

;orden: member -> move -> angle -> tf -> enemigo

;la funcion debe actualizar el mapa del juego y el estado del juego (Victory, Defeat, Draw)


(define (playLazy scene member move t tf angle seed)
  #t
  )
	

;################################################################

;################################################################
;no usar display ni write dentr de esta funcion

(define (sceneToString scene)
  (crearStr (getN scene) (getM scene) (getJugadores scene) (getEnemigos scene) (getObstaculos scene) scene 0 0 "")
  )

;(display (sceneToString (createScene 10 10 3 0 4655465)))
;(display (sceneToString (createScene 5 10 5 0 54567)))
;(display (sceneToString (createScene 8 15 3 0 457889)))


(define (crearStr n m jugadores enemigos obstaculos scene contN contM str)
  (if (or (= n 0) (= m 0))
      #f
      (if (= contN n)
          str
          (crearStr n m jugadores enemigos obstaculos scene (+ 1 contN) contM (string-append (crearFila m jugadores enemigos obstaculos scene contN contM str) "\n" str ))
          )
      )
  )

(define (crearFila m jugadores enemigos obstaculos scene contN contM str)
  (begin
    (display "N actual:")
    (display contN)
    (display "\n")
    (display "M actual:")
    (display contM)
    (display "\n")
    (display "str\n")
    (display str)
    (display "\n")
    (if (= contM m)
      str
      (if (buscarJugadores jugadores contN contM 0)
          (crearFila m jugadores enemigos obstaculos scene contN (+ 1 contM) (string-append str (buscarJugadores jugadores contN contM 1)))
          (if (buscarEnemigos enemigos contN contM 0)
              (crearFila m jugadores enemigos obstaculos scene contN (+ 1 contM) (string-append str (buscarEnemigos enemigos contN contM 1)))
              (if (buscarObstaculos obstaculos contN contM 0)
                  (crearFila m jugadores enemigos obstaculos scene contN (+ 1 contM) (string-append str (buscarObstaculos obstaculos contN contM 1)))
                  (crearFila m jugadores enemigos obstaculos scene contN (+ 1 contM) (string-append str "."))
                  )
              )
          )
      )
    )
  )


(define (buscarJugadores jugadores m n id)
  (if (null? jugadores)
      #f
      (if (and (= (getX (car jugadores)) m) (= (getY (car jugadores)) n))
          (if (= id 0)
              #t
              (getCaracter (car jugadores))
              )
          (buscarJugadores (cdr jugadores) m n id)
          )
      )
  )

(define (buscarEnemigos enemigos m n id)
  (if (null? enemigos)
      #f
      (if (and (= (getX (car enemigos)) m) (= (getY (car enemigos)) n))
          (if (= id 0)
              #t
              (getCaracter (car enemigos))
              )
          (buscarEnemigos (cdr enemigos) m n id)
          )
      )
  )


(define (buscarObstaculos obstaculos m n id)
  (if (null? obstaculos)
      #f
      (if (and (= (getX (car obstaculos)) m) (= (getY (car obstaculos)) n))
          (if (= id 0)
              #t
              (getCaracter (car obstaculos))
              )
          (buscarObstaculos (cdr obstaculos) m n id)
          )
      )
  )

;################################################################

