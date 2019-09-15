#!r6rs

;Aqui se importa r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6))
        (rnrs r5rs (6)))


;################################################################
;TDA scene

;el equipo del jugador (n jugadores)
;el equipo de la CPU (n jugadores)
;los obstaculos
;los proyectiles

;################################################################

;################################################################
;TDA jugador

;coordenada X
;coordenada Y
;angulo
;direccion (izq o drch)
;pasos

;################################################################

;################################################################
;TDA proyectil

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
;Funcion que entrega un numero en donde se movera el disparo en funcon del angulo de entrada

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

(define (createScene n m e d seed)
  (list (consJugador n m -90 1 0 seed 0) (consEnemigos n m e 90 0 0 seed (list))) ;agregar obstaculos al escenario 
  )

;angulo inicial -90 a la derecha (1) y pasos 0
(define (consJugador n m angulo dir pasos seed id)
  (if (= id 0)
      (list "Jugador" id (car (getListaRandom 1 (* seed 2) m)) (car (getListaRandom 1 (* seed 3) n)) angulo dir pasos)
      (list "Enemigo" id (car (getListaRandom 1 (* seed 2) m)) (car (getListaRandom 1 (* seed 3) n)) angulo dir pasos)
      )
  )


(define (consEnemigos n m e angulo dir pasos seed out)
  (if (= e 0)
      out
      (consEnemigos n m (- e 1) angulo dir pasos (* seed 2) (cons (consJugador n m angulo dir pasos seed e) out))
      )
  )

;################################################################

;################################################################
;se debe usar recursion contraria a createScene (cola)

(define (checkSpace scene)
  #t
  )

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
  #t
  )