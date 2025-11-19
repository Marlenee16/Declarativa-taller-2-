#lang racket

;;Ejercicio 1 – Contar elementos positivos en una lista

(define (contar-positivos lista)
  (length
   (filter (lambda (x) (> x 0))
           lista)))

(contar-positivos '(3 -2 7 0 -5 9))  


;;Ejercicio 2 – Lista de cuadrados pares

(define (cuadrados-pares lista)
  (map (lambda (x) (* x x))            
       (filter (lambda (x) (even? x))  
               lista)))

(cuadrados-pares '(1 2 3 4 5 6 7 8)) 


;;Ejercicio 3 – Factorial con recursión simple

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)  
 

 ;;Ejercicio 4 – Elevar cada número al cubo

(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))

(cubos '(2 3 4))


;;Ejercicio 5 – Sumar elementos impares

(define (sumar-impares lista)
  (foldl + 0 (filter odd? lista)))

(sumar-impares '(1 2 3 4 5 6 7)) 


;; Ejercicio 6 – Determinar si una lista contiene números negativos

(define (hay-negativos? lista)
  (ormap (lambda (x) (< x 0)) lista))

(hay-negativos? '(5 9 -3 2))    


;;Ejercicio 7 – Suma acumulada

(define (suma-acumulada lista)
  (reverse
   (foldl
    (lambda (x acc)
      (cons (+ x (if (null? acc) 0 (first acc))) acc))
    '()
    lista)))

(suma-acumulada '(1 2 3 4))   


;;Ejercicio 8 – Concatenar cadenas de texto en una lista

(define (concatenar-cadenas lista)
  (foldl (lambda (elem acc)
           (string-append acc elem)) 
         ""
         lista))

(displayln (concatenar-cadenas '("Hola" " " "Mundo")))


;;Ejercicio 9 – Dobles de los números mayores que 5

(define (dobles-mayores-a-5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

(displayln (dobles-mayores-a-5 '(3 6 8 2 10)))


;;Ejercicio 10 – Invertir el orden de una lista

(define (invertir lista)
  (foldl (lambda (elem acc)
           (cons elem acc))   
         '()                   
         lista))

(displayln (invertir '(1 2 3 4)))


;;Ejercicio 11 – Crear una función que reciba una función como parámetro

(define (aplicar-funcion f lista)
  (map f lista))

(define (cuadrado x)
  (* x x))

(displayln (aplicar-funcion cuadrado '(1 2 3 4)))


;;Ejercicio 12 – combinación de múltiples funciones

(define (promedio-mayores-a-5 lista)
  (define mayores (filter (lambda (x) (> x 5)) lista))
  (define suma (foldl + 0 mayores))
  (define cantidad (length mayores))
  (exact->inexact (/ suma cantidad)))

(displayln (promedio-mayores-a-5 '(3 8 10 4 9 2 7)))  