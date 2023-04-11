## Investigacion
## Functor en haskell

Un functor es un tipo de dato que nos permite aplicar una funcion a los valores que estan contenidos en el y produciendo un nuevo valor del mismo tipo.
```
Un functor encapsula un valor y transforma ese valor usando una funcion
```
## Definicion
Un functor se define con la clase "Functor" y su funcion principal es la fmap. 
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Donde "f" es el tipo del functor y "a" y "b" son tipos de datos. La función fmap toma una función de tipo (a -> b) y un valor del functor f a, y devuelve un valor del functor f b, aplicando la función a los valores contenidos en el contexto del functor.
## Conceptos que tenemos que conocer
- fmap: Es una función que toma una función y un functor, y aplica la función al functor.
```
fmap :: Functor f => (a -> b) -> f a -> f b
```
- map: Es una función que toma una función y una lista, y aplica la función a cada elemento de la lista.
```
map :: (a -> b) -> [a] -> [b]
```
# Diferencias entre fmap y map

- map se utiliza en listas y fmap se utiliza con functor
- fmap es generico y map esta limitado a listas 
- fmap como es un contenedor este devuelve el valor original convertido y las listas siempre devuelven un nuevo valor

##  Ideas

- Abstracción y reusabilidad: Puedo abstraer la logica del parseo para poder reutilizarlo 
- Flexibilidad: Puede implementar una nueva instancia de functor si quiero cambiar la entrada del JsonValue 
- Codigo  Limpio: Puedo volver el codigo mas limpio