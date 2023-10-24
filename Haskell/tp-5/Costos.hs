{-
Notación de Costo Constante (O(1)):

    Descripción: Indica que el tiempo de ejecución (o uso de memoria) del algoritmo no depende del tamaño de la entrada; es constante.
    Ejemplo: Acceso a un elemento específico en un arreglo o lista enlazada.

Notación de Costo Lineal (O(n)):

    Descripción: Indica que el tiempo de ejecución (o uso de memoria) del algoritmo es proporcional al tamaño de la entrada.
    Ejemplo: Recorrer todos los elementos de una lista o arreglo una vez.

Notación de Costo Cuadrático (O(n^2)):

    Descripción: Indica que el tiempo de ejecución (o uso de memoria) del algoritmo es proporcional al cuadrado del tamaño de la entrada.
    Ejemplo: Algoritmos que contienen dos bucles anidados, donde cada bucle recorre la entrada completa.

-}

head' :: [a] -> a
head' (x:xs) = x

-- O(1)