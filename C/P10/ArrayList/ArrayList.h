#include <iostream>

using namespace std;

struct ArrayListSt;

typedef ArrayListSt* ArrayList;

// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
ArrayList newArrayList();

//Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad);

// Devuelve la cantidad de elementos existentes
int lengthAL(ArrayList xs);

// Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs);

// Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs);

// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs);