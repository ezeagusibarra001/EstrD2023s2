#include <iostream>

using namespace std;

struct ArrayListSt;

typedef ArrayListSt* ArrayList;

// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
ArrayList newArrayList();

//Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad);