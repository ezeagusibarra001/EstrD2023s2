#include <iostream>
#include "ArrayList.h"

using namespace std;

struct ArrayListSt
{
    int cantidad;   // cantidad de elementos
    int *elementos; // array de elementos
    int capacidad;  // tamaño del array
};

ArrayList newArrayList()
{
    ArrayListSt *l = new ArrayListSt;
    l->cantidad = 0;
    l->capacidad = 16;
    l->elementos = new int[l->capacidad];
    return l;
}

ArrayList newArrayListWith(int capacidad){
    ArrayListSt *l = new ArrayListSt;
    l->cantidad = 0;
    l->capacidad = capacidad;
    l->elementos = new int[l->capacidad];
    return l;
}