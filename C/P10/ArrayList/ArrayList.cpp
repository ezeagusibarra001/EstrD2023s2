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

ArrayList newArrayListWith(int capacidad)
{
    ArrayListSt *l = new ArrayListSt;
    l->cantidad = 0;
    l->capacidad = capacidad;
    l->elementos = new int[l->capacidad];
    return l;
}

int lengthAL(ArrayList xs)
{
    return xs->cantidad;
}

int get(int i, ArrayList xs)
{
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs)
{
    xs->elementos[i] = x;
    xs->cantidad += 1;
}

void resize(int capacidad, ArrayList xs)
{
    int *nuevoArray = new int[capacidad];

    int i = 0;
    while (i < xs->cantidad && i < capacidad)
    {
        nuevoArray[i] = xs->elementos[i];
        i += 1;
    }
    delete xs->elementos;

    xs->elementos = nuevoArray;
    xs->cantidad = i;
    xs->capacidad = capacidad;
}