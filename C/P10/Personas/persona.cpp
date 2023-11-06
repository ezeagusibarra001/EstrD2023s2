#include <iostream>
#include "persona.h"

using namespace std;

struct PersonaSt
{
    string nombre;
    int edad;
};

/// Recordatorio: (*loquesea) representa
/// lo que hay en esa direccion de memoria

Persona consPersona(string nombre, int edad)
{
    PersonaSt *p = new PersonaSt;

    p->nombre = nombre;
    p->edad = edad;

    return p;
}

void imprimirP(Persona p)
{
    cout << "Persona " << p << " {" << endl;
    cout << "  nombre: " << p->nombre << endl;
    cout << "  edad: " << p->edad << endl;
    cout << "}" << endl;
}

string nombre(Persona p)
{

    return p->nombre;
}

int edad(Persona p)
{
    return p->edad;
}

void crecer(Persona p)
{
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p)
{
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2)
{
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2)
{
    if (esMayorQueLaOtra(p1, p2))
    {
        return p1;
    }
    else
    {
        return p2;
    }
}