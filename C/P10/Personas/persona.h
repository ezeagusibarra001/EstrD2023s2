#include <iostream>

using namespace std;

struct PersonaSt;

typedef PersonaSt* Persona;

//Crea una persona
Persona consPersona(string nombre, int edad);

//Imprime la persona en cuestion
void imprimirP (Persona p);

//Devuelve el nombre de una persona
string nombre(Persona p);

//Devuelve la edad de una persona
int edad(Persona p);

//Aumenta en uno la edad de la persona.
void crecer(Persona p);

//Modifica el nombre una persona.
void cambioDeNombre(string nombre, Persona p);

//Dadas dos personas indica si la primera es mayor que la segunda.
bool esMayorQueLaOtra(Persona p1, Persona p2);

//Dadas dos personas devuelve a la persona que sea mayor.
Persona laQueEsMayor(Persona p1, Persona p2);