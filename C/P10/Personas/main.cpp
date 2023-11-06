#include <iostream>
#include "persona.h"

using namespace std;

int main()
{
    Persona p1 = consPersona("Ezequiel", 22);
    Persona p2 = consPersona("Carla", 20);
    imprimirP(p1);
    cout << nombre(p1) << endl;
    cout << edad(p1) << endl;
    crecer(p1);
    imprimirP(p1);
    cambioDeNombre("Lucia", p1);
    imprimirP(p1);
    cout << esMayorQueLaOtra(p1, p2) << endl;
    imprimirP(laQueEsMayor(p1, p2));
    return 0;
}