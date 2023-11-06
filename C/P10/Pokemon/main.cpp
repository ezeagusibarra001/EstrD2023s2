#include <iostream>
#include "Entrenadores.h"

using namespace std;

int main()
{
    /// Testeos Pokemones.
    Pokemon p1 = consPokemon("Fuego");
    Pokemon p2 = consPokemon("Agua");
    Pokemon p3 = consPokemon("Planta");

    imprimirP(p1);
    imprimirP(p2);

    cout << "" << endl;

    cout << tipoDePokemon(p1) << endl;
    cout << tipoDePokemon(p2) << endl;

    cout << "" << endl;

    perderEnergia(50, p1);
    perderEnergia(90, p2);

    cout << "" << endl;

    cout << energia(p1) << endl;
    cout << energia(p2) << endl;

    cout << "" << endl;

    return 0;
}