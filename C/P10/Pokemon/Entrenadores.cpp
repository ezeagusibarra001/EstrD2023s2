#include <iostream>
#include "Pokemones.h"
#include "Entrenadores.h"
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};