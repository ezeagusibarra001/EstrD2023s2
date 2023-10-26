#include <iostream>
using namespace std;

int f1()
{
    int x = 0;
    int y = 2;
    x = x + y;
    // 2
    return x;
}

int f2()
{
    int x = 0;
    int y = 0;
    while (y < 5)
    {
        x += y;
        y++;
    }
    return x;
}

int f3()
{
    int y = 0;
    bool b = true;
    while (b)
    {
        y++;
        b = !b;
    }
    return y;
}

struct Par
{
    int x;
    int y;
};

// Propósito: construye un par
Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p){
    return p.x;
}
// Propósito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    if(p.x > p.y){
        return p.x;
    }else{
        return p.y;
    };
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    Par p2;
    p2.x = p.y;
    p2.y = p.x;
    return p2;
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    Par p;
    p.x = n / m;
    p.y = n % m;
    return p;
}

int main()
{
    Par p = consPar(2, 5);
    Par p2 = swap(p);
    Par p3 = divisionYResto(5, 2);
    // cout << "F1: " << f1() << endl;
    // cout << "F2: " << f2() << endl;
    // cout << "F3: " << f3() << endl;
    cout << "x: " << fst(p) << endl;
    cout << "y: " << snd(p) << endl;
    cout << "max: " << maxDelPar(p) << endl;
    cout << "swap x: " << fst(p2) << endl;
    cout << "swap y: " << snd(p2) << endl;
    cout << "div x: " << fst(p3) << endl;
    cout << "div y: " << snd(p3) << endl;

}