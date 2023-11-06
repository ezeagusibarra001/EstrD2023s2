#include <iostream>
#include <string.h>
using namespace std;

// Propósito: imprime n veces un string s
void printN(int n, string s)
{
    if (n != 0)
    {
        cout << s << endl;
        printN(n - 1, s);
    }
}

void iPrintN(int n, string s)
{
    while (n != 0)
    {
        cout << s << endl;
        n = n - 1;
    }
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n)
{
    if (n != 0)
    {
        cout << n << endl;
        cuentaRegresiva(n - 1);
    }
    else
    {
        cout << n << endl;
    }
}

void iCuentaRegresiva(int n)
{
    while (n != 0)
    {
        cout << n << endl;
        n = n - 1;
    }
    cout << n << endl;
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea
void desdeCeroHastaN(int n)
{
    if (n > 0)
    {
        desdeCeroHastaN(n - 1);
    }
    cout << n << endl;
}

void iDesdeCeroHastaN(int n)
{
    for (int i = 0; i <= n; i++)
    {
        cout << i << endl;
    }
}

// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++)
int mult(int n, int m)
{
    if (n == 0)
    {
        return 0;
    }
    return m + mult(n - 1, m);
}

int iMult(int n, int m)
{
    int res = 0;
    for (int i = 0; i < m; i++)
    {
        res += n;
    }
    return res;
}

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char
void primerosN(int n, string s)
{
    if (n != 0)
    {
        primerosN(n - 1, s);
    }
    cout << s[n] << endl;
}

void iPrimerosN(int n, string s)
{
    for (int i = 0; i < n; i++)
    {
        cout << s[i] << endl;
    }
}

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s)
{
    if (s[0] == '\0')
    {
        return false;
    }
    else
    {
        return s[0] == c || pertenece(c, s.substr(1));
    }
}

bool iPertenece(char c, string s)
{
    int i = 0;
    while (s[i] != '\0' && s[i] != c)
    {
        i += 1;
    }
    return s[i] != '\0' && s[i] == c;
}

bool unoSi(bool c)
{
    if (c)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

// Propósito: devuelve la cantidad de apariciones de un char c en el string s
int apariciones(char c, string s)
{
    if (s[0] == '\0')
    {
        return 0;
    }
    else
    {
        return unoSi(s[0] == c) + apariciones(c, s.substr(1));
    }
}

int iApariciones(char c, string s)
{
    int cant = 0;
    for (int i = 0; s[i] != '\0' ; i++)
    {
        cant += unoSi(s[i] == c);
    }
    return cant;
}

int main()
{
    // printN(3, "hola");
    // iPrintN(3, "chau");
    // cuentaRegresiva(4);
    //  iCuentaRegresiva(4);
    //  desdeCeroHastaN(2);
    // iDesdeCeroHastaN(4);
    // cout << iMult(4, 4) << endl;
    // primerosN(2, "hola");
    // if (iPertenece('x', "casa"))
    // {
    //     cout << "ES VERDADERO" << endl;
    // }
    // else
    // {
    //     cout << "FALSO" << endl;
    // }
    // cout << apariciones('a', "adasa") << endl;
    cout << iApariciones('a', "adasa") << endl;
}