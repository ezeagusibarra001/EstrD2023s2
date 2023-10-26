#include <iostream>
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

int main()
{
    //printN(3, "hola");
    //iPrintN(3, "chau");
    //cuentaRegresiva(4);
    iCuentaRegresiva(4);
}