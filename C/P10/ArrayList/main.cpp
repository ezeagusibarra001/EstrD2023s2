#include <iostream>
#include "ArrayList.h"

using namespace std;

int main()
{
    ArrayList l1 = newArrayList();
    for (int i = 0; i < 10; i++)
    {
        set(i, i, l1);
    }
    cout <<"ANTES DEL RSIZE: "<< get(8, l1) << endl;
    resize(12, l1);
    cout << "ESTE" << get(4, l1) << endl;
    return 0;
}