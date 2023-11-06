#include <iostream>
#include "ArrayList.h"

using namespace std;

int main()
{
    ArrayList l1 = newArrayList();
    cout << l1 << endl;
    cout << lengthAL(l1) << endl;
    cout << get(2, l1) << endl;
    set(2, 69, l1);
    cout << get(2, l1) << endl;
    return 0;
}