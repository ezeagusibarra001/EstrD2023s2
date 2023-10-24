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

int main()
{
    cout << "F1: " << f1() << endl;
    cout << "F2: " << f2() << endl;
    cout << "F3: " << f3() << endl;
}