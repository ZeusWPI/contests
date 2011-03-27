#include <iostream>
#include <string>
#include <map>
using namespace std;

int main(int argc, char ** argv)
{
    int n, k;
    map<string, string> dict;
    cin >> n >> k;

    for(int i = 0; i < n; i++) {
        string gia, nl;
        cin >> gia >> nl;
        dict[gia] = nl;
    }

    for(int i = 0; i < k; i++) {
        string gia;
        cin >> gia;
        map<string, string>::iterator result = dict.find(gia);
        if(result == dict.end()) cout << "???" << endl;
        else cout << result->second << endl;
    }

    return 0;
}
