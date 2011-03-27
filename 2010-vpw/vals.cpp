#include <iostream>
#include <string>
#include <vector>
using namespace std;

void vals()
{
    bool links_omlaag[256];
    bool rechts_omlaag[256];
    bool links_omhoog[256];
    bool rechts_omhoog[256];
    bool resultaat[256];
    bool gelezen[256];
    bool omhoog_of_omlaag = false;

    for(char c = 'a'; c <= 'z'; c++) {
        links_omlaag[c] = rechts_omlaag[c] = links_omhoog[c] = rechts_omhoog[c] = false;
        resultaat[c] = true;
        gelezen[c] = false;
    }

    int n;
    cin >> n;

    vector<pair<string, string> > omlagen;
    vector<pair<string, string> > omhogen;

    for(int i = 0; i < n; i++) {
        string links, rechts, code;
        cin >> links >> rechts >> code;

        for(int j = 0; j < links.size(); j++) gelezen[links[j]] = true;
        for(int j = 0; j < rechts.size(); j++) gelezen[rechts[j]] = true;

        if(code == "evenwicht") {
            for(int j = 0; j < links.size(); j++) resultaat[links[j]] = false;
            for(int j = 0; j < rechts.size(); j++) resultaat[rechts[j]] = false;
        } else if(code =="omlaag") {
            omhoog_of_omlaag = true;
            omlagen.push_back(make_pair(links, rechts));
        } else {
            omhoog_of_omlaag = true;
            omhogen.push_back(make_pair(links, rechts));
        }
    }

    for(int i = 0; i < omlagen.size(); i++) {
        string links, rechts;
        for(int j = 0; j < omlagen[i].first.size(); j++) {
            if(resultaat[omlagen[i].first[j]]) links += omlagen[i].first[j];
        }
        for(int j = 0; j < omlagen[i].second.size(); j++) {
            if(resultaat[omlagen[i].second[j]]) rechts += omlagen[i].second[j];
        }

        if(links.size() == 0 && rechts.size() == 0) {
            cout << "Inconsistente gegevens." << endl;
            return;
        }

        for(int j = 0; j < links.size(); j++) links_omlaag[links[j]] = true;
        for(int j = 0; j < rechts.size(); j++) rechts_omlaag[rechts[j]] = true;
    }

    for(int i = 0; i < omhogen.size(); i++) {
        string links, rechts;
        for(int j = 0; j < omhogen[i].first.size(); j++) {
            if(resultaat[omhogen[i].first[j]]) links += omhogen[i].first[j];
        }
        for(int j = 0; j < omhogen[i].second.size(); j++) {
            if(resultaat[omhogen[i].second[j]]) rechts += omhogen[i].second[j];
        }

        if(links.size() == 0 && rechts.size() == 0) {
            cout << "Inconsistente gegevens." << endl;
            return;
        }

        for(int j = 0; j < links.size(); j++) links_omhoog[links[j]] = true;
        for(int j = 0; j < rechts.size(); j++) rechts_omhoog[rechts[j]] = true;
    }

    /* Check for consistence. */
    for(char c = 'a'; c <= 'z'; c++) {
        if(links_omhoog[c] && rechts_omhoog[c] ||
                links_omlaag[c] && rechts_omlaag[c] ||
                links_omlaag[c] && links_omhoog[c] ||
                rechts_omlaag[c] && rechts_omhoog[c]) {
            cout << "Inconsistente gegevens." << endl;
            return;
        }
    }

    /* Check for result. */
    int count = 0;
    char vals_geld = 'a';
    for(char c = 'a'; c <= 'z'; c++) {
        if(resultaat[c] && gelezen[c]) {
            count++;
            vals_geld = c;
        }
    }

    if(count > 1) {
        cout << "Te weining gegevens." << endl;
        return;
    } else if(count == 0) {
        cout << "Inconsistente gegevens." << endl;
        return;
    }

    if(links_omhoog[vals_geld] || rechts_omlaag[vals_geld]) {
        cout << "Het valse geldstuk " << vals_geld << " is zwaarder." << endl;
    } else if(links_omlaag[vals_geld] || rechts_omhoog[vals_geld]) {
        cout << "Het valse geldstuk " << vals_geld << " is lichter." << endl;
    } else {
        cout << "Te weining gegevens." << endl;
    }
}

int main(int argc, char **argv)
{
    int n;
    cin >> n;
    for(int i = 0; i < n; i++) {
        vals();
    }
}
