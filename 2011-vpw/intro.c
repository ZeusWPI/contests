#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    int min, max, cases, i;
    fscanf(stdin, "%d", &cases);
    fscanf(stdin, "%d", &min);
    max = min;
    for(i = 1; i < cases; i++) {
        int x;
        fscanf(stdin, "%d", &x);
        min = min < x ? min : x;
        max = max > x ? max : x;
    }
    
    printf("%d %d\n", min, max);
    return 0;
}
