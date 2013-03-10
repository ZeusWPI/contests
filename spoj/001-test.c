#include <stdio.h>

int main(int argc, char **argv) {
    int number;

    scanf("%d\n", &number);
    while(number != 42) {
        printf("%d\n", number);
        scanf("%d\n", &number);
    }

    return 0;
}
