#include <stdio.h>
#include <stdlib.h>

void test_case(void) {
    int counterweights_size;
    int *counterweights;
    int weight;
    int i, j;
    int success = 0;

    scanf("%d", &counterweights_size);
    counterweights = calloc(counterweights_size, sizeof(int));
    for(i = 0; i < counterweights_size; i++) scanf("%d", &counterweights[i]);
    scanf("%d", &weight);

    for(i = 0; i + 1 < counterweights_size; i++) {
        for(j = i + 1; j < counterweights_size; j++) {
            success |= weight == counterweights[i] + counterweights[j];
        }
    }

    printf("%d %s\n", weight, success ? "JA" : "NEEN");
    free(counterweights);
}

int main(int argc, char **argv) {
    int cases, i;
    scanf("%d", &cases);
    for(i = 0; i < cases; i++) test_case();
    return 0;
}
