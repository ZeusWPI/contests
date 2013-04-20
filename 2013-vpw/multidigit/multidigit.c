#include <stdio.h>
#include <stdlib.h>

void put_replicates(int digit, int *costs, int costs_size) {
    int i;
    int cost = 1;
    for(i = digit; i < costs_size; i = i * 10 + digit) {
        costs[i] = cost;
        cost++;
    }
}

void put_costs(int *costs, int costs_size) {
    int i;
    int j;

    for(i = 1; i < costs_size; i++) {
        /* Only if we can make this number... */
        if(costs[i] != 0) {
            for(j = 1; j <= i; j++) {
                if(costs[j] != 0) {
                    int cost = costs[i] + costs[j];
                    int add = i + j;
                    int mul = i * j;

                    if(add < costs_size &&
                            (costs[add] == 0 || costs[add] > cost)) {
                        costs[add] = cost;
                    }

                    if(mul < costs_size &&
                            (costs[mul] == 0 || costs[mul] > cost)) {
                        costs[mul] = cost;
                    }
                }
            }
        }
    }
}

void test_case(void) {
    int digits_size;
    int *digits;
    int goal;
    int *costs;
    int i;

    scanf("%d", &digits_size);
    digits = calloc(digits_size, sizeof(int));
    for(i = 0; i < digits_size; i++) scanf("%d", &digits[i]);
    scanf("%d", &goal);
    costs = calloc(goal + 1, sizeof(int));

    for(i = 0; i < digits_size; i++) put_replicates(digits[i], costs, goal + 1);
    put_costs(costs, goal + 1);
    printf("%d\n", costs[goal]);

    free(digits);
    free(costs);
}

int main(int argc, char **argv) {
    int cases, i;
    scanf("%d", &cases);
    for(i = 0; i < cases; i++) test_case();
    return 0;
}
