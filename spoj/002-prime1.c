#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *sieve_create(unsigned long size) {
    unsigned long i, j;
    char *sieve;

    sieve = malloc(size * sizeof(char));
    memset(sieve, 1, size * sizeof(char));

    /* Known */
    if(size > 0) sieve[0] = 0;
    if(size > 1) sieve[1] = 0;

    i = 2;
    while(i < size) {
        if(sieve[i]) {
            for(j = 2 * i; j < size; j += i) sieve[j] = 0;
        }

        i++;
    }

    return sieve;
}

void sieve_free(char *sieve) {
    free(sieve);
}

void primes_in_range(char *sieve, unsigned long sieve_size,
        unsigned long m, unsigned long n) {
    char *range;
    unsigned long range_size;
    unsigned long prime, i;

    range_size = n - m + 1;
    range = malloc(range_size * sizeof(char));
    memset(range, 1, range_size * sizeof(char));

    /* Known */
    if(m <= 1 && 1 <= n) range[1 - m] = 0;

    prime = 2;
    while(prime < sieve_size && prime * prime <= n) {
        if(sieve[prime]) {
            i = m % prime ? prime - m % prime : 0;
            if(m + i == prime) i += prime; /* For really small numbers */

            while(i < range_size) {
                range[i] = 0;
                i += prime;
            }
        }

        prime++;
    }

    for(i = 0; i < range_size; i++) {
        if(range[i]) printf("%lu\n", i + m);
    }

    free(range);
}

int main(int argc, char **argv) {
    int cases;

    unsigned long i;
    unsigned long m, n;

    /* Problem-specific */
    unsigned long sieve_size = 31623;
    char *sieve = sieve_create(sieve_size);

    scanf("%d\n", &cases);
    for(i = 0; i < cases; i++) {
        scanf("%lu %lu\n", &m, &n);
        primes_in_range(sieve, sieve_size, m, n);
        printf("\n");
    }

    sieve_free(sieve);

    return 0;
}
