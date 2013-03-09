#include <stdio.h>

/* Simple power operator */
unsigned int power(unsigned int base, unsigned int exp) {
    unsigned int result = 1;
    while(exp--) result *= base;
    return result;
}

/* Amount of numbers with n digits */
unsigned int n_digits(unsigned int n) {
    if(n == 0) {
        return 0;
    } else {
        return power(10, n) - power(10, n - 1);
    }
}

/* The nth digit in the row */
int nth_digit(unsigned int n) {
    char buffer[512];
    unsigned int number = 0;
    unsigned int digits = 1;
    unsigned int range = 0;

    /* We count 0-based like normal people */
    n--;

    /* First find the number of digits in the number */
    range = n_digits(digits) * digits;
    while(range < n) {
        n -= range;
        digits++;
        range = n_digits(digits) * digits;
    }

    /* Now find out the actual number */
    number = power(10, digits - 1);
    number += n / digits;

    /* Find the digit in the number, the easy way */
    sprintf(buffer, "%u", number);
    return (int) (buffer[n % digits] - '0');
}

/* Main function */
int main(int argc, char **argv) {
    int cases;
    int i;
    unsigned int nth;

    scanf("%d\n", &cases);
    for(i = 0; i < cases; i++) {
        scanf("%u\n", &nth);
        printf("%d\n", nth_digit(nth));
    }

    return 0;
}
