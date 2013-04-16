#include <stdio.h>
#include <stdlib.h>

#define BUFFER_SIZE 1000000

int read_big_int(char *buffer, int buffer_size) {
    int i;
    int c;
    c = fgetc(stdin);
    while(c != EOF && c != '\n') {
        buffer[i] = (char) c;
        i++;
        c = fgetc(stdin);
    }

    return i;
}

int palindrome_is_greater(char *buffer, int buffer_size) {
    int i, j;

    j = buffer_size / 2;
    i = (buffer_size % 2 == 0) ? j - 1 : j;

    while(i >= 0 && buffer[i] == buffer[j]) {
        i--;
        j++;
    }

    if(i < 0) return 0; /* Equal */
    return buffer[i] > buffer[j];
}

void write(char *buffer, int buffer_size) {
    fwrite(buffer, sizeof(char), buffer_size, stdout);
}

void write_reversed(char *buffer, int buffer_size) {
    int i;
    for(i = buffer_size - 1; i >= 0; i--) {
        fputc(buffer[i], stdout);
    }
}

/* Returns carry */
int increment(char *buffer, int buffer_size) {
    int i = buffer_size - 1;

    while(i >= 0) {
        if(buffer[i] == '9') {
            buffer[i] = '0';
            i--;
        } else {
            buffer[i]++;
            return 0;
        }
    }

    return 1;
}

void test_case(void) {
    char *buffer = malloc(BUFFER_SIZE);
    int size;
    int even;
    int half;
    int carry;
    int i;

    size = read_big_int(buffer, BUFFER_SIZE);
    even = size % 2 == 0;
    half = even ? size / 2 : size / 2 + 1;

    if(!palindrome_is_greater(buffer, size)) {
        carry = increment(buffer, half);
    } else {
        carry = 0;
    }

    if(carry) {
        printf("1");
        for(i = 1; i < size; i++) printf("0");
        printf("1\n");
    } else {
        write(buffer, half);
        write_reversed(buffer, size / 2);
        printf("\n");
    }

    free(buffer);
}

int main(int argc, char **argv) {
    int cases;
    int i;
    scanf("%d\n", &cases);
    for(i = 0; i < cases; i++) {
        test_case();
    }

    return 0;
}
