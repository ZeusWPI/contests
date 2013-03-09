#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char color;
    int x;
    int y;
    int width;
    int height;
} sheet;

sheet read_sheet(void) {
    sheet sheet;
    int space;
    sheet.color = fgetc(stdin);

    /* Optional space */
    space = fgetc(stdin);
    if(' ' != (char) space) ungetc(space, stdin);

    fscanf(stdin, "%d %d %d %d\n", &sheet.x, &sheet.y,
            &sheet.width, &sheet.height);

    return sheet;
}

void put_sheet(
        unsigned char *film, int film_width, int film_height, sheet sheet) {
    int x;
    int y;

    for(y = sheet.y; y < sheet.y + sheet.height; y++) {
        for(x = sheet.x; x < sheet.x + sheet.width; x++) {
            film[y * film_width + x] = 1;
        }
    }
}

int unput_sheet(
        unsigned char *film, int film_width, int film_height, sheet sheet) {
    int overlapping = 0;
    int x;
    int y;

    for(y = sheet.y; y < sheet.y + sheet.height; y++) {
        for(x = sheet.x; x < sheet.x + sheet.width; x++) {
            if(film[y * film_width + x]) overlapping++;
            film[y * film_width + x] = 0;
        }
    }

    return overlapping;
}

void test_case(void) {
    int num_sheets;
    int i;
    int film_width, film_height;
    sheet *sheets;
    unsigned char *film;
    int purple = 0;

    /* Read sheets */
    fscanf(stdin, "%d\n", &num_sheets);
    sheets = calloc(num_sheets, sizeof(sheet));
    for(i = 0; i < num_sheets; i++) sheets[i] = read_sheet();

    /* Make film */
    film_width = film_height = 0;
    for(i = 0; i < num_sheets; i++) {
        int w = sheets[i].x + sheets[i].width;
        int h = sheets[i].y + sheets[i].height;
        film_width = w > film_width ? w : film_width;
        film_height = h > film_height ? h : film_height;
    }
    film = calloc(film_width * film_height, sizeof(unsigned char));

    /* Set all red sheets */
    for(i = 0; i < num_sheets; i++) {
        if(sheets[i].color == 'R') {
            put_sheet(film, film_width, film_height, sheets[i]);
        }
    }

    /* Test all blue sheets */
    for(i = 0; i < num_sheets; i++) {
        if(sheets[i].color == 'B') {
            purple += unput_sheet(film, film_width, film_height, sheets[i]);
        }
    }

    printf("%d\n", purple);
    free(sheets);
    free(film);
}

int main(int argc, char **argv) {
    int cases;
    int i;

    fscanf(stdin, "%d\n", &cases);
    for(i = 0; i < cases; i++) test_case();
    return 0;
}
