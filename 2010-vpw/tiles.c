#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int height;
    int width;
} tile;

tile rotate(tile a) {
    tile b;
    b.height = a.width;
    b.width = a.height;
    return b;
}

char fit_tile(char **floor, int floor_height, int floor_width,
        tile tile, int y, int x) {
    int i, j;

    if(y + tile.height > floor_height || x + tile.width > floor_width) {
        return 0;
    }

    for(i = 0; i < tile.height; i++) {
        for(j = 0; j < tile.width; j++) {
            if(floor[y + i][x + j]) return 0;
        }
    }

    return 1;
}

void fill_tile(char **floor, tile tile, int y, int x, char value) {
    int i, j;
    for(i = 0; i < tile.height; i++) {
        for(j = 0; j < tile.width; j++) {
            floor[y + i][x + j] = value;
        }
    }
}

void put_tile(char **floor, tile tile, int y, int x) {
    fill_tile(floor, tile, y, x, 1); 
}

void unput_tile(char **floor, tile tile, int y, int x) {
    fill_tile(floor, tile, y, x, 0); 
}

char free_position(char **floor, int floor_height, int floor_width,
        int *yp, int *xp) {
    int y, x;
    
    for(y = 0; y < floor_height; y++) {
        for(x = 0; x < floor_width; x++) {
            if(!floor[y][x]) {
                *yp = y;
                *xp = x;
                return 1;
            }
        }
    }

    return 0;
}

char solve(char **floor, int floor_height, int floor_width,
        tile *tiles, int num_tiles) {
    int i, y, x;

    if(!free_position(floor, floor_height, floor_width, &y, &x)) {
        return 1;
    }

    for(i = 0; i < num_tiles; i++) {
        tile t = tiles[i];
        tile r = rotate(t);

        if(fit_tile(floor, floor_height, floor_width, t, y, x)) {
            put_tile(floor, t, y, x);
            if(solve(floor, floor_height, floor_width, tiles, num_tiles)) {
                return 1;
            }
            unput_tile(floor, t, y, x);
        }

        if(fit_tile(floor, floor_height, floor_width, r, y, x)) {
            put_tile(floor, r, y, x);
            if(solve(floor, floor_height, floor_width, tiles, num_tiles)) {
                return 1;
            }
            unput_tile(floor, r, y, x);
        }
    }

    return 0;
}

void test_case(void) {
    int floor_height, floor_width;
    int num_tiles;
    tile *tiles;
    char **floor;

    int i, x, y;

    scanf("%d %d", &floor_height, &floor_width);
    scanf("%d", &num_tiles);

    tiles = (tile *) malloc(num_tiles * sizeof(tile));

    for(i = 0; i < num_tiles; i++) {
        scanf("%d %d", &tiles[i].height, &tiles[i].width);
    }

    floor = (char **) malloc(floor_height * sizeof(char *));
    for(y = 0; y < floor_height; y++) {
        floor[y] = (char *) malloc(floor_width * sizeof(char));
        for(x = 0; x < floor_width; x++) {
            floor[y][x] = 0;
        }
    }

    if(solve(floor, floor_height, floor_width, tiles, num_tiles)) {
        printf("JA\n");
    } else {
        printf("NEEN\n");
    }

    for(y = 0; y < floor_height; y++) free(floor[y]);
    free(floor);

    free(tiles);
}

int main(int argc, char **argv) {
    int cases;
    int i;
    scanf("%d", &cases);
    for(i = 0; i < cases; i++) {
        test_case();
    }
    return 0;
}
