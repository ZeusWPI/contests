/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.*;

/**
 *
 * @author Pieter
 */
public class Lights {
    public static void main(String[] args) throws IOException {
       BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        for(int i = 0; i < n; i++) {
            boolean result = run(reader);
            if(result == false){
                System.out.println("verkeerd");
            } else {
                System.out.println("correct");
            }
        }
    }

    public static final int UP = 0;
    public static final int LEFT = 1;
    public static final int RIGHT = 2;
    public static final int DOWN = 3;

    public static int width, height;
    public static char[][] matrix;

    public static boolean run(BufferedReader reader) throws IOException {
        String[] size = reader.readLine().split(" ");
        width = Integer.parseInt(size[1]);
        height = Integer.parseInt(size[0]);

        matrix = new char[height+2][width+2];

        for(int i = 0; i < height + 2; i++) {
            matrix[i] = reader.readLine().toCharArray();
        }

        for( int i = 0 ; i < width; i ++ ) {
            char result = bounce(1, i+1, DOWN);
            if (result != matrix[0][i+1]) return false;

            result = bounce(height, i + 1, UP);
            if (result != matrix[height+1][i+1]) return false;

        }




        return true;
    }

    public static char bounce(int row, int column, int dir) {
        while(row >= 1 && row < height +1 && column >= 1 && column < width + 1) {
            switch(matrix[row][column]){
                case '/': switch(dir) {
                    case UP: dir = RIGHT;
                        break;
                    case DOWN: dir = LEFT;
                        break;
                    case LEFT: dir = DOWN;
                        break;
                    case RIGHT: dir = UP;
                        break;
                } break;
                case '\\': switch(dir) {
                    case UP: dir = LEFT;
                        break;
                    case DOWN: dir = RIGHT;
                        break;
                    case LEFT: dir = UP;
                        break;
                    case RIGHT: dir = DOWN;
                        break;
                } break;
            }

            switch(dir) {
                  case UP: row--;
                        break;
                    case DOWN: row++;
                        break;
                    case LEFT: column--;
                        break;
                    case RIGHT: column++;
                        break;
            }
        }
        return matrix[row][column];
    }
}
