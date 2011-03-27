/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Pieter
 */
public class Pipes {
    public static void main(String[] args) throws IOException {
       BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        for(int i = 0; i < n; i++) {
            run(reader);
        }
    }

    public static int size;
    public static Pipe[][] field;

    public static void run(BufferedReader reader) throws IOException {
        size = Integer.parseInt(reader.readLine());
        char[] pieces = reader.readLine().toCharArray();
        List<Pipe> pipes = new ArrayList<Pipe>();
        for(int i = 0; i < pieces.length; i++) {
            pipes.add(new Pipe(pieces[i]));
        }

        field = new Pipe[size+2][size+2];
        Pipe emptyPipe = new Pipe((char)('A' - 1));
        for(int i = 0; i < size; i++) {
            field[0][i] = emptyPipe;
            field[i][0] = emptyPipe;
            field[size+1][i] = emptyPipe;
            field[i][size+1] = emptyPipe;
        }

        for(int i = 0; i < size; i++) {
           pieces = reader.readLine().toCharArray();
           for(int j = 0; j < pieces.length; j++) {
               if(pieces[j] == '?') continue;
               else field[i+1][j+1] = new Pipe(pieces[j]);
           }
        }

        // recursive backtracking
        putPipes(pipes);

        // print result
        for(int i = 1; i < size + 1; i++) {
            for(int j = 1; j < size + 1; j++) {
                Pipe p = field[i][j];
                char x = (char)('A' - 1 + (p.up ? 1 : 0) + (p.right ? 2 : 0) + (p.down ? 4 : 0) + (p.left ? 8: 0));
                System.out.print(x);
            }
            System.out.println();
        }
        System.out.println();
    }

    public static boolean fits(Pipe pipe, int row, int column) {
        return (field[row-1][column] == null || !(pipe.up ^ field[row-1][column].down)) &&
               (field[row][column-1] == null || !(pipe.left ^ field[row][column-1].right)) &&
               (field[row+1][column] == null || !(pipe.down ^ field[row+1][column].up)) &&
               (field[row][column+1] == null || !(pipe.right ^ field[row][column+1].left));
    }

    public static boolean putPipes(List<Pipe> pipes) {
        if(pipes.size() == 0) return true;
        Pipe pipe = pipes.remove(pipes.size() - 1);
        for(int i = 1; i <= size; i++) {
            for(int j = 1; j <= size; j++) {
                if(field[i][j] != null) continue;
                if(!fits(pipe, i, j)) continue;

                field[i][j] = pipe;
                if(putPipes(pipes)) return true;
                field[i][j] = null;
            }
        }
        pipes.add(pipe);
        return false;
    }

    public static class Pipe {
        public boolean up, left, top, right, down;

        public Pipe(char x) {
            x -= 'A' - 1;
            up = (x & 1) > 0;
            down = (x & 4) > 0;
            left = (x & 8) > 0;
            right = (x & 2) > 0;
        }
    }
}
