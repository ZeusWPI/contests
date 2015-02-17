import java.io.*;

/**
 * @author The Hurry-Coward Isomorphism
 */
public class Main {
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

    public static int run(BufferedReader reader) throws IOException {
        int n = Integer.parseInt(reader.readLine());

    }
}
