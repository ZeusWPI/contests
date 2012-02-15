
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;



/**
 * Mooi priem
 * @author pieter
 */
public class MooiPriem {

	/* generate all prime numbers upto (inclusive) n
	*/
	public static ArrayList<Integer> generate(int m, int n)
	{
		BitSet sieve = new BitSet();
		double sqrt = Math.sqrt(n) + 1;
		for(int i = 4; i <= n;i +=2)
		{
			sieve.set(i);
		}
		for(int i = 3;i<=sqrt; i+=2){
			for(int j= 2; i*j <= n;j++){
				sieve.set(i*j);
			}
		}

		if(m <= 2) m = 2;
		ArrayList<Integer> list = new ArrayList<Integer>();
		for (int i=m; i<=n; i++) {
			if(! sieve.get(i)) list.add(i);
		}
		return list;
	}

	public static void main(String[] args) throws IOException {
        String input;
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		reader.readLine();
		while((input = reader.readLine()) != null) {
			String[] numbers = input.split(" ");
			solve(Integer.parseInt(numbers[0]), Integer.parseInt(numbers[1]));
		}
    }

	public static void solve(int lower, int upper) {
		int minimumDistance = -1;
		int minimumPrime = -1;

		List<Integer> primes = generate(lower, upper);
		if(primes.size() == 0 || upper < 2) {
			System.out.println("geen priemgetal gevonden");
			return;
		}

		int pow = 1;
		while(primes.get(0) > (pow << 1)) {
			pow = pow << 1;
		}
		minimumPrime = primes.get(0);
		if(Math.abs(minimumPrime - pow) < Math.abs(minimumPrime - (pow << 1))) {
			minimumDistance = Math.abs(minimumPrime - pow);
		}
		else minimumDistance = Math.abs(minimumPrime - (pow << 1));

		for(int i = 1; i < primes.size(); i++) {
			int currPrime = primes.get(i);

			while(currPrime > (pow << 1)) pow = pow << 1;

			int distMin = Math.abs(currPrime - pow);
			if(distMin < minimumDistance) {
				minimumDistance = distMin;
				minimumPrime = currPrime;
			}
			int distMax = Math.abs(currPrime - (pow << 1));
			if(distMax < minimumDistance) {
				minimumDistance = distMax;
				minimumPrime = currPrime;
			}
		}

		System.out.println(minimumPrime);
	}
}
