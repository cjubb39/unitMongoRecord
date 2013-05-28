import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Parser {

	public static final String snExtract = "\\s*\\d+-\\d+-(\\d+)\\s*";

	public static ArrayList<Unit> analyze(File inFile, String lotIndicator, int numUnits) {
		Scanner input = null;
		ArrayList<Unit> units = new ArrayList<Unit>();
		int SN = 0;

		double[][] outputReadings = null;

		try {
			input = new Scanner(inFile);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		// Set-up scanner preferences
		input.useDelimiter(System.getProperty("line.separator"));
		// input.useDelimiter("\n");
		/*
		 * System.out.println(input.next() + "\n"); System.exit(0);
		 */
		for (int counter = 0; counter < numUnits; counter++) {
			// look for first serial number
			while (!input.hasNext("\\s*" + lotIndicator + ".*")) {
				input.next();
			}

			Matcher snMatcher = Pattern.compile(snExtract)
					.matcher(input.next());

			// extract SN
			if (snMatcher.find()) {
				SN = Integer.parseInt(snMatcher.group(1));
				System.out.println("SN: " + SN);
			}

			// look for single 0
			boolean indicator = false;
			while (!indicator) {
				while (!input.hasNext("\\s*0\\s*")) {
					input.next();
				}

				// make sure more zeros do not follow
				input.next(); // eats first zero

				if (!input.hasNext("\\s*0\\s*")) {
					indicator = true;
				}
			} // should be on resistance values

			// read until zero
			while (!input.hasNext("\\s*0\\s*")) {
				input.next();
			}

			input.next(); // eat that zero

			outputReadings = new double[4][11];
			for (int i = 0; i < 4; i++) {
				double reading = Double.parseDouble(input.next().trim());
				int j = 0;

				while (reading < 1000) { // continue up until resistance
											// measurements (which should be
											// >1000)

					outputReadings[i][j] = reading;
					reading = Double.parseDouble(input.next().trim());
					j++;
					
					if (i == 3 && j == 1){
						break;
					}
				}

				// read until zero
				while (!input.hasNext("\\s*0\\s*")) {
					input.next();
				}

				input.next(); // eat that zero
				// read until not zero
				while (input.hasNext("\\s*0\\s*")) {
					input.next();
				} // now on next non-resistor, non-zero value
			}
			
			units.add(new Unit(SN, outputReadings));
		}
		
		return units;
	}

	public static void output(ArrayList<Unit> units) {
		// TODO Auto-generated method stub
		
	}
	
}
