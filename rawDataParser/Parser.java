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
		double[][] resistances = null;

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

			outputReadings = new double[4][11];
			resistances = new double[2][4];
			
			//get input, then output resistance values
			resistances[0][0] = Double.parseDouble(input.next().trim());
			resistances[1][0] = Double.parseDouble(input.next().trim());
			
			for (int i = 0; i < 3; i++) {				
				// read until zero
				while (!input.hasNext("\\s*0\\s*")) {
					input.next();
				}

				//input.next(); // eat that zero
				
				//go until non-zero
				while (input.hasNext("\\s*0\\s*")){
					input.next();
				}
				
				
				double reading = Double.parseDouble(input.next().trim());
				int j = 0;

				while (reading < 1000) { // continue up until resistance
											// measurements (which should be
											// >1000)
					
					outputReadings[i][j] = reading;
					reading = Double.parseDouble(input.next().trim());
					j++;	
				}
				
				resistances[0][i + 1] = reading;
				resistances[1][i + 1] = Double.parseDouble(input.next().trim());
			}
			
			//get second RT measurement--formatted differently (w.r.t to 0s)
			
			// read until zero
			while (!input.hasNext("\\s*0\\s*")) {
				input.next();
			}

			input.next(); // eat that zero
			// read until not zero
			while (input.hasNext("\\s*0\\s*")) {
				input.next();
			} // now on next non-resistor, non-zero value
			
			//get 0psi
			outputReadings[3][0] = Double.parseDouble(input.next().trim());
			
			while (input.hasNext("\\s*0\\s*")){
				input.next();
			}
			
			//get max psi
			outputReadings[3][1] = Double.parseDouble(input.next().trim());
			
			while (input.hasNext("\\s*0\\s*")){
				input.next();
			}
			//get 0psi
			outputReadings[3][2] = Double.parseDouble(input.next().trim());
			
			units.add(new Unit(SN, outputReadings, resistances));
		}
		
		return units;
	}

	public static void output(ArrayList<Unit> units) {
		// TODO Auto-generated method stub
		
	}
	
}
