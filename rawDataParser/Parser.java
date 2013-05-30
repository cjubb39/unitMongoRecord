import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Parser {

	public static final String snExtract = "\\s*\\d+-\\d+-(\\d+)\\s*";

	/**
	 * Reading in from raw data file and constructs unit objects represented by
	 * that raw data
	 * 
	 * REQUIREMENTS: Unit resistances be strictly greater than 1000 and all
	 * outputs be lower than 1000
	 * 
	 * @param inFile
	 *            File in which raw data is stored
	 * @param lotIndicator
	 *            Lot prefix to unit
	 * @param numUnits
	 *            Number of units to be analyzed from file. Throws exception if
	 *            greater than number of units in file
	 * @return ArrayList of Units from file
	 */
	public static ArrayList<Unit> analyze(File inFile, String lotIndicator,
			int numUnits) {
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

		// repeat for numUnits
		for (int counter = 0; counter < numUnits; counter++) {
			// look for first serial number
			while (!input.hasNext("\\s*" + lotIndicator + ".*")) {
				input.next();
			}

			// extract SN
			Matcher snMatcher = Pattern.compile(snExtract)
					.matcher(input.next());
			if (snMatcher.find()) {
				SN = Integer.parseInt(snMatcher.group(1));
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
			} // input.next() called now should return resistance values

			outputReadings = new double[4][11];
			resistances = new double[2][4];

			// get input, then output resistance values
			resistances[0][0] = Double.parseDouble(input.next().trim());
			resistances[1][0] = Double.parseDouble(input.next().trim());

			/*
			 * read for first RT, Cold, Hot tests. Second RT must be done
			 * separately due to different formatting resulting from 0s being
			 * entered for data points not check (i.e. those between 0 and
			 * maxPSI).
			 */
			for (int i = 0; i < 3; i++) {
				// read until zero
				while (!input.hasNext("\\s*0\\s*")) {
					input.next();
				}

				// go until non-zero
				while (input.hasNext("\\s*0\\s*")) {
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

			// get second RT measurement--formatted differently (w.r.t to 0s)

			// read until zero
			while (!input.hasNext("\\s*0\\s*")) {
				input.next();
			}

			input.next(); // eat that zero
			// read until not zero
			while (input.hasNext("\\s*0\\s*")) {
				input.next();
			} // now on next non-resistor, non-zero value

			// get 0psi
			outputReadings[3][0] = Double.parseDouble(input.next().trim());

			// continue until next entry is non-zero
			while (input.hasNext("\\s*0\\s*")) {
				input.next();
			}

			// get max psi
			outputReadings[3][1] = Double.parseDouble(input.next().trim());

			// continue until next entry is non-zero
			while (input.hasNext("\\s*0\\s*")) {
				input.next();
			}
			// get 0psi
			outputReadings[3][2] = Double.parseDouble(input.next().trim());

			units.add(new Unit(SN, outputReadings, resistances));
		}

		return units;
	}

	/**
	 * Outputs characteristic of units as required by enterUnits.c program
	 * 
	 * @param units
	 *            List of units to be output
	 */
	public static void outputC(ArrayList<Unit> units) {
		// number of units
		System.out.println(units.size());

		for (Unit u : units) {
			// info for each unit
			System.out.println(u.getSN());
			System.out.println(u.getChTCR());
			System.out.println(u.getCrTCR());
			System.out.println(u.getRhTCR());

			System.out.println(u.getChNull());
			System.out.println(u.getCrNull());
			System.out.println(u.getRhNull());

			System.out.println(u.getLin());
			System.out.println(u.getHyst());
			System.out.println(u.getNullSet());
			System.out.println(u.getNfsoSet());

			// confirm info for each unit
			System.out.println(1);
		}
	}
}
