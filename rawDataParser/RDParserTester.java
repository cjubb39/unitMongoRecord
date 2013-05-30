import java.io.File;
import java.util.ArrayList;

public class RDParserTester {

	/**
	 * Main tester for program. Reads from file and outputs unit characteristics
	 * formatted for entry into enterUnits.c
	 * 
	 * @param args
	 *            args[0] should be the name of the file containing raw data
	 *            (file should have UNIX line endings). args[1] should be the
	 *            lotIndicator of the units (the prefix on the serial number);
	 *            for example, for SN 8061-2-90, the lotIndicator is 8061-2.
	 *            args[2] should be the integer number of units to be read from
	 *            the raw data file.
	 */
	public static void main(String[] args) {
		File input = new File(args[0]);

		ArrayList<Unit> units = Parser.analyze(input, args[1],
				Integer.parseInt(args[2]));

		// output formatted for C code
		Parser.outputC(units);

	}

}
