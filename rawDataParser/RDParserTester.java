import java.io.File;
import java.util.ArrayList;


public class RDParserTester {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		File input = new File(args[0]);
		
		ArrayList<Unit> units = Parser.analyze(input, "8061-2", 5);
		
		Parser.output(units);
	}

}
