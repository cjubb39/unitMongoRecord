import java.io.File;
import java.util.ArrayList;


public class RDParserTester {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		File input = new File(args[0]);
		
		ArrayList<Unit> units = Parser.analyze(input, "8061-2", 1);
		
		//for (Unit u : units){
		Unit u = units.get(0);
			double[][] temp = u.getReadings();
			for (double[] d : temp){
				for (double dd : d){
					System.out.println(dd);
				}
				System.out.println("\n");
			}
			
		//}
		//Parser.output(units);
			
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
		
	}

}
