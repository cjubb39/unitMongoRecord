
public class Unit {
	
	private int SN;
	private double[][] readings;
	private float chTCR, crTCR, rhTCR;
	private float chNull, crNull, rhNull;
	private float lin, hyst, nullSet, nfsoSet;
	
	public Unit(int SN, double[][] readings){
		this.SN = SN;
		this.readings = readings;
		
		this.initialize();
	}

	private void initialize() {
		this.calcTCRS();
		this.calcNull();
		this.calcMisc();
	}

	private void calcTCRS() {
		// TODO Auto-generated method stub
		
	}

	private void calcNull() {
		// TODO Auto-generated method stub
		
	}

	private void calcMisc() {
		// TODO Auto-generated method stub
		
	}
}
