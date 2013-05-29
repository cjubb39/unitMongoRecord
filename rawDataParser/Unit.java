import org.apache.commons.math3.stat.regression.SimpleRegression;

public class Unit {

	private static final int HOT = 280;
	private static final int COLD = -65;
	private static final int ROOM_TEMP = 80;
	private static final int PRESSURE_STEP = 5;

	private int SN;
	private double[][] readings;
	private double[] inputR, outputR;
	private double FS;
	private double chTCR, crTCR, rhTCR;
	private double chNull, crNull, rhNull;
	private double lin, hyst, nullSet, nfsoSet;
	private double FSmultiplier, resistanceMultiplier;

	public Unit(int SN, double[][] readings, double[][] resistance) {
		this.SN = SN;
		this.readings = readings;
		this.inputR = resistance[0];
		this.outputR = resistance[1];

		this.initialize();
	}

	private void initialize() {
		double max, min;
		min = this.readings[0][0];
		max = this.readings[0][(int) Math.floor((this.readings[0].length) / 2)];
		this.FS = max - min;
		this.FSmultiplier = 100 / this.FS;

		this.calcTCRS();
		this.calcNullShift();
		this.calcMisc();
	}

	private void calcTCRS() {
		this.chTCR = this.resistanceMultiplier
				* (this.inputR[2] - this.inputR[1]) / (HOT - COLD);
		this.crTCR = this.resistanceMultiplier
				* (this.inputR[0] - this.inputR[1]) / (ROOM_TEMP - COLD);
		this.rhTCR = this.resistanceMultiplier
				* (this.inputR[2] - this.inputR[0]) / (HOT - ROOM_TEMP);
	}

	private void calcNullShift() {
		this.chNull = this.FSmultiplier
				* (this.readings[2][0] - this.readings[1][0]) / (HOT - COLD);
		this.crNull = this.FSmultiplier
				* (this.readings[0][0] - this.readings[1][0])
				/ (ROOM_TEMP - COLD);
		this.rhNull = this.FSmultiplier
				* (this.readings[2][0] - this.readings[0][0])
				/ (HOT - ROOM_TEMP);
	}

	private void calcMisc() {
		this.calcLinearity();
		this.calcHysteresis();
		this.calcNullSet();
		this.calcNFSO();
	}

	private void calcLinearity() {
		SimpleRegression reg = new SimpleRegression(true);

		// add data to regression object
		for (int i = 0, pressure = 0; i < (this.readings[0].length / 2); i++, pressure += PRESSURE_STEP) {
			reg.addData(this.readings[0][i], pressure);
		}

		// get max difference between expected (from regression) and observation
		double linDiff, maxLinDiff = 0;
		for (int i = 0, pressure = 0; i < (this.readings[0].length / 2); i++, pressure += PRESSURE_STEP) {
			linDiff = Math.abs(reg.predict(pressure) - this.readings[0][i]);

			maxLinDiff = (linDiff > maxLinDiff) ? linDiff : maxLinDiff;
		}

		this.lin = maxLinDiff / this.FS;
	}

	private void calcHysteresis() {
		double difference, maxDifference = 0;
		for (int i = 0, j = this.readings[0].length - 1; i < j; i++, j--) {
			difference = Math.abs(this.readings[0][i] - this.readings[0][j]);
			if (difference > maxDifference) {
				maxDifference = difference;
			}
		}
		this.hyst = maxDifference / this.FSmultiplier;
	}

	private void calcNullSet() {
		this.nullSet = this.readings[3][0] - this.readings[0][0];
	}

	private void calcNFSO() {
		double firstNFSO, secondNFSO;
		firstNFSO = this.readings[0][(int) Math
				.floor(this.readings[0].length / 2)] - this.readings[0][0];
		secondNFSO = this.readings[3][1] - this.readings[3][0];
		this.nfsoSet = secondNFSO - firstNFSO;
	}

	public double[][] getReadings() {
		return this.readings;
	}

	public int getSN() {
		return this.SN;
	}

	public double getChTCR() {
		return this.chTCR;
	}

	public double getCrTCR() {
		return this.crTCR;
	}

	public double getRhTCR() {
		return this.rhTCR;
	}

	public double getChNull() {
		return this.chNull;
	}

	public double getCrNull() {
		return this.crNull;
	}

	public double getRhNull() {
		return this.rhNull;
	}

	public double getLin() {
		return this.lin;
	}

	public double getHyst() {
		return this.hyst;
	}

	public double getNullSet() {
		return this.nullSet;
	}

	public double getNfsoSet() {
		return this.nfsoSet;
	}
}