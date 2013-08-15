import org.apache.commons.math3.stat.regression.SimpleRegression;

public class Unit {

	private static final int HOT = 280;
	private static final int COLD = -65;
	private static final int ROOM_TEMP = 80;
	private static final int NUM_TEMPS = 3;
	private static final int PRESSURE_STEP = 5;

	private int SN;
	private String fullSN;
	private double[][] readings;
	private double[] inputR;
	// private double[] outputR; //capacity to use outputR in future
	private double[] FS;
	private double chTCR, crTCR, rhTCR;
	private double chTCGF, crTCGF, rhTCGF;
	private double chNull, crNull, rhNull;
	private double lin, hyst, nullSet, nfsoSet;
	private double FSmultiplier, resistanceMultiplier;

	/**
	 * Constructor. All necessary calculations completed on construction as
	 * called by this.initialize().
	 * 
	 * @param SN
	 *            Serial Number of Unit
	 * @param readings
	 *            Voltage output from units. readings[i][j] corresponds to the
	 *            jth pressure tested at the ith temperature
	 * @param resistance
	 *            Input and output resistance. resistance[0][j] corresponds to
	 *            the input resistance at the jth temperature; resistance[1][j]
	 *            corresponds to the output resistance at the jth temperature
	 */
	public Unit(int SN, String fullSN, double[][] readings,
			double[][] resistance) {
		this.SN = SN;
		this.fullSN = fullSN;
		this.readings = readings;
		this.inputR = resistance[0];
		// this.outputR = resistance[1];

		this.initialize();
	}

	/**
	 * Calculates necessary characteristics from this.readings array. This
	 * includes FSO, TCRs, Null Shifts, Least-Squares Linearity, Hysteresis,
	 * Null Set, and NFSO Set as well as some helpful multipliers.
	 */
	private void initialize() {
		this.calcFS();

		this.FSmultiplier = 100 / this.FS[0];
		this.resistanceMultiplier = 100 / this.inputR[0];

		this.calcTCRS();
		this.calcTCGFS();
		this.calcNullShift();
		this.calcMisc();
	}

	/**
	 * Calculates FSO at each temperature.
	 */
	private void calcFS() {
		this.FS = new double[NUM_TEMPS];

		// initial RT reading formatting differently than subsequent readings
		double max, min;
		min = this.readings[0][0];
		max = this.readings[0][(int) Math.floor((this.readings[0].length) / 2)];
		this.FS[0] = max - min;

		// subsequent readings
		for (int i = 1; i < NUM_TEMPS; i++) {
			min = this.readings[i][0];
			max = this.readings[i][1];
			this.FS[i] = max - min;
		}
	}

	/**
	 * The Cold-Hot, Cold-Room, Room-Hot TCRs are calculated. (Resistance[warmer
	 * temp] - Resistance[colder temp]) / (warmerTemp - colderTemp) * (100 /
	 * RTinputR) * 100
	 * 
	 * UNITS: (%/100F)
	 */
	private void calcTCRS() {
		this.chTCR = this.resistanceMultiplier
				* (this.inputR[2] - this.inputR[1]) / (HOT - COLD) * 100;
		this.crTCR = this.resistanceMultiplier
				* (this.inputR[0] - this.inputR[1]) / (ROOM_TEMP - COLD) * 100;
		this.rhTCR = this.resistanceMultiplier
				* (this.inputR[2] - this.inputR[0]) / (HOT - ROOM_TEMP) * 100;
	}

	/**
	 * The Cold-Hot, Cold-Room, Room-Hot TCGFs are calculated. (FSO[warmer temp]
	 * - FSO[colder temp]) / (warmerTemp - colderTemp) * (100 / RT-FSO) * 100
	 * 
	 * UNITS: (%/100F)
	 */
	private void calcTCGFS() {
		this.chTCGF = this.FSmultiplier * (this.FS[2] - this.FS[1])
				/ (HOT - COLD) * 100;
		this.crTCGF = this.FSmultiplier * (this.FS[0] - this.FS[1])
				/ (ROOM_TEMP - COLD) * 100;
		this.rhTCGF = this.FSmultiplier * (this.FS[2] - this.FS[0])
				/ (HOT - ROOM_TEMP) * 100;
	}

	/**
	 * The Cold-Hot, Cold-Room, Room-Hot Null Shifts are calculated.
	 * (ouputV[warmer temp] - outputV[colder temp]) / (warmerTemp - colderTemp)
	 * * (100 / FSO[RT]) * 100
	 * 
	 * UNITS: (%/100F)
	 */
	private void calcNullShift() {
		this.chNull = this.FSmultiplier
				* (this.readings[2][0] - this.readings[1][0]) / (HOT - COLD)
				* 100;
		this.crNull = this.FSmultiplier
				* (this.readings[0][0] - this.readings[1][0])
				/ (ROOM_TEMP - COLD) * 100;
		this.rhNull = this.FSmultiplier
				* (this.readings[2][0] - this.readings[0][0])
				/ (HOT - ROOM_TEMP) * 100;
	}

	/**
	 * Calls for calculation of remaining characteristics: Linearity,
	 * Hysteresis, Null Set, NFSO Set
	 */
	private void calcMisc() {
		this.calcLinearity();
		this.calcHysteresis();
		this.calcNullSet();
		this.calcNFSOSet();
	}

	/**
	 * Least-squares best fit line calculated. this.lin = (max residual from
	 * BFL) / FSO * 100
	 * 
	 * UNITS: %FSO
	 */
	private void calcLinearity() {
		SimpleRegression reg = new SimpleRegression(true);

		// add data to regression object
		for (int i = 0, pressure = 0; i < (this.readings[0].length / 2); i++, pressure += PRESSURE_STEP) {
			reg.addData(pressure, this.readings[0][i]);
		}

		// get max difference between expected (from regression) and observation
		// Note "predict" value is the expected valued on the least-squares best
		// fit line
		double linDiff, maxLinDiff = 0;
		for (int i = 0, pressure = 0; i < (this.readings[0].length / 2); i++, pressure += PRESSURE_STEP) {
			linDiff = Math.abs(reg.predict(pressure) - this.readings[0][i]);
			maxLinDiff = (linDiff > maxLinDiff) ? linDiff : maxLinDiff;
		}

		this.lin = maxLinDiff * this.FSmultiplier;
	}

	/**
	 * Max Hysteresis calculated. Using output data from first RT reading, max
	 * hysteresis is calculated: maxDifference / FSO * 100
	 * 
	 * UNITS: %FSO
	 */
	private void calcHysteresis() {
		double difference, maxDifference = 0;
		// looking for biggest difference between value going up and going down
		for (int i = 0, j = this.readings[0].length - 1; i < j; i++, j--) {
			difference = Math.abs(this.readings[0][i] - this.readings[0][j]);
			maxDifference = (difference > maxDifference) ? difference
					: maxDifference;
		}
		this.hyst = maxDifference * this.FSmultiplier;
	}

	/**
	 * Calculates difference between 0psi readings at RT readings.
	 * 
	 * UNITS: mV
	 */
	private void calcNullSet() {
		this.nullSet = this.readings[3][0] - this.readings[0][0];
	}

	/**
	 * Calculates difference in Normalized FSO between RT readings.
	 * 
	 * UNITS: mV
	 */
	private void calcNFSOSet() {
		double firstNFSO, secondNFSO;
		firstNFSO = this.readings[0][(int) Math
				.floor(this.readings[0].length / 2)] - this.readings[0][0];
		secondNFSO = this.readings[3][1] - this.readings[3][0];
		this.nfsoSet = secondNFSO - firstNFSO;
	}

	/**
	 * Getter for readings[][] array
	 * 
	 * @return Array of output readings at temperatures and pressures
	 */
	public double[][] getReadings() {
		return this.readings;
	}

	/**
	 * Getter for Serial Number
	 * 
	 * @return Serial Number of unit
	 */
	public int getSN() {
		return this.SN;
	}

	/**
	 * Getter for full Serial Number (including lot indicator)
	 * 
	 * @return Full serial number of unit
	 */
	public String getFullSN() {
		return this.fullSN;
	}

	/**
	 * Getter for Cold-Hot TCR
	 * 
	 * @return Cold-Hot TCR of unit
	 */
	public double getChTCR() {
		return this.chTCR;
	}

	/**
	 * Getter for Cold-Room TCR
	 * 
	 * @return Cold-Room TCR of unit
	 */
	public double getCrTCR() {
		return this.crTCR;
	}

	/**
	 * Getter for Room-Hot TCR
	 * 
	 * @return Room-Hot TCR of unit
	 */
	public double getRhTCR() {
		return this.rhTCR;
	}

	/**
	 * Getter for Cold-Hot TCGF
	 * 
	 * @return Cold-Hot TCGF of unit
	 */
	public double getChTCGF() {
		return this.chTCGF;
	}

	/**
	 * Getter for Cold-Room TCGF
	 * 
	 * @return Cold-Room TCGF of unit
	 */
	public double getCrTCGF() {
		return this.crTCGF;
	}

	/**
	 * Getter for Room-Hot TCGF
	 * 
	 * @return Room-Hot TCGF of unit
	 */
	public double getRhTCGF() {
		return this.rhTCGF;
	}

	/**
	 * Getter for Cold-Hot Null Shift
	 * 
	 * @return Cold-Hot Null Shift of unit
	 */
	public double getChNull() {
		return this.chNull;
	}

	/**
	 * Getter for Cold-Room Null Shift
	 * 
	 * @return Cold-Room Null Shift of unit
	 */
	public double getCrNull() {
		return this.crNull;
	}

	/**
	 * Getter for Room-Hot Null Shift
	 * 
	 * @return Room-Hot Null Shift of unit
	 */
	public double getRhNull() {
		return this.rhNull;
	}

	/**
	 * Getter for Linearity
	 * 
	 * @return Linearity of unit
	 */
	public double getLin() {
		return this.lin;
	}

	/**
	 * Getter for Max hysteresis
	 * 
	 * @return Max hysteresis of unit
	 */
	public double getHyst() {
		return this.hyst;
	}

	/**
	 * Getter for Null Set
	 * 
	 * @return Null Set of unit
	 */
	public double getNullSet() {
		return this.nullSet;
	}

	/**
	 * Getter for NFSO Set
	 * 
	 * @return NFSO set of unit
	 */
	public double getNfsoSet() {
		return this.nfsoSet;
	}
}