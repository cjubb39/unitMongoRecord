#include <stdio.h>
#include "fileOperations.h"

FILE * initializeFile(FILE *file){
	// create file
	file = fopen("unitsToAdd.js", "w+");

	// make sure file created properly
	if (!file){
		printf("Failed to open file");
		return 0;
	}

	// header for file
	fprintf(file, "use oilFluidExp\n");

	return file;
}

int closeFile(FILE *file){
	// close file.  Return error if necesssary
	if (fclose(file)){
		printf("Error closing file");
		return 1;
	}

	return 0;
}

int enterUnit(FILE *file, const char *fluid, const char *group){
	float chTCR, crTCR, rhTCR;
	float chTCGF, crTCGF, rhTCGF;
	float chNull, crNull, rhNull;
	float lin, hyst, nullSet, nfsoSet;
	int SN;
	char fullSN[30];

	printf("Enter Serial Number: ");
	scanf("%d", &SN);

	printf("Enter full Serial Number (including Lot Indicator)");
	scanf("%s", fullSN);

	// TCR
	printf("Enter Cold-Hot TCR: ");
	scanf("%f", &chTCR);

	printf("Enter Cold-Room TCR: ");
	scanf("%f", &crTCR);

	printf("Enter Room-Hot TCR: ");
	scanf("%f", &rhTCR);

	// TCGF
	printf("Enter Cold-Hot TCGF: ");
	scanf("%f", &chTCGF);

	printf("Enter Cold-Room TCGF: ");
	scanf("%f", &crTCGF);

	printf("Enter Room-Hot TCGF: ");
	scanf("%f", &rhTCGF);


	// Null Shifts
	printf("Enter Cold-Hot Null Shift: ");
	scanf("%f", &chNull);

	printf("Enter Cold-Room Null Shift: ");
	scanf("%f", &crNull);

	printf("Enter Room-Hot Null Shift: ");
	scanf("%f", &rhNull);

	// Misc Data
	printf("Enter Linearity: ");
	scanf("%f", &lin);

	printf("Enter Hysteresis: ");
	scanf("%f", &hyst);

	printf("Enter Null Set: ");
	scanf("%f", &nullSet);

	printf("Enter NSFO Set: ");
	scanf("%f", &nfsoSet);

	// confirm
	int indicator;
	printf("Correct? Enter 1 for yes ");
	scanf("%d", &indicator);

	if (indicator != 1){
		return 42;
	}

	// begin mongodb command to add unit to database
	fprintf(file, "db.units.update({SN: %d, Group: \"%s\"}, {$set: {SN: %d, " \
		"fullSN: \"%s\", Fluid: \"%s\", Group: \"%s\", chTCR: %2.3f, crTCR: %2.3f,"\
		" rhTCR: %2.3f, chTCGF: %2.3f, crTGF: %2.3f, rhTGF: %2.3f, chNull: %2.3f, "\
		"crNull: %2.3f, rhNull: %2.3f, lin: %2.3f, hyst: %2.3f, nullSet: %2.3f, "\
		"nfsoSet: %2.3f}}, true)\n", SN, group, SN, fullSN, fluid, group, chTCR, \
		crTCR, rhTCR, chTCGF, crTCGF, chTCGF, chNull, crNull, rhNull, lin, hyst, \
		nullSet, nfsoSet);
	fflush(file);

	return 0;
}