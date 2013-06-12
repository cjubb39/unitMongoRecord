#include <stdio.h>
#include "fileOperations.h"

#define BAD_ARGUMENTS 1;
#define FILE_PROBLEM 2;

int main(int argc, char **argv)
{
	int numUnits;
	const char *fluid;
	const char *group;

	// make sure arguments correct
	if (argc == 3) {
		fluid = argv[1];
		printf("FLUID: %s\n", fluid);

		group = argv[2];
		printf("GROUP: %s\n", group);

		printf("Enter number of units\n");
		scanf("%d", &numUnits);
	} else {
		return BAD_ARGUMENTS;
	}

	// create js file
	FILE *file = NULL;
	file = initializeFile(file);
	if (!file){
		return FILE_PROBLEM;
	}

	// add each unit to js file
	int i = 0;
	while(i < numUnits)
	{
		if (enterUnit(file, fluid, group) == 0){
			++i;
		}
	}

	// close js file
	closeFile(file);

	return 0;
}