#include <stdio.h>

int main(int argc, char **argv)
{
	if (argc != 3){
		printf("Use two arguments: $./endLineScrub <inFilename> <outFilename>\n");
		return 1;
	}

	FILE *input = fopen((const char *)argv[1], "r");
	FILE *output = fopen((const char *)argv[2], "w");

	char buf[200];
	// continue until end of file
	while(fgets(buf, sizeof buf, input) != NULL){
		int i = 0;
		// we look for '\r' character
		while(*(buf + i) != '\0'){
			if(*(buf + i) == '\r'){
				
				// if \r\n, remove \r
				if (*(buf + i + 1) == '\n'){
					int j = 0;
					while(*(buf + i + j) != '\0'){
						*(buf + i + j) = *(buf + i + (++j));
					}

				// if just \r, change to \n
				} else {
					*(buf + i) = '\n';
				}
			}
			i++;
		}

		fputs((const char *) buf, output);
		fflush(output);
	}

	fclose(output);
	fclose(input);
	return 0;
}