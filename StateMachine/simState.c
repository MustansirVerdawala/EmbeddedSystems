#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void recurr(char garbage[], char nextStates[8][8], int comm, char currState){
	if (garbage[nextStates[comm][currState-'A'] - 'A']!='0'){
		garbage[nextStates[comm][currState-'A'] - 'A']='0';
		recurr(garbage, nextStates, 0, nextStates[comm][currState-'A']);
		recurr(garbage, nextStates, 1, nextStates[comm][currState-'A']);
	}
}

int garbageCollection(char garbage[], char nextStates[8][8], char currState){
	recurr(garbage, nextStates, 0, currState);
	recurr(garbage, nextStates, 1, currState);

	for (int i=0; i<8; i++){
		if (garbage[i]!='0'){
			return 1;
		}
	}
	return 0;
}

int main(int argc, char * argv[])
{
	printf("\n**********************************************************************************");
	printf("\n\n%c", 'F');

	char currState='F';

	char currStates[]={'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'};
	char nextStates[8][8]={{'H', 'E', 'A', 'B', 'C', 'G', 'D', 'F'}, {'G', 'D', 'C', 'C', 'F', 'B', 'H', 'E'}};
	int status[8]={1,1,1,1,1,1,1,1};
	char inp[10];

	do{

		printf("\n\n");
		fgets(inp, 6, stdin);

		if ((inp[0]=='0' || inp[0]=='1')){
			if (status[nextStates[inp[0]-'0'][currState-'A']-'A']==1){
				currState=nextStates[inp[0]-'0'][currState-'A'];
				printf(">>%c", currState);
			}else{
				printf(">>Next State doesn't exist. Please check your State Machine Configuration.");
			}
		}
		else if (inp[0]=='c' && (inp[2]=='0' || inp[2]=='1') && (inp[4]>='A' && inp[4] <='H')){
			if (status[inp[4]-'A']==1){
				nextStates[inp[2]-'0'][currState-'A']=inp[4];
			}else{
				printf(">>The State you have specified doesn't exist. Please check your State Machine Configuration.");
			}
		}

		else if (inp[0]=='p'){
			printf("\n>>");
			for (int i=0; i<8; i++){
				if (status[currStates[i]-'A']==1){
					if (status[nextStates[0][i]-'A']==1 && status[nextStates[1][i]-'A']==1){
						printf("\n%10c %10c %10c", currStates[i], nextStates[0][i], nextStates[1][i]);
					}else if (status[nextStates[0][i]-'A']==0 && status[nextStates[1][i]-'A']==1){
						printf("\n%10c (Deleted)%c %10c", currStates[i], nextStates[0][i], nextStates[1][i]);
					}else if (status[nextStates[0][i]-'A']==1 && status[nextStates[1][i]-'A']==0){
						printf("\n%10c %10c (Deleted)%c", currStates[i], nextStates[0][i], nextStates[1][i]);
					}else{
						printf("\n%10c (Deleted)%c (Deleted)%c", currStates[i], nextStates[0][i], nextStates[1][i]);
					}
				}
			}
		}

		else if (inp[0]=='g'){
			char garbage[8]={'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'};

			if (garbageCollection(garbage, nextStates, currState)==1){
				printf("\n>>Garbage:");
				for (int i=0; i<8; i++){
					if (garbage[i]!='0'){
						printf(" %c,", garbage[i]);
					}
				}
			}else{
				printf("\n>>No Garbage");
			}
		}

		else if (inp[0]=='d'){
			if (inp[2]>='A' && inp[2]<='H'){
				if (status[inp[2]-'A']==1){
					printf("\n>>Deleted");
					status[inp[2]-'A']=0;
				}else{
					printf("\n>>Not Deleted");
				}
			}else{
				char garbage[8]={'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'};

				if (garbageCollection(garbage, nextStates, currState)==1){
					printf("\n>>Deleted:");
					for (int i=0; i<8; i++){
						if (garbage[i]!='0'){
							printf(" %c,", garbage[i]);
							status[garbage[i]-'A']=0;
						}
					}
				}else{
					printf("\n>>No States Deleted");
				}
			}
		}
	
	}while (inp[0]!='q');

    exit(0);
}
