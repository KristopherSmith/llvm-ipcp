#include "stdio.h"

void add(int, int);
int mult(int, int);
int main(void){
	int z=20;
	printf("Yo hommie\n");
//	int y=add(z);
//	printf("Addition value: %d \n", y);

	int a = 5;
	int b = 10;
	int product = mult(a, b);
	printf("Multiplication value: %d \n", product);
	
	// second time
	mult(a, b);
	// third time
	mult(a, b);	
	printf("Multiplication value: %d \n", product);
	
	
}

void add(int x, int y){
	x += y;
}

int mult(int x, int y) {
	int sum = 0;
	for (int i = 0; i < y; i++) {
		add(sum, x);
	}
	return sum;
}

