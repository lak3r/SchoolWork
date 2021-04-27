#include <iostream>
#include <math.h>
#include <fcntl.h> //this is giving me read/write???
//#include <stdio.h> //not needed here but is on system64
//#include <unistd.h> //not needed here but is on system64

using namespace std;

struct node{
	char key[16];
	char val[16];
};


int main(int argc, char* argv[]){
	
	//base test case
	if(argc>1 and (string(argv[1]) == "-hw")){
		cout << "Hello World\n";
		exit(0);
	}
	else if(argc == 3){
		
	}
	else{
		cout << "Invalid input";
		exit(0);
	}
	
	
	return 1;
}
