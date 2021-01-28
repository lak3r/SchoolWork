#include <iostream>
#include <string>
#include <fstream>

#include "helper.h"

using namespace std;

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	ifstream ifs;
	
	if(argc>0 and (string(argv[1]) == "-test")){
		//These two lines are a test
		helper help;
		help.test("Hello World\n");
	}
	
	ifs.open(string(argv[1]));
	if(ifs.is_open()){
		string test;
		getline( ifs, test);
		cout<< test;
	}
	


}