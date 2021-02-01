#include <iostream>
#include <string>
#include <fstream>

#include "helper.h"

using namespace std;

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	ifstream ifs;
	helper help;
	dataPoint *head;
	
	if(argc>0 and (string(argv[1]) == "-test")){
		//This is a test
		help.test("Hello World\n");
	}
	
	ifs.open(string(argv[1]));
	while(!ifs.is_open()){ //this doesn't actually work yet
		cout<<"\nWhat file would you like to open? ";
		cin>> fileName;
		ifs.open(fileName);
	}
	if(ifs.is_open()){
		head = help.readData(ifs);
	}
	

	help.clearData(head);
}

