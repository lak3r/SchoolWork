#include <iostream>
#include <string>
#include <fstream>

#include "helper.h"

using namespace std;

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	string equation; //might make this an enum tbd
	double temp; //temperture assumed Kelvin
	string holder; //to be replaced shortly
	ifstream ifs;
	helper help;
	dataPoint *head;
	dataPoint *guess = new dataPoint; //initial guess
	
	if(argc>0 and (string(argv[1]) == "-test")){
		//This is a test
		help.test("Hello World\n");
	}
	
	ifs.open(string(argv[1]));
	while(!ifs.is_open()){ //this doesn't actually work yet
		cout <<"\nWhat file would you like to open? ";
		cin >> fileName;
		ifs.open(fileName);
	}
	if(ifs.is_open()){
		getline(ifs, holder);
		cout << holder << "\n";
		
		ifs >> equation;
		cout << equation << "\n";
		
		ifs >> guess->valueX >> guess->valueY;
		cout << guess->valueX << "   " << guess->valueY << "\n";
		
		ifs >> holder >> temp >> holder;
		cout << "temp: " << temp << " K\n";
		
		
		head = help.readData(ifs);
		
		help.convertToSIUnits(head);
	}
	
	delete guess;
	help.clearData(head);
	
	return 1;
}

