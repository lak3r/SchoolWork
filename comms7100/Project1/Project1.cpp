#include <iostream>
#include <string>
#include <fstream>
#include <cmath>

#include "helper.h"

using namespace std;

//fits
	long double vdw(long double temp, long double aGuess, long double bGuess, long double volume);
	long double rk(long double temp, long double aGuess, long double bGuess, long double volume);
	long double dieterici(long double temp, long double aGuess, long double bGuess, long double volume);
	long double berthelot(long double temp, long double aGuess, long double bGuess, long double volume);
	

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	string equation; //might make this an enum tbd
	double temp; //temperture assumed Kelvin
	string holder; //to be replaced shortly
	ifstream ifs;
	helper help;
	dataPoint *head;
	long double aGuess, bGuess; //the inital guess
	long double lambda = 0.001; //starting value of lambda
	long double error;
	int count = 0;
	long double (*fit)(long double, long double, long double, long double);
	
	//test functions
	if(argc>0 and (string(argv[1]) == "-test")){
		help.test("Hello World\n");
		exit(EXIT_SUCCESS);
	}
	
	if(argc>0 and (string(argv[1]) == "-echo")){
		cout << "echo test: " << string(argv[2]) << "\n";
		//cout << "to loer: " << help.convertToLowerCase(string(argv[2])) << "\n"
		exit(EXIT_SUCCESS);
	}
	//end test functions
	
	//it's prefered to give the file via command line
	ifs.open(string(argv[1]));
	while(!ifs.is_open()){
		//this loop won't exit until it opens a valid file.
		//That means you don't have to use cammand line if you don't wantS
		cout <<"\nWhat file would you like to open? ";
		cin >> fileName;
		ifs.open(fileName);
	}
	if(ifs.is_open()){
		getline(ifs, holder);
		cout << holder << "\n";
		
		ifs >> equation;
		cout << equation << "\n";
		if(equation == "vdw") fit = vdw;
		else if(equation == "rk") fit = rk;
		else if(equation == "dieterici") fit = dieterici;
		else if(equation == "berthelot") fit = berthelot;
		
		ifs >> aGuess >> bGuess;
		cout << aGuess << "   " << bGuess << "\n";
		
		ifs >> holder >> temp >> holder;
		cout << "temp: " << temp << " K\n";
		
		//Note that for this the data will be:
		//Vm as the X data 
		//p as the Y data
		head = help.readData(ifs);
		help.printData(head);
		
		help.convertToSIUnits(head);
		cout << "Converted to SI units" << "\n";
		help.printData(head);
	}
	else{
		cout << "How did you even get here?" << "\n";
		cout << "You're file isn't open and you skipped the safeguard somehow" << "\n";
		exit(EXIT_FAILURE);
	}
	
	while(count < 5){
		
		error = help.error(head, temp, aGuess, bGuess, fit);
		cout << error << "\n";
		

		count = 5;
	}
	
	
	help.clearData(head);
	
	return 1;
}

//fits
long double vdw(long double temp, long double aGuess, long double bGuess, long double volume){
	//cout<<"van der Waals ";
	
	long double gasR = 1; //google it
	
	return ((gasR * temp)/(volume - bGuess)) - (aGuess/pow(volume, 2));
	//p=((RT)/(V -b)) - (a/(V^2))
}

long double rk(long double temp, long double aGuess, long double bGuess, long double volume){
	//cout<<"Redlich-Kwong ";
	
	long double gasR = 1; //google it
	return ((gasR * temp)/(volume - bGuess)) - (aGuess/(pow(temp, 0.5) * volume * (volume + bGuess)));
}

long double dieterici(long double temp, long double aGuess, long double bGuess, long double volume){
	//cout<<"Dieterici ";
	
	long double gasR = 1; //google it
	return (gasR * temp * exp((-1 * aGuess)/(gasR * temp * volume)))/(volume - bGuess);
}

long double berthelot(long double temp, long double aGuess, long double bGuess, long double volume){
	//cout<< "Berthelot ";
	
	long double gasR = 1; //google it
	return ((gasR * temp)/(volume * bGuess)) - (aGuess/(temp * volume * volume));
}

