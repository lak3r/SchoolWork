#include <iostream>
#include <string>
#include <fstream>
#include <cmath>

#include "helper.h"

using namespace std;

//fits
	long double vdw(long double temp, long double aGuess, long double bGuess, long double volume, int version);
	long double rk(long double temp, long double aGuess, long double bGuess, long double volume, int version);
	long double dieterici(long double temp, long double aGuess, long double bGuess, long double volume, int version);
	long double berthelot(long double temp, long double aGuess, long double bGuess, long double volume, int version);
	

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	double temp; //temperture assumed Kelvin
	string holder; //to be replaced shortly
	ifstream ifs;
	helper help;
	dataPoint *head;
	long double aGuess, bGuess; //the inital guess
	long double lambda = 0.001; //starting value of lambda
	long double error;
	int count = 0;
	long double (*fit)(long double, long double, long double, long double, int);
	long double beta[2], alpha[2][2], alphaMod[2][2], deltaGuess[2];
	
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
		
		ifs >> holder;
		cout << holder << "\n";
		if(holder == "vdw") fit = vdw;
		else if(holder == "rk") fit = rk;
		else if(holder == "dieterici") fit = dieterici;
		else if(holder == "berthelot") fit = berthelot;
		
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
	
	for(int i=0; i< 25 or count < 5; i++){
		
		error = help.error(head, temp, aGuess, bGuess, fit);
		cout << error << "\n";
		

		count = 5;
	}
	
	
	help.clearData(head);
	
	return 1;
}

//fits
long double vdw(long double temp, long double aGuess, long double bGuess, long double volume, int version){
	//cout<<"van der Waals ";
	
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume - bGuess)) - (aGuess/pow(volume, 2));
			break;
		case 1: //partial wrt a
			pressure = -1 / (volume * volume);
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp) / pow(volume - bGuess, 2);
			break;
		default:
			cout << "That's not an option. -vdw" << "\n";
			break;
	}
	
	
	return pressure;
}

long double rk(long double temp, long double aGuess, long double bGuess, long double volume, int version){
	//cout<<"Redlich-Kwong ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume - bGuess)) - (aGuess/(pow(temp, 0.5) * volume * (volume + bGuess)));
			break;
		case 1: //partial wrt a
			pressure = -1 / (pow(temp, 0.5) * volume * (volume + bGuess));
			break;
		case 2: //partial wrt b
			pressure = ((gasR * temp)/pow(volume - bGuess, 2)) + (aGuess/(pow(temp, 0.5) * volume * pow(volume + bGuess, 2)));
			break;
		default:
			cout << "That's not an option. -rk" << "\n";
			break;
	}
	
	
	return pressure;
}

long double dieterici(long double temp, long double aGuess, long double bGuess, long double volume, int version){
	//cout<<"Dieterici ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = (gasR * temp * exp((-1 * aGuess)/(gasR * temp * volume)))/(volume - bGuess);;
			break;
		case 1: //partial wrt a
			pressure = (-1 * exp((-1 * aGuess)/(gasR * temp * volume))) / (volume * (volume - bGuess));
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp * exp((-1 * aGuess)/(gasR * temp * volume)))/pow(volume - bGuess, 2);
			break;
		default:
			cout << "That's not an option. -dieterici" << "\n";
			break;
	}
	
	
	return pressure;
}

long double berthelot(long double temp, long double aGuess, long double bGuess, long double volume, int version){
	//cout<< "Berthelot ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume * bGuess)) - (aGuess/(temp * volume * volume));;
			break;
		case 1: //partial wrt a
			pressure = -1 / (temp * volume * volume);
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp) / pow(volume - bGuess, 2);
			break;
		default:
			cout << "That's not an option. -berthelot" << "\n";
			break;
	}
	
	
	return pressure;
}

