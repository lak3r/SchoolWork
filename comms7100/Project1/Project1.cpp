#include <iostream>
#include <string>
#include <fstream>
#include <cmath>
#include <algorithm>

#include "helper.h"

using namespace std;

//fits
	long double vdw(long double temp, long double *guess, long double M, long double volume, int version);
	long double rk(long double temp, long double *guess, long double M, long double volume, int version);
	long double dieterici(long double temp, long double *guess, long double M, long double volume, int version);
	long double berthelot(long double temp, long double *guess, long double M, long double volume, int version);
	

int main(int argc, char* argv[]){
	
	//variables
	string fileName; //file to be processed
	double temp; //temperture assumed Kelvin
	string holder; //holds strings so they can be processed
	ifstream ifs; //infut file
	ofstream gFile; //output file to be formatted as a csv
	helper help;
	dataPoint *head; //pointer to the first data point
	long double *guess, *deltaGuess; //this will be created with malloc 
	long double M; //number of parameters (ld for math later)
	long double lambda = 0.001; //starting value of lambda
	long double error, newError, deltaError;
	int count = 0;
	long double (*fit)(long double, long double*, long double, long double, int);
	long double beta[2], alpha[2][2], alphaMod[2][2], alphaSolv[0][2];
	bool fullReset = true;
	long double variance, standDev[2];
	
	//test function
	if(argc>0 and (string(argv[1]) == "-test")){
		help.test("Hello World\n");
		exit(EXIT_SUCCESS);
	}
	
	
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
		holder = help.convertToLowerCase(holder);
		cout << holder << "\n";
		if(holder == "vdw"){
			fit = vdw;
			M = 2;
		}
		else if(holder == "rk"){
			fit = rk;
			M = 2;
		}
		else if(holder == "dieterici"){
			fit = dieterici;
			M = 2;
		}
		else if(holder == "berthelot"){
			fit = berthelot;
			M = 2;
		}
		else if(holder == "viral"){
			//fit = viral; this needs to be formatted
			ifs >> M;
		}
		guess = (long double *)malloc(M * sizeof(long double));
		deltaGuess = (long double *)malloc(M * sizeof(long double));
		
		ifs >> guess[0] >> guess[1];
		cout << guess[0] << "   " << guess[1] << "\n";
		
		ifs >> holder >> temp >> holder;
		cout << "temp: " << temp << " K\n";
		
		//Note that for this the data will be:
		//Vm as the X data 
		//p as the Y data
		head = help.readData(ifs);
		help.printData(head);
		ifs.close();
		
		help.convertToSIUnits(head);
		cout << "Converted to SI units" << "\n";
		help.printData(head);
	}
	else{
		cout << "How did you even get here?" << "\n";
		cout << "You're file isn't open and you skipped the safeguard somehow" << "\n";
		exit(EXIT_FAILURE);
	}
	
	//the good stuff
	for(int i=0; count < 5; i++){
		cout << "\n" << "------------------------------------------------------------------------------" << "\n";
		cout << "cycle: " << i +1 << "\n";
		cout << "lambda: " << lambda << "\n";
		if(fullReset){
			error = help.error(head, temp, guess, M, fit);
			cout << "The error is: " << error << "\n";
			
			beta[0] = help.beta(head, temp, guess, M, 1, fit);
			beta[1] = help.beta(head, temp, guess, M, 2, fit);
			
			cout << "beta array:\n\t[" << beta[0] << ", " << beta[1] << "]" << "\n";
			
			help.alpha(head, temp, guess, M, alpha, fit);
		}
		//printing alpha
		cout << "alpha array: \n\t[" << alpha[0][0] << ", " << alpha[0][1] << "]" << "\n";
		cout << "\t[" << alpha[1][0] << ", " << alpha[1][1] << "]" << "\n";
		
		help.modifyAlpha(alpha, alphaMod, lambda);
		cout << "Modified alpha array: \n\t[" << alphaMod[0][0] << ", " << alphaMod[0][1] << "]" << "\n";
		cout << "\t[" << alphaMod[1][0] << ", " << alphaMod[1][1] << "]" << "\n";
		
		help.solveLinSys(alphaMod, beta, alphaSolv[0]);
		cout<< "delta is:\n\t a: " << alphaSolv[0][0] << "  b: " << alphaSolv[0][1] << "\n";
		
		for(int j=0; j<M; j++){
			deltaGuess[j] = guess[j] + alphaSolv[0][j];
		}
		cout << "Old parameters" << "\n\t" << "a: " << guess[0] << "  b: " << guess[1] << "\n";
		cout << "New parameters" << "\n\t" << "a: " << guess[0] + alphaSolv[0][0] << "  b: " << guess[1] + alphaSolv[0][1] << "\n";
		
		newError = help.error(head, temp, deltaGuess, M, fit);
		cout << "Old error: " << error << "\n";
		cout << "New error: " << newError << "\n";
		deltaError = error - newError; 
		
		if(deltaError <= 0){
			lambda *= 10;
			fullReset = false;
		}
		else if(deltaError > 0){
			lambda /= 10;
			fullReset = true;
			guess[0] += alphaSolv[0][0];
			guess[1] += alphaSolv[0][1];
			cout << "new parameters accepted\n" ;
		}
		
		if((deltaError>0 and deltaError<10e-5) or (deltaError<0 and deltaError> -10e-5)) count ++;
	}
	
	//Final Statistics
	{
		cout << "\n" << "------------------------------------------------------------------------------" << "\n";
		cout << "Final Statistics" << "\n";
		
		cout << "chi square: " << newError << "\n";
		
		variance = help.variance(head, temp, guess, M, fit);
		cout << "sample variance: " << variance << "\n";
		
		cout << "variance-covariance matrix C: " << "\n";
		help.invert(alpha, alphaMod);
		cout << "\t[" << alphaMod[0][0] << ", " << alphaMod[0][1] << "]" << "\n";
		cout << "\t[" << alphaMod[1][0] << ", " << alphaMod[1][1] << "]" << "\n";
		
		cout << "Standard Deviation:" << "\n";
		standDev[0] = pow(variance * alphaMod[0][0], 0.5);
		standDev[1] = pow(variance * alphaMod[1][1], 0.5);
		cout << "\ta: " << standDev[0] <<  "   b: " << standDev[1] << "\n";
		
		cout << "correlation coefficient: " << (variance * alphaMod[0][1]) / (standDev[0] * standDev[1]) << "\n";
		
		cout << "coefficient of determination R squared: " << 1 - (newError / help.sumSquared(head)) << "\n";
		cout << "R bar squared: " << help.rBarSquared(head, temp, guess, M, fit) << "\n";
		cout << "R-Factor: " << help.rFactor(head, temp, guess, M, fit) * 100 << "%" << "\n";
		
		cout << "Final parameters\n\ta: " << guess[0] << "  b: " << guess[1] << "\n";
	}
	
	//output file for graphing
	{
		gFile.open(help.changeExtention(argv[1]));
		gFile << "Volume,Observed Pressure,Calculated Pressure" << "\n";
		
		dataPoint *current = head;
		while(current->next != NULL){
			gFile << current->valueX << "," << current->valueY << "," << (*fit)(temp, guess, M, current->valueX, 0) << "\n";
			current = current->next;
		}
		
		gFile.close();
	}
	

	help.clearData(head);
	free(deltaGuess);
	free(guess);
	return 1;
}

//fits
long double vdw(long double temp, long double *guess, long double M, long double volume, int version){
	//cout<<"van der Waals ";
	
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume - guess[1])) - (guess[0]/pow(volume, 2));
			//cout << "just the function pressure is: " << pressure << "\n";
			break;
		case 1: //partial wrt a
			pressure = -1 / (volume * volume);
			//cout << "partial wrt a pressure is: " << pressure << "\n";
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp) / pow(volume - guess[1], 2);
			//cout << "partial wrt b pressure is: " << pressure << "\n";
			break;
		default:
			//cout << "That's not an option. -vdw" << "\n";
			break;
	}
	
	
	return pressure;
}

long double rk(long double temp, long double *guess, long double M, long double volume, int version){
	//cout<<"Redlich-Kwong ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume - guess[1])) - (guess[0]/(pow(temp, 0.5) * volume * (volume + guess[1])));
			break;
		case 1: //partial wrt a
			pressure = -1 / (pow(temp, 0.5) * volume * (volume + guess[1]));
			break;
		case 2: //partial wrt b
			pressure = ((gasR * temp)/pow(volume - guess[1], 2)) + (guess[0]/(pow(temp, 0.5) * volume * pow(volume + guess[1], 2)));
			break;
		default:
			cout << "That's not an option. -rk" << "\n";
			break;
	}
	return pressure;
}

long double dieterici(long double temp, long double *guess, long double M, long double volume, int version){
	//cout<<"Dieterici ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = (gasR * temp * exp((-1 * guess[0])/(gasR * temp * volume)))/(volume - guess[1]);;
			break;
		case 1: //partial wrt a
			pressure = (-1 * exp((-1 * guess[0])/(gasR * temp * volume))) / (volume * (volume - guess[1]));
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp * exp((-1 * guess[0])/(gasR * temp * volume)))/pow(volume - guess[1], 2);
			break;
		default:
			cout << "That's not an option. -dieterici" << "\n";
			break;
	}
	
	
	return pressure;
}

long double berthelot(long double temp, long double *guess, long double M, long double volume, int version){
	//cout<< "Berthelot ";
	long double gasR = 8.31447; //pa m^3 K^-1 mol^-1
	long double pressure = 0.0;
	
	switch(version){
		case 0: //just the functions
			pressure = ((gasR * temp)/(volume - guess[1])) - (guess[0]/(temp * volume * volume));
			break;
		case 1: //partial wrt a
			pressure = -1 / (temp * volume * volume);
			break;
		case 2: //partial wrt b
			pressure = (gasR * temp) / pow(volume - guess[1], 2);
			break;
		default:
			cout << "That's not an option. -berthelot" << "\n";
			break;
	}
	
	
	return pressure;
}

