#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <algorithm>

#include "helper.h"

using namespace std;


void helper::test(string t){	
	cout<<t;
	cout << convertToLowerCase(t);
}

string helper::convertToLowerCase(std::string tbc){
	transform(tbc.begin(), tbc.end(), tbc.begin(), ::tolower);
	return tbc;
}


//consider looking into malloc for this? Probably not cause I'm using a struct.
dataPoint* helper::readData(ifstream& input){
	string holder;
	dataPoint *head, *current;
	
	head = new dataPoint;
	input >> head->unitsX;
	input >> head->unitsY;
	input >> head->valueX >> head->valueY;
	current = head;
	
	for(int i=0; input; i++){
		current->next = new dataPoint;
		current->next->previous = current;
		current = current->next;
		input >> current->valueX >> current->valueY;
		current->unitsX = current->previous->unitsX;
		current->unitsY = current->previous->unitsY;	
	}
	
	delete current;
	
	return(head);
}

void helper::clearData(dataPoint *current){
	
	dataPoint *next = current-> next;
	while(next != NULL){
		delete current;
		current = next;
		next = current ->next;
	}
}

void helper::convertToSIUnits(dataPoint *head){
	//There are a limited number of options so this isn't bad
	//making a struct with the consts needed to convertToSIUnits
	
	dataPoint *current = head;
	int i =0; //temp
	
	while(current != NULL){
		//cout<< "in the convert loop. line: " << i <<"\n";
		
		//For the X variable
		if(current->unitsX == "dm^3/mol") current->valueX *= dm3mol;
		else if(current->unitsX == "m^3/mol") current->valueX *= m3mol;
		else if(current->unitsX == "cm^3/mol") current->valueX *= cm3mol;
		else if(current->unitsX == "L^3/mol") current->valueX *= lmol;
		else cout << "Wrong town boyo" << "\n";
		current->unitsX = "m^3/mol";
		//cout << "units are: " << current->unitsX << "\n";
		
		if(current->unitsY == "pa") current->valueY *= pa;
		else if(current->unitsY == "megapa") current->valueY *= megapa;
		else if(current->unitsY == "kilobar") current->valueY *= kilobar;
		else if(current->unitsY == "bar") current->valueY *= bar;
		else if(current->unitsY == "atm") current->valueY *= atm;
		else if(current->unitsY == "torr") current->valueY *= torr;
		else if(current->unitsY == "mmHg") current->valueY *= mmhg;
		else cout << "How did you end up here?" << "\n";
		current->unitsY = "pa";
		//cout << "units are: " << current->unitsY << "\n";
		
		current = current->next;
		i++;
	}
}

void helper::printData(dataPoint *head){
	dataPoint *current = head;
	
	for(int i= 0; current->next != NULL; i++){
		if(i<10 or i%10==0){
			cout << "Point " << i+1 << ":  ";
			cout <<	current->valueX << " " << current->unitsX << "    ";
			cout << current->valueY << " " << current->unitsY << "\n";
		}
		
		current = current->next;
	}
}



//maths
long double helper::error(dataPoint *head, long double temp, long double aGuess, long double bGuess, 
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
	//chi-squared
	long double S = 0;
	dataPoint *current = head;
	
	while(current->next != NULL){
		S += pow((current->valueY) - ((*fit)(temp, aGuess, bGuess, current->valueX, 0)), 2);
		//cout << "In the Error. running total: " << S << "\n";
		current = current->next;
	}
	
	return S;
}

long double helper::beta(dataPoint *head, long double temp, long double aGuess, long double bGuess, int derivative,
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
	//chi-squared
	long double beta = 0;
	dataPoint *current = head;
	
	while(current->next != NULL){
		beta += ((current->valueY) - (*fit)(temp, aGuess, bGuess, current->valueX, 0))
				* (*fit)(temp, aGuess, bGuess, current->valueX, derivative);
		current = current->next;
	}
	
	return beta;
}

void helper::alpha(dataPoint *head, long double temp, long double aGuess, long double bGuess, long double alpha[2][2],
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
							
	dataPoint *current = head;
	
	for(int i =1 ;i<3; i++){
		for(int j =1; j<3; j++){
			//cout << "in alpha[" << i-1 << "][" << j-1 << "]" << "\n";
			alpha[i-1][j-1] = 0;
			while(current->next != NULL){
				alpha[i-1][j-1] += (*fit)(temp, aGuess, bGuess, current->valueX, i)
				    				* (*fit)(temp, aGuess, bGuess, current->valueX, j);				
				current = current->next;
				
				
			}
			//cout << "in the alpha tot: " << alpha[i-1][j-1] << "\n";
			current = head;
		}
	}
}

void helper::modifyAlpha(long double alpha[2][2], long double alphaMod[2][2], long double lambda){
	
	for(int i=0; i<2; i++){
		for(int j=0; j<2; j++){
			alphaMod[i][j] = alpha[i][j];
			if(i==j) alphaMod[i][j] *= (1 + lambda);
		}
	}
	//the problem is here I think
}

void helper::solveLinSys(long double A[2][2], long double b[2], long double solutions[2]){
	long double augmented[2][3]={{A[0][0], A[0][1], b[0]},{A[1][0], A[1][1], b[1]}};
	
	//divide each row by first elliment
	for(int i=0; i <2; i++){
		for(int j=2; j>=0; j--){
			//cout << "dividing by: " << augmented[i][0] << "\n";
			augmented[i][j] /= augmented[i][0];
		}
	}
	
	
	//subtract first row from seccond row
	for(int i=0;i<3;i++){
		augmented[1][i]-= augmented[0][i];
	}
	
	//second row now only has one variable in it
	solutions[1] = augmented[1][2] / augmented[1][1]; //that's b
	
	//backsubsitute into the other equation
	solutions[0] = (augmented[0][2] - (augmented[0][1] * solutions[1]));
	
	//cout<< "bottom of lin solv\n a: " <<  solutions[0] << "  b: " << solutions[1] << "\n";
}

long double helper::variance(dataPoint *head, long double temp, long double aGuess, long double bGuess,
					long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
	//sample variance
	long double variance = 0.0;
	dataPoint *current = head;
	long double N = 0;
	
	while(current->next != NULL){
		variance += pow(current->valueY - (*fit)(temp, aGuess, bGuess, current->valueX, 0), 2);
		N ++;
		current = current->next;
	}
	
	variance *= 1 / (N -2);
	return(variance);
}

long double helper::rBarSquared(dataPoint *head, long double temp, long double aGuess, long double bGuess,
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
	long double rBarSquared = 1;
	dataPoint *current = head;
	long double N = 0;
	
	//get N
	while(current->next != NULL){
		N++;
		current = current->next;
	}
	
	rBarSquared -= (error(head, temp, aGuess, bGuess, fit) *  (N -1 )) /
					(sumSquared(head) * (N - 3));
	
	return rBarSquared;
}

long double helper::sumSquared(dataPoint *head){
	dataPoint *current = head;
	long double N = 0;
	long double mean = 0;
	long double sumSquares = 0;
	
	while(current->next != NULL){
		mean += current->valueY;
		N++;
		current = current->next;
	}
	mean /= N;
	
	current = head;
	while(current->next != NULL){
		sumSquares += pow(current->valueY - mean, 2);
		current = current->next;
	}
	
	return sumSquares;
}

long double helper::rFactor(dataPoint *head, long double temp, long double aGuess, long double bGuess,
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
	//r factor
	dataPoint *current = head;
	long double rFactor = 0;
	long double hold;
	
	while(current->next != NULL){
		hold = current->valueY - (*fit)(temp, aGuess, bGuess, current->valueX, 0);
		if(hold<0) hold *= -1;
		rFactor += hold;
		current = current->next;
	}
	
	hold = 0;
	current = head;
	while(current->next!=NULL){
		if(current->valueY >= 0) hold += current->valueY;
		else if(current->valueY < 0) hold -= current->valueY;
		current = current->next;
	}
	
	rFactor /= hold;
	
	return rFactor;
}