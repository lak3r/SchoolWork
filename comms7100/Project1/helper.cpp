#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <algorithm>

#include "helper.h"

using namespace std;


void helper::test(string t){	
	cout<<t;
	transform(t.begin(), t.end(), t.begin(), ::tolower);
	cout << t;
}
/*
string helper::convertToLowerCase(std::string tbc){
	for(int i=0; i< tbc.end(); i++){
		if(tbc[i] >= 'A' && tbc[i] <= 'Z'){
			tbc[i] += 32;
		}
	}
	return tbc;
}
*/

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
	
	for(int i= 0; current != NULL; i++){
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
		//cout << "In the Error. running total: " << S << "\n";
		current = current->next;
	}
	
	return beta;
}

void helper::alpha(dataPoint *head, long double temp, long double aGuess, long double bGuess, long double alpha[2][2],
						long double (*fit)(long double temp, long double aGuess, long double bGuess, long double volume, int version)){
							
	dataPoint *current = head;
	
	for(int i =1 ;i<3; i++){
		for(int j =1; j<3; j++){
			while(current->next != NULL){
				alpha[i-1][j-1]=(*fit)(temp, aGuess, bGuess, current->valueX, i)
								* (*fit)(temp, aGuess, bGuess, current->valueX, j);				
				current = current->next;
			}
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
	cout << "printing alphaMod array:" << "\n";
	for(int i=0;i<2;i++){
		for(int j=0;j<2;j++){
			cout << alphaMod[i][j] << "   ";
		}
		cout << "\n";
	}
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

long double helper::r(){
	//correlation coeffiecient r
	return 0.0;
}

