#include <iostream>
#include <fstream>
#include <string>

#include "helper.h"

using namespace std;


void helper::test(string t){	
	cout<<t;
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
	
	cout <<	current->valueX << " " << current->unitsX << "    ";
	cout << current->valueY << " " << current->unitsY << "\n";
	
	
	for(int i=0; input; i++){
		current->next = new dataPoint;
		current->next->previous = current;
		current = current->next;
		input >> current->valueX >> current->valueY;
		current->unitsX = current->previous->unitsX;
		current->unitsY = current->previous->unitsY;
		
		if(i<10 or i%10==0 or i==989){
			cout << "Point " << i+1 << ":  ";
			cout <<	current->valueX << " " << current->unitsX << "    ";
			cout << current->valueY << " " << current->unitsY << "\n";
		}
	}
	
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
	
	while(current->next != NULL){
		cout<< "in the convert loop. line: " << i <<"\n";
		
		//For the X variable
		if(current->unitsX == "dm^3/mol") cout << "units are: " << current->unitsX << "\n";
		else if(current->unitsX == "m^3/mol") cout << "units are: " << current->unitsX << "\n";
		else if(current->unitsX == "cm^3/mol") cout << "units are: " << current->unitsX << "\n";
		else if(current->unitsX == "L^3/mol") cout << "units are: " << current->unitsX << "\n";
		else cout << "Wrong town boyo" << "\n";
		
		if(current->unitsY == "pa") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "megapa") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "kilobar") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "bar") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "atm") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "torr") cout << "units are: " << current->unitsY << "\n";
		else if(current->unitsY == "mmHg") cout << "units are: " << current->unitsY << "\n";
		else cout << "How did you end up here?" << "\n";
		
		current = current->next;
		i++;
	}
}

void vdw(double temp){
	//van der Waals
	
	double r = 1; //google it
	
	
	//p=((RT)/(V -b)) - (a/(V^2))
}

void rk(double temp){
	//Redlich-Kwong
	
	double r = 1; //google it
}

void dieterici(double temp){
	//Dieterici
	
	double r = 1; //google it
}

void berthelot(double temp){
	//Berthelot
	
	double r = 1; //google it
}