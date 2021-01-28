#include <iostream>
#include <fstream>
#include <string>
#include "helper.h"


using namespace std;


void helper::test(string t){
	
	cout<<t;
	
}

void helper::readData(ifstream& input){
	string holder;
	
	for(int i=0; i<4; i++){
	
		getline(input, holder);
		cout<< holder << "\n";
	}
}