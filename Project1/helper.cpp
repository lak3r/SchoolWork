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
	dataPoint *head;
	
	for(int i=0; i<5; i++){
		getline(input, holder);
		cout<< holder << "\n";
	}
	
	for(int i=0; input; i++){
		//head = new dataPoint
		
		getline(input, holder);
		if(i<10 or i%5==0) cout<<holder<<"\n";
	}

}

void helper::clearData(dataPoint *head){
	while(head.next != NULL){
		
	}
	
}