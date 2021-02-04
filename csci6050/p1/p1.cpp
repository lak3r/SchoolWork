#include <iostream>
#include <fstream>
#include <math.h>

using namespace std;

//prototypes
double sigmoid(double val);

int main(int argc, char* argv[]){
	
	//base test case
	if(argc>1 and (string(argv[1]) == "-hw")){
		cout << "Hello World\n";
		return 0;
	}
	cout << "top of the line " << "\n";
	//variables
	//ifstream file;
	int numInputs = 3, numNeurons = 3;
	int buffer = 1000; //hmmm not sure how big this should be
	
	//make this malloc later
	double inputAR[] = {1, 2, 3 };
	double weightsAR[][3]={{1,2,3},{4,5,6},{5,6,7}};
	double outputAR[3];
	double totSum = 0;
	
	
	
	//file.open(string(argv[1]));
	
	for(int i=0; i<numNeurons; i++){
		outputAR[i] = 0;
		for(int j=0; j<numInputs; j++){
			outputAR[i] += inputAR[j] * weightsAR[j][i];
			cout << inputAR[j] << " * " << weightsAR[j][i] << " = " << inputAR[j] * weightsAR[j][i] << "\n";
		}
		cout << "sum: " << outputAR[i] << "   ";
		outputAR[i] = sigmoid(outputAR[i]);
		totSum += outputAR[i];
		cout << "Output: " << outputAR[i] << "\n";
	}
	
	cout << "final output: " << totSum << "\n";
	

	//file.close();
	return 1;
}

//will either be 0 or 1
double sigmoid(double val){
	return(1.0/(1.0 + exp(-val)));
}