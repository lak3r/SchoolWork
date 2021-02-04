#include <iostream>
#include <math.h>
#include <fcntl.h>

using namespace std;

//prototypes
double sigmoid(double val);

int main(int argc, char* argv[]){
	
	//base test case
	if(argc>1 and (string(argv[1]) == "-hw")){
		cout << "Hello World\n";
		return 0;
	}
	//cout << "top of the line " << "\n";
	
	//variables
	double numInputs, numNeurons;
	double totSum = 0;
	int fileDescrip, rc; //whatever rc is
	double *inputAR, **weightsAR, *outputAR;
	
	
	fileDescrip = open(argv[1], O_RDONLY);
	
	rc = read(fileDescrip, &numInputs, sizeof(double));
	rc = read(fileDescrip, &numNeurons, sizeof(double));
	
	
	//malloc allacuations
	inputAR = (double *)malloc(numInputs * sizeof(double));
	outputAR = (double *)malloc(numNeurons * sizeof(double));
	weightsAR = (double **)malloc(numInputs * sizeof(double *));
	for(int i =0; i < numInputs; i++){
		weightsAR[i] = (double *)malloc(numNeurons * sizeof(double));
	}
	
	//assignments
	for(int i=0; i<numInputs; i++){
		rc = read(fileDescrip, &inputAR[i], sizeof(double));
		//cout << "input: " << inputAR[i] << "\n";
	}
	for(int i=0; i<numInputs; i++){
		//cout << "weights for " << i << ": ";
		for(int j=0; j<numNeurons; j++){
			rc = read(fileDescrip, &weightsAR[i][j], sizeof(double));
			//cout << weightsAR[i][j] << " , ";
		}
		//cout << "\n";
	}
	
	//cout<< "numNeurons: "<<numNeurons<<"\n";
	//the math actually
	for(int i=0; i<numNeurons; i++){
		outputAR[i] = 0;
		for(int j=0; j<numInputs; j++){
			outputAR[i] += inputAR[j] * weightsAR[j][i];
			//cout << inputAR[j] << " * " << weightsAR[j][i] << " = " 
			//<< inputAR[j] * weightsAR[j][i] << "\n";
		}
		//cout << "sum: " << outputAR[i] << "   ";
		outputAR[i] = sigmoid(outputAR[i]);
		totSum += outputAR[i];
		//cout << "Output: " << outputAR[i] << "\n";
		//cout << "Bottom of loop " << i << "\n";
	}
	
	//cout << "final output: " << totSum << "\n";
	printf("%.6lf", totSum);
	
	
	//return things to the void
	free(inputAR);
	free(weightsAR);
	free(outputAR);
	close(fileDescrip);
	return 1;
}

//will either be 0 or 1
double sigmoid(double val){
	return(1.0/(1.0 + exp(-val)));
}