#include <iostream>
#include <fstream>

using namespace std;

int main(int argc, char* argv[]){
	
	//base test case
	if(argc>0 and (string(argv[1]) == "-hw")){
		cout << "Hello World\n";
		return 1;
	}
	
	//variables
	ifstream file;
	
	file.open(string(argv[1]));
	
	
	
	file.close();
}