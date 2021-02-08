#ifndef HELPER_H
#define HELPER_H



struct dataPoint {
	long double valueX;
	std::string unitsX;
	long double valueY;
	std::string unitsY;
	dataPoint *previous = NULL;
	dataPoint *next = NULL;
};

class helper{
	//For Vm, these can be as follows:
	public:
		const long double dm3mol = .001;//dm^3/mol - dm3 mol-1
		const long double m3mol = 1.0;//m^3/mol - m3 mol-1
		const long double cm3mol = 0.000001;//cm^3/mol - cm3 mol-1
		const long double lmol = .001;//L/mol - liter/mol (1 L = 10-3 m3)

	//For p, there are more choices:
	public:
		const long double pa = 1;//pa - Pa
		const long double megapa = 1000000;//megapa - MPa
		const long double kilobar = 100000000;//kilobar - Kbar
		const long double bar = 100000;//bar - bar (1 bar = 105 Pa)
		const long double atm = 101325;//atm - atm (1 atm = 101325 Pa)
		const long double torr = 133.322;//torr - torr (1 torr = 760/101325 Pa)
		const long double mmhg = 133.322;//mmHg
	
	//function prototypes
	public:
		void test(std::string t);
		dataPoint* readData(std::ifstream& input);
		void clearData(dataPoint *head);
		void convertToSIUnits(dataPoint *head);
		
		//fits
		void vdw();
		void rk();
		void dieterici();
		void berthelot();

};

#endif