#ifndef HELPER_H
#define HELPER_H

struct units {
	//For Vm, these can be as follows:
	public:
		const long double dm3mol = 1;//dm^3/mol - dm3 mol-1
		const long double m3mol = 1;//m^3/mol - m3 mol-1
		const long double cm3mol = 1;//cm^3/mol - cm3 mol-1
		const long double lmol = 1;//L/mol - liter/mol (1 L = 10-3 m3)

	//For p, there are more choices:
	public:
		const long double pa = 1;//pa - Pa
		const long double megapa = 1;//megapa - MPa
		const long double kilobar = 1;//kilobar - Kbar
		const long double bar = 1;//bar - bar (1 bar = 105 Pa)
		const long double atm = 1;//atm - atm (1 atm = 101325 Pa)
		const long double torr = 1;//torr - torr (1 torr = 760/101325 Pa)
		const long double mmhg = 1;//mmHg
};


struct dataPoint {
	long double valueX;
	std::string unitsX;
	long double valueY;
	std::string unitsY;
	dataPoint *previous = NULL;
	dataPoint *next = NULL;
};

class helper{
	public:
		void test(std::string t);
		dataPoint* readData(std::ifstream& input);
		void clearData(dataPoint *head);
		void convertToSIUnits(dataPoint *head);

};

#endif