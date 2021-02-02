#ifndef HELPER_H
#define HELPER_H

struct units {
	//For Vm, these can be as follows:
	public const long double dm3mol;//dm^3/mol - dm3 mol-1
	public const long double m3mol;//m^3/mol - m3 mol-1
	public const long double cm3mol;//cm^3/mol - cm3 mol-1
	public const long double lmol;//L/mol - liter/mol (1 L = 10-3 m3)

	//For p, there are more choices:
	public const long double pa;//pa - Pa
	public const long double megapa;//megapa - MPa
	public const long double kilobar;//kilobar - Kbar
	public const long double bar;//bar - bar (1 bar = 105 Pa)
	public const long double atm;//atm - atm (1 atm = 101325 Pa)
	public const long double torr;//torr - torr (1 torr = 760/101325 Pa)
	public const long double mmhg;//mmHg
}


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