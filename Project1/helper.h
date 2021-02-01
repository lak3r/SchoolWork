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
	public:
		void test(std::string t);
		dataPoint* readData(std::ifstream& input);
		void clearData(dataPoint *head);
		void convertToSIUnits(dataPoint *head);

};

#endif