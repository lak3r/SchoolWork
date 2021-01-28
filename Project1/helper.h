#ifndef HELPER_H
#define HELPER_H
struct dataPoint {
	double valueX;
	std::string unitsX;
	double valueY;
	std::string unitsY;
};

class helper{
	public:
		void test(std::string t);
		void readData(std::ifstream& input);

};

#endif