#pragma once
#include<sstream>
using namespace std;
string dbl2str(double x)
{
	stringstream ss;
	string str;
	ss << x;
	ss >> str;
	return str;
}
string bool2str(bool b)
{
	return (b ? "True" : "False");
}
template<typename T>string tostring(T x)
{
	stringstream ss;
	string str;
	ss << x;
	ss >> str;
	return str;
}