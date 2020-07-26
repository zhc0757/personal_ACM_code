#include<algorithm>
#include<climits>
using namespace std;
//完全最短路算法 Floyd
class CFloyd
{
	static const unsigned N = 110;
	unsigned adjacencyMatrix[N][N];
	unsigned n;
	unsigned shortestDis[N][N];
public:
	//start from 1
	void init(unsigned n_)
	{
		unsigned i, j;
		n = n_;
		for (i = 1; i <= n; ++i)
			for (j = 1; j <= n; ++j)
				adjacencyMatrix[i][j] = INT_MAX;
	}
	void setAWay(unsigned a, unsigned b, unsigned weight)
	{
		if (weight < adjacencyMatrix[a][b]) {
			adjacencyMatrix[a][b] = weight;
			adjacencyMatrix[b][a] = weight;
		}
	}
	void calculate()
	{
		unsigned i, j, k;
		for (i = 1; i <= n; ++i)
			for (j = 1; j <= n; ++j)
				shortestDis[i][j] = adjacencyMatrix[i][j];
		for (k = 1; k <= n; ++k)
			for (i = 1; i <= n; ++i)
				for (j = 1; j <= n; ++j)
					/*if (shortestDis[i][k] < INT_MAX&& shortestDis[k][j] < INT_MAX)*/shortestDis[i][j] = min(shortestDis[i][j], shortestDis[i][k] + shortestDis[k][j]);
	}
	unsigned shortestDistance(unsigned from, unsigned to)
	{
		return shortestDis[from][to];
	}
};
