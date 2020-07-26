#include<cmath>
#include<vector>
#include<cstring>
#include<list>
#include<algorithm>
#include<functional>
#include<cstdlib>
#include<bitset>
#include<climits>
#include<queue>
#include<stack>
#include<string>
using namespace std;
float log(float a, float N)
{
	return log10(N) / log10(a);
}
double fpi(unsigned int n)
{
	unsigned i;
	double t,p,m;
	t = 0;
	p = 0;
	for (i = 1; i <= n; i++) {
		m = i*i;
		p = 1 / m;
		t = t + p;
	}
	return sqrt(6 * t);
}
unsigned maxnum(float x[],unsigned int n)
{
	unsigned i,m;
	m = 0;
	for (i = 0; i < n; i++) {
		if (x[i] > x[m]) {
			m = i;
		}
	}
	return m;
}
unsigned minnum(float x[], unsigned int n)
{
	unsigned i, m;
	m = 0;
	for (i = 0; i < n; i++) {
		if (x[i] < x[m]) {
			m = i;
		}
	}
	return m;
}
float*amax(float x[], unsigned n)
{
	unsigned i, j, m,p;
	float*t = new float[n];
	bool*nonchosen = new bool[n];
	for (i = 0; i < n; i++) {
		nonchosen[i] = 1;
	}
	p = minnum(x, n);
	for (i = 0; i < n; i++) {
		m = p;
		for (j = 0; j < n; j++) {
			if ((x[j] > x[m]) && nonchosen[j]) {
				nonchosen[m] = 1;
				m = j;
				nonchosen[j] = 0;
			}
		}
		t[i] = x[m];
	}
	delete [] nonchosen;
	return t;
}
float*amin(float x[], unsigned n)
{
	unsigned i, j, m, p;
	float*t = new float[n];
	bool*nonchosen = new bool[n];
	for (i = 0; i < n; i++) {
		nonchosen[i] = 1;
	}
	p = maxnum(x, n);
	for (i = 0; i < n; i++) {
		m = p;
		for (j = 0; j < n; j++) {
			if ((x[j] < x[m]) && nonchosen[j]) {
				nonchosen[m] = 1;
				m = j;
				nonchosen[j] = 0;
			}
		}
		t[i] = x[m];
	}
	delete[] nonchosen;
	return t;
}
//并查集
struct uf_note
{
	unsigned type;
	vector<unsigned>v;
};
class union_find
{
	unsigned l;
	uf_note*ufn;
public:
	void init(unsigned length)
	{
		unsigned i, j, k, t;
		l = length;
		ufn = new uf_note[l];
		for (i = 0; i < l; ++i)ufn[i].type = i;
	}
	void addonerela(unsigned x, unsigned y)
	{
		if (ufn[x].type != ufn[y].type) {
			unsigned len = ufn[ufn[y].type].v.size(), i, t = ufn[y].type;
			ufn[ufn[x].type].v.push_back(t);
			ufn[t].type = ufn[x].type;
			for (i = 0; i < len; ++i) {
				ufn[ufn[t].v[i]].type = ufn[x].type;
				ufn[ufn[x].type].v.push_back(ufn[t].v[i]);
			}
			ufn[t].v.clear();
		}
	}
	bool relative(unsigned x, unsigned y)
	{
		return ufn[x].type == ufn[y].type;
	}
	void del()
	{
		unsigned i;
		for (i = 0; i < l; ++i)ufn[i].v.clear();
		delete[]ufn;
	}
};
//并查集2
class union_find2
{
	size_t l;
	size_t*type;
	bool inUse;
	size_t findType(size_t x)
	{
		return x == type[x] ? x : findType(type[x]);
	}
public:
	union_find2()
	{
		inUse = false;
	}
	void init(size_t length)
	{
		size_t i;
		l = length;
		type = new size_t[l];
		for (i = 0; i < l; ++i)type[i] = i;
		inUse = true;
	}
	void unite(size_t x, size_t y)
	{
		type[findType(y)] = findType(x);
	}
	bool same(size_t x, size_t y)
	{
		return findType(x) == findType(y);
	}
	void del()
	{
		delete[]type;
		inUse = false;
	}
	~union_find2()
	{
		if (inUse)del();
	}
};
//并查集3
struct uf_note3
{
	unsigned type;
	list<size_t>lst;
};
class union_find3
{
	unsigned l;
	uf_note3*ufn;
public:
	void init(unsigned length)
	{
		unsigned i, j, k, t;
		l = length;
		ufn = new uf_note3[l];
		for (i = 0; i < l; ++i)ufn[i].type = i;
	}
	void unite(unsigned x, unsigned y)
	{
		if (ufn[x].type != ufn[y].type) {
			unsigned t = ufn[y].type;
			list<size_t>::iterator it, end = ufn[t].lst.end();
			ufn[ufn[x].type].lst.push_back(t);
			ufn[t].type = ufn[x].type;
			for (it = ufn[t].lst.begin(); it != end; ++it) {
				ufn[*it].type = ufn[x].type;
				//ufn[ufn[x].type].lst.push_back(*it);
			}
			list<size_t>::iterator itt = ufn[ufn[x].type].lst.end();
			--itt;
			ufn[ufn[x].type].lst.splice(itt, ufn[t].lst);
			//ufn[t].lst.clear();
		}
	}
	bool same(unsigned x, unsigned y)
	{
		return ufn[x].type == ufn[y].type;
	}
	void del()
	{
		unsigned i;
		for (i = 0; i < l; ++i)ufn[i].lst.clear();
		delete[]ufn;
	}
};
//求逆序数
template<typename tn>class inverse_number
{
	unsigned l;
	tn*a1;
	tn*a2;
	unsigned long long in_merge(unsigned left, unsigned mid, unsigned right)
	{
		unsigned i = left, j = mid + 1, k = left;
		unsigned long long y = 0;
		while (i <= mid&&j <= right) {
			if (a1[i] <= a1[j]) {
				a2[k] = a1[i];
				++k;
				++i;
			}
			else {
				a2[k] = a1[j];
				++k;
				++j;
				y += j - k;
			}
		}
		while (i <= mid) {
			a2[k] = a1[i];
			++k;
			++i;
		}
		while (j <= right) {
			a2[k] = a1[j];
			++k;
			++j;
		}
		memcpy(a1 + left, a2 + left, (right - left + 1) * sizeof(tn));
		//for (i = left; i <= right; ++i)a1[i] = a2[i];
		return y;
	}
	unsigned long long in_mergesort(unsigned left, unsigned right)
	{
		if (left < right) {
			unsigned mid = (left + right) >> 1;
			unsigned long long y = 0;
			y += in_mergesort(left, mid);
			y += in_mergesort(mid + 1, right);
			y += in_merge(left, mid, right);
			return y;
		}
		else return 0;
	}
public:
	void init(tn*x, unsigned length)
	{
		l = length;
		a1 = new tn[l];
		memcpy(a1, x, l * sizeof(tn));
		//for (unsigned i = 0; i < l; ++i)a1[i] = x[i];
		a2 = new tn[l];
	}
	unsigned long long inv_num()
	{
		return in_mergesort(0, l - 1);
	}
	void del()
	{
		delete[]a1, a2;
	}
};
//字符串匹配，输出最前的
template<typename tn>int KMP(tn*a1, unsigned l1, tn*a2, unsigned l2)
{
	int*next = new int[l2];
	int i, checknum = -1;
	for (i = 0; i < l2; ++i) {
		next[i] = checknum;
		while (checknum != -1 && a2[i] != a2[checknum])checknum = next[checknum];
		++checknum;
	}
	//for (i = 0; i < l2; ++i)cout << next[i] << ' ';
	checknum = 0;
	for (i = 0; i < l1; ++i) {
		while (checknum != -1 && a1[i] != a2[checknum])checknum = next[checknum];
		++checknum;
		if (checknum >= l2) {
			delete[]next;
			return i - l2 + 2;
		}

	}
	delete[]next;
	return -1;
}
//字符串匹配，输出匹配数（不重复）
template<typename tn>int KMP2(tn*a1, unsigned l1, tn*a2, unsigned l2)
{
	int*next = new int[l2];
	int i, checknum = -1, y = 0;
	for (i = 0; i < l2; ++i) {
		next[i] = checknum;
		while (checknum != -1 && a2[i] != a2[checknum])checknum = next[checknum];
		++checknum;
	}
	checknum = 0;
	for (i = 0; i < l1; ++i) {
		while (checknum != -1 && a1[i] != a2[checknum])checknum = next[checknum];
		++checknum;
		if (checknum >= l2) {
			checknum = 0;
			++y;
		}

	}
	delete[]next;
	return y;
}
//二分法（可能有错误）
namespace nsBisection
{
	//x1<x2
	template<typename T, typename F>T bm(F const&func, T x1, T x2, T epsilon = 1)
	{
		T a, b, c;
		a = x1;
		b = x2;
		while (abs(a - b) > epsilon) {
			c = (a + b) / 2;
			if (func(c)*func(a) < 0)b = c;
			else a = c;
		}
		return (a + b) / 2;
	}
	template<typename T, typename F>T bmint(F const&func, T x1, T x2)
	{
		T a, b, c, fa, fc;
		a = x1;
		b = x2;
		while (abs(a - b) > 1) {
			c = (a + b) / 2;
			fa = func(a);
			fc = func(c);
			if (fc == 0)return c;
			if ((fa > 0) ^ (fc > 0))b = c;
			else a = c;
		}
		return a;
	}
};
//最长回文串
size_t Manacher(const char*chrS)
{
	const char cchr1 = '$';
	const char cchr2 = '#';
	size_t len = strlen(chrS), i, t = 2, y = 0;
	size_t rightest = 0;  //已访问到的字符中在最右的字符下标
	size_t mid = 0;  //已访问到的字符中最右字符所在的回文串的对称中心下标
	size_t L = (len << 1) + 2;  //扩充后的字符串长
	char*NewS = new char[L + 1];  //扩充后的字符串，保证回文串长度为奇数
	size_t*PSR1 = new size_t[L + 1];  //以NewS[i]为对称中心的回文串半径+1
	NewS[0] = cchr1;
	NewS[1] = cchr2;
	for (i = 0; i < len; ++i) {
		NewS[t] = chrS[i];
		++t;
		NewS[t] = cchr2;
		++t;
	}
	NewS[L] = '\0';
	for (i = 1; i < L; ++i) {
		PSR1[i] = rightest > i ? min(PSR1[(mid << 1) - i], rightest - i) : 1;
		while (NewS[i + PSR1[i]] == NewS[i - PSR1[i]])++PSR1[i];
		t = PSR1[i] + i;
		if (t > rightest) {
			rightest = t;
			mid = i;
		}
		t = PSR1[i] - 1;
		if (t > y)y = t;
	}
	delete[]NewS;
	delete[]PSR1;
	return y;
}
////最长回文串2
//flag是用来填充数组的
template<typename tn>long Manacher(const tn*s, long len, tn flag)
{
	long t1;
	long i, t2 = 1, y = 0, rightest = 0, mid = 0, L = (len << 1) + 1;
	tn*NewS = new tn[L];
	long*PSR1 = new long[L];
	NewS[0] = flag;
	for (i = 0; i < len; ++i) {
		NewS[t2] = s[i];
		++t2;
		NewS[t2] = flag;
		++t2;
	}
	for (i = 0; i < L; ++i) {
		PSR1[i] = rightest > i ? min(PSR1[(mid << 1) - i], rightest - i) : 1;
		t1 = i - PSR1[i];
		t2 = i + PSR1[i];
		while (t1 >= 0 && t2 < L&&NewS[t1] == NewS[t2]) {
			++PSR1[i];
			t1 = i - PSR1[i];
			t2 = i + PSR1[i];
		}
		t1 = PSR1[i] + i;
		if (t1 > rightest) {
			rightest = t1;
			mid = i;
		}
		t1 = PSR1[i] - 1;
		if (t1 > y)y = t1;
	}
	delete[]NewS;
	delete[]PSR1;
	return y;
}
//三分法
double _3section(double(*f)(double), double a, double b, double epsilon, bool findMax = true)
{
	double x1, x2;
	while (b - a > epsilon) {
		x1 = 2 * a / 3 + b / 3;
		x2 = a / 3 + 2 * b / 3;
		if (findMax ? (f(x1) > f(x2)) : (f(x1) < f(x2)))b = x2;
		else a = x1;
	}
	return (a + b) / 2;
}
//博弈论
//f：可以取走的石子个数（可改变当前状态的方式），如unsigned f[]={1,3,5};
void getSG(unsigned*f, unsigned*sg, unsigned n)
{
	unsigned m = n + 1, i, j;
	size_t l = sizeof(bool)*m;
	bool*hash = new bool[m];
	memset(sg, 0, l);
	for (i = 1; i <= n; ++i) {
		memset(hash, 0, m);
		for (j = 1; f[j] <= i; ++j)hash[sg[i - f[j]]] = true;
		for (j = 0; j <= n; ++j)
			if (!hash[j]) {
				sg[i] = j;
				break;
			}
	}
	delete[]hash;
}
//莫队算法
struct moduiNode
{
	size_t l, r, id;
};
//从1开始，长度不计第0个，区间为闭区间
void modui(long long*arr, size_t arrLen, moduiNode*nodeArr, size_t nodeArrLen, long long maxT, function<long long(long long, size_t)>addFunc, function<long long(long long, size_t)>subtractFunc, long long*keys)
{
	size_t*pos = new size_t[arrLen + 1];
	size_t*vis = new size_t[maxT];//指示区间内不同值出现的次数
	size_t i, l = 1, r = 0;
	size_t dis = sqrt(arrLen);
	long long num = 0;
	pos[0] = 0;
	for (i = 1; i <= arrLen; ++i)pos[i] = i / dis;
	for (i = 1; i <= nodeArrLen; ++i)nodeArr[i].id = i;
	sort(nodeArr + 1, nodeArr + (nodeArrLen + 1), [=](moduiNode x, moduiNode y)->bool {
		return (pos[x.l] == pos[y.l]) ? (x.r < y.r) : (pos[x.l] < pos[y.l]);
	});
	memset(vis, 0, maxT * sizeof(size_t));
	memset(keys, 0, (nodeArrLen + 1) * sizeof(long long));
	for (i = 1; i <= nodeArrLen; ++i) {
		while (r < nodeArr[i].r) {
			++r;
			++vis[arr[r]];
			num += addFunc(arr[r], vis[arr[r]]);
		}
		while (r > nodeArr[i].r) {
			--vis[arr[r]];
			num -= subtractFunc(arr[r], vis[arr[r]]);
			--r;
		}
		while (l < nodeArr[i].l) {
			--vis[arr[l]];
			num -= subtractFunc(arr[l], vis[arr[l]]);
			++l;
		}
		while (l > nodeArr[i].l) {
			--l;
			++vis[arr[l]];
			num += addFunc(arr[l], vis[arr[l]]);
		}
		keys[nodeArr[i].id] = num;
	}
	delete[]pos;
	delete[]vis;
}
//高斯消元法
//浮点数
template<typename T>bool asZero(T x)
{
	return abs(x) < 1e-7;
}
template<typename T>class GaussianElimination2
{
public:
	struct equation
	{
		const static int N = 510;
		static int n;// NumOfUnknownNumbers
		T c[N];//[0, n-1]为未知数系数，n为等号另一边的常数项
		/*int*c;
		equation()
		{
		c = new int[n];
		cout << "new\n";
		}
		equation(const equation&x)
		{
		c = new int[n];
		cout << "new\n";
		memcpy(c, x.c, sizeof(int)*n);
		}
		equation&operator=(const equation&x)
		{
		memcpy(c, x.c, sizeof(int)*n);
		return *this;
		}
		~equation()
		{
		cout << "del\n";
		delete[]c;
		}*/
		void clear()
		{
			memset(c, 0, sizeof(T)*N);
		}
		bool valid()const
		{
			int i;
			for (i = 0; i < n; ++i)if (!asZero(c[i]))return true;
			return false;
		}
		//返回0则有未知数系数不为0，返回1则未知数系数和常数项为0，返回-1则未知数系数为0，常数项不为零（无解）
		short check()const
		{
			return valid() ? 0 : (asZero(c[n]) ? 1 : -1);
		}
		equation&operator*=(T k)
		{
			int i;
			for (i = 0; i <= n; ++i)c[i] *= k;
			return *this;
		}
		equation&operator/=(T k)
		{
			int i;
			for (i = 0; i <= n; ++i)c[i] /= k;
			return *this;
		}
		equation&operator+=(const equation&eq)
		{
			int i;
			for (i = 0; i <= n; ++i)c[i] += eq.c[i];
			return *this;
		}
		equation&operator-=(const equation&eq)
		{
			int i;
			for (i = 0; i <= n; ++i)c[i] -= eq.c[i];
			return *this;
		}
		const equation operator*(T k)const
		{
			int i;
			equation y;
			for (i = 0; i <= n; ++i)y.c[i] = c[i] * k;
			return y;
		}
	};
protected:
	vector<equation>v;//Equation Group
	int m;
public:
	GaussianElimination2()
	{
		m = 0;
	}
	void add(const equation&e)
	{
		v.push_back(e);
		++m;
	}
	//0表示有唯一解，1表示有多个解，-1表示无解
	short solve(equation&s)
	{
		int i, j, the = 0;
		T t;
		//第the条方程，第i个元
		for (i = 0; i < equation::n&&the < m; ++i) {
			//寻找the和后面第一个未知数x[i]的系数不为0的方程
			for (j = the; j < m; ++j)if (v[j].c[i]) {
				swap(v[j], v[the]);//找到则将其与第the条方程交换位置，保证方程组按顺序排列
				break;
			}
			//若有方程未知数x[i]的系数不为0，则用这条方程（第the条）消去其他方程的x[i]
			if (j < m) {
				//消去第the前的方程的元
				for (j = 0; j < the; ++j) {
					t = v[j].c[i] / v[the].c[i];
					if (!asZero(t)) {
						v[j] -= (v[the] * t);
					}
				}
				//消去第the后的方程的元
				for (j = the + 1; j < m; ++j) {
					t = v[j].c[i] / v[the].c[i];
					if (!asZero(t)) {
						v[j] -= v[the] * t;
					}
				}
				//检查消元后是否有未知数系数全为0的方程
				for (j = 0; j < m; ++j) {
					if (!v[j].valid()) {
						if (!asZero(v[j].c[equation::n]))return -1;//未知数系数全为0，常数项不为0，无解
						else {
							v.erase(v.begin() + j);//未知数系数全为0，常数项也为0，不影响结果，删去
							if (j <= the)--the;
							--m;
							--j;
						}
					}
				}
				++the;//指向下一条方程
			}
		}
		//if (m > equation::n)abort();
		if (m == equation::n) {
			for (i = 0; i < m; ++i)s.c[i] = v[i].c[equation::n] / v[i].c[i];
			return 0;
		}
		return 1;
	}
	void clear()
	{
		v.clear();
		m = 0;
	}
};
template<typename T>int GaussianElimination2<T>::equation::n = 10;
//高斯消元法（异或方程组）
class XorGaussianElimination
{
public:
	struct equation
	{
		const static int N = 50;
		static int n;// NumOfUnknownNumbers
		bool c[N];//[0, n-1]为未知数系数，n为等号另一边的常数项
		void clear()
		{
			memset(c, 0, sizeof(bool)*N);
		}
		bool valid()const
		{
			int i;
			for (i = 0; i < n; ++i)if (c[i])return true;
			return false;
		}
		//返回0则有未知数系数不为0，返回1则未知数系数和常数项为0，返回-1则未知数系数为0，常数项不为零（无解）
		short check()const
		{
			return valid() ? 0 : (c[n] ? -1 : 1);
		}
		equation&operator^=(const equation&x)
		{
			int i;
			for (i = 0; i <= n; ++i)c[i] ^= x.c[i];
			return *this;
		}
		const equation operator^(const equation&x)const
		{
			equation y;
			int i;
			for (i = 0; i <= n; ++i)y.c[i] = c[i] ^ x.c[i];
			return y;
		}
	};
protected:
	vector<equation>v;//Equation Group
	int m;
public:
	XorGaussianElimination()
	{
		m = 0;
	}
	void add(const equation&e)
	{
		v.push_back(e);
		++m;
	}
	//0表示有唯一解，1表示有多个解，-1表示无解
	short solve(equation&s)
	{
		int i, j, the = 0;
		//第the条方程，第i个元
		for (i = 0; i < equation::n&&the < m; ++i) {
			//寻找the和后面第一个未知数x[i]的系数不为0的方程
			for (j = the; j < m; ++j)if (v[j].c[i]) {
				if (j != the)swap(v[j], v[the]);//找到则将其与第the条方程交换位置，保证方程组按顺序排列
				break;
			}
			//若有方程未知数x[i]的系数不为0，则用这条方程（第the条）消去其他方程的x[i]
			if (j < m) {
				//消去第the前的方程的元
				for (j = 0; j < the; ++j) {
					if (v[j].c[i])v[j] ^= v[the];
				}
				//消去第the后的方程的元
				for (j = the + 1; j < m; ++j) {
					if (v[j].c[i])v[j] ^= v[the];
				}
				//检查消元后是否有未知数系数全为0的方程
				for (j = 0; j < m; ++j) {
					if (!v[j].valid()) {
						if (v[j].c[equation::n])return -1;//未知数系数全为0，常数项不为0，无解
						else {
							v.erase(v.begin() + j);//未知数系数全为0，常数项也为0，不影响结果，删去
							if (j <= the)--the;
							--m;
							--j;
						}
					}
				}
				++the;//指向下一条方程
			}
		}
		//if (m > equation::n)abort();
		if (m == equation::n) {
			for (i = 0; i < m; ++i)s.c[i] = v[i].c[equation::n];
			return 0;
		}
		return 1;
	}
	void clear()
	{
		v.clear();
		m = 0;
	}
};
int XorGaussianElimination::equation::n = N - 1;
//高斯消元法（异或方程组，bitset）
class BitSetXorGaussianElimination
{
public:
	const static int N = 405;
	int n;
protected:
	vector<bitset<N>>v;//Equation Group
	int m;
	bool valid(const bitset<N>&eq)
	{
		int i;
		for (i = 0; i < n; ++i)if (eq[i])return true;
		return false;
	}
public:
	BitSetXorGaussianElimination()
	{
		m = 0;
	}
	void init(int numOfUnknownNum)
	{
		n = numOfUnknownNum;
	}
	void add(const bitset<N>&e)
	{
		v.push_back(e);
		++m;
	}
	//0表示有唯一解，正整数为自由元个数，表示有多个解，-1表示无解
	int solve(bitset<N>&s)
	{
		int i, j, the = 0;
		//第the条方程，第i个元
		for (i = 0; i < n&&the < m; ++i) {
			//寻找the和后面第一个未知数x[i]的系数不为0的方程
			for (j = the; j < m; ++j)if (v[j][i]) {
				if (j != the)swap(v[j], v[the]);//找到则将其与第the条方程交换位置，保证方程组按顺序排列
				break;
			}
			//若有方程未知数x[i]的系数不为0，则用这条方程（第the条）消去其他方程的x[i]
			if (j < m) {
				//消去第the前的方程的元
				for (j = 0; j < the; ++j) {
					if (v[j][i])v[j] ^= v[the];
				}
				//消去第the后的方程的元
				for (j = the + 1; j < m; ++j) {
					if (v[j][i])v[j] ^= v[the];
				}
				//检查消元后是否有未知数系数全为0的方程
				for (j = 0; j < m; ++j) {
					if (!valid(v[j])) {
						if (v[j][n])return -1;//未知数系数全为0，常数项不为0，无解
						else {
							v.erase(v.begin() + j);//未知数系数全为0，常数项也为0，不影响结果，删去
							if (j <= the)--the;
							--m;
							--j;
						}
					}
				}
				++the;//指向下一条方程
			}
		}
		//if (m > equation::n)abort();
		if (m == n) {
			for (i = 0; i < m; ++i)s[i] = v[i][n];
			return 0;
		}
		return n - m;
	}
	void clear()
	{
		v.clear();
		m = 0;
	}
};
//线性基
class LinearBasis
{
	struct node
	{
		int num, side;
		node() {}
		node(int Num, int Side)
		{
			num = Num;
			side = Side;
		}
	};
	static const int MaxN = 500010;
	static const int bitLen = 30;
	int*a;
	int n;
	node f[MaxN][bitLen];
public:
	//从1开始
	void init(int*A, int N)
	{
		const int bitLen_1 = bitLen - 1;
		int i, j;
		node now;
		a = A;
		n = N;
		memset(f[n + 1], 0, sizeof(f[n + 1]));
		for (i = n; i > 0; --i) {
			memcpy(f[i], f[i + 1], sizeof(f[i]));
			now = node(a[i], i);
			for (j = bitLen_1; j >= 0; --j) {
				if (now.num & (1 << j)) {
					if (f[i][j].num) {
						if (now.side < f[i][j].side)swap(f[i][j], now);
						now.num ^= f[i][j].num;
					}
					else {
						swap(f[i][j], now);
						break;
					}
				}
			}
		}
	}
	//[l,r]
	bool exist(int l, int r, int k)
	{
		int i;
		for (i = bitLen - 1; i >= 0; --i) {
			if (k&(1 << i)) {
				if (f[l][i].num == 0 || f[l][i].side > r)break;
				k ^= f[l][i].num;
			}
		}
		return !k;
	}
};
//线性基（bitset）版
class LinearBasis2
{
public:
	struct node
	{
		static const int bitLen = 30;
		int side;
		bitset<bitLen>num;
		node() {}
		node(bitset<bitLen>&Num, int Side)
		{
			num = Num;
			side = Side;
		}
	};
protected:
	static const int MaxN = 500010;
	bitset<node::bitLen>*a;
	int n;
	node f[MaxN][node::bitLen];
public:
	//从1开始
	void init(bitset<node::bitLen>*A, int N)
	{
		const int bitLen_1 = node::bitLen - 1;
		int i, j;
		node now;
		a = A;
		n = N;
		memset(f[n + 1], 0, sizeof(f[n + 1]));
		for (i = n; i > 0; --i) {
			memcpy(f[i], f[i + 1], sizeof(f[i]));
			now = node(a[i], i);
			for (j = bitLen_1; j >= 0; --j) {
				if (now.num[j]) {
					if (f[i][j].num.any()) {
						if (now.side < f[i][j].side)swap(f[i][j], now);
						now.num ^= f[i][j].num;
					}
					else {
						swap(f[i][j], now);
						break;
					}
				}
			}
		}
	}
	//[l,r]
	bool exist(int l, int r, bitset<node::bitLen>k)
	{
		int i;
		for (i = node::bitLen - 1; i >= 0; --i) {
			if (k[i]) {
				if (f[l][i].num.none() || f[l][i].side > r)break;
				k ^= f[l][i].num;
			}
		}
		return k.none();
	}
};
//最短路算法(Dijkstra)
const int MY_INF = INT_MAX >> 1;
class Dijkstra
{
	static const int N = 110;
	int adjacencyMatrix[N][N];//MY_INF表示不相邻，到自身的距离为MY_INF，[1, n]
	int n;
public:
	void init(int n_)
	{
		int i, j;
		n = n_;
		for (i = 0; i < N; ++i)for (j = 0; j < N; ++j)adjacencyMatrix[i][j] = MY_INF;
	}
	void setAWay(int a, int b, int weight)
	{
		if (weight < adjacencyMatrix[a][b]) {
			adjacencyMatrix[a][b] = weight;
			adjacencyMatrix[b][a] = weight;
		}
	}
	int shortestDistence(int from, int to)
	{
		bool visited[N];
		int dis[N];
		int t, k, i, j, s;
		memset(visited, 0, N * sizeof(bool));
		for (i = 1; i <= n; ++i)dis[i] = adjacencyMatrix[from][i];
		visited[from] = true;
		dis[from] = 0;
		for (i = 1; i <= n; ++i) {
			t = MY_INF;
			for (j = 1; j <= n; ++j)if (!visited[j] && t > dis[j]) {
				k = j;
				t = dis[k];
			}
			if (t == MY_INF)break;
			visited[k] = true;
			for (j = 1; j <= n; ++j) {
				s = dis[k] + adjacencyMatrix[k][j];
				if (!visited[j] && dis[j] > s)dis[j] = s;
			}
		}
		return dis[to];
	}
};
//最小生成树算法
namespace nsKruskal {
	struct KruEdge
	{
		int a, b, weight;
		KruEdge(int A = 0, int B = 0, int Weight = 0)
		{
			a = A;
			b = B;
			weight = Weight;
		}
	};
	bool KruEdgeLessThan(const KruEdge&x, const KruEdge&y)
	{
		return x.weight < y.weight;
	}
	int Kruskal(vector<KruEdge>&edgeArr, size_t n)
	{
		sort(edgeArr.begin(), edgeArr.end(), KruEdgeLessThan);
		union_find2 uf;
		uf.init(n);
		size_t i, inSet = 1, len = edgeArr.size();
		int s = 0;
		for (i = 0; i < len&&inSet < n; ++i) {
			if (!uf.same(edgeArr[i].a, edgeArr[i].b)) {
				++inSet;
				uf.unite(edgeArr[i].a, edgeArr[i].b);
				s += edgeArr[i].weight;
			}
		}
		return s;
	}
}
//自适应辛普森积分（抛物线法）
namespace nsSimpson
{
	inline double Simpson(function<double(double)>&f, double a, double b)
	{
		return (f(a) + 4 * f((a + b) / 2) + f(b))*(b - a) / 6;
	}
	//call this
	double integral(function<double(double)>f, double a, double b, double epsilon = 1e-5)
	{
		double c = (a + b) / 2;
		double s = Simpson(f, a, b), s1 = Simpson(f, a, c), s2 = Simpson(f, c, b);
		double s_ = s1 + s2;
		double ds = s_ - s;
		if (abs(ds) <= 15 * epsilon)return s_ + ds / 15;
		else {
			double t = epsilon / 2;
			return integral(f, a, c, t) + integral(f, c, b, t);
		}
	}
}
//拓扑排序
class topologicalSort
{
	size_t n;
	vector<size_t>*from;
	size_t*out;
	size_t*y;
public:
	void init(size_t N)
	{
		n = N;
		size_t n_ = n + 1;
		size_t size = sizeof(size_t)*n_;
		from = new vector<size_t>[n_];
		out = new size_t[n_];
		memset(out, 0, size);
		y = new size_t[n_];
		memset(y, 0, size);
	}
	//从1开始，a在b前
	void addRela(size_t a, size_t b)
	{
		from[b].push_back(a);
		++out[a];
	}
	size_t*solve()
	{
		priority_queue<size_t>q;
		size_t i, tmp, len = n;
		for (i = 1; i <= n; ++i)if (out[i] == 0)q.push(i);
		while (!q.empty()) {
			tmp = q.top();
			y[len--] = tmp;
			q.pop();
			for (i = 0; i < from[tmp].size(); ++i) {
				--out[from[tmp][i]];
				if (out[from[tmp][i]] == 0)q.push(from[tmp][i]);
			}
		}
		return y;
	}
	void del()
	{
		delete[]from, out, y;
	}
};
//求欧拉路径（有向图）
class CFleury
{
	int numOfVertices, numOfEdges;
	vector<int>*adjList;
	int*in;
	int*out;
	int begin()
	{
		int i, a = 0, b = 0, c = 0, r1 = -1, r2 = -1;
		for (i = 0; i < numOfVertices; ++i) {
			if (in[i] != 0 || out[i] != 0) {
				r1 = i;
				if (in[i] != out[i]) {
					++a;
					if (out[i] == in[i] + 1) {
						++b;
						r2 = i;
					}
					else ++c;
				}
			}
		}
		return (a == 0) ? r1 : ((a == 2 && b == 1 && c == 1) ? r2 : -1);
	}
	void subsolve(stack<int>&path, int cur)
	{
		int t;
		while (!adjList[cur].empty()) {
			t = adjList[cur].back();
			adjList[cur].pop_back();
			subsolve(path, t);
		}
		path.push(cur);
	}
public:
	void init(int NumOfVertices)
	{
		numOfEdges = 0;
		numOfVertices = NumOfVertices;
		//size_t n = numOfVertices + 1;
		adjList = new vector<int>[numOfVertices];
		in = new int[numOfVertices];
		out = new int[numOfVertices];
		size_t size = numOfVertices * sizeof(int);
		memset(in, 0, size);
		memset(out, 0, size);
	}
	//start from 0
	void addAnEdge(int from, int to)
	{
		++out[from];
		++in[to];
		adjList[from].push_back(to);
		++numOfEdges;
	}
	bool solve(stack<int>&path)
	{
		int start = begin();
		if (start == -1)return false;
		else {
			subsolve(path, start);
			if (path.size() != numOfEdges + 1) {
				while (!path.empty())path.pop();
				return false;
			}
			return true;
		}
	}
	bool solve(vector<int>&path)
	{
		stack<int>stackPath;
		bool y = solve(stackPath);
		if (y)while (!stackPath.empty()) {
			path.push_back(stackPath.top());
			stackPath.pop();
		}
		return y;
	}
	void clear()
	{
		delete[]adjList, in, out;
	}
};
//强连通分量
class CTarjan
{
	unsigned time;
	size_t n, bloks;
	vector<size_t>*adjList;
	size_t*dfn;
	size_t*low;
	bool*inStack;
	stack<size_t>stck;
	vector<size_t>cnt;//此处，有多个分量时可能有问题
	void calculate(size_t u)
	{
		size_t v, len = adjList[u].size(), i;
		++time;
		low[u] = time;
		dfn[u] = time;
		inStack[u] = true;
		stck.push(u);
		for (i = 0; i < len; ++i) {
			v = adjList[u][i];
			if (dfn[v] == 0) {
				calculate(v);
				low[u] = min(low[u], low[v]);
			}
			else if (inStack[v])low[u] = min(low[u], dfn[v]);
		}
		if (dfn[u] == low[u]) {
			cnt.push_back(0);
			do {
				++cnt[bloks];
				v = stck.top();
				stck.pop();
				inStack[v] = false;
			} while (u != v);
			++bloks;
		}
	}
public:
	void init(size_t N)
	{
		time = 0;
		bloks = 0;
		n = N;
		size_t n1 = n + 1;
		adjList = new vector<size_t>[n1];
		dfn = new size_t[n1];
		low = new size_t[n1];
		inStack = new bool[n1];
		size_t size = n1 * sizeof(size_t);
		memset(dfn, 0, size);
		memset(low, 0, size);
		memset(inStack, 0, n1 * sizeof(bool));
	}
	//start from 1
	void addAnEdge(size_t from, size_t to)
	{
		adjList[from].push_back(to);
	}
	void solve()
	{
		calculate(1);
	}
	//start from 0
	size_t getNumOfVertices(size_t th)
	{
		return cnt[th];
	}
	size_t getNumOfBlocks()
	{
		return bloks;
	}
	void clear()
	{
		delete[]adjList, dfn, low, inStack;
		cnt.clear();
		while (!stck.empty())stck.pop();
	}
};

namespace nsHugeNumber
{
	//大整数加法
	inline int getNum(char chr)
	{
		return (int)(chr - '0');
	}
	inline char getChar(int n)
	{
		return (char)(n + '0');
	}
	const string add_equalLen(const string&a, const string&b)
	{
		int l = (int)a.length(), i;
		bool mark = false;
		int t;
		string y;
		for (i = l - 1; i >= 0; --i) {
			t = getNum(a[i]) + getNum(b[i]);
			if (mark)++t;
			mark = (t >= 10);
			if (mark)t -= 10;
			y = getChar(t) + y;
		}
		return mark ? '1' + y : y;
	}
	const string add(const string&a, const string&b)
	{
		size_t la = a.length(), lb = b.length();
		if (la != lb) {
			return la > lb ?
				add_equalLen(a, string().append(la - lb, '0') + b) :
				add_equalLen(string().append(lb - la, '0') + a, b);
		}
		else return add_equalLen(a, b);
	}
	//大整数乘法
	const string multiply(const string&a, int k)
	{
		int l = a.length(), i;
		div_t t;
		string y;
		t.quot = 0;
		for (i = l - 1; i >= 0; --i) {
			t = div(t.quot + k*getNum(a[i]), 10);
			y = getChar(t.rem) + y;
		}
		return t.quot > 0 ? getChar(t.quot) + y : y;
	}
	const string multiply(const string&a, const string&b)
	{
		if (a.length() < b.length())return multiply(b, a);
		else {
			int lb = b.length(), i, lb_1 = lb - 1;
			string y;
			string t;
			for (i = lb_1; i >= 0; --i) {
				t = multiply(a, getNum(b[i]));
				y = add(t.append(lb_1 - i, '0'), y);
			}
			return y;
		}
	}
}
