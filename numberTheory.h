//����
#include<cmath>
#include<ctime>
#include<cstdlib>
#include<vector>
#include<cstring>
using namespace std;
//ŷ������
//���� https://baike.baidu.com/item/%E6%AC%A7%E6%8B%89%E5%87%BD%E6%95%B0
//�����ۣ���������n��ŷ��������С��n������������n���ʵ�������Ŀ����(1)=1����
unsigned long long phi(unsigned long long n)
{
	unsigned long long y = n, i;
	for (i = 2; i*i <= n; ++i)//��2��ʼ��������֪��һ������iΪ�������������ǣ����и�С��������ì��
		if (n%i == 0) {
			//y = y*(i - 1) / i;
			//����Ϊ�Ż�ʽ������ʽ�ȼ�
			y -= y / i;
			while (n%i == 0)//Լȥ����i
				n /= i;
		}//����������ʱ����n�ĵ�һ��������Ϊ��������Ϊ���������и�С������������ֻ����ǰ��ѭ����Լȥ����������ì��
	if (n != 1)
		//y = y*(n - 1) / n;
		y -= y / n;
	return y;
}
//Ī����˹����
//���� https://baike.baidu.com/item/%E8%8E%AB%E6%AF%94%E4%B9%8C%E6%96%AF%E5%87%BD%E6%95%B0
short miu(unsigned n)
{
	if (n == 1)return 1;
	else {
		unsigned i, sqrtN = sqrt(n);
		bool minus1 = true;
		for (i = 2; i <= sqrtN; ++i) {
			if (n%i == 0) {
				n /= i;
				if (n%i == 0)
					return 0;
				minus1 = !minus1;
				sqrtN = sqrt(n);
			}
		}
		return minus1 ? -1 : 1;
	}
}

//�������ж�
namespace nsMillerRabin
{
	bool witness(unsigned long long a, unsigned long long n)
	{
		static const double ln2 = log(2.0);
		unsigned long long t, d = 1, x, n_ = n - 1;
		int i;
		for (i = (int)ceil(log(n_) / ln2) - 1; i >= 0; --i)
		{
			x = d;
			d = (d*d) % n;
			if (d == 1 && x != 1 && x != n_)return true;
			if ((n_ & (1 << i)) > 0)d = (d*a) % n;
		}
		return d != 1;
	}
	bool prime(unsigned long long n)
	{
		if (n == 2)return true;
		if (n == 1 || (!(n & 1)))return false;
		const unsigned CHECKING_TIME = 50;
		unsigned long long a, n_ = n - 2;
		srand(time(0));
		for (unsigned i = 0; i < CHECKING_TIME; ++i) {
			a = rand()*n_ / RAND_MAX + 1;
			if (witness(a, n))return false;
		}
		return true;
	}
}

//ŷ��ɸ��������ɸ����
void EulerSieve(vector<unsigned>&prime, unsigned n)
{
	unsigned l = n + 1, i, j, t;
	bool*isPrime = new bool[l];
	memset(isPrime, 1, sizeof(bool)*l);
	isPrime[0] = false;
	isPrime[1] = false;
	for (i = 2; i <= n; ++i) {
		if (isPrime[i])prime.push_back(i);
		t = i*prime.front();
		for (j = 0; j < prime.size() && t <= n; t = i*prime[++j]) {
			isPrime[t] = false;
			if (i%prime[j] == 0)break;
		}
	}
	delete[]isPrime;
}

//ȡģ������
//https://baike.baidu.com/item/%E5%BF%AB%E9%80%9F%E5%B9%82
long long quickPowMod(long long a, long long b, long long p)
{
	long long y = 1;
	a %= p;
	while (b) {
		if (b & 1) {
			y = y * a%p;
			b--;
		}
		b >>= 1;
		a = a * a%p;
	}
	return y;
}
//��Ԫ
//https://blog.csdn.net/arrowlll/article/details/52629448
//��a��bȡģ����Ԫ
namespace nsInverseElement
{
	//��չŷ������㷨
	//https://baike.baidu.com/item/%E6%89%A9%E5%B1%95%E6%AC%A7%E5%87%A0%E9%87%8C%E5%BE%B7%E7%AE%97%E6%B3%95/1053275
	//a, b����
	//https://zhuanlan.zhihu.com/p/51481046
	void exEuclideanAlg(long long a, long long b, long long&x, long long&y)
	{
		if (b == 0) {
			x = 1;
			y = 0;
		}
		else {
			exEuclideanAlg(b, a%b, y, x);
			y -= x * (a / b);
		}
	}
	long long getInverseEleExEA(long long a, long long b)
	{
		long long x, y;
		exEuclideanAlg(a, b, x, y);
		return (x + b) % b;
	}
	//����С��������Ԫ
	//bΪ����
	//https://baike.baidu.com/item/%E8%B4%B9%E9%A9%AC%E5%B0%8F%E5%AE%9A%E7%90%86
	long long getInverseEleFermat(long long a, long long b)
	{
		return quickPowMod(a, b - 2, b);
	}
#ifdef USING_FERMAT
#define GET_INVERSE_ELE getInverseEleFermat
#else
#define GET_INVERSE_ELE getInverseEleExEA
#endif
	//�������C(n, m) mod p��pΪ����
	long long C(long long n, long long m, long long p)
	{
		if (m > n - m)return C(n, n - m, p);
		else {
			long long y = 1, t = n - m + 1, i, fact_m = 1;
			for (i = 1; i <= m; ++i) {
				y = y * t%p;
				t++;
				fact_m = fact_m * i % p;
			}
			return y * GET_INVERSE_ELE(fact_m, p) % p;
		}
	}
}
//���������������ģC(n, m) mod q
namespace nsLucas {
	long long C(long long n, long long m, long long p)
	{
		if (m > n)return 0;
		long long y = 1, a, b;
		int i;
		for (i = 1; i <= m; ++i) {
			a = (n + i - m) % p;
			b = i % p;
			y = y * (a*quickPowMod(b, p - 2, p) % p) % p;
		}
		return y;
	}
	//¬��˹����
	//https://baike.baidu.com/item/lucas/4326261
	//pΪ����
	long long LucasC(long long n, long long m, long long p)
	{
		if (m == 0)return 1;
		else {
			lldiv_t div_n = div(n, p), div_m = div(m, p);
			return C(div_n.rem, div_m.rem, p)*LucasC(div_n.quot, div_m.quot, p) % p;
		}
	}
}
