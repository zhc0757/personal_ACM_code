#include<cmath>
#include<complex>
#include<cstring>
#include<algorithm>
using namespace std;
class CFastFourierTransform_bigNumber
{
	const double PI = acos(-1.0);
	size_t len;
	complex<double>*za, *zb;
	int*xy;
	inline short charToShort(char chr)
	{
		return chr - '0';
	}
	inline char intToChar(int n)
	{
		return n + '0';
	}
	//以下为快速傅里叶变换部分
	//  进行FFT和IFFT前的反转变换
	//  位置i和（i二进制反转后的位置）互换
	//  len必须去2的幂
	void change(complex<double>*x, size_t Len)
	{
		size_t i, t2 = Len >> 1, k, t = Len - 1, j = t2;
		for (i = 1; i < t; ++i) {
			if (i < j)swap(x[i], x[j]);
			//交换护卫小标反转的元素，i < j保证交换一次
			//i做正常的+1，j左反转类型的+1，始终保持i和j是反转的
			k = t2;
			while (j >= k) {
				j -= k;
				k >>= 1;
			}
			if (j < k)j += k;
		}
	}
	//  FFT
	//  len必须为2 ^ k形式
	//  isOn == true时是DFT，isOn == false时是IDFT
	void fft(complex<double>*x, size_t Len, bool isOn)
	{
		const double K = -2 * PI;
		short on = isOn ? 1 : -1;
		size_t h, i, j, k, tmp2, tmp3, tmp4;
		complex<double>wn, w, u, t;
		double sita;
		change(x, Len);
		for (h = 2; h <= Len; h <<= 1) {
			sita = (K*on) / h;
			wn = complex<double>(cos(sita), sin(sita));
			tmp2 = h >> 1;
			for (j = 0; j < Len; j += h) {
				w = complex<double>(1);
				tmp3 = j + tmp2;
				for (k = j; k < tmp3; ++k) {
					u = x[k];
					tmp4 = k + tmp2;
					t = w*x[tmp4];
					x[k] = u + t;
					x[tmp4] = u - t;
					w *= wn;
				}
			}
		}
		if (!isOn)for (i = 0; i < Len; ++i)x[i] = complex<double>(x[i].real() / Len, x[i].imag());
	}

	//  求卷积
	//  用于大数乘法
	void conv(complex<double>*a, complex<double>*b, int*ans, size_t Len)
	{
		size_t i;
		fft(a, Len, true);
		fft(b, Len, true);
		for (i = 0; i < Len; ++i)a[i] *= b[i];
		fft(a, Len, false);
		//精度复原
		for (i = 0; i < Len; ++i)ans[i] = (int)(a[i].real() + 0.5);
	}
	//  进制恢复
	//  用于大数乘法
	void turn(int*ans, size_t Len, int unit)
	{
		size_t i;
		for (i = 0; i < Len; ++i) {
			ans[i + 1] += ans[i] / unit;
			ans[i] %= unit;
		}
	}
public:
	void init(char*x, char*y)
	{
		size_t lenX = strlen(x), lenY = strlen(y);
		len = 1;
		size_t maxLen = max(lenX, lenY), i;
		while (len < maxLen)len <<= 1;
		len <<= 1;
		za = new complex<double>[len];
		zb = new complex<double>[len];
		for (i = 0; i < lenX; ++i)za[i] = complex<double>(charToShort(x[lenX - i - 1]));
		//for (; i < len; ++i)za[i] = complex<double>(0);
		for (i = 0; i < lenY; ++i)zb[i] = complex<double>(charToShort(y[lenY - i - 1]));
		//for (; i < len; ++i)zb[i] = complex<double>(0);
		xy = new int[len];
		//memset(xy, 0, sizeof(int)*len);
	}
	void solve()
	{
		conv(za, zb, xy, len);
		turn(xy, len, 10);
		while (xy[len - 1] == 0)--len;
	}
	size_t getAnsLen()
	{
		return len;
	}
	void getAns(char*ans)
	{
		if (len) {
			size_t i, l = len - 1;
			for (i = l; i > 0; --i)ans[l - i] = intToChar(xy[i]);
			ans[l] = intToChar(xy[0]);
			ans[len] = '\0';
		}
	}
	void clear()
	{
		delete[]za, zb, xy;
	}
};
