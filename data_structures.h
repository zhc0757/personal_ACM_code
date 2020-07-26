//数据结构
#include<cstdlib>
#include<typeinfo>
#include<limits>
#include<cmath>
#include<algorithm>
#include<functional>
#include<iterator>

//max,min
template<typename tn>struct RMQ_1D {
public:
	/*const int MaxN = 50010;
	int dp[MaxN][20]; 
	int logN[MaxN];*/
	int MaxN;
	tn**dp;
	int*logN; // logN[i]表示 log(i)的值。
	tn(*func)(tn, tn);
	int inline log2(int n)
	{
		int logn = 0, i;
		for (i = 1; i < n; i = i << 1) {
			++logn;
		}
		return logn;
	}
	void make(int length)
	{
		int i, t;
		MaxN = length + 1;
		t = log2(MaxN);
		dp = new tn*[t];
		for (i = 0; i < t; ++i)dp[i] = new tn[MaxN];
		logN = new int[MaxN];
	}
	void del()
	{
		int i, t = log2(MaxN);
		delete[]logN;
		for (i = 0; i < t; ++i)delete[]dp[i];
		delete[]dp;
	}
	void init_RMQ(int n, tn*num,tn(*f)(tn,tn)) {
		make(n);
		func = f;
		logN[0] = -1; 
		for (int i = 1; i <= n; ++i) {
			dp[0][i] = num[i]; 
			logN[i] = ((i&(i - 1)) == 0) ? logN[i - 1] + 1 : logN[i - 1]; // 注意括号的问题。 
		} 
		for (int j = 1; j <= logN[n]; ++j)
			for (int i = 1; i + (1 << j) - 1 <= n; ++i)
				dp[j][i] = f(dp[j - 1][i], dp[j - 1][i + (1 << (j - 1))]);
	} 
	//查询f(a(x),a(x+1),a(x+2),...,a(y))的值
	tn RMQ(int x,int y) { 
		int k = logN[y - x + 1];
		return func(dp[k][x], dp[k][y - (1 << k) + 1]);
	}
	void update(int k, tn x, unsigned time = 0)
	{
		int t = 1 << time;
		dp[time][k] = x;
		if (time + 1 <= logN[MaxN - 1]) {
			if (k > t)update(k - t, func(x, dp[time][k - t]), time + 1);
			if (k + t < MaxN - (1 << time) + 1)update(k, func(x, dp[time][k + t]), time + 1);
		}
	}
};

//树状数组
template<typename tn>struct BIT {
	tn*a;
	tn*b;
	int*index;
	int len;
	inline int lowbit(int n)
	{
		return n&(-n);
	}
	void init(tn*a0, int l) {
		int i, t, p, j;
		len = l;
		a = new tn[l + 1];
		b = new tn[l + 1];
		index = new int[l + 1];
		index[0] = 0;
		for (i = 0; i < l; ++i)a[i + 1] = a0[i];
		for (i = 1; i <= l; ++i) {
			p = lowbit(i);
			index[i] = maxf(index[i - 1], p);
			b[i] = a[i];
			for (j = 1; j < p; j = j << 1) {
				b[i] += b[i - j];
			}
		}
	}
	void add(int k, tn x)
	{
		int i;
		a[k] += x;
		for (i = k; i <= len; i += lowbit(i)) {
			b[i] += x;
		}
	}
	tn query(int right)
	{
		int i;
		tn s = 0;
		for (i = right; i > 0; i -= lowbit(i))s += b[i];
		return s;
	}
	void del()
	{
		delete[]a, b, index;
	}
};

//线段树
template<typename tn>struct segtree_node {
	tn m;
	int left, right;
};
template<typename tn>class segtree {
	int l;
	segtree_node<tn>*tree;
	tn(*f)(tn x, tn y);
	tn*a;
	int f2nmin(int x)
	{
		int y;
		for (y = 1; y < x; y = y << 1);
		return y;
	}
	tn build(int root, int left, int right)
	{
		int mid;
		tree[root].left = left;
		tree[root].right = right;
		if (left == right) {
			tree[root].m = a[left - 1];
			return tree[root].m;
		}
		else {
			mid = (left + right) >> 1;
			tree[root].m = f(build(root << 1, left, mid), build((root << 1) + 1, mid + 1, right));
			return tree[root].m;
		}
	}
public:
	//arr从0开始
	void init(tn*arr, int len, tn(*func)(tn x, tn y))
	{
		l = len;
		f = func;
		a = arr;
		tree = new segtree_node<tn>[f2nmin(l) << 1];
		//tree = new node<tn>[l << 1];
		build(1, 1, l);
	}
	tn find(int left, int right, int root = 1)
	{
		if (tree[root].left > right || tree[root].right < left)return 0;
		else if (tree[root].left >= left&&tree[root].right <= right)return tree[root].m;
		else return f(find(left, right, root << 1), find(left, right, (root << 1) + 1));
	}
	tn update(int pos, tn val, int root = 1)
	{
		if (pos < tree[root].left || tree[root].right < pos)return tree[root].m;
		else if (tree[root].left == pos&&tree[root].right == pos) {
			tree[root].m = val;
			return val;
		}
		else {
			tree[root].m = f(update(pos, val, root << 1), update(pos, val, (root << 1) + 1));
			return tree[root].m;
		}
	}
	void del()
	{
		delete[]tree;
	}
};

//带懒惰标记（求和）
template<typename tn>struct segtree3_node
{
	int left, right;
	tn mark, sum;
};
template<typename tn>class segtree3
{
	segtree3_node<tn>*tree;
	tn*a;
	int f2nmin(int x)
	{
		int y;
		for (y = 1; y < x; y = y << 1);
		return y;
	}
	tn build(int left, int right, int root)
	{
		tree[root].left = left;
		tree[root].right = right;
		tree[root].mark = 0;
		if (left == right)tree[root].sum = a[left - 1];
		else {
			int mid = (left + right) >> 1, root2 = root << 1;
			tree[root].sum = build(left, mid, root2) + build(mid + 1, right, root2 + 1);
		}
		return tree[root].sum;
	}
	void belazy(int root, tn val)
	{
		if (tree[root].left == tree[root].right)tree[root].sum += val;
		else {
			tree[root].mark += val;
			tree[root].sum += (tree[root].right - tree[root].left + 1)*val;
		}
	}
public:
	//arr从0开始
	void init(tn*arr, int length)
	{
		a = arr;
		tree = new segtree3_node<tn>[f2nmin(length) << 1];
		build(1, length, 1);
	}
	tn update(int left, int right, tn val, int root = 1)
	{
		if (left <= tree[root].left && tree[root].right <= right)belazy(root, val);
		else if (left <= tree[root].right && right >= tree[root].left) {
			if (tree[root].mark) {
				int root2 = root << 1;
				belazy(root2, tree[root].mark);
				belazy(root2 + 1, tree[root].mark);
				tree[root].mark = 0;
			}
			tree[root].sum = update(left, right, val, root << 1) + update(left, right, val, (root << 1) + 1);
		}
		return tree[root].sum;
	}
	tn query(int left, int right, int root = 1)
	{
		if (left <= tree[root].left && tree[root].right <= right)return tree[root].sum;
		else if (left > tree[root].right || right < tree[root].left)return 0;
		else {
			int root2 = root << 1;
			if (tree[root].mark) {
				belazy(root2, tree[root].mark);
				belazy(root2 + 1, tree[root].mark);
				tree[root].mark = 0;
			}
			return query(left, right, root2) + query(left, right, root2 + 1);
		}
	}
	void del()
	{
		delete[]tree;
	}
};

//字典树
const unsigned short charType = 26;
const char chr0 = 'a';
class Trie
{
	unsigned n;
	Trie*next[charType];
public:
	void init()
	{
		n = 0;
		//memset(next, NULL, sizeof(next));
		for (unsigned short i = 0; i < charType; ++i)next[i] = NULL;
	}
	Trie()
	{
		init();
	}
	void add(const char*s)
	{
		++n;
		if (s[0] != '\0') {
			unsigned short chr = s[0] - chr0;
			if (next[chr] == NULL)next[chr] = new Trie();
			next[chr]->add(s + 1);
		}
	}
	unsigned query(const char*s)
	{
		if (s[0] == '\0')return n;
		else {
			unsigned short chr = s[0] - chr0;
			return next[chr] != NULL ? next[chr]->query(s + 1) : 0;
		}
	}
	void del()
	{
		unsigned short i;
		for (i = 0; i < charType; ++i)if (next[i] != NULL) {
			//delete[]next[i];
			next[i]->del();
			delete next[i];
			next[i] = NULL;
		}
	}
	~Trie()
	{
		//del();
	}
};
//求该变量类型的最大值
template<typename tn>void typeMax(tn&x)
{
	if (typeid(tn) == typeid(unsigned long long))x = ULLONG_MAX;
	else if (typeid(tn) == typeid(long long))x = LLONG_MAX;
	else if (typeid(tn) == typeid(unsigned long))x = ULONG_MAX;
	else if (typeid(tn) == typeid(long))x = LONG_MAX;
	else if (typeid(tn) == typeid(unsigned))x = UINT_MAX;
	else if (typeid(tn) == typeid(int))x = INT_MAX;
	else if (typeid(tn) == typeid(unsigned short))x = USHRT_MAX;
	else if (typeid(tn) == typeid(short))x = SHRT_MAX;
	else if (typeid(tn) == typeid(unsigned char))x = UCHAR_MAX;
	else if (typeid(tn) == typeid(char))x = CHAR_MAX;
	else x = 0;
}
template<typename tn>class SBT
{
	size_t MaxN;
	tn INF;
	size_t tot, root;
	size_t *size, **ch;
	tn *key;
	void update(size_t x)
	{
		size[x] = size[ch[x][0]] + size[ch[x][1]] + (x != 0);
	}
	void rotate(size_t &x, size_t t)
	{
		size_t y = ch[x][t];
		ch[x][t] = ch[y][1 - t];
		ch[y][1 - t] = x;
		update(x);
		update(y);
		x = y;
	}
	void maintain(size_t &x, size_t t)
	{
		if (size[ch[ch[x][t]][t]] > size[ch[x][1 - t]]) rotate(x, t);
		else if (size[ch[ch[x][t]][1 - t]] > size[ch[x][1 - t]])
		{
			rotate(ch[x][t], 1 - t); rotate(x, t);
		}
		else return;
		maintain(ch[x][0], 0);
		maintain(ch[x][1], 1);
		maintain(x, 0);
		maintain(x, 1);
	}
	void _insert(size_t &x, tn k)
	{
		if (!x)
		{
			x = tot++;
			key[x] = k;
			size[x] = 1;
			ch[x][0] = ch[x][1] = 0;
		}
		else
		{
			++size[x];
			_insert(ch[x][k >= key[x]], k);
			maintain(x, k >= key[x]);
		}
	}
	tn _erase(size_t &x, tn k)
	{
		tn temp;
		--size[x];
		if (k == key[x] || ch[x][k >= key[x]] == 0)
		{
			temp = key[x];
			if (!ch[x][0] || !ch[x][1]) // 不能互换位置
				x = ch[x][0] + ch[x][1];
			else key[x] = _erase(ch[x][0], k + 1); // 小心 k+1 越界。
		}
		else temp = _erase(ch[x][k >= key[x]], k);
		return temp;
	}
public:
	size_t length()
	{
		return MaxN;
	}
	void init(size_t len)
	{
		size_t i;
		MaxN = len;
		size = new size_t[MaxN];
		ch = new size_t*[MaxN];
		for (i = 0; i < MaxN; ++i)ch[i] = new size_t[2];
		key = new tn[MaxN];
		tot = 1;
		root = 0;
		size[0] = 0;
		ch[0][0] = ch[0][1] = 0;
		//typeMax(INF);
		std::numeric_limits<tn>nl;
		INF = nl.max();
	}
	bool find(tn k)
	{
		size_t x = root;
		while (x)
		{
			if (key[x] == k) return 1;
			x = ch[x][k >= key[x]];
		}
		return 0;
	}
	size_t rank(tn k) // 小于等于 k 的个数，改成小于会错！！！
	{
		size_t x = root, ret = 0;
		while (x)
		{
			if (k >= key[x]) ret += size[ch[x][0]] + 1;
			x = ch[x][k >= key[x]];
		}
		return ret;
	}
	tn getKth(size_t k) // 找第 K 大，如果不存在返回 0。
	{
		size_t x = root;
		while (x)
		{
			if (k <= size[ch[x][0]]) x = ch[x][0];
			else
			{
				k -= size[ch[x][0]] + 1;
				if (k) x = ch[x][1];
				else return key[x];
			}
		}
		return 0;
	}
	tn preNum(tn k) // 比 k 小的最大数，不存在返回-INF。
	{
		size_t x = root;
		tn ret = -INF;
		while (x)
		{
			if (key[x] < k) ret = /*max(ret, key[x])*/ret > key[x] ? ret : key[x];
			x = ch[x][key[x]<k];
		}
		return ret;
	}
	tn nextNum(tn k) // 比 k 大的最小数，不存在返回 INF。
	{
		size_t x = root;
		tn ret = INF;
		while (x)
		{
			if (key[x] > k) ret = /*min(ret, key[x])*/ret < key[x] ? ret : key[x];
			x = ch[x][key[x] <= k];
		}
		return ret;
	}
	void insert(tn k)
	{
		_insert(root, k);
	}
	tn erase(tn k)
	{
		if (root) return _erase(root, k);
		return 0;
	}
	void del()
	{
		size_t i;
		for (i = 0; i < MaxN; ++i)delete[]ch[i];
		delete[]size, ch, key;
	}
};
//分块
template<typename T>class CBlock
{
	T*a;
	T**blockArr;
	size_t blockLen, l, NumOfBlocks, lastBlockLen;
	function<bool(T, T)>cmp;
public:
	void init(T*arr, size_t len, function<bool(T, T)>Cmp)
	{
		size_t i, j, k;
		cmp = Cmp;
		l = len;
		blockLen = sqrt(l);
		lastBlockLen = len%blockLen;
		a = arr;
		NumOfBlocks = len / blockLen + (lastBlockLen ? 1 : 0);
		blockArr = new T*[NumOfBlocks];
		j = 0;
		k = 0;
		blockArr[0] = new T[blockLen];
		for (i = 0; i < l; ++i) {
			blockArr[j][k] = a[i];
			++k;
			if (k >= blockLen) {
				k = 0;
				++j;
				blockArr[j] = new T[blockLen];
			}
		}
		size_t t = NumOfBlocks - 1;
		for (i = 0; i < t; ++i)sort(blockArr[i], blockArr[i] + blockLen, cmp);
		if (!lastBlockLen)lastBlockLen = blockLen;
		sort(blockArr[t], blockArr[t] + lastBlockLen, cmp);
	}
	//[left, right]
	size_t query(size_t left, size_t right, T v)
	{
		size_t leftBlock = left / blockLen, rightBlock = right / blockLen, ret = 0, i, t;
		if (leftBlock == rightBlock) {
			for (i = left; i <= right; ++i)if (cmp(a[i], v))++ret;
		}
		else {
			t = (leftBlock + 1)*blockLen;
			for (i = left; i < t; ++i)if (cmp(a[i], v))
				++ret;
			for (i = rightBlock*blockLen; i <= right; ++i)
				if (cmp(a[i], v))
					++ret;
			for (i = leftBlock + 1; i < rightBlock; ++i)
				ret += distance(blockArr[i], lower_bound(blockArr[i], blockArr[i] + blockLen, v, cmp));
		}
		return ret;
	}
	void change(size_t p, T x)
	{
		if (a[p] != x) {
			T old = a[p];
			size_t pos = 0, t;
			T*b = blockArr[p / blockLen];
			a[p] = x;
			while (b[pos] < old)++pos;
			b[pos] = x;
			if (x > old) {
				t = blockLen - 1;
				while (pos<t && b[pos]>b[pos + 1]) {
					swap(b[pos + 1], b[pos]);
					pos++;
				}
			}
			else {
				while (pos > 0 && b[pos] < b[pos - 1]) {
					swap(b[pos - 1], b[pos]);
					pos--;
				}
			}
		}
	}
	void del()
	{
		size_t i;
		for (i = 0; i < NumOfBlocks; ++i)delete[]blockArr[i];
		delete[]blockArr;
	}
};

//树链剖分，和&最大值
template<typename T>class CTreeLinkDivision
{
	struct SAdj
	{
		size_t next, go;
		SAdj()
		{
			next = 0;
			go = 0;
		}
	};
	struct arr
	{
		size_t l, r;
		//T y;
		T sum, Max;
		arr()
		{
			l = 0;
			r = 0;
			sum = 0;
			Max = 0;
		}
	};
	size_t n, cnt, *end, *deep, *f, *num, *son, *top, *tree, tot, *pre;
	SAdj*adj;
	T*w;
	arr*ar;
	//function<T(T, T)>func;
	void singlyAdd(size_t a, size_t b)
	{
		adj[++cnt].go = b;
		adj[cnt].next = end[a];
		end[a] = cnt;
	}
	void dfs1(size_t k, size_t fa, size_t d)
	{
		size_t i, go;
		deep[k] = d;
		f[k] = fa;
		num[k] = 1;
		for (i = end[k]; i; i = adj[i].next) {
			go = adj[i].go;
			if (go != fa) {
				dfs1(go, k, d + 1);
				num[k] += num[go];
				if (!son[k] || num[go] > num[son[k]])son[k] = go;
			}
		}
	}
	void dfs2(size_t k, size_t Number)
	{
		top[k] = Number;
		tree[k] = ++tot;
		pre[tree[k]] = k;
		if (son[k]) {
			dfs2(son[k], Number);
			size_t i, go;
			for (i = end[k]; i; i = adj[i].next) {
				go = adj[i].go;
				if (go != son[k] && go != f[k])dfs2(go, go);
			}
		}
	}
	void segtreeBuild(size_t k, size_t l, size_t r)
	{
		ar[k].l = l;
		ar[k].r = r;
		if (l == r) {
			ar[k].sum = w[pre[l]];
			ar[k].Max = ar[k].sum;
		}
		else {
			size_t mid = (l + r) >> 1;
			segtreeBuild(k << 1, l, mid);
			segtreeBuild((k << 1) + 1, mid + 1, r);
			//a[k].y = func(a[k << 1].y, a[(k << 1) + 1].y);
			ar[k].sum = ar[k << 1].sum + ar[(k << 1) + 1].sum;
			ar[k].Max = max(ar[k << 1].Max, ar[(k << 1) + 1].Max);
		}
	}
	void segtreeUpdate(size_t k, size_t x, size_t delta)
	{
		if (ar[k].l == ar[k].r) {
			ar[k].sum += delta/**(ar[k].r - ar[k].l + 1)*/;
			ar[k].Max += delta;
		}
		else {
			size_t mid = (ar[k].l + ar[k].r) >> 1;
			if (x <= mid)segtreeUpdate(k << 1, x, delta);
			else segtreeUpdate((k << 1) + 1, x, delta);
			ar[k].sum = ar[k << 1].sum + ar[(k << 1) + 1].sum;
			ar[k].Max = max(ar[k << 1].Max, ar[(k << 1) + 1].Max);
		}
	}
	T ask_sum(size_t k, size_t x, size_t y)
	{
		if (ar[k].l >= x&&ar[k].r <= y)return ar[k].sum;
		size_t mid = (ar[k].l + ar[k].r) >> 1;
		T o = 0;
		if (x <= mid) o += ask_sum(k << 1, x, y);
		if (y > mid) o += ask_sum((k << 1) + 1, x, y);
		ar[k].sum = ar[k << 1].sum + ar[(k << 1) + 1].sum;
		ar[k].Max = max(ar[k << 1].Max, ar[(k << 1) + 1].Max);
		return o;
	}
	T ask_max(size_t k, size_t x, size_t y)
	{
		if (ar[k].l >= x&&ar[k].r <= y) return ar[k].Max;
		size_t mid = (ar[k].l + ar[k].r) >> 1;
		T o = INT_MIN;
		if (x <= mid) o = ask_max(k << 1, x, y);
		if (y > mid) o = max(o, ask_max((k << 1) + 1, x, y));
		ar[k].sum = ar[k << 1].sum + ar[(k << 1) + 1].sum;
		ar[k].Max = max(ar[k << 1].Max, ar[(k << 1) + 1].Max);
		return o;
	}
public:
	//start from 1
	void init(size_t N/*, const function<T(T, T)>&Func*/)
	{
		size_t n_ = N + 1;
		n = N;
		//func = Func;
		adj = new SAdj[n_ << 1];
		cnt = 0;
		end = new size_t[n_];
		size_t size = sizeof(size_t)*n_;
		memset(end, 0, size);
		w = new T[n_];
		memset(w, 0, sizeof(T)*n_);
		deep = new size_t[n_];
		memset(deep, 0, size);
		f = new size_t[n_];
		memset(f, 0, size);
		num = new size_t[n_];
		memset(num, 0, size);
		son = new size_t[n_];
		memset(son, 0, size);
		top = new size_t[n_];
		memset(top, 0, size);
		tree = new size_t[n_];
		memset(tree, 0, size);
		tot = 0;
		pre = new size_t[n_];
		memset(pre, 0, size);
		ar = new arr[n_ << 2];
	}
	void add(size_t a, size_t b)
	{
		singlyAdd(a, b);
		singlyAdd(b, a);
	}
	void setWeight(size_t th, T weight)
	{
		w[th] = weight;
	}
	void build()
	{
		dfs1(1, 0, 1);
		dfs2(1, 1);
		segtreeBuild(1, 1, n);
	}
	void update(size_t th, T weight)
	{
		segtreeUpdate(1, tree[th], weight - w[th]);
		w[th] = weight;
	}
	/*T query(size_t u, size_t v)
	{

	}*/
	T qmax(size_t x, size_t y)
	{
		size_t f1 = top[x], f2 = top[y], t;
		T ans = INT_MIN;
		while (f1 != f2)
		{
			if (deep[f1] < deep[f2]) {
				t = f1;
				f1 = f2;
				f2 = t;
				t = x;
				x = y;
				y = t;
			}
			ans = max(ans, ask_max(1, tree[f1], tree[x]));
			x = f[f1];
			f1 = top[x];
		}
		ans = max(ans, (deep[x] > deep[y]) ? ask_max(1, tree[y], tree[x]) : ask_max(1, tree[x], tree[y]));
		return ans;
	}
	T qsum(size_t x, size_t y)
	{
		size_t f1 = top[x], f2 = top[y], t;
		T ans = 0;
		while (f1 != f2)
		{
			if (deep[f1] < deep[f2]) {
				t = f1;
				f1 = f2;
				f2 = t;
				t = x;
				x = y;
				y = t;
			}
			ans += ask_sum(1, tree[f1], tree[x]);
			x = f[f1];
			f1 = top[x];
		}
		ans += (deep[x] > deep[y]) ? ask_sum(1, tree[y], tree[x]) : ask_sum(1, tree[x], tree[y]);
		return ans;
	}
	void del()
	{
		delete[]adj, end, w, deep, f, num, son, top, tree, pre, ar;
	}
};

//扫描线（矩形并周长）
class CScanningLineH2
{
	struct po
	{
		int x, ya, yb, type;
	};
	static const int N = 50010, N4 = N * 4;
	int nn, tot, n;
	po a[N4];
	int Y[N4];

	//线段树
	struct node
	{
		int d, len, s, l, r, L, R;
	};
	node t[N4 * 4];
	void build(int bh, int l, int r)
	{
		t[bh].d = 0;
		t[bh].len = 0;
		t[bh].s = 0;
		t[bh].l = l;
		t[bh].r = r;
		t[bh].L = 0;
		t[bh].R = 0;

		if (l != r) {
			int mid = (l + r) >> 1;
			build(bh << 1, l, mid);
			build(bh << 1 | 1, mid + 1, r);
		}
	}
	void update(int bh)
	{
		int lc = bh << 1;
		int rc = bh << 1 | 1;
		if (t[bh].s > 0) {
			t[bh].len = Y[t[bh].r + 1] - Y[t[bh].l];
			t[bh].L = 1;
			t[bh].R = 1;
			t[bh].d = 1;
		}
		else if (t[bh].s <= 0) {
			if (t[bh].l == t[bh].r) {
				t[bh].d = 0;
				t[bh].len = 0;
				t[bh].L = 0;
				t[bh].R = 0;
			}
			else {
				t[bh].len = t[lc].len + t[rc].len;
				t[bh].d = t[lc].d + t[rc].d - t[lc].R*t[rc].L;
				t[bh].L = t[lc].L;
				t[bh].R = t[rc].R;
			}
		}
	}
	void add(int bh, int l, int r, int z)
	{
		if (t[bh].l >= l&&t[bh].r <= r) {
			t[bh].s += z;
			update(bh);
		}
		else {
			int mid = (t[bh].l + t[bh].r) >> 1;
			if (l <= mid) add(bh << 1, l, r, z);
			if (r > mid) add(bh << 1 | 1, l, r, z);
			update(bh);
		}
	}
public:
	void init(int n_)
	{
		n = n_;
		nn = 0;
		tot = 0;
	}
	void set(int xa, int ya, int xb, int yb)
	{
		tot++;
		Y[++nn] = ya;
		a[tot].x = xa;
		a[tot].ya = ya;
		a[tot].yb = yb;
		a[tot].type = 1;

		tot++;
		Y[++nn] = yb;
		a[tot].x = xb;
		a[tot].ya = ya;
		a[tot].yb = yb;
		a[tot].type = -1;
	}
	long long c()
	{
		sort(Y + 1, Y + nn + 1);
		nn = distance(Y + 1, unique(Y + 1, Y + 1 + nn));
		sort(a + 1, a + 1 + tot, [](const po &A, const po &B) {
			return A.x < B.x || (A.x == B.x&&B.type < A.type);
		});

		build(1, 1, nn);

		int x, y;
		int ans = 0, now_x = 0, now_blo = 0;
		for (int i = 1; i <= tot; i++) {
			x = distance(Y, lower_bound(Y + 1, Y + 1 + nn, a[i].ya));
			y = distance(Y, lower_bound(Y + 1, Y + 1 + nn, a[i].yb));
			add(1, x, y - 1, a[i].type);

			if (i > 1) ans += now_blo * 2 * (a[i].x - a[i - 1].x);
			ans += abs(t[1].len - now_x);
			now_x = t[1].len;
			now_blo = t[1].d;
		}
		return ans;
	}
};

//可持久化Trie树
template<typename T>class CPersistableTrie
{
	struct node
	{
		size_t left, right;
		node(size_t Left = 0, size_t Right = 0)
		{
			left = Left;
			right = Right;
		}
	};
	const int L = sizeof(T) * 8 - 1;
	size_t n, idx, m;
	size_t*root;
	node*son;
	size_t*sze;
public:
	void init(size_t n_)
	{
		n = n_;
		idx = 1;
		root = new size_t[n + 1];
		root[0] = 1;
		m = 60 * n;
		son = new node[m];
		sze = new size_t[m];
		memset(sze, 0, m * sizeof(size_t));
	}
	//start from 0
	void insert(size_t th, T x)
	{
		size_t now = ++idx, old = root[th];
		root[th + 1] = now;
		for (int i = L; i >= 0; --i) {
			sze[now] = sze[old] + 1;
			if ((x >> i) & 1) {
				son[now].left = son[old].left;
				son[now].right = ++idx;
				now = son[now].right;
				old = son[old].right;
			}
			else {
				son[now].right = son[old].right;
				son[now].left = ++idx;
				now = son[now].left;
				old = son[old].left;
			}
		}
		sze[now] = sze[old] + 1;
	}
	//跟x从高位到低位依次相符最多，即x同或a[i], l<=i<r最大
	T query(size_t l, size_t r, T x)
	{
		size_t old = root[l], now = root[r];
		T ans = 0;
		for (int i = L; i >= 0; --i) {
			if ((x >> i) & 1) {
				if (sze[son[now].right] != sze[son[old].right]) {
					now = son[now].right;
					old = son[old].right;
					ans = (ans << 1) + 1;
				}
				else {
					now = son[now].left;
					old = son[old].left;
					ans <<= 1;
				}
			}
			else {
				if (sze[son[now].left] != sze[son[old].left]) {
					now = son[now].left;
					old = son[old].left;
					ans = (ans << 1) + 1;
				}
				else {
					now = son[now].right;
					old = son[old].right;
					ans <<= 1;
				}
			}
		}
		return ans;
	}
	void del()
	{
		delete[]root, son, sze;
	}
};
