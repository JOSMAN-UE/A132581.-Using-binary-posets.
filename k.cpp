


/***************************
****************************
***************************/

/********************************************
OEIS A132582. 
(C) J.M. Montolio Aranda. 2020. Spain-UE.
The Function E(n). 
Counting antichains on binary poset.
OEIS A000372. D(n).
OEIS A132581 F(n). OEIS A132582 E(n). 
See docs for more.
Related to author works.
********************************************/

#pragma pack(16)

#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <ctype.h>
#include <cstdint>
#include <cinttypes>
#include <cstdbool>
#include <cstring>
#include <ctime>
#include <tuple>
#include <array>
#include <stack>
#include <map>
#include <bitset>
#include <cassert>
//#include <threads>
//#include <mutex>
#include <algorithm>
#include <numeric>
//#include <boost/multprecision/cpp_int.hpp>
// uint256_t
#include <conio.h>

using namespace std;
#define ATALI __attribute__((aligned))
#define ATHOT __attribute__((hot))

// is_eq is_lt etc
#define EQ ==
#define NE !=
#define LT <
#define GT >
#define LE <=
#define GE >=
// logical
#define AND &&
#define OR ||
#define NOT !
#define bor |
// binary
#define band &
#define bans &=
#define bxor ^
#define bnot ~

// *************************************
//            128 universe.
#define XWIDE 128

using LNUM=unsigned __int128;
using BNUM=uint_fast64_t;
using UINT=unsigned int;
//using XLRTYPE=tuple<LNUM,LNUM,LNUM>;
using XYMAPTYPE=map<LNUM,BNUM>;
using BS128=bitset<128>;

static array<LNUM,256> MASKPOW2 ATALI;
static array<LNUM,256> MASKNOT2 ATALI;
static array<LNUM,256> HMASK    ATALI; 
static deque<LNUM> REJE         ATALI;
//static deque<LNUM> YTOS;
//static deque<XLRTYPE> TRES;
static XYMAPTYPE XYMAP ATALI;
static XYMAPTYPE UUMAP ATALI;
static XYMAPTYPE::iterator xytor;
/***********************************/

void newl(void)
{
cout<<endl;
}

void binLNUM(LNUM p)
{
LNUM n;char ch;
int len=18;

n=p;
for(int b=len;b>=0;b--)
  {
  ch='0';if((n band MASKPOW2[b]) NE 0)ch='1';
  printf("%c",ch);if(b%4 EQ 0)printf(" ");
  }
newl();
}

void texLNUM(LNUM p)
{
const int BX=20,cc=10;
char buf[BX],CHZ='0';
int k,m;LNUM n;

n=p;
for(int i=0;i LT BX;i++)buf[i]=CHZ;
k=BX-1;buf[k]=0;
while(n NE 0)
  {
  m=n%cc;buf[--k]=CHZ+m;
  n=(n-m);n=n/cc;
  }
printf("%s \n",buf);
}

static bool Bnomap ATALI;
static bool Bmap;

BNUM Ynomap(LNUM lwtf) ATHOT;
BNUM Ynomap(LNUM lwtf)
{
LNUM wtf;
BS128 bs;
int qbs;
 
Bnomap=false;wtf=lwtf;
bs=wtf;qbs=bs.count();
if(qbs EQ 0){Bnomap=true;return 1;}
else if(qbs EQ 1){Bnomap=true;return 2;}
return 0;
}

BNUM Ymap(LNUM wtf) ATHOT;
BNUM Ymap(LNUM wtf)
{
BNUM y;

Bmap=false;
xytor=UUMAP.find(wtf);
if(xytor NE UUMAP.end())
  {
  Bmap=true;y=xytor->second;return y;
  }
xytor=XYMAP.find(wtf);
if(xytor NE XYMAP.end())
  {
  Bmap=true;y=xytor->second;
  UUMAP[wtf]=y;return y;
  }
return 0;
}

void insert(LNUM x,BNUM y) ATHOT;
void insert(LNUM x,BNUM y)
{
//Ymap(x);if(NOT Bmap)
XYMAP[x]=y;
}

int highbit(LNUM p) ATHOT;
int highbit(LNUM p)
{
LNUM x;int hbit;

x=p;hbit=-1;
for(short int bit=0;bit LT XWIDE;bit++)
  {
  const LNUM tmp=(x band MASKPOW2[bit]);
  if(tmp NE 0)hbit=bit; 
  }
return hbit;
}

LNUM xleft(LNUM p,int bit) ATHOT;
LNUM xleft(LNUM p,int bit)
{
LNUM lef=p;
// Clear hibit.
lef bans MASKNOT2[bit];
return lef;
}

LNUM xright(LNUM p,int bit) ATHOT;
LNUM xright(LNUM p,int bit)
{
LNUM rig=p;
// Clear cone dleq(*,bit); see docs.
rig bans HMASK[bit];
return rig;
}

BNUM EFUNC(LNUM xlar0) ATHOT;
BNUM EFUNC(LNUM xlar0)
{
BNUM FSUM ATALI (0);
int higbit;
LNUM xrig0;BNUM y0;

y0=Ynomap(xlar0);if(Bnomap)return y0;
REJE.clear();higbit=highbit(xlar0);
xrig0=xright(xlar0,higbit);
REJE.push_back(xrig0);
while(NOT REJE.empty())
  {
  LNUM xlar;BNUM ylar;

  xlar=REJE.front();REJE.pop_front();
  ylar=Ynomap(xlar);
  if(Bnomap)FSUM+=ylar;
  else
    {
    LNUM xlef,xrig ATALI;
    BNUM ylef,yrig;
    int higbit;bool blef,brig;

    higbit=highbit(xlar);
    xlef=xleft(xlar,higbit);
    xrig=xright(xlar,higbit);
    ylef=Ymap(xlef);blef=Bmap;
    yrig=Ymap(xrig);brig=Bmap;

    if(blef AND brig)
      {
      ylar=ylef+yrig;insert(xlar,ylar);FSUM+=ylar;
      }

    if(false AND blef AND (NOT brig))
      {
      FSUM+=ylef;
      //insert(xlef,ylef);
      }
    else REJE.push_back(xlef);

    if(false AND (NOT blef) AND brig)
      {
      FSUM+=yrig;
      //insert(xrig,yrig);
      }
    else REJE.push_back(xrig);
    }
  }
insert(xlar0,FSUM);
return FSUM;
}

// COLD CODE ***********************
void initHMASK(void)
{
LNUM maskones(0);

for(int b=0;b LT XWIDE;b++)
  {maskones<<=1;maskones++;
  }
for(int h=0;h LT XWIDE;h++)
  {
  LNUM imask=maskones;
  for(int bit=0;bit LE h;bit++) 
    { // dleq(bit,h).
    const int tmp=(bit band h);
    if(tmp EQ bit)imask=imask band MASKNOT2[bit];
    HMASK[h]=imask;
    }
  }
}

void initTAB(void)
{
LNUM VPOW(1);
for(int n=0;n LT XWIDE;n++)
  {
  LNUM VNOT=(bnot VPOW);
  MASKPOW2[n]=VPOW;MASKNOT2[n]=VNOT;
  VPOW<<=1;
  }
}

void initXYMAP(void)
{
XYMAP.clear();
// Some sizes |S|. 
XYMAP[0]=1; // 0>1. terminal set. 
// 2e always 2.
for(int n=0;n LT XWIDE;n++)XYMAP[MASKPOW2[n]]=2;
// Some A132581.
XYMAP[1]=2;XYMAP[3]=3;XYMAP[7]=5;
XYMAP[15]= 6;XYMAP[31]=11;XYMAP[63]=14;
XYMAP[127]=19;XYMAP[255]=20;
// pending 2 bit (x,y)
}

void state(void)
{
cout<<"XYMAP= "<<XYMAP.size()<<endl;
cout<<"UUMAP= "<<UUMAP.size()<<endl;
}

LNUM binposet(int p)
{
LNUM t=MASKPOW2[p]-1;
return t;
}

void mytime(void)
{
time_t t,*a=&t;
time(a);printf("TIME %s \n",ctime(a));
}

void ALGPOSET(void)
{
BNUM SUM,RESU ATALI;
LNUM code;

SUM=0;
for(int n=1;n LE 127;n++) 
  {
  mytime();
  code=binposet(n);
  RESU=EFUNC(code);SUM+=RESU;

  cout<<"E(n)"<<n<<" = "<<RESU<<endl;
  cout<<"F(n)"<<n<<" = "<<SUM<<endl;
  state();
  cout<<endl;
  }
mytime();
}


int main(void)
{
cout<<"OEIS A132582. The function E(n)."<<endl; 
cout<<"(C) JM M Aranda Spain-UE 2020."<<endl;
cout<<"    Related to author works."<<endl;
cout<<endl;

initTAB();
initHMASK();
initXYMAP();
ALGPOSET();

return EXIT_SUCCESS;
}

/***********************
** END SOURCE CODE
***********************/
