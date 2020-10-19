


/********************************************
OEIS A132582. 
(C) J.M. Montolio Aranda. 2020. Spain-UE.
The Function E(n). Using binary posets.
Related to author works.
********************************************/

#pragma pack(8)

#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <ctype.h>
#include <cstdint>
#include <cinttypes>
#include <cstdbool>
#include <cstring>
#include <ctime>
#include <array>
#include <map>
#include <stack>
#include <bitset>

using namespace std;


#define ATALI __attribute__((aligned))
#define EQ ==
#define NE !=
#define LT <
#define GT >
#define LE <=
#define GE >=

#define AND &&
#define OR ||
#define NOT !
#define bor |
#define band &
#define bans &=
#define bxor ^
#define bnot ~

// *************************************

typedef bitset<256> BS256;
using BS256=bitset<256>;

#define XWIDE 128
using LNUM=unsigned __int128;

using BNUM=uint_fast64_t;
using XYMAPTYPE=map<LNUM,BNUM>;
static XYMAPTYPE XYMAP;
static XYMAPTYPE::iterator xytor;
static stack<LNUM> Q;
static array<LNUM,256> MASKPOW2;
static array<LNUM,256> MASKNOT2;

/***********************************/
static bool FINDED;
BNUM finder(LNUM lwtf)
{
LNUM wtf;
 
FINDED=false;wtf=lwtf;
if(wtf LE 2)
  {
  if(wtf EQ 0){FINDED=true;return 1;}
  if(wtf EQ 1){FINDED=true;return 2;}
  if(wtf EQ 2){FINDED=true;return 2;}
  }
xytor=XYMAP.find(wtf);
if(xytor NE XYMAP.end())
  {FINDED=true;return xytor->second;
  }
return 0;
}
void insert(LNUM x,BNUM y)
{
finder(x);
if(!FINDED)XYMAP[x]=y;
}

int popcount(LNUM x)
{
bitset<256> bs;
int v;

bs=x;v=bs.count();
return v;
}

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

LNUM xleft(LNUM p,int higbit)
{
LNUM lef=p;
// Clear hibit.
if(higbit LT 0)return lef;
lef bans MASKNOT2[higbit];
return lef;
}

LNUM xright(LNUM p,int higbit)
{
LNUM rig=p;
const LNUM MN0(MASKNOT2[0]);
const LNUM MNH(MASKNOT2[higbit]);

if(higbit LT 0)return rig;
// Clear the bits. See docs.
rig bans MN0;rig bans MNH;
for(int bit=1;bit LT higbit;bit++) 
  {
  const int tmp=(bit band higbit);
  if(tmp EQ bit)rig bans MASKNOT2[bit];
  }
return rig;
}

BNUM FUNC(LNUM xlar0)
{
LNUM xrig0;
BNUM FX0(0);
int higbit;

higbit=highbit(xlar0);
//xlef0=xleft(xlar0,higbit);
xrig0=xright(xlar0,higbit);

Q.push(xrig0);
while(NOT Q.empty())
  {
  BNUM ylar,yl,yr;LNUM xlar,xlef,xrig;

  xlar=Q.top();Q.pop();ylar=finder(xlar);
  if(FINDED)FX0+=ylar;
  else
    {
    bool bl,br,blar;

    higbit=highbit(xlar);xlef=xleft(xlar,higbit);
    xrig=xright(xlar,higbit);
    blar=bl=br=false;yl=finder(xlef);bl=FINDED;
    if(bl)
      {yr=finder(xrig);br=FINDED;
      if(br)
        {blar=true;ylar=yl+yr;
        }
      }
    if(blar)
      {insert(xlar,ylar);FX0+=ylar;
      }
      else
      {
      if(bl)FX0+=yl; else Q.push(xlef);
      if(br)FX0+=yr; else Q.push(xrig);
      }
    }
  }
insert(xlar0,FX0);
return FX0; 
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
XYMAP.clear(); // 0>1. See docs.
XYMAP[0]=1; // 2e always 2.
for(int n=0;n LT XWIDE;n++)XYMAP[MASKPOW2[n]]=2;
// Some A132581 values.
XYMAP[ 3]= 3;  //2
XYMAP[ 7]= 5;  //3
XYMAP[15]= 6;  //4
XYMAP[31]=11;  //5
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

void ALGBINPOSET(void)
{
BNUM SUM(0);

initXYMAP();
for(int n=0;n LT XWIDE;n++) 
  {
  BNUM RESU;LNUM code;

  mytime();
  code=binposet(n);
  RESU=FUNC(code);SUM+=RESU;
  cout<<"RESULT(n)"<<n<<" = "<<RESU<<endl;
  cout<<"SUM(n)"<<n<<" = "<<SUM<<endl;
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
ALGBINPOSET();
return EXIT_SUCCESS;
}

/***********************
END SOURCE CODE
***********************/
