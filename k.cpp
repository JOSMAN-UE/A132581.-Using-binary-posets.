


/********************************************
  OEIS A132581. 
  (C) J.M.M.Aranda.Spain-UE. 2020.
  The Function F(n). 
  Antichains on posets. See doc.
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
#include <array>
#include <map>
#include <stack>
#include <bitset>
#include <conio.h>

using namespace std;

#define ATALI __attribute__((aligned))
#define ATHOT __attribute__((hot))
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

//using BS256=bitset<256>;

#define XWIDE 128
using LNUM=unsigned __int128;
using BNUM=uint64_t;
using XYMAPTYPE=map<LNUM,BNUM>;
static XYMAPTYPE XYMAP;
static XYMAPTYPE::iterator xytor;
static stack<LNUM> Q;
static array<LNUM,256> MASKPOW2;
static array<LNUM,256> MASKNOT2;
static array<LNUM,256> HMASK;

//******************************

void newl(void)
{
cout<<endl;
}

void binLNUM(LNUM p)
{
LNUM n;char ch;
int len=20;

n=p;
for(int b=len;b>=0;b--)
  {
  ch='0';if((n band MASKPOW2[b]) NE 0)ch='1';
  printf("%c",ch);if(b%4 EQ 0)printf(" ");
  }
newl();
}

void printLNUM(LNUM p)
{
const int BX=16,cc=10;
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

static bool FINDED;
BNUM finder(LNUM lwtf)
{
LNUM wtf;
bitset<128> bs; // ///////////  128 +++++++++
int qbs;
 
FINDED=false;wtf=lwtf;
bs=wtf;qbs=bs.count();
// See doc.
if(qbs EQ 0){FINDED=true;return 1;}
if(qbs EQ 1){FINDED=true;return 2;}

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

LNUM xright(LNUM p,int bit)
{
LNUM rig=p;
// Clear bits. dleq(*,bit). see doc.
rig bans HMASK[bit];
return rig;
}

BNUM FUNC(LNUM xlar0)
{
BNUM FX0(0);
Q.push(xlar0);
while(NOT Q.empty())
  {
  BNUM ylar,yl,yr;
  LNUM xlar,xlef,xrig;

  xlar=Q.top();Q.pop();ylar=finder(xlar);
  if(FINDED)FX0+=ylar;
  else
    {
    bool bl,br,blar;
    int higbit;

    higbit=highbit(xlar);
    xlef=xleft(xlar,higbit);
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
// See doc chapter (x,l,r).
XYMAP[0]=1; 
for(int n=0;n LT XWIDE;n++)XYMAP[MASKPOW2[n]]=2;
for(int n=1;n LT XWIDE;n++)XYMAP[MASKPOW2[n]+1]=3;
// 
XYMAP[ 3]= 3;  //2
XYMAP[ 7]= 5;  //3
XYMAP[15]= 6;  //4
XYMAP[31]=11;  //5
XYMAP[63]=14;  //6
XYMAP[127]=19; //7
XYMAP[255]=20; //8
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
mytime();
for(int n=0;n LE 127;n++) 
  {
  BNUM RESU;LNUM code;

  //mytime();
  code=binposet(n);RESU=FUNC(code);
  printf("n=%4d F=%lld \n",n,RESU);
  }
mytime();
}


int main(void)
{
cout<<"OEIS A132581. The function F(n)."<<endl; 
cout<<"(C) JM M Aranda Spain-UE. 2020."<<endl;
cout<<"    Counting antichains, posets"<<endl;
cout<<"    Related to author works."<<endl;
cout<<endl;

initTAB();
initHMASK();
initXYMAP();

ALGBINPOSET();
return EXIT_SUCCESS;
}

/***********************
END SOURCE CODE
***********************/


