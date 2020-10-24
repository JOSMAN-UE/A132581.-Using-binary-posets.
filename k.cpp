
/**********************************/
////////////////////////////////////

/********************************************
  OEIS A132581. 
  The Function F(n). 
  Antichains on posets.
  and using theory rules. See docs.
  Less than 2' from F(1) to F(130).
  on small computer, one single task.

  (C) J.M.M.Aranda.Spain-UE. 2020.
  Related to author works.

OEIS: A000372. Dedekind numbers.
      A132581. my F(n)
********************************************/

//
// warning bug mingw gcc pragma pack(16) 
// give map segfault nnnn.
//

#include <cstdbool>
#include <bitset>
#include <cstdio>
#include <iostream>
#include <conio.h>
#include <ctime>
#include <map>
#include <stack>

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

#define XWIDE 128
#define ZWIDE 131

using U128=unsigned __int128;
using U64=uint64_t;
using U32=uint32_t;
static stack<U128> Q ATALI;
static array<U128,XWIDE> MASKPOW2 ATALI;
static array<U128,XWIDE> MASKNOT2;
static array<U128,XWIDE> HMASK ATALI;
static array<U64,ZWIDE> FNARR;

using  XYMAPTYPE=map<U128,U32>;
static XYMAPTYPE XYMAP ATALI;
static XYMAPTYPE::iterator tera;
//******************************

void newl(void)
{
cout<<endl;
}

void binU128(U128 p)
{
U128 n;char ch;
int len=20;

n=p;
for(int b=len;b>=0;b--)
  {
  ch='0';if((n band MASKPOW2[b]) NE 0)ch='1';
  printf("%c",ch);if(b%4 EQ 0)printf(" ");
  }
newl();
}

void printU128(U128 p)
{
const int BX=40,cc=10;
char buf[BX],CHZ='0';
int k,m;U128 n;

n=p;
for(int i=0;i LT BX;i++)buf[i]=CHZ;
k=BX-1;buf[k]=0;
while(n NE 0)
  {
  m=n%cc;buf[--k]=CHZ+m;
  n=(n-m);n=n/cc;
  }
printf("U128: %s \n",buf);
}

int highbit(U128 pa) ATHOT;
int highbit(U128 pa)
{
U128 x,po(1);int hbit;
x=pa;hbit=-1;
for(int bit=0;bit LT XWIDE;bit++)
  {
  const U128 tmp=(x band po);
  if(tmp NE 0)hbit=bit;
  po<<=1; 
  }
if(hbit LT 0)exit(2);
return hbit;
}

static bool BOOMAP;
U32 finder(U128 pl) ATHOT;
U32 finder(U128 pl)
{
U32 y;
if(pl LE 2)
  {
  BOOMAP=true;
  if(pl EQ 0)return 1;
  if(pl EQ 1)return 2;
  if(pl EQ 2)return 2;
  }

//
  {
  tera=XYMAP.find(pl);
  if(tera NE XYMAP.end())
    {
    BOOMAP=true;
    y=tera->second;return y;
    }
  }

//
BOOMAP=false;
return 0;
}

void insert(U128 x,U32 y) ATHOT;
void insert(U128 x,U32 y)
{
XYMAP[x]=y;
}


U128 xleft(U128 p,int higbit) ATHOT;
U128 xleft(U128 p,int higbit)
{
U128 lef=p;    // Clear hibit.
lef bans MASKNOT2[higbit];
return lef;
}

U128 xright(U128 p,int bit) ATHOT;
U128 xright(U128 p,int bit)
{
U128 rig;
// Clear bits. dleq(*,bit). see doc.
rig=p;
rig bans HMASK[bit];
return rig;
}

U128 binposet(int n) ATHOT;
U128 binposet(int n)
{
U128 t=MASKPOW2[n]-1;
return t;
}

U64 FUNCB(U128 p0) ATHOT;
U64 FUNCB(U128 p0)
{
U64 FSUM;

if(p0 EQ 0)return 1;
if(p0 EQ 1)return 2;

FSUM=0;
while(NOT Q.empty())Q.pop();
Q.push(p0);
while(NOT Q.empty())
  {
  U32 ylar,ylef,yrig;U128 xlar,xlef,xrig;
  int higbit;bool blef,brig;
  //
  xlar=Q.top();Q.pop();higbit=highbit(xlar);
  if(xlar EQ 0){FSUM++;continue;};
  //
  ylar=finder(xlar);
  if(BOOMAP){FSUM+=ylar;continue;}
  //
  xlef=xleft(xlar,higbit);
  xrig=xright(xlar,higbit);
  ylef=finder(xlef);blef=BOOMAP;

  if(xrig EQ xlef){brig=blef;yrig=ylef;}
  else 
    {
    yrig=finder(xrig);brig=BOOMAP;
    }
  //
  if(blef AND brig)
    {
    ylar=ylef+yrig;
    insert(xlar,ylar);FSUM+=ylar;
    }
  else
    {
    if(blef)FSUM+=ylef; else Q.push(xlef);
    if(brig)FSUM+=yrig; else Q.push(xrig);
    }
  } // end main while

insert(p0,FSUM);
return FSUM;
}

U64 FUNCA(int pn) ATHOT;    
U64 FUNCA(int pn)    
{
U128 code0,xrig0;
U64 FSUM;
U64 VB;int hbit0;

if(pn EQ 0)return 1; 
code0=binposet(pn);
hbit0=highbit(code0);
xrig0=xright(code0,hbit0);
FSUM=0;
if(pn GE 1)FSUM=FNARR[pn-1];
if(xrig0 EQ 0)VB=1;
else VB=FUNCB(xrig0);
FSUM+=VB;
return FSUM; 
}

U64 FUNCD(int pn);    
U64 FUNCD(int pn)    
{
U64 VFNX,VFAA;
int pnx=pn;

VFNX=0;
if(pnx EQ    1 )VFNX = FNARR[   0]+FNARR[   0]; 
else if(pnx EQ    2 )VFNX = FNARR[   1]+FNARR[   0]; 
else if(pnx EQ    3 )VFNX = FNARR[   2]+FNARR[   1]; 
else if(pnx EQ    4 )VFNX = FNARR[   3]+FNARR[   0]; 
else if(pnx EQ    5 )VFNX = FNARR[   4]+FNARR[   3]; 
else if(pnx EQ    6 )VFNX = FNARR[   5]+FNARR[   2]; 
else if(pnx EQ    7 )VFNX = FNARR[   6]+FNARR[   3]; 
else if(pnx EQ    8 )VFNX = FNARR[   7]+FNARR[   0]; 
else if(pnx EQ    9 )VFNX = FNARR[   8]+FNARR[   7]; 
else if(pnx EQ   10 )VFNX = FNARR[   9]+FNARR[   6]; 
else if(pnx EQ   12 )VFNX = FNARR[  11]+FNARR[   4]; 
else if(pnx EQ   14 )VFNX = FNARR[  13]+FNARR[   6]; 
else if(pnx EQ   15 )VFNX = FNARR[  14]+FNARR[   7]; 
else if(pnx EQ   16 )VFNX = FNARR[  15]+FNARR[   0]; 
else if(pnx EQ   17 )VFNX = FNARR[  16]+FNARR[  15]; 
else if(pnx EQ   18 )VFNX = FNARR[  17]+FNARR[  14]; 
else if(pnx EQ   24 )VFNX = FNARR[  23]+FNARR[   8]; 
else if(pnx EQ   28 )VFNX = FNARR[  27]+FNARR[  12]; 
else if(pnx EQ   30 )VFNX = FNARR[  29]+FNARR[  14]; 
else if(pnx EQ   31 )VFNX = FNARR[  30]+FNARR[  15]; 
else if(pnx EQ   32 )VFNX = FNARR[  31]+FNARR[   0]; 
else if(pnx EQ   32 )VFNX = FNARR[  31]+FNARR[ 0]; 
else if(pnx EQ   33 )VFNX = FNARR[  32]+FNARR[  31]; 
else if(pnx EQ   34 )VFNX = FNARR[  33]+FNARR[  30]; 
else if(pnx EQ   48 )VFNX = FNARR[  47]+FNARR[  16]; 
else if(pnx EQ   56 )VFNX = FNARR[  55]+FNARR[  24]; 
else if(pnx EQ   60 )VFNX = FNARR[  59]+FNARR[  28]; 
else if(pnx EQ   62 )VFNX = FNARR[  61]+FNARR[  30]; 
else if(pnx EQ   63 )VFNX = FNARR[  62]+FNARR[  31]; 
else if(pnx EQ   64 )VFNX = FNARR[  63]+FNARR[   0]; 
else if(pnx EQ   65 )VFNX = FNARR[  64]+FNARR[  63]; 
else if(pnx EQ   66 )VFNX = FNARR[  65]+FNARR[  62]; 
else if(pnx EQ   96 )VFNX = FNARR[  95]+FNARR[  32]; 
else if(pnx EQ  112 )VFNX = FNARR[ 111]+FNARR[  48]; 
else if(pnx EQ  120 )VFNX = FNARR[ 119]+FNARR[  56]; 
else if(pnx EQ  124 )VFNX = FNARR[ 123]+FNARR[  60]; 
else if(pnx EQ  126 )VFNX = FNARR[ 125]+FNARR[  62]; 
else if(pnx EQ  127 )VFNX = FNARR[ 126]+FNARR[  63]; 
else if(pnx EQ  128 )VFNX = FNARR[ 127]+FNARR[   0];
else if(pnx EQ  129 )VFNX = FNARR[ 128]+FNARR[ 127]; 
else if(pnx EQ  130 )VFNX = FNARR[ 129]+FNARR[ 126]; 

if(VFNX EQ 0LL)VFAA=FUNCA(pnx);
else
  {
  printf("  - this solved by theory rules. \n");
  VFAA=VFNX;
  }
FNARR[pnx]=VFAA;
return VFAA;
}

void initTAB(void)
{
U128 VPOW(1);
for(int n=0;n LT XWIDE;n++)
  {
  U128 VNOT=(bnot VPOW);
  MASKPOW2[n]=VPOW;MASKNOT2[n]=VNOT;VPOW<<=1;
  }
}
void initHMASK(void)
{
U128 maskones(0);

for(int b=0;b LT XWIDE;b++)
  {maskones<<=1;maskones++;
  }
for(int h=0;h LT XWIDE;h++)
  {
  U128 imask=maskones;
  for(int bit=0;bit LE h;bit++) 
    { // dleq(bit,h).
    const int tmp=(bit band h);
    if(tmp EQ bit)imask=imask band MASKNOT2[bit];
    HMASK[h]=imask;
    }
  }
}

void initMAPS(void)
{
XYMAP.clear();

for(int i=0;i LT ZWIDE;i++)FNARR[i]=0;
FNARR[0]=1;
insert(0,1);insert(1,2);insert(2,2);
}

void ALGBINPOSET(void)
{
for(int n=0;n LT ZWIDE;n++) 
  {
  U64 RESU;
  printf("** n= %d \n",n);
  RESU=FUNCD(n);
  printf("n=%4d F=%lld \n",n,RESU);
  }
}


int main(void)
{
time_t pini,pend;

cout<<"OEIS A132581. The function F(n)."<<endl; 
cout<<"(C) JM M Aranda Spain-UE. 2020."<<endl;
cout<<"    Counting antichains on posets."<<endl;
cout<<"    and using theory rules."<<endl;
cout<<"    Related to author works."<<endl;
cout<<endl;

printf("Init tables. \n");
initTAB();initHMASK();initMAPS();

//
time(&pini);
ALGBINPOSET();
time(&pend);
printf("START %s \n",ctime(&pini));
printf("END   %s \n",ctime(&pend));

return EXIT_SUCCESS;
}


/***************************************
int powdose(int e)
{
int r=1;

for(int i=0;i LT e;i++)r*=2;
return r;
}

void printrule(int a,int b,int c)
{
printf("if($1$ EQ %4d )$2$ = $3$[%4d]+$3$[%4d]; \n"
,a,b,c);
}

void theFrules(void)
{
int L=9;

printf("RULE ONE \n");
for(int a=1;a LT L;a++)
for(int b=0;b LT L;b++)
  {
  int e,f;
  e=(p(a)-1)*p(b);f=(p(a-1)-1)*p(b);
  if(e GT 256)continue;
  pr(e,e-1,f);
  }

printf("RULE TWO \n");
for(int a=1;a LT L;a++)
for(int b=0;b LT L;b++)
  {
  int e,f;
  e=(p(a)-1)*p(b);f=(p(a-1)-1)*p(b);
  if(e GT 256)continue;
  pr(e,e-1,f);
  }

printf("RULES 3,4,5 \n");
for(int a=0;a LT L;a++)
  {
  int e;
  e=p(a);
  pr(e,e-1,0);
  pr(e+1,e,e-1);
  pr(e+2,e+1,e-2);
  }
printf("RULE 08 \n");
for(int a=0;a LT L;a++)
for(int b=0;b LE a;b++)
  {
  int e,f,g;
  e=p(a);
  f=p(a)-p(b);
  g=p(a)-p(a-b);
  pr(e,f,g);
  }
exit(0);
}
if(pnx EQ  128 )VFNX = FNARR[ 127]+FNARR[   0]; 
if(pnx EQ  129 )VFNX = FNARR[ 128]+FNARR[ 127]; 
if(pnx EQ  130 )VFNX = FNARR[ 129]+FNARR[ 126]; 
if(pnx EQ  192 )VFNX = FNARR[ 191]+FNARR[  64]; 
if(pnx EQ  224 )VFNX = FNARR[ 223]+FNARR[  96]; 
if(pnx EQ  240 )VFNX = FNARR[ 239]+FNARR[ 112]; 
if(pnx EQ  248 )VFNX = FNARR[ 247]+FNARR[ 120]; 
if(pnx EQ  252 )VFNX = FNARR[ 251]+FNARR[ 124]; 
if(pnx EQ  254 )VFNX = FNARR[ 253]+FNARR[ 126]; 
if(pnx EQ  255 )VFNX = FNARR[ 254]+FNARR[ 127]; 
if(pnx EQ  256 )VFNX = FNARR[ 255]+FNARR[   0]; 
***************************************/




/***********************
END SOURCE CODE
***********************/


