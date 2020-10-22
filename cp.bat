echo on
prompt $g
cls
del %1.exe
rem
g++ -march=native -Ofast -o %1.exe %1.cpp -Wall -Wextra    
REM

