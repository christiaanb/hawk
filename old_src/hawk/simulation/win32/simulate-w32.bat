@echo off
rem %1 contains SCRIPT to run

set HAWKLIB=%HAWKROOT%\simulation\lib
set DEMOLIB=%HAWKROOT%\simulation\demo
set DLXLIB=%HAWKROOT%\simulation\dlx
set OA64LIB=%HAWKROOT%\simulation\oa-64
set P6LIB=%HAWKROOT%\models\p6
set PIPELIB=%HAWKROOT%\models\pipeline+speculative

c:\hugs\runhugs -P;%HAWKLIB%;%DEMOLIB%;%DLXLIB%;%OA64LIB%;%P6LIB%;%PIPELIB% %1
