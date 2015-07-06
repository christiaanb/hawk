@ECHO OFF

set HAWKROOT_MS=%HAWKROOT%
set HAWKROOT=%HAWKROOT:\=/%
SET EXEC=%HAWKROOT_MS%\simulation\bin\%0

bash %EXEC% %1 %2 %3 %4 %5 %6 %7 %8 %9
