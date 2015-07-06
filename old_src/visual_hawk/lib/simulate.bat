@echo off
@rem %1 contains SCRIPT to run


@rem this assumes the user has write access to the visio demo
@rem probably we ought to do something like:
@rem if [ ! -e /tmp/$USER.Probes ]  ; then
@rem      mkdir /tmp/$USER.Probes
@rem fi
@rem WORKING_DIR=/tmp

set WORKING_DIR=%VISUAL_HAWKROOT%\Nest
cd %WORKING_DIR%


set HAWK=%HAWKROOT%\simulation\lib
set DLX=%HAWKROOT%\simulation\dlx
set OA64=%HAWKROOT%\simulation\oa-64
set P6=%HAWKROOT%\models\p6
set PIPE=%HAWKROOT%\models\pipeline+speculative
set VISIO=%VISUAL_HAWKROOT%\lib

set CMBIA=%HAWKROOT%\models\columbia



c:\hugs98\runhugs -P;%HAWK%;%DLX%;%OA64%;%P6%;%PIPE%;%VISIO%;%CMBIA% %1
