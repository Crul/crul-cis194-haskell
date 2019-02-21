@echo off
cd src
CALL :Profile HW01Tests
CALL :Profile HW02Tests
CALL :Profile HW03Tests
CALL :Profile HW04Tests
CALL :Profile HW05Tests
CALL :Profile HW05 , "clues/dog-original.jpg clues/dog.jpg clues/transactions.json clues/victims.json clues/new-ids.json clues/new-transactions.json"
CALL :Profile HW06Tests
CALL :Profile HW06
CALL :Profile HW07Tests
cd ..

EXIT /B %ERRORLEVEL%

:Profile
ghc %~1.hs -rtsopts -main-is %~1
%~1.exe %~2 +RTS -s -h -i0.001
hp2ps -c %~1.hp
ps2pdf %~1.ps
EXIT /B 0
