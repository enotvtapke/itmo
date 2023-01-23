chcp 437
mkdir "C:/lab6"
cd "C:/lab6"
@echo off
(
	echo list disk
) > tmp
diskpart /s tmp | findstr "Disk.[0-9]" > c:/lab6/diskpart.txt
rm tmp
ver > c:/lab6/ver.txt
mkdir "C:/lab6/TEST"
copy "C:/lab6/" "C:/lab6/TEST/"
cd TEST
copy /b *.txt merged.txt
for %%i in (*) do if not %%i == merged.txt del %%i