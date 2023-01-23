@echo off

set classpath=..\..\..\..\..

start %java_home%\bin\rmiregistry -J-Djava.class.path=..\..\..\..\..
start %java_home%\bin\java info.kgeorgy.ja.stupnikov.rmi.Server
