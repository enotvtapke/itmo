set course_rep=%cd%\..\..\java-advanced-2022
set source_root=%cd%\..\java-solutions\info\kgeorgiy\ja\stupnikov\implementor

javac -d %cd% -cp %source_root%\;%course_rep%\modules\info.kgeorgiy.java.advanced.implementor\ %source_root%\*.java
jar cfm %cd%\Implementor.jar %source_root%\META-INF\MANIFEST.MF info\kgeorgiy\ja\stupnikov\implementor\*.class
rmdir /s /q %cd%\info