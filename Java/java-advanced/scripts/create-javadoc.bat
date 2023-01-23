set course_rep=%cd%\..\..\java-advanced-2022
set test_code=%course_rep%\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor
set source_root=%cd%\..\java-solutions\info\kgeorgiy\ja\stupnikov\implementor
set link=https://docs.oracle.com/en/java/javase/17/docs/api/

javadoc -d %cd%\..\javadoc -link %link% -cp %source_root%\;%course_rep%\lib\ -private -author -version %test_code%\Impler.java %test_code%\JarImpler.java %test_code%\ImplerException.java %source_root%\Implementor.java