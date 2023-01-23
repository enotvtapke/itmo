grammar PythonPreprocessor;

@parser::header {
    import java.util.*;
    import java.lang.*;
    import java.util.stream.Collectors;
}

@parser::members {
    public StringBuilder translated = new StringBuilder();
    Set<String> memory = new HashSet<String>();
    Integer ident = 0;
}

file returns [String res]
: (stats+=stat)* EOF
{
String stats = $stats.stream().map(it -> it.res).collect(Collectors.joining());
translated.append(stats);
}
;

stat returns [String res]
: idents ANY NEWLINE
{
int curIdent = $idents.count;
if (curIdent > ident) {
    $res = "{" + $ANY.text + $NEWLINE.text;
} else if (curIdent < ident) {
    $res = "}".repeat(ident - curIdent) + $ANY.text + $NEWLINE.text;
} else {
    $res = $ANY.text + $NEWLINE.text;
}
ident = curIdent;
}
;

idents returns [Integer count] locals [int i=0]
: ( IDENT {$i += 1;} )* { $count = $i; }
;

ANY : ~[ \t]~[\r\n]+ ;
NEWLINE
    : '\r'? '\n'
    ;
IDENT : ('\t'|'    ') ;

