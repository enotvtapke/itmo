#include <stdio.h>
#include <stdbool.h>
bool a;
bool b;
int c;
bool d;
int j;

int main() {
a=1;
b=2;
scanf("%d", &c);
printf("%d\n", a+c*b);
b=10;
d=a*b;
printf("%d\n", d/c-b);
if (a==b){
    printf("%d\n", b);
    }
else if (b==c){
    printf("%d\n", c);
    }
else{
    printf("%d\n", a);
    }
j=5;
while (j<10){
    printf("%d\n", j);
    printf("%d\n", j*2);
    scanf("%d", &c);
    if (j>5){
    printf("%d\n", j);
        }
j=j+2;
    }
printf("%d\n", 22);
a=true;
b=false;
d=a&&b||a;
printf("%d\n", d);

    return 0;
}
