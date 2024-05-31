VAR     a,b,c : INTEGER.

BEGIN
    a:=2;
    b:=0;
    c:=0
END;

CASE a OF
1,2  : a := 100;
6    : a := 50;
b    : a := 4
ELSE
    a := 8
END;

DISPLAY a.
