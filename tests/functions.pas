program FunctionsTest;
var a : integer, vysl : integer; 

function random (n : integer) : integer;
    var nahodne_cislo : integer; 
begin
    nahodne_cislo := 67;
end 

function dalsi_random ( q : string, a : string) : string;
    var nahodne : integer;
begin
    nahodne := 42;
end

begin 
    a := dalsi_random("hahaha");
    writeln(a);
    b := random(40)
end.
