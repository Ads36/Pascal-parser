program StringTest;
var y : string, z : string;

begin
    y:='ahoj';
    writeln(z);
    z := 'slunicko';
    z := y + z;
    if('slunicko' == y) then writeln(y);
    else writeln(z)
end.
