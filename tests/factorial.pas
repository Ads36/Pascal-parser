program Example;
var a : integer, vysl : integer;
begin
    writeln("Zadejte cislo pro vypocet faktorialu: ");
    readln(a);
    if a < 0 then
        writeln('Faktorial nelze spocitat')
    else
    begin
    vysl := 1;
    for i := 1 to a do
        begin
            vysl := vysl * a;
        end;
        writeln('Vysledek je:');
        writeln(vysl)
    end
end.  
