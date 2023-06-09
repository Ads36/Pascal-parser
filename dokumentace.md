# Dokumentace programu

## Přehled
Tento program je parser podmnožiny programovacího jazyka Pascal. Parsuje programový kód a vytváří abstraktní syntaktický strom (AST). Parser podporuje deklarace proměnných, aritmetické a logické výrazy, řídicí struktury (if-then-else, while, for), vstupní a výstupní operace a deklarace a volání funkcí.

## Použití
Pro spuštění parseru Pascalu spusťte program a jako první argument zadejte vstupní soubor. Vstupní soubor musí obsahovat zdrojový kód Pascal programu, který chcete parsovat.

Program se nejdříve musí překompilovat:
```
> ghc .\PascalParser.hs
```
Po kompilaci se již může spustit.
Příklad použití:
```
> .\PascalParser.exe .\tests\all.pas
```
Program na standartní výstup vypíše buď textová reprezentace abstrakntího stromu výrazu nebo error (pokud se nepodaří naparsovat). 
## Struktura programu
Program se skládá z následujících částí:

### Datové typy
- `ParsedTypes`: Reprezentuje podporované datové typy v parseru.
- `Command`: Definuje různé typy příkazů, které mohou být vykonány.
- `Functions`: Reprezentuje deklarace funkcí.
- `Expression`: Reprezentuje výrazy a volání funkcí.
- `BoolExpression`: Reprezentuje logické výrazy používané ve řídicích strukturách.

### Parser
- `pascalParser`: Parsuje celý zdrojový kód.
- `languageDefinition`: Definuje syntaxi jazyka Pascal a rezervovaná klíčová slova.
- `parseStringLiteral`: Parsuje textové řetězce uzavřené v jednoduchých nebo dvojitých uvozovkách.
- `parseProgram`: Parsuje deklaraci programu.
- `parseVariables`: Parsuje deklarace proměnných.
- `parseVariable`: Parsuje jednu deklaraci proměnné.
- `parseFunctionBody`: Parsuje deklarace funkcí.
- `parseCommand`: Parsuje různé typy příkazů.
- `parseAssignment`: Parsuje přiřazení hodnoty proměnné.
- `parseArithmeticExpression`: Parsuje aritmetické výrazy.
- `parseExpressionTerm`: Parsuje výrazové termíny (čísla, řetězce, proměnné a volání funkcí).
- `parseDouble`: Parsuje desetinnou konstantu.
- `parseInteger`: Parsuje celočíselnou konstantu.
- `parseFunctionExpression`: Parsuje výrazy volání funkcí.
- `parseIdExpr`: Parsuje výrazy identifikátorů.
- `parseBoolExpression`: Parsuje logické výrazy používané ve řídicích strukturách.
- `tryParseWholeProgram`: Volá `pascalParser` a vrací abstraktní syntaktický strom (AST).

## Závislosti
Program vyžaduje následující knihovny:
- `System.Environment`
- `System.IO`
- `Text.ParserCombinators.Parsec`
- `Text.ParserCombinators.Parsec.Token`
- `Text.ParserCombinators.Parsec.Expr`
- `Text.ParserCombinators.Parsec.Language`
- `Control.Monad.Identity`

## Závěrem
Díky programování tohoto parseru jsem se naučil pracovat s knihovnami uvedenými výše a také zdokonalil svoje programovací skills v Haskellu.