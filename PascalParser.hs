import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PToken
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Control.Monad.Identity (Identity)

-- data types suported by parser
data ParsedTypes =
    ParsedNone
    | ParsedInteger
    | ParsedDouble
    | ParsedString
    | ParsedFunction
	deriving Show

-- commands 
data Command = 
    Empty
	| Assign String Expression
	| Writeln Expression
	| Readln String
	| Block [Command]
	| If BoolExpression Command Command
	| While BoolExpression Command
	| Expression Expression
    | For Command String Command
	deriving Show

-- fucntion data type: name, parameters, return type, local variables, commands
data Functions = Function String [ (String, ParsedTypes) ] ParsedTypes [ (String, ParsedTypes) ] Command deriving Show

-- expressions
data Expression = 
    IntegerConst Int
	| StringConst String
	| DoubleConst Double
	| Var String
	| Addition Expression Expression
	| Subtraction Expression Expression
	| Multiplication Expression Expression
	| Division Expression Expression
	| Parantheses Expression
	| FunctionCall String [ Expression ]
    | Program String
	deriving Show

-- special kind of expressions, used in if and while commands
data BoolExpression = 
    IsEqual Expression Expression
	| IsNotEqual Expression Expression
	| IsGreater Expression Expression
	| IsLess Expression Expression
	| IsGreaterOrEqual Expression Expression
	| IsLessOrEqual Expression Expression
	deriving Show

-- pascal language definition, key words
languageDefiniton :: GenLanguageDef String u Identity
languageDefiniton = emptyDef {
	commentStart = "{",
    commentEnd = "}",
    commentLine = "//",
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_',
    opStart = opLetter emptyDef,
    opLetter = oneOf "+-=:/<>",
    reservedOpNames	= [":=", "+", "*", "/", "=", "<>", "-", "<", ">", "<=", ">=", "=="],
    reservedNames = ["begin", "do", "double", "else", "end",
					"if", "integer", "readln", "string", "then",
                    "var", "while", "writeln", "function"] }

-- library functions, used for parsing different tokens
tokenParser = PToken.makeTokenParser languageDefiniton
whiteSpaces = PToken.whiteSpace tokenParser
integer = PToken.integer tokenParser
double = PToken.float tokenParser
parentheses = PToken.parens tokenParser
semicolon = PToken.semi tokenParser
identifier = PToken.identifier tokenParser
reserved = PToken.reserved tokenParser
reservedOp = PToken.reservedOp tokenParser
dot = PToken.dot tokenParser
comma = PToken.comma tokenParser
colon = PToken.colon tokenParser

-- parses whole program file
pascalParser :: Parser ([(String, ParsedTypes)], [Functions], Command)
pascalParser = 
    do
        whiteSpaces
        programName <- parseProgram 
        parsedVariables <- option [] parseVariables
        functionDeclares <- many parseFunctionBody
        reserved "begin"
        abstractSyntaxTree <- (parseCommand `sepEndBy` semicolon)
        reserved "end"
        dot
        eof		-- end of file
        return (parsedVariables, functionDeclares, Block abstractSyntaxTree)

-- parses string, string can be either in single or double quotes
parseStringLiteral :: Parser String
parseStringLiteral =
		try ( do 
            string1 <- parseString '\''
            string2 <- parseStringLiteral
            return (string1 ++ "\'" ++ string2) )
	<|> try ( do 
            string1 <- parseString '\''
            return string1 )

	<|> try ( do 
            string1 <- parseString '\"'
            string2 <- parseStringLiteral
            return (string1 ++ "\"" ++ string2) )
	<|> try ( do 
            string1 <- parseString '\"'
            return string1 )
	where 
		parseString :: Char -> Parser String 
		parseString quotMarks = do
			char quotMarks
			str <- manyTill anyChar (char quotMarks)
			return str

parseProgram :: Parser Expression
parseProgram = do
    reserved "program"
    name <- identifier
    semicolon
    return (Program name)

--parses variable declarations
parseVariables :: Parser [ (String, ParsedTypes) ]
parseVariables = do
	reserved "var"
	variables <- parseVariable `sepBy1` comma
	semicolon
	return variables

-- parses variable declaration
parseVariable :: Parser (String, ParsedTypes)
parseVariable = do
	variableName <- identifier
	colon
	typeName <- reservedTypeName
	return (variableName, typeName)
	where
		reservedTypeName = do
			try (do{reserved "integer";
			return ParsedInteger})
			<|>
			try (do{reserved "double";
			return ParsedDouble})
			<|>
			try (do{reserved "string";
			return ParsedString})

-- parses function declaration
parseFunctionBody :: Parser Functions
parseFunctionBody = do
	reserved "function"
	id <- identifier
	parameters <- parentheses (option [] (parseVariable `sepBy1` comma))
	colon
	typeName <- reservedTypeName
	semicolon	
	variables <- option [] parseVariables
	commands <- option Empty parseCommand
	return (Function id parameters typeName variables commands)
	where
		reservedTypeName = do
			try (do{reserved "integer";
			return ParsedInteger})
			<|>
			try (do{reserved "double";
			return ParsedDouble})
			<|>
			try (do{reserved "string";
			return ParsedString})

-- parses all kinds of commands - assignment, writeln, readln, if, while, block, for
parseCommand :: Parser Command
parseCommand = do
		lookAhead semicolon
		return Empty
	<|> 
    	try parseAssignment
    <|> do
    	reserved "begin"
    	commands <- (parseCommand `sepEndBy` semicolon)
    	reserved "end"
    	return (Block commands)
    <|> do
    	reserved "writeln"
    	expression <- parentheses parseArtihmeticExpression
    	return (Writeln expression)
    <|> do
    	reserved "readln"
    	id <- parentheses identifier
    	return (Readln id)
    <|> do
    	reserved "if"
    	condition <- parseBoolExpression
    	reserved "then"
    	commands1 <- option Empty parseCommand
    	optional semicolon
    	reserved "else"
    	commands2 <- option Empty parseCommand
    	return (If condition commands1 commands2)
    <|> do
    	reserved "while"
    	condition <- parseBoolExpression
    	reserved "do"
    	commands <- parseCommand
    	return (While condition commands)
    <|> do
        reserved "for"
        id <- parseAssignment
        reserved "to"
        to <- (try (many1 digit)) <|>  (try identifier) 
        whiteSpaces
        reserved "do" 
        commands <- parseCommand
        return (For id to commands)
   	<|> do
   		expression <- parseArtihmeticExpression
   		return (Expression expression)

-- parses variable assignment
parseAssignment :: Parser Command
parseAssignment = do
    variable <- identifier
    reservedOp ":="
    expression <- parseArtihmeticExpression
    return (Assign variable expression)

-- parses arithmetic expressions, special library function call
parseArtihmeticExpression :: Parser Expression
parseArtihmeticExpression = buildExpressionParser operators parseExpressionTerm 
    where
		operators = [[ operator' "*" Multiplication, operator' "/" Division ],
					[ operator' "+" Addition, operator' "-" Subtraction ]]
		operator' operatorChar operatorExpression = Infix (do { reservedOp operatorChar; return operatorExpression }) AssocLeft

-- parses expresions - numbers, strings, variables, function calls
parseExpressionTerm :: Parser Expression
parseExpressionTerm = 
    try parseDouble
    <|> try parseInteger
    <|> try (do {s <- parseStringLiteral; return (StringConst s)})
	<|> try parseIdExpr
	<|> do
		expression <- parentheses parseArtihmeticExpression
		return (Parantheses expression)
	<|> try parseFunctionExpression

parseDouble :: Parser Expression
parseDouble = do
	sign <- option 1 plusOrMinus
	parsedDouble <- double
	return (DoubleConst (fromRational sign*parsedDouble))
    where
		-- to ensure parsing plus and minus before number
        plusOrMinus = do 
            s <- oneOf "+-"
            return (if s == '-' then -1.0 else 1.0)

parseInteger :: Parser Expression
parseInteger = do
    sign <- option 1 plusOrMinus
    parsedInteger <- integer
    return (IntegerConst (fromInteger (sign*parsedInteger)))
    where
		-- to ensure parsing plus and minus before number
        plusOrMinus = do 
            s <- oneOf "+-"
            return (if s == '-' then -1 else 1)

parseFunctionExpression :: Parser Expression
parseFunctionExpression = do
	name <- identifier
	parameters <- parentheses (option [] (parseArtihmeticExpression `sepBy1` comma))
	return (FunctionCall name parameters)

-- Identifier expression parser
parseIdExpr :: Parser Expression
parseIdExpr = do
	name <- identifier
	-- just identifier, used in conditions
	notFollowedBy (parentheses (option [] (parseArtihmeticExpression `sepBy1` comma)))
	return (Var name)

parseBoolExpression :: Parser BoolExpression
parseBoolExpression = do
    try (parentheses parseBoolExpression) 
    <|> do
		expressionLeft <- parseArtihmeticExpression
		whiteSpaces
		binOperator <- operator'
		whiteSpaces
		expressionRight <- parseArtihmeticExpression
		return (binOperator expressionLeft expressionRight)
	where
		-- operator searches for one of the operators
		operator' = do
			try (do{reserved ">=";
			return IsGreaterOrEqual})
			<|>
			try (do{reserved ">";
			return IsGreater})
			<|>
			try (do{reserved "<=";
			return IsLessOrEqual})
			<|>
			try (do{reserved "<";
			return IsLess})
			<|>
			try (do{reserved "<>";
			return IsNotEqual})
			<|>
			try (do{reserved "==";
			return IsEqual})

-- parses whole Pascal program, returns either error or abstract syntax tree
tryParseWholeProgram :: String -> ([(String, ParsedTypes)], [Functions], Command)
tryParseWholeProgram input =
	case parse pascalParser "a" input of
		Left e -> error ("Parser error:\n" ++ show e)
		Right abstractSyntaxTree -> abstractSyntaxTree

main :: IO ()
main = do
		arguments <- getArgs
		if length arguments == 0 
			then error "No arguments, first argument must be input file"
			else do
				let filename = head arguments
				input <- readFile filename
				let ast = tryParseWholeProgram input
				print ast
				return ()
