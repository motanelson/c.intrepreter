
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace CMiniIDE
{
    // ==========================================
    // TOKEN
    // ==========================================

    class Token
    {
        public string Type;
        public string Value;
        public int Line;

        public Token(string type, string value, int line)
        {
            Type = type;
            Value = value;
            Line = line;
        }
    }

    // ==========================================
    // LEXER
    // ==========================================

    class Lexer
    {
        private string source;
        private int pos;
        private int line;

        public Lexer(string source)
        {
            this.source = source;
            pos = 0;
            line = 1;
        }

        private char Current()
        {
            return pos < source.Length ? source[pos] : '\0';
        }

        private char Peek(int offset = 1)
        {
            int p = pos + offset;
            return p < source.Length ? source[p] : '\0';
        }

        private void Advance()
        {
            if (Current() == '\n')
                line++;

            pos++;
        }

        public List<Token> Tokenize()
        {
            List<Token> tokens = new List<Token>();

            while (pos < source.Length)
            {
                char ch = Current();

                if (char.IsWhiteSpace(ch))
                {
                    Advance();
                    continue;
                }

                // Single-line comments
                if (ch == '/' && Peek() == '/')
                {
                    while (pos < source.Length && Current() != '\n')
                        Advance();

                    continue;
                }

                // Multi-line comments
                if (ch == '/' && Peek() == '*')
                {
                    Advance();
                    Advance();

                    while (pos < source.Length &&
                           !(Current() == '*' && Peek() == '/'))
                    {
                        Advance();
                    }

                    if (pos >= source.Length)
                        throw Error("Unterminated comment");

                    Advance();
                    Advance();
                    continue;
                }

                // Numbers
                if (char.IsDigit(ch))
                {
                    StringBuilder number = new StringBuilder();

                    while (char.IsDigit(Current()) || Current() == '.')
                    {
                        number.Append(Current());
                        Advance();
                    }

                    if (!double.TryParse(
                        number.ToString(),
                        NumberStyles.Float,
                        CultureInfo.InvariantCulture,
                        out _))
                    {
                        throw Error("Invalid number: " + number);
                    }

                    tokens.Add(new Token("NUM", number.ToString(), line));
                    continue;
                }

                // Strings
                if (ch == '"')
                {
                    int startLine = line;
                    Advance();

                    StringBuilder str = new StringBuilder();
                    bool closed = false;

                    while (pos < source.Length)
                    {
                        ch = Current();

                        if (ch == '"')
                        {
                            Advance();
                            closed = true;
                            break;
                        }

                        if (ch == '\\')
                        {
                            Advance();

                            switch (Current())
                            {
                                case 'n': str.Append('\n'); break;
                                case 't': str.Append('\t'); break;
                                case 'r': str.Append('\r'); break;
                                case '"': str.Append('"'); break;
                                case '\\': str.Append('\\'); break;
                                case '0': str.Append('\0'); break;
                                default: str.Append(Current()); break;
                            }

                            Advance();
                        }
                        else
                        {
                            str.Append(ch);
                            Advance();
                        }
                    }

                    if (!closed)
                        throw new Exception(
                            $"[Line {startLine}] Unterminated string");

                    tokens.Add(new Token("STR", str.ToString(), startLine));
                    continue;
                }

                // Identifiers
                if (char.IsLetter(ch) || ch == '_')
                {
                    StringBuilder id = new StringBuilder();

                    while (char.IsLetterOrDigit(Current()) ||
                           Current() == '_')
                    {
                        id.Append(Current());
                        Advance();
                    }

                    tokens.Add(new Token("ID", id.ToString(), line));
                    continue;
                }

                // Two-character operators
                string two = new string(new char[] { ch, Peek() });

                if (new string[]
                {
                    "==", "!=", "<=", ">=",
                    "&&", "||", "++", "--"
                }.Contains(two))
                {
                    tokens.Add(new Token(two, two, line));
                    Advance();
                    Advance();
                    continue;
                }

                // Single-character operators
                if ("+-*/%<>=!;,(){}[]".Contains(ch))
                {
                    tokens.Add(new Token(ch.ToString(), ch.ToString(), line));
                    Advance();
                    continue;
                }

                throw Error("Unexpected character: '" + ch + "'");
            }

            tokens.Add(new Token("EOF", "", line));

            return tokens;
        }

        private Exception Error(string message)
        {
            return new Exception($"[Line {line}] {message}");
        }
    }

    // ==========================================
    // AST NODE
    // ==========================================

    class Node
    {
        public string Type;
        public string Name = "";
        public string Op = "";

        public object Value;
        public Node Left;
        public Node Right;
        public Node Target;
        public Node Init;
        public Node Cond;
        public Node Step;
        public Node Body;
        public Node ThenBranch;
        public Node ElseBranch;

        public List<Node> Args = new List<Node>();
        public List<Node> Statements = new List<Node>();
        public List<string> Parameters = new List<string>();

        public Node(string type)
        {
            Type = type;
        }
    }

    // ==========================================
    // PARSER
    // ==========================================

    class Parser
    {
        private List<Token> tokens;
        private int pos;

        private Token Current => tokens[pos];

        public Parser(List<Token> tokens)
        {
            this.tokens = tokens;
            pos = 0;
        }

        private Token Match(string type)
        {
            if (Current.Type == type)
                return tokens[pos++];

            return null;
        }

        private Token Expect(string type)
        {
            Token t = Match(type);

            if (t == null)
            {
                throw new Exception(
                    $"[Line {Current.Line}] Expected '{type}', found '{Current.Value}'");
            }

            return t;
        }

        private bool IsType()
        {
            return Current.Type == "ID" &&
                new string[] { "int", "void", "char", "float" }
                .Contains(Current.Value);
        }

        private void ReadType()
        {
            if (!IsType())
                throw new Exception(
                    $"[Line {Current.Line}] Expected data type");

            pos++;

            Match("*");
        }

        public Node Parse()
        {
            Node program = new Node("Program");

            while (Current.Type != "EOF")
            {
                program.Statements.Add(ParseDeclarationOrFunction());
            }

            return program;
        }

        private Node ParseDeclarationOrFunction()
        {
            if (IsType())
            {
                ReadType();

                string name = Expect("ID").Value;

                if (Match("(") != null)
                {
                    Node func = new Node("Function");
                    func.Name = name;

                    if (Current.Type != ")")
                    {
                        do
                        {
                            ReadType();
                            func.Parameters.Add(Expect("ID").Value);
                        }
                        while (Match(",") != null);
                    }

                    Expect(")");

                    func.Body = ParseBlock();

                    return func;
                }

                Node decl = new Node("VarDecl");
                decl.Name = name;

                if (Match("=") != null)
                    decl.Init = ParseExpression();

                Expect(";");

                return decl;
            }

            return ParseStatement();
        }

        private Node ParseBlock()
        {
            Expect("{");

            Node block = new Node("Block");

            while (Current.Type != "}" &&
                   Current.Type != "EOF")
            {
                block.Statements.Add(ParseStatement());
            }

            Expect("}");

            return block;
        }

        private Node ParseStatement()
        {
            if (Current.Type == "{")
                return ParseBlock();

            // Variable declaration
            if (IsType())
            {
                ReadType();

                Node decl = new Node("VarDecl");
                decl.Name = Expect("ID").Value;

                if (Match("=") != null)
                    decl.Init = ParseExpression();

                Expect(";");

                return decl;
            }

            // printf
            if (Current.Type == "ID" &&
                Current.Value == "printf")
            {
                pos++;
                Expect("(");

                Node print = new Node("Printf");

                if (Current.Type != ")")
                {
                    print.Args.Add(ParseExpression());

                    while (Match(",") != null)
                        print.Args.Add(ParseExpression());
                }

                Expect(")");
                Expect(";");

                return print;
            }

            // If / Else
            if (Current.Type == "ID" &&
                Current.Value == "if")
            {
                pos++;

                Expect("(");
                Node cond = ParseExpression();
                Expect(")");

                Node node = new Node("If");
                node.Cond = cond;
                node.ThenBranch = ParseStatement();

                if (Current.Type == "ID" &&
                    Current.Value == "else")
                {
                    pos++;
                    node.ElseBranch = ParseStatement();
                }

                return node;
            }

            // While
            if (Current.Type == "ID" &&
                Current.Value == "while")
            {
                pos++;

                Expect("(");

                Node node = new Node("While");
                node.Cond = ParseExpression();

                Expect(")");

                node.Body = ParseStatement();

                return node;
            }

            // For
            if (Current.Type == "ID" &&
                Current.Value == "for")
            {
                pos++;

                Expect("(");

                Node node = new Node("For");

                if (Current.Type != ";")
                {
                    if (IsType())
                    {
                        ReadType();

                        node.Init = new Node("VarDecl");
                        node.Init.Name = Expect("ID").Value;

                        Expect("=");

                        node.Init.Init = ParseExpression();
                    }
                    else
                    {
                        node.Init = ParseExpression();
                    }
                }

                Expect(";");

                node.Cond = Current.Type != ";"
                    ? ParseExpression()
                    : new Node("NUM") { Value = 1.0 };

                Expect(";");

                if (Current.Type != ")")
                    node.Step = ParseExpression();

                Expect(")");

                node.Body = ParseStatement();

                return node;
            }

            // Return
            if (Current.Type == "ID" &&
                Current.Value == "return")
            {
                pos++;

                Node node = new Node("Return");

                if (Current.Type != ";")
                    node.Value = ParseExpression();

                Expect(";");

                return node;
            }

            Node expr = ParseExpression();

            Expect(";");

            Node statement = new Node("ExprStmt");
            statement.Left = expr;

            return statement;
        }

        private Node ParseExpression()
        {
            return ParseAssignment();
        }

        private Node ParseAssignment()
        {
            Node left = ParseLogicalOr();

            if (Match("=") != null)
            {
                Node node = new Node("Assign");
                node.Target = left;
                node.Right = ParseAssignment();

                return node;
            }

            return left;
        }

        private Node ParseLogicalOr()
        {
            Node left = ParseLogicalAnd();

            while (Match("||") != null)
                left = Binary("||", left, ParseLogicalAnd());

            return left;
        }

        private Node ParseLogicalAnd()
        {
            Node left = ParseEquality();

            while (Match("&&") != null)
                left = Binary("&&", left, ParseEquality());

            return left;
        }

        private Node ParseEquality()
        {
            Node left = ParseRelational();

            while (Current.Type == "==" ||
                   Current.Type == "!=")
            {
                string op = tokens[pos++].Type;
                left = Binary(op, left, ParseRelational());
            }

            return left;
        }

        private Node ParseRelational()
        {
            Node left = ParseAdditive();

            while (new string[] { "<", ">", "<=", ">=" }
                .Contains(Current.Type))
            {
                string op = tokens[pos++].Type;
                left = Binary(op, left, ParseAdditive());
            }

            return left;
        }

        private Node ParseAdditive()
        {
            Node left = ParseMultiplicative();

            while (Current.Type == "+" ||
                   Current.Type == "-")
            {
                string op = tokens[pos++].Type;
                left = Binary(op, left, ParseMultiplicative());
            }

            return left;
        }

        private Node ParseMultiplicative()
        {
            Node left = ParseUnary();

            while (Current.Type == "*" ||
                   Current.Type == "/" ||
                   Current.Type == "%")
            {
                string op = tokens[pos++].Type;
                left = Binary(op, left, ParseUnary());
            }

            return left;
        }

        private Node ParseUnary()
        {
            if (Match("!") != null)
            {
                Node node = new Node("Unary");
                node.Op = "!";
                node.Right = ParseUnary();
                return node;
            }

            if (Match("-") != null)
            {
                Node node = new Node("Unary");
                node.Op = "-";
                node.Right = ParseUnary();
                return node;
            }

            return ParsePostfix();
        }

        private Node ParsePostfix()
        {
            Node expr = ParsePrimary();

            if (Match("++") != null)
            {
                Node node = new Node("Postfix");
                node.Op = "++";
                node.Target = expr;
                return node;
            }

            if (Match("--") != null)
            {
                Node node = new Node("Postfix");
                node.Op = "--";
                node.Target = expr;
                return node;
            }

            return expr;
        }

        private Node ParsePrimary()
        {
            Token t = Current;

            if (Match("(") != null)
            {
                Node expr = ParseExpression();
                Expect(")");
                return expr;
            }

            if (t.Type == "NUM")
            {
                pos++;

                return new Node("NUM")
                {
                    Value = double.Parse(
                        t.Value,
                        CultureInfo.InvariantCulture)
                };
            }

            if (t.Type == "STR")
            {
                pos++;

                return new Node("STR")
                {
                    Value = t.Value
                };
            }

            if (t.Type == "ID")
            {
                pos++;

                return new Node("Variable")
                {
                    Name = t.Value
                };
            }

            throw new Exception(
                $"[Line {t.Line}] Unexpected token: '{t.Value}'");
        }

        private Node Binary(string op, Node left, Node right)
        {
            Node node = new Node("Binary");
            node.Op = op;
            node.Left = left;
            node.Right = right;

            return node;
        }
    }

    // ==========================================
    // ENVIRONMENT
    // ==========================================

    class Env
    {
        private Dictionary<string, object> vars =
            new Dictionary<string, object>();

        private Env parent;

        public Env(Env parent = null)
        {
            this.parent = parent;
        }

        public void Declare(string name, object value)
        {
            vars[name] = value;
        }

        public void Set(string name, object value)
        {
            if (vars.ContainsKey(name))
            {
                vars[name] = value;
                return;
            }

            if (parent != null)
            {
                parent.Set(name, value);
                return;
            }

            vars[name] = value;
        }

        public object Get(string name)
        {
            if (vars.ContainsKey(name))
                return vars[name];

            if (parent != null)
                return parent.Get(name);

            throw new Exception(
                $"Undefined variable '{name}'");
        }
    }

    // ==========================================
    // RETURN SIGNAL
    // ==========================================

    class ReturnSignal
    {
        public object Value;

        public ReturnSignal(object value)
        {
            Value = value;
        }
    }

    // ==========================================
    // INTERPRETER
    // ==========================================

    class Interpreter
    {
        private Env globalEnv = new Env();

        private int stepCounter = 0;
        private const int MAX_STEPS = 500000;

        private Dictionary<string, Node> functions =
            new Dictionary<string, Node>();

        public int ExitCode = 0;

        private object Number(double value)
        {
            return value;
        }

        private double ToNumber(object value)
        {
            if (value == null)
                return 0;

            if (value is bool)
                return (bool)value ? 1 : 0;

            return Convert.ToDouble(
                value,
                CultureInfo.InvariantCulture);
        }

        private bool IsTrue(object value)
        {
            if (value == null)
                return false;

            if (value is string)
                return ((string)value).Length > 0;

            return ToNumber(value) != 0;
        }

        private string ToText(object value)
        {
            if (value == null)
                return "";

            if (value is double)
                return ((double)value).ToString(
                    "G",
                    CultureInfo.InvariantCulture);

            return Convert.ToString(
                value,
                CultureInfo.InvariantCulture);
        }

        public object Execute(Node program)
        {
            // Register functions and global declarations
            foreach (Node node in program.Statements)
            {
                if (node.Type == "Function")
                {
                    functions[node.Name] = node;
                }
                else
                {
                    Eval(node, globalEnv);
                }
            }

            if (!functions.ContainsKey("main"))
                return 0.0;

            Node main = functions["main"];

            object result = CallFunction(main, new List<object>());

            if (result != null)
                ExitCode = (int)ToNumber(result);

            return result;
        }

        private object CallFunction(Node func, List<object> args)
        {
            Env env = new Env(globalEnv);

            for (int i = 0; i < func.Parameters.Count; i++)
            {
                object value = i < args.Count ? args[i] : 0.0;
                env.Declare(func.Parameters[i], value);
            }

            object result = Eval(func.Body, env);

            if (result is ReturnSignal)
                return ((ReturnSignal)result).Value;

            return 0.0;
        }

        private object Eval(Node node, Env env)
        {
            if (node == null)
                return null;

            stepCounter++;

            if (stepCounter > MAX_STEPS)
            {
                throw new Exception(
                    "Execution limit exceeded (possible infinite loop)");
            }

            switch (node.Type)
            {
                case "Program":
                    return Execute(node);

                case "Block":
                    {
                        Env blockEnv = new Env(env);

                        foreach (Node stmt in node.Statements)
                        {
                            object result = Eval(stmt, blockEnv);

                            if (result is ReturnSignal)
                                return result;
                        }

                        return null;
                    }

                case "VarDecl":
                    {
                        object value = node.Init != null
                            ? Eval(node.Init, env)
                            : Number(0);

                        env.Declare(node.Name, value);
                        return null;
                    }

                case "Assign":
                    {
                        if (node.Target.Type != "Variable")
                            throw new Exception("Invalid assignment target");

                        object value = Eval(node.Right, env);

                        env.Set(node.Target.Name, value);

                        return value;
                    }

                case "Postfix":
                    {
                        if (node.Target.Type != "Variable")
                            throw new Exception("Invalid increment/decrement target");

                        double oldValue = ToNumber(
                            env.Get(node.Target.Name));

                        double newValue = oldValue +
                            (node.Op == "++" ? 1 : -1);

                        env.Set(node.Target.Name, Number(newValue));

                        return oldValue;
                    }

                case "If":
                    {
                        if (IsTrue(Eval(node.Cond, env)))
                            return Eval(node.ThenBranch, env);

                        if (node.ElseBranch != null)
                            return Eval(node.ElseBranch, env);

                        return null;
                    }

                case "While":
                    {
                        while (IsTrue(Eval(node.Cond, env)))
                        {
                            object result = Eval(node.Body, env);

                            if (result is ReturnSignal)
                                return result;
                        }

                        return null;
                    }

                case "For":
                    {
                        Env forEnv = new Env(env);

                        if (node.Init != null)
                            Eval(node.Init, forEnv);

                        while (IsTrue(Eval(node.Cond, forEnv)))
                        {
                            object result = Eval(node.Body, forEnv);

                            if (result is ReturnSignal)
                                return result;

                            if (node.Step != null)
                                Eval(node.Step, forEnv);
                        }

                        return null;
                    }

                case "Return":
                    {
                        object value = node.Value != null
                            ? Eval(node.Value as Node, env)
                            : Number(0);

                        return new ReturnSignal(value);
                    }

                case "Printf":
                    Printf(node.Args, env);
                    return null;

                case "ExprStmt":
                    return Eval(node.Left, env);

                case "Binary":
                    {
                        object left = Eval(node.Left, env);

                        // Logical short-circuit evaluation
                        if (node.Op == "&&" && !IsTrue(left))
                            return Number(0);

                        if (node.Op == "||" && IsTrue(left))
                            return Number(1);

                        object right = Eval(node.Right, env);

                        double l = ToNumber(left);
                        double r = ToNumber(right);

                        switch (node.Op)
                        {
                            case "+":
                                if (left is string || right is string)
                                    return ToText(left) + ToText(right);
                                return Number(l + r);

                            case "-": return Number(l - r);
                            case "*": return Number(l * r);

                            case "/":
                                if (r == 0)
                                    throw new Exception("Division by zero");

                                return Number(Math.Floor(l / r));

                            case "%":
                                if (r == 0)
                                    throw new Exception("Modulo by zero");

                                return Number(l % r);

                            case "<": return Number(l < r ? 1 : 0);
                            case ">": return Number(l > r ? 1 : 0);
                            case "<=": return Number(l <= r ? 1 : 0);
                            case ">=": return Number(l >= r ? 1 : 0);

                            case "==":
                                return Number(
                                    Equals(left, right) || l == r ? 1 : 0);

                            case "!=":
                                return Number(
                                    !Equals(left, right) && l != r ? 1 : 0);

                            case "&&":
                                return Number(IsTrue(right) ? 1 : 0);

                            case "||":
                                return Number(IsTrue(right) ? 1 : 0);
                        }

                        return Number(0);
                    }

                case "Unary":
                    {
                        object value = Eval(node.Right, env);

                        if (node.Op == "!")
                            return Number(!IsTrue(value) ? 1 : 0);

                        return Number(-ToNumber(value));
                    }

                case "Variable":
                    return env.Get(node.Name);

                case "NUM":
                case "STR":
                    return node.Value;
            }

            throw new Exception("Unknown AST node: " + node.Type);
        }

        // ======================================
        // PRINTF
        // ======================================

        private void Printf(List<Node> args, Env env)
        {
            if (args.Count == 0)
                return;

            string format = ToText(Eval(args[0], env));

            int argIndex = 1;

            Regex regex = new Regex(
                @"%%|%[-+0-9.]*[dsfci]");

            int last = 0;

            foreach (Match match in regex.Matches(format))
            {
                Console.Write(format.Substring(
                    last,
                    match.Index - last));

                if (match.Value == "%%")
                {
                    Console.Write("%");
                    last = match.Index + match.Length;
                    continue;
                }

                if (argIndex >= args.Count)
                {
                    Console.Write(match.Value);
                    last = match.Index + match.Length;
                    continue;
                }

                object value = Eval(args[argIndex++], env);

                string spec = match.Value;
                char conversion = spec[spec.Length - 1];

                string formatted;

                switch (conversion)
                {
                    case 'd':
                    case 'i':
                        formatted = ((long)ToNumber(value)).ToString();
                        break;

                    case 'f':
                        formatted = ToNumber(value).ToString(
                            "F6",
                            CultureInfo.InvariantCulture);
                        break;

                    case 'c':
                        formatted = value is string
                            ? ToText(value)
                            : ((char)(int)ToNumber(value)).ToString();
                        break;

                    case 's':
                        formatted = ToText(value);
                        break;

                    default:
                        formatted = ToText(value);
                        break;
                }

                Console.Write(formatted);

                last = match.Index + match.Length;
            }

            Console.Write(format.Substring(last));
        }
    }

    // ==========================================
    // MAIN PROGRAM
    // ==========================================

    class Program
    {
        static void Main(string[] args)
        {
            Console.BackgroundColor = ConsoleColor.White;
            Console.ForegroundColor = ConsoleColor.Black;
            Console.Clear();
            Console.OutputEncoding = Encoding.UTF8;

            string fileName = args.Length > 0
                ? args[0]
                : "input.c";

            Console.WriteLine("==================================");
            Console.WriteLine("          C-MINI IDE");
            Console.WriteLine("       C Interpreter");
            Console.WriteLine("==================================");

            Console.WriteLine("Input file: " + fileName);
            Console.WriteLine();

            if (!File.Exists(fileName))
            {
                Console.ForegroundColor = ConsoleColor.Red;

                Console.WriteLine(
                    "Error: Input file not found: " + fileName);

                Console.ResetColor();
                return;
            }

            Stopwatch timer = Stopwatch.StartNew();

            try
            {
                // Read source code
                string source = File.ReadAllText(fileName);

                Console.WriteLine("[Starting execution...]");
                Console.WriteLine();

                // Lexical analysis
                Lexer lexer = new Lexer(source);
                List<Token> tokens = lexer.Tokenize();

                // Syntax analysis
                Parser parser = new Parser(tokens);
                Node ast = parser.Parse();

                // Interpretation
                Interpreter interpreter = new Interpreter();

                interpreter.Execute(ast);

                timer.Stop();

                Console.WriteLine();
                Console.ForegroundColor = ConsoleColor.Cyan;

                Console.WriteLine(
                    $"[Finished with exit code {interpreter.ExitCode} in {timer.Elapsed.TotalMilliseconds:F1}ms]");

                Console.ResetColor();
            }
            catch (Exception ex)
            {
                timer.Stop();

                Console.WriteLine();

                Console.ForegroundColor = ConsoleColor.Red;

                Console.WriteLine(
                    "Runtime / Compilation Error: " + ex.Message);

                Console.ResetColor();
            }
        }
    }
}
