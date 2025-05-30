using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Drawing;
using System.Linq;
using System.Text;

namespace MiniCompilerPro
{
    // Enhanced Token class with position information
    public class Token
    {
        public string Type { get; set; }
        public string Value { get; set; }
        public int Line { get; set; }
        public int Column { get; set; }

        public Token(string type, string value, int line = 1, int column = 1)
        {
            Type = type;
            Value = value;
            Line = line;
            Column = column;
        }

        public override string ToString() => $"[{Line}:{Column}] {Type}: '{Value}'";
    }

    // Enhanced Lexer with better tokenization and error handling
    public class Lexer
    {
        private string _input;
        private int _position;
        private int _line = 1;
        private int _column = 1;

        private static readonly Dictionary<string, string> TokenPatterns = new()
        {
            {"IF", @"\bif\b"},
            {"ELSE", @"\belse\b"},
            {"WHILE", @"\bwhile\b"},
            {"FOR", @"\bfor\b"},
            {"RETURN", @"\breturn\b"},
            {"INT", @"\bint\b"},
            {"FLOAT", @"\bfloat\b"},
            {"STRING", @"\bstring\b"},
            {"BOOL", @"\bbool\b"},
            {"TRUE", @"\btrue\b"},
            {"FALSE", @"\bfalse\b"},
            {"FUNCTION", @"\bfunction\b"},
            {"IDENTIFIER", @"[a-zA-Z_][a-zA-Z0-9_]*"},
            {"FLOAT_NUMBER", @"\d+\.\d+"},
            {"NUMBER", @"\d+"},
            {"STRING_LITERAL", @"""[^""]*"""},
            {"EQ", @"=="},
            {"NE", @"!="},
            {"LE", @"<="},
            {"GE", @">="},
            {"LT", @"<"},
            {"GT", @">"},
            {"AND", @"&&"},
            {"OR", @"\|\|"},
            {"ASSIGN", @"="},
            {"PLUS", @"\+"},
            {"MINUS", @"-"},
            {"MULT", @"\*"},
            {"DIV", @"/"},
            {"MOD", @"%"},
            {"LPAREN", @"\("},
            {"RPAREN", @"\)"},
            {"LBRACE", @"\{"},
            {"RBRACE", @"\}"},
            {"SEMICOLON", @";"},
            {"COMMA", @","},
            {"NEWLINE", @"\r?\n"},
            {"WHITESPACE", @"[ \t]+"},
            {"COMMENT", @"//.*"}
        };

        public Lexer(string input)
        {
            _input = input;
            _position = 0;
            _line = 1;
            _column = 1;
        }

        public List<Token> Tokenize()
        {
            List<Token> tokens = new();
            
            while (_position < _input.Length)
            {
                bool matched = false;
                
                foreach (var pattern in TokenPatterns)
                {
                    Regex regex = new Regex("^" + pattern.Value);
                    Match match = regex.Match(_input.Substring(_position));
                    
                    if (match.Success)
                    {
                        string tokenValue = match.Value;
                        
                        if (pattern.Key == "NEWLINE")
                        {
                            _line++;
                            _column = 1;
                        }
                        else if (pattern.Key != "WHITESPACE" && pattern.Key != "COMMENT")
                        {
                            tokens.Add(new Token(pattern.Key, tokenValue, _line, _column));
                        }
                        
                        _position += match.Length;
                        if (pattern.Key != "NEWLINE")
                            _column += match.Length;
                        
                        matched = true;
                        break;
                    }
                }
                
                if (!matched)
                {
                    throw new CompilerException($"Unknown token '{_input[_position]}' at line {_line}, column {_column}");
                }
            }
            
            return tokens;
        }
    }

    // Enhanced AST Node with more node types
    public abstract class ASTNode
    {
        public abstract string Accept(IASTVisitor visitor);
    }

    public class ProgramNode : ASTNode
    {
        public List<ASTNode> Statements { get; set; } = new();
        public override string Accept(IASTVisitor visitor) => visitor.VisitProgram(this);
    }

    public class AssignmentNode : ASTNode
    {
        public string Identifier { get; set; }
        public ASTNode Expression { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitAssignment(this);
    }

    public class BinaryOpNode : ASTNode
    {
        public ASTNode Left { get; set; }
        public string Operator { get; set; }
        public ASTNode Right { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitBinaryOp(this);
    }

    public class NumberNode : ASTNode
    {
        public string Value { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitNumber(this);
    }

    public class IdentifierNode : ASTNode
    {
        public string Name { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitIdentifier(this);
    }

    public class IfNode : ASTNode
    {
        public ASTNode Condition { get; set; }
        public ASTNode ThenBranch { get; set; }
        public ASTNode ElseBranch { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitIf(this);
    }

    public class WhileNode : ASTNode
    {
        public ASTNode Condition { get; set; }
        public ASTNode Body { get; set; }
        public override string Accept(IASTVisitor visitor) => visitor.VisitWhile(this);
    }

    public class BlockNode : ASTNode
    {
        public List<ASTNode> Statements { get; set; } = new();
        public override string Accept(IASTVisitor visitor) => visitor.VisitBlock(this);
    }

    // Visitor pattern for AST traversal
    public interface IASTVisitor
    {
        string VisitProgram(ProgramNode node);
        string VisitAssignment(AssignmentNode node);
        string VisitBinaryOp(BinaryOpNode node);
        string VisitNumber(NumberNode node);
        string VisitIdentifier(IdentifierNode node);
        string VisitIf(IfNode node);
        string VisitWhile(WhileNode node);
        string VisitBlock(BlockNode node);
    }

    // Enhanced Parser with recursive descent parsing
    public class Parser
    {
        private List<Token> _tokens;
        private int _position;
        private Token CurrentToken => _position < _tokens.Count ? _tokens[_position] : null;

        public Parser(List<Token> tokens)
        {
            _tokens = tokens;
            _position = 0;
        }

        public ProgramNode Parse()
        {
            ProgramNode program = new ProgramNode();
            
            while (CurrentToken != null)
            {
                program.Statements.Add(ParseStatement());
            }
            
            return program;
        }

        private ASTNode ParseStatement()
        {
            if (CurrentToken?.Type == "IF")
                return ParseIfStatement();
            else if (CurrentToken?.Type == "WHILE")
                return ParseWhileStatement();
            else if (CurrentToken?.Type == "LBRACE")
                return ParseBlock();
            else
                return ParseAssignment();
        }

        private ASTNode ParseIfStatement()
        {
            Consume("IF");
            Consume("LPAREN");
            var condition = ParseExpression();
            Consume("RPAREN");
            var thenBranch = ParseStatement();
            
            ASTNode elseBranch = null;
            if (CurrentToken?.Type == "ELSE")
            {
                Consume("ELSE");
                elseBranch = ParseStatement();
            }
            
            return new IfNode { Condition = condition, ThenBranch = thenBranch, ElseBranch = elseBranch };
        }

        private ASTNode ParseWhileStatement()
        {
            Consume("WHILE");
            Consume("LPAREN");
            var condition = ParseExpression();
            Consume("RPAREN");
            var body = ParseStatement();
            
            return new WhileNode { Condition = condition, Body = body };
        }

        private ASTNode ParseBlock()
        {
            Consume("LBRACE");
            BlockNode block = new BlockNode();
            
            while (CurrentToken?.Type != "RBRACE" && CurrentToken != null)
            {
                block.Statements.Add(ParseStatement());
            }
            
            Consume("RBRACE");
            return block;
        }

        private ASTNode ParseAssignment()
        {
            var identifier = Consume("IDENTIFIER").Value;
            Consume("ASSIGN");
            var expression = ParseExpression();
            Consume("SEMICOLON");
            
            return new AssignmentNode { Identifier = identifier, Expression = expression };
        }

        private ASTNode ParseExpression()
        {
            return ParseComparison();
        }

        private ASTNode ParseComparison()
        {
            var expr = ParseTerm();
            
            while (CurrentToken?.Type == "EQ" || CurrentToken?.Type == "NE" || 
                   CurrentToken?.Type == "LT" || CurrentToken?.Type == "GT" ||
                   CurrentToken?.Type == "LE" || CurrentToken?.Type == "GE")
            {
                var op = Consume(CurrentToken.Type).Value;
                var right = ParseTerm();
                expr = new BinaryOpNode { Left = expr, Operator = op, Right = right };
            }
            
            return expr;
        }

        private ASTNode ParseTerm()
        {
            var expr = ParseFactor();
            
            while (CurrentToken?.Type == "PLUS" || CurrentToken?.Type == "MINUS")
            {
                var op = Consume(CurrentToken.Type).Value;
                var right = ParseFactor();
                expr = new BinaryOpNode { Left = expr, Operator = op, Right = right };
            }
            
            return expr;
        }

        private ASTNode ParseFactor()
        {
            var expr = ParsePrimary();
            
            while (CurrentToken?.Type == "MULT" || CurrentToken?.Type == "DIV")
            {
                var op = Consume(CurrentToken.Type).Value;
                var right = ParsePrimary();
                expr = new BinaryOpNode { Left = expr, Operator = op, Right = right };
            }
            
            return expr;
        }

        private ASTNode ParsePrimary()
        {
            if (CurrentToken?.Type == "NUMBER" || CurrentToken?.Type == "FLOAT_NUMBER")
            {
                return new NumberNode { Value = Consume(CurrentToken.Type).Value };
            }
            else if (CurrentToken?.Type == "IDENTIFIER")
            {
                return new IdentifierNode { Name = Consume("IDENTIFIER").Value };
            }
            else if (CurrentToken?.Type == "LPAREN")
            {
                Consume("LPAREN");
                var expr = ParseExpression();
                Consume("RPAREN");
                return expr;
            }
            
            throw new CompilerException($"Unexpected token: {CurrentToken?.Type} at line {CurrentToken?.Line}");
        }

        private Token Consume(string expectedType)
        {
            if (CurrentToken?.Type == expectedType)
            {
                var token = CurrentToken;
                _position++;
                return token;
            }
            
            throw new CompilerException($"Expected {expectedType} but found {CurrentToken?.Type} at line {CurrentToken?.Line}");
        }
    }

    // Enhanced Symbol Table with scope management
    public class SymbolTable
    {
        private Dictionary<string, SymbolInfo> _symbols = new();
        private Dictionary<string, string> _types = new();

        public class SymbolInfo
        {
            public string Name { get; set; }
            public string Type { get; set; }
            public int Line { get; set; }
            public bool IsInitialized { get; set; }
        }

        public void Declare(string name, string type, int line)
        {
            if (_symbols.ContainsKey(name))
                throw new CompilerException($"Variable '{name}' already declared at line {line}");
            
            _symbols[name] = new SymbolInfo { Name = name, Type = type, Line = line, IsInitialized = false };
        }

        public void Initialize(string name, int line)
        {
            if (!_symbols.ContainsKey(name))
                throw new CompilerException($"Undeclared variable '{name}' at line {line}");
            
            _symbols[name].IsInitialized = true;
        }

        public bool IsDeclared(string name) => _symbols.ContainsKey(name);
        public bool IsInitialized(string name) => _symbols.ContainsKey(name) && _symbols[name].IsInitialized;
        public string GetType(string name) => _symbols.ContainsKey(name) ? _symbols[name].Type : null;
        public Dictionary<string, SymbolInfo> GetAllSymbols() => _symbols;
    }

    // Semantic Analyzer with comprehensive checks
    public class SemanticAnalyzer : IASTVisitor
    {
        private SymbolTable _symbolTable = new();
        private List<string> _errors = new();

        public List<string> Analyze(ProgramNode program)
        {
            _errors.Clear();
            program.Accept(this);
            return _errors;
        }

        public string VisitProgram(ProgramNode node)
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
            return "";
        }

        public string VisitAssignment(AssignmentNode node)
        {
            try
            {
                if (!_symbolTable.IsDeclared(node.Identifier))
                    _symbolTable.Declare(node.Identifier, "int", 1); // Default to int for simplicity
                
                _symbolTable.Initialize(node.Identifier, 1);
                node.Expression.Accept(this);
            }
            catch (CompilerException ex)
            {
                _errors.Add(ex.Message);
            }
            return "";
        }

        public string VisitBinaryOp(BinaryOpNode node)
        {
            node.Left.Accept(this);
            node.Right.Accept(this);
            return "";
        }

        public string VisitNumber(NumberNode node) => "";

        public string VisitIdentifier(IdentifierNode node)
        {
            if (!_symbolTable.IsDeclared(node.Name))
                _errors.Add($"Undeclared variable '{node.Name}'");
            return "";
        }

        public string VisitIf(IfNode node)
        {
            node.Condition.Accept(this);
            node.ThenBranch.Accept(this);
            node.ElseBranch?.Accept(this);
            return "";
        }

        public string VisitWhile(WhileNode node)
        {
            node.Condition.Accept(this);
            node.Body.Accept(this);
            return "";
        }

        public string VisitBlock(BlockNode node)
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
            return "";
        }

        public SymbolTable GetSymbolTable() => _symbolTable;
    }

    // Three-Address Code Generator
    public class ThreeAddressCode
    {
        public string Operator { get; set; }
        public string Operand1 { get; set; }
        public string Operand2 { get; set; }
        public string Result { get; set; }

        public override string ToString()
        {
            if (Operator == "=")
                return $"{Result} = {Operand1}";
            return $"{Result} = {Operand1} {Operator} {Operand2}";
        }
    }

    // IR Generator with three-address code
    public class IRGenerator : IASTVisitor
    {
        private List<ThreeAddressCode> _instructions = new();
        private int _tempCounter = 1;

        public List<ThreeAddressCode> Generate(ProgramNode program)
        {
            _instructions.Clear();
            _tempCounter = 1;
            program.Accept(this);
            return _instructions;
        }

        private string NewTemp() => $"t{_tempCounter++}";

        public string VisitProgram(ProgramNode node)
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
            return "";
        }

        public string VisitAssignment(AssignmentNode node)
        {
            var exprResult = node.Expression.Accept(this);
            _instructions.Add(new ThreeAddressCode
            {
                Operator = "=",
                Operand1 = exprResult,
                Result = node.Identifier
            });
            return "";
        }

        public string VisitBinaryOp(BinaryOpNode node)
        {
            var leftResult = node.Left.Accept(this);
            var rightResult = node.Right.Accept(this);
            var temp = NewTemp();
            
            _instructions.Add(new ThreeAddressCode
            {
                Operator = node.Operator,
                Operand1 = leftResult,
                Operand2 = rightResult,
                Result = temp
            });
            
            return temp;
        }

        public string VisitNumber(NumberNode node) => node.Value;
        public string VisitIdentifier(IdentifierNode node) => node.Name;

        public string VisitIf(IfNode node)
        {
            var condResult = node.Condition.Accept(this);
            var elseLabel = $"L{_tempCounter++}";
            var endLabel = $"L{_tempCounter++}";
            
            _instructions.Add(new ThreeAddressCode { Operator = "ifFalse", Operand1 = condResult, Result = elseLabel });
            node.ThenBranch.Accept(this);
            _instructions.Add(new ThreeAddressCode { Operator = "goto", Result = endLabel });
            _instructions.Add(new ThreeAddressCode { Operator = "label", Result = elseLabel });
            node.ElseBranch?.Accept(this);
            _instructions.Add(new ThreeAddressCode { Operator = "label", Result = endLabel });
            
            return "";
        }

        public string VisitWhile(WhileNode node)
        {
            var startLabel = $"L{_tempCounter++}";
            var endLabel = $"L{_tempCounter++}";
            
            _instructions.Add(new ThreeAddressCode { Operator = "label", Result = startLabel });
            var condResult = node.Condition.Accept(this);
            _instructions.Add(new ThreeAddressCode { Operator = "ifFalse", Operand1 = condResult, Result = endLabel });
            node.Body.Accept(this);
            _instructions.Add(new ThreeAddressCode { Operator = "goto", Result = startLabel });
            _instructions.Add(new ThreeAddressCode { Operator = "label", Result = endLabel });
            
            return "";
        }

        public string VisitBlock(BlockNode node)
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
            return "";
        }
    }

    // Basic Optimizer
    public class Optimizer
    {
        public List<ThreeAddressCode> Optimize(List<ThreeAddressCode> instructions)
        {
            var optimized = new List<ThreeAddressCode>(instructions);
            
            // Constant folding
            optimized = ConstantFolding(optimized);
            
            // Dead code elimination
            optimized = DeadCodeElimination(optimized);
            
            return optimized;
        }

        private List<ThreeAddressCode> ConstantFolding(List<ThreeAddressCode> instructions)
        {
            var result = new List<ThreeAddressCode>();
            
            foreach (var instr in instructions)
            {
                if (IsArithmeticOp(instr) && IsConstant(instr.Operand1) && IsConstant(instr.Operand2))
                {
                    var value = EvaluateConstantExpression(instr);
                    result.Add(new ThreeAddressCode
                    {
                        Operator = "=",
                        Operand1 = value.ToString(),
                        Result = instr.Result
                    });
                }
                else
                {
                    result.Add(instr);
                }
            }
            
            return result;
        }

        private List<ThreeAddressCode> DeadCodeElimination(List<ThreeAddressCode> instructions)
        {
            // Simple dead code elimination - remove unused temporaries
            var usedVars = new HashSet<string>();
            var result = new List<ThreeAddressCode>();
            
            // First pass: collect all used variables
            foreach (var instr in instructions)
            {
                if (instr.Operand1 != null) usedVars.Add(instr.Operand1);
                if (instr.Operand2 != null) usedVars.Add(instr.Operand2);
            }
            
            // Second pass: keep instructions that define used variables
            foreach (var instr in instructions)
            {
                if (instr.Result == null || usedVars.Contains(instr.Result) || !instr.Result.StartsWith("t"))
                {
                    result.Add(instr);
                }
            }
            
            return result;
        }

        private bool IsArithmeticOp(ThreeAddressCode instr) =>
            instr.Operator == "+" || instr.Operator == "-" || instr.Operator == "*" || instr.Operator == "/";

        private bool IsConstant(string operand) => int.TryParse(operand, out _);

        private int EvaluateConstantExpression(ThreeAddressCode instr)
        {
            int op1 = int.Parse(instr.Operand1);
            int op2 = int.Parse(instr.Operand2);
            
            return instr.Operator switch
            {
                "+" => op1 + op2,
                "-" => op1 - op2,
                "*" => op1 * op2,
                "/" => op2 != 0 ? op1 / op2 : 0,
                _ => 0
            };
        }
    }

    // Target Code Generator
    public class TargetCodeGenerator
    {
        public List<string> GenerateAssembly(List<ThreeAddressCode> instructions)
        {
            var assembly = new List<string>();
            assembly.Add("; Generated Assembly Code");
            assembly.Add(".data");
            assembly.Add(".text");
            assembly.Add("main:");
            
            foreach (var instr in instructions)
            {
                switch (instr.Operator)
                {
                    case "=":
                        assembly.Add($"    MOV {instr.Result}, {instr.Operand1}");
                        break;
                    case "+":
                        assembly.Add($"    ADD {instr.Result}, {instr.Operand1}, {instr.Operand2}");
                        break;
                    case "-":
                        assembly.Add($"    SUB {instr.Result}, {instr.Operand1}, {instr.Operand2}");
                        break;
                    case "*":
                        assembly.Add($"    MUL {instr.Result}, {instr.Operand1}, {instr.Operand2}");
                        break;
                    case "/":
                        assembly.Add($"    DIV {instr.Result}, {instr.Operand1}, {instr.Operand2}");
                        break;
                    case "ifFalse":
                        assembly.Add($"    CMP {instr.Operand1}, 0");
                        assembly.Add($"    JE {instr.Result}");
                        break;
                    case "goto":
                        assembly.Add($"    JMP {instr.Result}");
                        break;
                    case "label":
                        assembly.Add($"{instr.Result}:");
                        break;
                }
            }
            
            assembly.Add("    HALT");
            return assembly;
        }
    }

    // Custom Exception class
    public class CompilerException : Exception
    {
        public CompilerException(string message) : base(message) { }
    }

    // Enhanced Professional UI
    public partial class CompilerForm : Form
    {
        private TabControl mainTabControl;
        private TextBox sourceCodeTextBox;
        private RichTextBox tokensTextBox;
        private RichTextBox astTextBox;
        private RichTextBox semanticTextBox;
        private RichTextBox irTextBox;
        private RichTextBox optimizedTextBox;
        private RichTextBox assemblyTextBox;
        private RichTextBox errorsTextBox;
        private RichTextBox symbolTableTextBox;
        private Button compileButton;
        private Button clearButton;
        private Button loadSampleButton;
        private StatusStrip statusStrip;
        private ToolStripStatusLabel statusLabel;
        private MenuStrip menuStrip;

        public CompilerForm()
        {
            InitializeComponent();
            this.Text = "Professional Mini Compiler";
            this.Size = new Size(1200, 800);
            this.StartPosition = FormStartPosition.CenterScreen;
            this.Icon = SystemIcons.Application;
        }

        private void InitializeComponent()
        {
            // Menu Strip
            menuStrip = new MenuStrip();
            var fileMenu = new ToolStripMenuItem("File");
            fileMenu.DropDownItems.Add("New", null, (s, e) => ClearAll());
            fileMenu.DropDownItems.Add("Load Sample", null, LoadSample_Click);
            fileMenu.DropDownItems.Add(new ToolStripSeparator());
            fileMenu.DropDownItems.Add("Exit", null, (s, e) => Close());
            
            var helpMenu = new ToolStripMenuItem("Help");
            helpMenu.DropDownItems.Add("About", null, (s, e) => 
                MessageBox.Show("Professional Mini Compiler v2.0\nDeveloped for Compiler Design Course", "About"));
            
            menuStrip.Items.Add(fileMenu);
            menuStrip.Items.Add(helpMenu);

            // Main container
            var mainPanel = new Panel { Dock = DockStyle.Fill, Padding = new Padding(10) };
            
            // Top panel for source code and buttons
            var topPanel = new Panel { Height = 250, Dock = DockStyle.Top };
            
            // Source code panel
            var sourcePanel = new GroupBox 
            { 
                Text = "Source Code", 
                Dock = DockStyle.Fill, 
                Font = new Font("Segoe UI", 9F, FontStyle.Bold),
                ForeColor = Color.DarkBlue
            };
            
            sourceCodeTextBox = new TextBox
            {
                Multiline = true,
                Dock = DockStyle.Fill,
                Font = new Font("Consolas", 10F),
                ScrollBars = ScrollBars.Both,
                BackColor = Color.FromArgb(240, 248, 255),
                BorderStyle = BorderStyle.FixedSingle
            };
            sourcePanel.Controls.Add(sourceCodeTextBox);
            
            // Button panel
            var buttonPanel = new Panel { Width = 120, Dock = DockStyle.Right };
            
            compileButton = new Button
            {
                Text = "Compile",
                Size = new Size(100, 35),
                Location = new Point(10, 20),
                BackColor = Color.FromArgb(0, 120, 215),
                ForeColor = Color.White,
                FlatStyle = FlatStyle.Flat,
                Font = new Font("Segoe UI", 9F, FontStyle.Bold)
            };
            compileButton.FlatAppearance.BorderSize = 0;
            compileButton.Click += CompileButton_Click;
            
            clearButton = new Button
            {
                Text = "Clear",
                Size = new Size(100, 30),
                Location = new Point(10, 65),
                BackColor = Color.FromArgb(255, 69, 58),
                ForeColor = Color.White,
                FlatStyle = FlatStyle.Flat,
                Font = new Font("Segoe UI", 9F)
            };
            clearButton.FlatAppearance.BorderSize = 0;
            clearButton.Click += (s, e) => ClearAll();
            
            loadSampleButton = new Button
            {
                Text = "Load Sample",
                Size = new Size(100, 30),
                Location = new Point(10, 105),
                BackColor = Color.FromArgb(52, 199, 89),
                ForeColor = Color.White,
                FlatStyle = FlatStyle.Flat,
                Font = new Font("Segoe UI", 9F)
            };
            loadSampleButton.FlatAppearance.BorderSize = 0;
            loadSampleButton.Click += LoadSample_Click;
            
            buttonPanel.Controls.AddRange(new Control[] { compileButton, clearButton, loadSampleButton });
            
            topPanel.Controls.Add(sourcePanel);
            topPanel.Controls.Add(buttonPanel);

            // Tab control for results
            mainTabControl = new TabControl 
            { 
                Dock = DockStyle.Fill,
                Font = new Font("Segoe UI", 9F),
                Appearance = TabAppearance.FlatButtons
            };

            // Create tabs
            CreateResultTabs();

            // Status strip
            statusStrip = new StatusStrip();
            statusLabel = new ToolStripStatusLabel("Ready") { Spring = true, TextAlign = ContentAlignment.MiddleLeft };
            statusStrip.Items.Add(statusLabel);

            // Add all controls
            mainPanel.Controls.Add(mainTabControl);
            mainPanel.Controls.Add(topPanel);
            
            this.Controls.Add(mainPanel);
            this.Controls.Add(statusStrip);
            this.Controls.Add(menuStrip);
            this.MainMenuStrip = menuStrip;
        }

        private void CreateResultTabs()
        {
            // Tokens tab
            var tokensTab = new TabPage("🔤 Tokens");
            tokensTextBox = CreateRichTextBox();
            tokensTab.Controls.Add(tokensTextBox);

            // AST tab
            var astTab = new TabPage("🌳 AST");
            astTextBox = CreateRichTextBox();
            astTab.Controls.Add(astTextBox);

            // Semantic tab
            var semanticTab = new TabPage("🧠 Semantic");
            semanticTextBox = CreateRichTextBox();
            semanticTab.Controls.Add(semanticTextBox);

            // IR tab
            var irTab = new TabPage("💾 IR");
            irTextBox = CreateRichTextBox();
            irTab.Controls.Add(irTextBox);

            // Optimized IR tab
            var optimizedTab = new TabPage("⚡ Optimized IR");
            optimizedTextBox = CreateRichTextBox();
            optimizedTab.Controls.Add(optimizedTextBox);

            // Assembly tab
            var assemblyTab = new TabPage("🛠️ Assembly");
            assemblyTextBox = CreateRichTextBox();
            assemblyTab.Controls.Add(assemblyTextBox);

            // Errors tab
            var errorsTab = new TabPage("❌ Errors");
            errorsTextBox = CreateRichTextBox();
            errorsTextBox.ForeColor = Color.DarkRed;
            errorsTab.Controls.Add(errorsTextBox);

            // Symbol Table tab
            var symbolTableTab = new TabPage("📋 Symbol Table");
            symbolTableTextBox = CreateRichTextBox();
            symbolTableTab.Controls.Add(symbolTableTextBox);

            // Add all tabs to the main tab control
            mainTabControl.TabPages.AddRange(new TabPage[]
            {
                tokensTab,
                astTab,
                semanticTab,
                irTab,
                optimizedTab,
                assemblyTab,
                errorsTab,
                symbolTableTab
            });
        }

        private RichTextBox CreateRichTextBox()
        {
            return new RichTextBox
            {
                Dock = DockStyle.Fill,
                Font = new Font("Consolas", 10F),
                ReadOnly = true,
                BackColor = Color.WhiteSmoke,
                BorderStyle = BorderStyle.FixedSingle,
                WordWrap = false
            };
        }

        private void CompileButton_Click(object sender, EventArgs e)
        {
            try
            {
                statusLabel.Text = "Compiling...";
                Application.DoEvents();

                string source = sourceCodeTextBox.Text;
                tokensTextBox.Clear();
                astTextBox.Clear();
                semanticTextBox.Clear();
                irTextBox.Clear();
                optimizedTextBox.Clear();
                assemblyTextBox.Clear();
                errorsTextBox.Clear();
                symbolTableTextBox.Clear();

                // Lexical Analysis
                var lexer = new Lexer(source);
                var tokens = lexer.Tokenize();
                tokensTextBox.Text = string.Join(Environment.NewLine, tokens);

                // Parsing
                var parser = new Parser(tokens);
                var programNode = parser.Parse();
                astTextBox.Text = ASTToString(programNode);

                // Semantic Analysis
                var semanticAnalyzer = new SemanticAnalyzer();
                var errors = semanticAnalyzer.Analyze(programNode);
                if (errors.Count > 0)
                {
                    errorsTextBox.Text = string.Join(Environment.NewLine, errors);
                    statusLabel.Text = "Semantic errors found.";
                }
                else
                {
                    errorsTextBox.Text = "No semantic errors.";
                    statusLabel.Text = "Compiled successfully.";
                }

                // Symbol Table
                var symbolTable = semanticAnalyzer.GetSymbolTable();
                symbolTableTextBox.Text = SymbolTableToString(symbolTable);

                // IR Generation
                var irGenerator = new IRGenerator();
                var ir = irGenerator.Generate(programNode);
                irTextBox.Text = string.Join(Environment.NewLine, ir);

                // Optimization
                var optimizer = new Optimizer();
                var optimizedIR = optimizer.Optimize(ir);
                optimizedTextBox.Text = string.Join(Environment.NewLine, optimizedIR);

                // Assembly Generation
                var targetCodeGen = new TargetCodeGenerator();
                var assembly = targetCodeGen.GenerateAssembly(optimizedIR);
                assemblyTextBox.Text = string.Join(Environment.NewLine, assembly);
            }
            catch (CompilerException ex)
            {
                errorsTextBox.Text = ex.Message;
                statusLabel.Text = "Compilation failed.";
            }
            catch (Exception ex)
            {
                errorsTextBox.Text = "Internal error: " + ex.Message;
                statusLabel.Text = "Compilation failed.";
            }
        }

        private string ASTToString(ASTNode node, int indent = 0)
        {
            if (node == null) return "";
            string pad = new string(' ', indent * 2);
            switch (node)
            {
                case ProgramNode prog:
                    return pad + "Program\n" + string.Join("\n", prog.Statements.Select(s => ASTToString(s, indent + 1)));
                case AssignmentNode assign:
                    return $"{pad}Assignment: {assign.Identifier}\n{ASTToString(assign.Expression, indent + 1)}";
                case BinaryOpNode bin:
                    return $"{pad}BinaryOp: {bin.Operator}\n{ASTToString(bin.Left, indent + 1)}\n{ASTToString(bin.Right, indent + 1)}";
                case NumberNode num:
                    return $"{pad}Number: {num.Value}";
                case IdentifierNode id:
                    return $"{pad}Identifier: {id.Name}";
                case IfNode ifn:
                    return $"{pad}If\n{pad}  Condition:\n{ASTToString(ifn.Condition, indent + 2)}\n{pad}  Then:\n{ASTToString(ifn.ThenBranch, indent + 2)}" +
                           (ifn.ElseBranch != null ? $"\n{pad}  Else:\n{ASTToString(ifn.ElseBranch, indent + 2)}" : "");
                case WhileNode wn:
                    return $"{pad}While\n{pad}  Condition:\n{ASTToString(wn.Condition, indent + 2)}\n{pad}  Body:\n{ASTToString(wn.Body, indent + 2)}";
                case BlockNode block:
                    return $"{pad}Block\n" + string.Join("\n", block.Statements.Select(s => ASTToString(s, indent + 1)));
                default:
                    return pad + node.GetType().Name;
            }
        }

        private string SymbolTableToString(SymbolTable table)
        {
            var sb = new StringBuilder();
            sb.AppendLine("Name\tType\tInitialized\tLine");
            foreach (var sym in table.GetAllSymbols().Values)
            {
                sb.AppendLine($"{sym.Name}\t{sym.Type}\t{sym.IsInitialized}\t{sym.Line}");
            }
            return sb.ToString();
        }

        private void ClearAll()
        {
            sourceCodeTextBox.Clear();
            tokensTextBox.Clear();
            astTextBox.Clear();
            semanticTextBox.Clear();
            irTextBox.Clear();
            optimizedTextBox.Clear();
            assemblyTextBox.Clear();
            errorsTextBox.Clear();
            symbolTableTextBox.Clear();
            statusLabel.Text = "Ready";
        }

        private void LoadSample_Click(object sender, EventArgs e)
        {
            sourceCodeTextBox.Text =
@"x = 5;
y = 10;
if (x < y) {
    z = x + y;
} else {
    z = x - y;
}
while (z > 0) {
    z = z - 1;
}";
        }
    }

    // Entry point
    static class Program
    {
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new CompilerForm());
        }
    }
}
