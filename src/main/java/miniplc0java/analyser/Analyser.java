package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.CompileError;
import miniplc0java.error.ErrorCode;
import miniplc0java.error.ExpectedTokenError;
import miniplc0java.error.TokenizeError;
import miniplc0java.instruction.Instruction;
import miniplc0java.instruction.Operation;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;
import miniplc0java.tokenizer.Tokenizer;
import miniplc0java.util.Pos;
import org.checkerframework.checker.units.qual.C;

import java.util.*;

public final class Analyser {

    Tokenizer tokenizer;
    ArrayList<Instruction> instructions;

    /**
     * 当前偷看的 token
     */
    Token peekedToken = null;

    /**
     * 符号表
     */
    HashMap<String, SymbolEntry> symbolTable = new HashMap<>();

    /**
     * 优先矩阵表
     */
    HashMap<String, Integer> OPPrec = new HashMap<String, Integer>();

    HashMap<String, Operation> String2OP = new HashMap<>();

    /**
     * 下一个变量的栈偏移
     */
    int nextOffset = 0;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();

        this.OPPrec.put("*", 3);
        this.OPPrec.put("/", 3);
        this.OPPrec.put("+", 2);
        this.OPPrec.put("-", 2);
        this.OPPrec.put(">", 1);
        this.OPPrec.put("<", 1);
        this.OPPrec.put(">=", 1);
        this.OPPrec.put("<=", 1);
        this.OPPrec.put("==", 1);
        this.OPPrec.put("!=", 1);

        this.String2OP.put("*", Operation.MUL);
        this.String2OP.put("/", Operation.DIV);
        this.String2OP.put("+", Operation.ADD);
        this.String2OP.put("-", Operation.SUB);
        this.String2OP.put(">", Operation.GT);
        this.String2OP.put("<", Operation.LT);
        this.String2OP.put(">=", Operation.LE);
        this.String2OP.put("<=", Operation.GE);
        this.String2OP.put("==", Operation.EQ);
        this.String2OP.put("!=", Operation.NEQ);
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     *
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        var token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     *
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     *
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 获取下一个变量的栈偏移
     *
     * @return
     */
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    /**
     * 添加一个符号
     *
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private void addSymbol(String name, boolean isInitialized, boolean isConstant, Pos curPos, Token typeToken) throws AnalyzeError {
        if (this.symbolTable.get(name) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            this.symbolTable.put(name, new SymbolEntry(isConstant, isInitialized, getNextVariableOffset(), typeToken.getTokenType()));
        }
    }

    /**
     * 设置符号为已赋值
     *
     * @param name   符号名称
     * @param curPos 当前位置（报错用）
     * @throws AnalyzeError 如果未定义则抛异常
     */
    private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            entry.setInitialized(true);
        }
    }

    /**
     * 获取变量在栈上的偏移
     *
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 栈偏移
     * @throws AnalyzeError
     */
    private int getOffset(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.getStackOffset();
        }
    }

    /**
     * 获取变量是否是常量
     *
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 是否为常量
     * @throws AnalyzeError
     */
    private boolean isConstant(String name, Pos curPos) throws AnalyzeError {
        var entry = this.symbolTable.get(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.isConstant();
        }
    }

    /**
     * program -> decl_stmt* function*
     */
    private void analyseProgram() throws CompileError {
        while(true) {
            if(check(TokenType.LET_KW) || check(TokenType.CONST_KW)){
                analyseDeclareStatement();
            } else {
                break;
            }
        }
        while(true) {
            if(check(TokenType.FN_KW)) {
                analyseFunction();
            } else {
                break;
            }
        }
        expect(TokenType.EOF);
    }

    /**
     * decl_stmt -> let_decl_stmt | const_decl_stmt
     */
    private void analyseDeclareStatement() throws CompileError {
        if(check(TokenType.LET_KW)){
            analyseLetDeclare();
        }
        else {
            analyseConstDeclare();
        }
    }

    /**
     * let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
     * @throws CompileError
     */
    private void analyseLetDeclare() throws CompileError {
        expect(TokenType.LET_KW);

        System.out.println("analyse let declare");
        var nameToken = expect(TokenType.Ident);

        // : 冒号
        expect(TokenType.Colon);
        var type = expect(TokenType.Ty);

        // 变量初始化了吗
        boolean initialized = false;

        // 下个 token 是等于号吗？如果是的话分析初始化
        if (nextIf(TokenType.Eq) != null) {
            // 分析初始化的表达式
            initialized = true;
            analyseExpression();
        }

        // 分号
        expect(TokenType.Semicolon);

        // 加入符号表，请填写名字和当前位置（报错用）
        String name = (String) nameToken.getValue();
        addSymbol(name, initialized, false, nameToken.getStartPos());

        // 如果没有初始化的话在栈里推入一个初始值
        if (!initialized) {
            instructions.add(new Instruction(Operation.LIT, 0));
        } // TODO 如果有初始化的话怎么办, analysisExpression会自动压栈? 会
    }

    /**
     * const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
     */
    private void analyseConstDeclare() throws CompileError {
        expect(TokenType.CONST_KW);

        System.out.println("analyse Constant declare");

        // 变量名
        var nameToken = expect(TokenType.Ident);

        // 加入符号表
        String name = (String) nameToken.getValue();
        addSymbol(name, true, true, nameToken.getStartPos());

        // : 冒号
        expect(TokenType.Colon);
        var type = expect(TokenType.Ty);

        // = 等号
        expect(TokenType.Eq);

        // 常表达式
        var value = analyseExpression();

        // 分号
        expect(TokenType.Semicolon);

        // 这里把常量值直接放进栈里，位置和符号表记录的一样。
        // 更高级的程序还可以把常量的值记录下来，遇到相应的变量直接替换成这个常数值，
        // 我们这里就先不这么干了。
        instructions.add(new Instruction(Operation.LIT, value));
    }


    /**
     * function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
     * TODO 如何把返回值类型利用？
     */
    private void analyseFunction() throws CompileError {
        expect(TokenType.FN_KW);

        System.out.println("analyse Function declare");

        // 函数名
        var nameToken = expect(TokenType.Ident);

        // 函数名加入符号表
        String name = (String) nameToken.getValue();

        expect(TokenType.LParen);
        analyseFunctionParamList();
        expect(TokenType.RParen);
        expect(TokenType.Arrow);

        var type = expect(TokenType.Ty);  // 返回值类型
        addSymbol(name, true, true, nameToken.getStartPos(), type);
        analyseBlockStatement();
    }

    /**
     * function_param_list -> function_param (',' function_param)*
     * TODO 参数值如何放到符号表里？
     */
    private void analyseFunctionParamList() throws CompileError {
        analyseFunctionParam();
        while(nextIf(TokenType.Comma) != null) {
            analyseFunctionParam();
        }
    }

    /**
     * function_param -> 'const'? IDENT ':' ty
     * TODO 参数值如何放到符号表里？
     */
    private void analyseFunctionParam() throws CompileError {
        expect(TokenType.CONST_KW);

        // 变量名
        var nameToken = expect(TokenType.Ident);
        expect(TokenType.Colon);

        // 类型
        var type = expect(TokenType.Ty);

        // 加入符号表
        String name = (String) nameToken.getValue();
        addSymbol(name, true, true, nameToken.getStartPos(), type);
    }

    /**
     * block_stmt -> '{' stmt* '}'
     * TODO 如何解决这个0和多次的问题？？
     */
    private void analyseBlockStatement() throws CompileError {
        expect(TokenType.LBrace);
        while(true) {
            if(check(TokenType.RBrace)){
                break;
            }else {
                analyseStatement();
            }
        }
        expect(TokenType.RBrace);
    }

    /**
        stmt ->
            expr_stmt
            | decl_stmt 'let' | 'const'
            | if_stmt  'if'
            | while_stmt  'while'
            | return_stmt  'return'
            | block_stmt  '{'
            | empty_stmt  ';'
     */
    private void analyseStatement() throws CompileError {
        if(check(TokenType.IF_KW)) {
            analyseIfStatement();
        } else if(check(TokenType.WHILE_KW)) {
            analyseWhileStatement();
        } else if(check(TokenType.RETURN_KW)) {
            analyseReturnStatement();
        } else if(check(TokenType.LET_KW) || check(TokenType.CONST_KW)) {
            analyseDeclareStatement();
        } else if(check(TokenType.LBrace)) {
            analyseBlockStatement();
        } else if(check(TokenType.Semicolon)) {
            expect(TokenType.Semicolon);
        } else {
            analyseExpressionStatement();
        }
    }


    /**
     * expr_stmt -> expr ';'
     */
    private void analyseExpressionStatement() throws CompileError{
        analyseExpression();
        expect(TokenType.Semicolon);
    }

    /**
     * if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
     */
    private void analyseIfStatement() throws CompileError {
        expect(TokenType.IF_KW);
        analyseExpression();
        analyseBlockStatement();
        if(nextIf(TokenType.ELSE_KW) != null) {
            if(check(TokenType.IF_KW)) {
                analyseIfStatement();
            }
            else {
                analyseBlockStatement();
            }
        }
    }

    /**
     * while_stmt -> 'while' expr block_stmt
     * TODO 需要根据条件判断吗
     */
    private void analyseWhileStatement() throws CompileError {
        expect(TokenType.WHILE_KW);
        analyseExpression(); // condition
        analyseBlockStatement();
    }

    private void analyseReturnStatement() throws CompileError {
        expect(TokenType.RETURN_KW);
        if(nextIf(TokenType.Semicolon) == null) {
            analyseExpression();
        }
    }

    private void analyseExpression(int minPrec) throws CompileError {
        if(check(TokenType.Ident)) {
            var nameToken = expect(TokenType.Ident);
            String name = (String) nameToken.getValue();
            if(nextIf(TokenType.Assign) != null) {  // assign_expr -> IDENT '=' expr
                analyseExpression(1);
                // var offset = getOffset(name, nameToken.getStartPos());
                instructions.add(new Instruction(Operation.STO, 0));
            } else if(nextIf(TokenType.LParen) != null) {  // call_expr -> IDENT '(' call_param_list? ')'
                if(check(TokenType.RParen)) {
                    expect(TokenType.RParen);
                }else {
                    analyseCallParamList();
                    instructions.add(new Instruction(Operation.CALL));
                }
            } else {  // IDENT TODO 查符号表看有没有, 然后压入栈
                // var offset = getOffset(name, nameToken.getStartPos());
                System.out.println("LOD");
                instructions.add(new Instruction(Operation.LOD, 0));
            }
        } else if(check(TokenType.Minus)) {  // negate_expr -> '-' expr
            expect(TokenType.Minus);
            // 计算结果需要被 0 减
            instructions.add(new Instruction(Operation.LIT, 0));
            analyseExpression(1);
            instructions.add(new Instruction(Operation.SUB));
        } else if(check(TokenType.LParen)) {  // group_expr -> '(' expr ')'
            expect(TokenType.LParen);
            analyseExpression(1);
            expect(TokenType.RParen);
        } else { // literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL | CHAR_LITERAL
            if(check(TokenType.Uint)) {
                var token = expect(TokenType.Uint);
                int value = (int)token.getValue();
                instructions.add(new Instruction(Operation.LIT, value));
            } else if(check(TokenType.String)) {
                var token = expect(TokenType.String);
                String value = token.getValue().toString();
                instructions.add(new Instruction(Operation.LIT, value));
            } else if(check(TokenType.Char)) {
                var token = expect(TokenType.Char);
                String value = token.getValue().toString();
                instructions.add(new Instruction(Operation.LIT, value));  // TODO char 的操作数
            } else {
                throw new ExpectedTokenError(List.of(TokenType.Ident, TokenType.Uint, TokenType.LParen), next());
            }
        }

        if(check(TokenType.AS_KW)) { // as_expr -> expr 'as' ty
            expect(TokenType.AS_KW);
            var type = expect(TokenType.Ty);
            // TODO 附加的先不管
        } else { // operator_expr -> expr binary_operator expr
            while(true) {
                var token = peek();
                String name = token.getValue().toString();
                if (OPPrec.get(name) == null || OPPrec.get(name) < minPrec) {
                    return;
                }
                next();
                String op = token.getValue().toString();
                int prec = OPPrec.get(op);
                int nextMinPrec = prec + 1;
                analyseExpression(nextMinPrec);
                instructions.add(new Instruction(String2OP.get(op)));
            }
        }
    }


    private void analyseCallParamList() throws CompileError {
        analyseExpression();
        while(nextIf(TokenType.Comma) != null) {
            analyseExpression();
        }
    }













    /* ----------------- 之前的 ----------------- */
    //<语句序列> ::= {<语句>}
    private void analyseStatementSequence() throws CompileError {
        // 语句序列 -> 语句*
        System.out.println("analyseStatementSequence");
        // 语句 -> 赋值语句 | 输出语句 | 空语句

        while (true) {
            // 如果下一个 token 是……
            var peeked = peek();
            if (peeked.getTokenType() == TokenType.Ident) { // 如果是标识符
                analyseAssignmentStatement();
                // 调用相应的分析函数
                // 如果遇到其他非终结符的 FIRST 集呢？
            } else if (peeked.getTokenType() == TokenType.Print) { // 如果是pirnt
                analyseOutputStatement();
            } else if (peeked.getTokenType() == TokenType.Semicolon) {
                expect(TokenType.Semicolon);
            } else {
                break;
            }
        }
        System.out.println("analyseStatementSequence End");
    }

    // <常表达式> ::= [<符号>]<无符号整数>
    private int analyseConstantExpression() throws CompileError {
        // 常表达式 -> 符号? 无符号整数
        boolean negative = false;
        if (nextIf(TokenType.Plus) != null) {
            negative = false;
        } else if (nextIf(TokenType.Minus) != null) {
            negative = true;
        }

        var token = expect(TokenType.Uint);

        int value = (int) token.getValue();
        if (negative) {
            value = -value;
        }
        return value;
    }

    // <表达式> ::= <项>{<加法型运算符><项>}
    private void analyseExpression() throws CompileError {
        // 表达式 -> 项 (加法运算符 项)*
        // 项
        System.out.println("analyseExpression");
        analyseItem();

        while (true) {
            // 预读可能是运算符的 token
            var op = peek();
            if (op.getTokenType() != TokenType.Plus && op.getTokenType() != TokenType.Minus) {
                break;
            }

            // 运算符
            next();

            // 项
            analyseItem();

            // 生成代码
            if (op.getTokenType() == TokenType.Plus) {
                instructions.add(new Instruction(Operation.ADD));
            } else if (op.getTokenType() == TokenType.Minus) {
                instructions.add(new Instruction(Operation.SUB));
            }
        }
        System.out.println("analyseExpression End");
    }

    // <赋值语句> ::= <标识符>'='<表达式>';'
    private void analyseAssignmentStatement() throws CompileError {
        // 赋值语句 -> 标识符 '=' 表达式 ';'
        System.out.println("analyseAssignmentStatement");
        var nameToken = expect(TokenType.Ident);
        expect(TokenType.Equal);
        // 分析这个语句
        analyseExpression();
        // 标识符是什么？
        String name = (String) nameToken.getValue();
        var symbol = symbolTable.get(name);
        if (symbol == null) {
            // 没有这个标识符
            throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ nameToken.getStartPos());
        } else if (symbol.isConstant) {
            // 标识符是常量
            throw new AnalyzeError(ErrorCode.AssignToConstant, /* 当前位置 */ nameToken.getStartPos());
        }
        // 设置符号已初始化
        initializeSymbol(name, nameToken.getStartPos());

        // 把结果保存
        var offset = getOffset(name, nameToken.getStartPos());
        instructions.add(new Instruction(Operation.STO, offset));
        System.out.println("analyseAssignmentStatement End");
    }

    // TODO
    private void analyseOutputStatement() throws CompileError {
        // 输出语句 -> 'print' '(' 表达式 ')' ';'
        System.out.println("analyseOutputStatement");
        expect(TokenType.Print);
        expect(TokenType.LParen);

        analyseExpression();

        expect(TokenType.RParen);
        expect(TokenType.Semicolon);

        instructions.add(new Instruction(Operation.WRT));
    }

    //<项> ::= <因子>{<乘法型运算符><因子>}
    private void analyseItem() throws CompileError {
        // 项 -> 因子 (乘法运算符 因子)*
        System.out.println("analyseItem");
        // 因子
        analyseFactor();
        while (true) {
            // 预读可能是运算符的 token
            Token op = peek();

            if (op.getTokenType() != TokenType.Mult && op.getTokenType() != TokenType.Div) {
                break;
            }

            // 运算符
            next();

            // 因子
            analyseFactor();
            // 生成代码
            if (op.getTokenType() == TokenType.Mult) {
                instructions.add(new Instruction(Operation.MUL));
            } else if (op.getTokenType() == TokenType.Div) {
                instructions.add(new Instruction(Operation.DIV));
            }
        }
        System.out.println("analyseItem End");
    }

    // nextIf: 偷看下一个 token，如果 token 的类型与 tt 相同则前进一个 token 并返回它
    private void analyseFactor() throws CompileError {
        // 因子 -> 符号? (标识符 | 无符号整数 | '(' 表达式 ')')
        System.out.println("analyseFactor");
        boolean negate;
        if (nextIf(TokenType.Minus) != null) {
            negate = true;
            // 计算结果需要被 0 减
            instructions.add(new Instruction(Operation.LIT, 0));
        } else {
            nextIf(TokenType.Plus);
            negate = false;
        }

        if (check(TokenType.Ident)) {
            // 是标识符

            // 加载标识符的值
            var nameToken = expect(TokenType.Ident);  // 这一行是自己加的
            String name = (String) nameToken.getValue();
            var symbol = symbolTable.get(name);
            if (symbol == null) {
                // 没有这个标识符
                throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ nameToken.getStartPos());
            } else if (!symbol.isInitialized) {
                // 标识符没初始化
                throw new AnalyzeError(ErrorCode.NotInitialized, /* 当前位置 */ nameToken.getStartPos());
            }
            var offset = getOffset(name, nameToken.getStartPos());
            System.out.println("LODLODLOD");
            instructions.add(new Instruction(Operation.LOD, offset));
        } else if (check(TokenType.Uint)) {
            // 是整数
            // 加载整数值
            // System.out.println("无符号整数");
            var token = expect(TokenType.Uint);
            int value = (int)token.getValue();
            instructions.add(new Instruction(Operation.LIT, value));
        } else if (check(TokenType.LParen)) {
            // 是表达
            expect(TokenType.LParen);
            analyseExpression();
            // 调用相应的处理函数
            expect(TokenType.RParen);
            System.out.println("RParen");
        } else {
            // 都不是，摸了
            throw new ExpectedTokenError(List.of(TokenType.Ident, TokenType.Uint, TokenType.LParen), next());
        }

        if (negate) {
            instructions.add(new Instruction(Operation.SUB));
        }
        System.out.println("analyseFactor End");
    }
}
