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
//    HashMap<String, SymbolEntry> symbolTable = new HashMap<>();

    /**
     * 分程序索引表
     */
    ArrayList<HashMap<String, SymbolEntry>> indexTable = new ArrayList<>();

    /**
     * 获取当前的符号表
     * @return
     */
//    private HashMap<String, SymbolEntry> getCurrentSymbolTable() {
//        return indexTable.get(indexTable.size() - 1);
//    }

    /**
     * 递归向上查询符号
     */
    private SymbolEntry findSymbol(String name) throws AnalyzeError {
        HashMap<String, SymbolEntry> map;
        SymbolEntry symbol = null;
        for(int i = indexTable.size()-1; i>=0; i--) {
            map = indexTable.get(i);
            symbol = map.get(name);
            if(symbol != null) break;
        }
        return symbol;
    }

    /**
     * 优先矩阵表
     */
    HashMap<String, Integer> OPPrec = new HashMap<String, Integer>();

    HashMap<String, Operation> String2OP = new HashMap<>();

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

        this.indexTable.add(new HashMap<>());
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
     * 添加一个符号
     *
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private void addSymbol(String name, boolean isInitialized, boolean isConstant, Pos curPos, Token typeToken) throws AnalyzeError {
        // 默认加入当前的符号表
        HashMap<String, SymbolEntry> symbolTable = indexTable.get(indexTable.size() - 1);
        if (symbolTable.get(name) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            symbolTable.put(name, new SymbolEntry(isConstant, isInitialized, typeToken.getTokenType()));
        }
    }

//    /**
//     * 设置符号为已赋值
//     *
//     * @param name   符号名称
//     * @param curPos 当前位置（报错用）
//     * @throws AnalyzeError 如果未定义则抛异常
//     */
//    private void initializeSymbol(SymbolEntry entry, Pos curPos) throws AnalyzeError {
//        var entry = symbolTable.get(name);
//        if (entry == null) {
//            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
//        } else { entry.setInitialized(true);
//
//    }

    /**
     * 获取变量是否是常量
     *
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 是否为常量
     * @throws AnalyzeError
     */
    private boolean isConstant(String name, Pos curPos, HashMap<String, SymbolEntry> symbolTable) throws AnalyzeError {
        var entry = symbolTable.get(name);
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
     * TODO 检查值是否一致
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
            analyseExpression(1);
        }

        // 分号
        expect(TokenType.Semicolon);

        // 加入符号表，请填写名字和当前位置（报错用）
        String name = (String) nameToken.getValue();
        addSymbol(name, initialized, false, nameToken.getStartPos(), type);

        // 如果没有初始化的话在栈里推入一个初始值
        if (!initialized) {
            instructions.add(new Instruction(Operation.LIT, 0));
        }
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

        // : 冒号
        expect(TokenType.Colon);
        var type = expect(TokenType.Ty);

        addSymbol(name, true, true, nameToken.getStartPos(), type);

        // = 等号
        expect(TokenType.Eq);

        // 常表达式
        analyseExpression(1);

        // 分号
        expect(TokenType.Semicolon);
    }


    /**
     * function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
     * TODO 如何把返回值类型利用？
     */
    private void analyseFunction() throws CompileError {
        expect(TokenType.FN_KW);

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
        // 创建符号表
        indexTable.add(new HashMap<>());
        while(true) {
            if(check(TokenType.RBrace)){
                break;
            }else {
                analyseStatement();
            }
        }
        expect(TokenType.RBrace);
        HashMap<String, SymbolEntry> map = indexTable.get(indexTable.size()-1);
        indexTable.remove(map);
        // 删除符号表
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
        analyseExpression(1);
        expect(TokenType.Semicolon);
    }

    /**
     * if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
     */
    private void analyseIfStatement() throws CompileError {
        expect(TokenType.IF_KW);
        analyseExpression(1);
        instructions.add(new Instruction(Operation.JUMP, 0)); // jump 到 else 处
        int index1 = instructions.size() - 1;

        analyseBlockStatement();
        instructions.add(new Instruction(Operation.JUMP, 0)); // jump 到 最后
        int index2 = instructions.size() - 1;

        if(nextIf(TokenType.ELSE_KW) != null) {
            if(check(TokenType.IF_KW)) {
                analyseIfStatement();
            }
            else {
                var offset = instructions.size() - 1 - index1;
                instructions.set(index1, new Instruction(Operation.JUMP, offset));
                analyseBlockStatement();
            }
        }
        var offset = instructions.size() - 1 - index2;
        instructions.set(index2, new Instruction(Operation.JUMP, offset));
    }

    /**
     * while_stmt -> 'while' expr block_stmt
     */
    private void analyseWhileStatement() throws CompileError {
        expect(TokenType.WHILE_KW);
        analyseExpression(1); // condition
        instructions.add(new Instruction(Operation.JUMP, 0)); // jump 到 while 外面
        int index1 = instructions.size() - 1;

        analyseBlockStatement();

        instructions.add(new Instruction(Operation.NOCONJUMP, index1 - (instructions.size() - 1))); // 无条件跳转到while开头

        int offset = instructions.size() - 1 - index1;
        instructions.set(index1, new Instruction(Operation.JUMP, offset));
    }

    private void analyseReturnStatement() throws CompileError {
        expect(TokenType.RETURN_KW);
        if(nextIf(TokenType.Semicolon) == null) {
            analyseExpression(1);
        }
    }

    private void analyseExpression(int minPrec) throws CompileError {

        if(check(TokenType.Ident)) {
            var nameToken = expect(TokenType.Ident);
            String name = (String) nameToken.getValue();
            if(nextIf(TokenType.Assign) != null) {  // assign_expr -> IDENT '=' expr 赋值语句
                analyseExpression(1);
                var symbol = findSymbol(name);
                if (symbol == null) {
                    // 没有这个标识符
                    throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ nameToken.getStartPos());
                } else if (symbol.isConstant) {
                    // 标识符是常量
                    throw new AnalyzeError(ErrorCode.AssignToConstant, /* 当前位置 */ nameToken.getStartPos());
                } else if (symbol.isFunction) {
                    // 标识符是函数
                    throw new AnalyzeError(ErrorCode.ExpectedVariableOrConstant, nameToken.getStartPos());
                }
                // 设置符号已初始化
                symbol.setInitialized(true);
                instructions.add(new Instruction(Operation.STO));

            } else if(nextIf(TokenType.LParen) != null) {  // call_expr -> IDENT '(' call_param_list? ')'
                var symbol = findSymbol(name);
                if (symbol == null) {
                    // 没有这个标识符
                    throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ nameToken.getStartPos());
                } else if(symbol.isFunction == false) {
                    // 不是函数
                    throw new AnalyzeError(ErrorCode.ExpectedFunction, /* 当前位置 */ nameToken.getStartPos());
                }
                if(check(TokenType.RParen)) {
                    expect(TokenType.RParen);
                }else {
                    instructions.add(new Instruction(Operation.STACKALLOC, 1)); // 分配返回值
                    analyseCallParamList();
                    instructions.add(new Instruction(Operation.CALL));
                }
            } else {  // IDENT
                var symbol = findSymbol(name);
                if (symbol == null) {
                    // 没有这个标识符
                    throw new AnalyzeError(ErrorCode.NotDeclared, /* 当前位置 */ nameToken.getStartPos());
                } else if (symbol.isInitialized != true) {
                    // 标识符没有初始化
                    throw new AnalyzeError(ErrorCode.NotInitialized, /* 当前位置 */ nameToken.getStartPos());
                } else if (symbol.isFunction) {
                    // 标识符是函数
                    throw new AnalyzeError(ErrorCode.ExpectedVariableOrConstant, nameToken.getStartPos());
                }
                instructions.add(new Instruction(Operation.LOD));
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
        analyseExpression(1);
        while(nextIf(TokenType.Comma) != null) {
            analyseExpression(1);
        }
    }
}
