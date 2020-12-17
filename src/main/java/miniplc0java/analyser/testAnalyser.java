package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.CompileError;
import miniplc0java.error.ErrorCode;
import miniplc0java.error.ExpectedTokenError;
import miniplc0java.error.TokenizeError;
import miniplc0java.instruction.Instruction;
import miniplc0java.instruction.Operation;
import miniplc0java.tokenizer.StringIter;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;
import miniplc0java.tokenizer.Tokenizer;
import miniplc0java.util.Pos;
import org.checkerframework.checker.units.qual.C;

import java.io.*;

import java.util.*;

public final class testAnalyser {

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

    public testAnalyser(Tokenizer tokenizer) {
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
//                    analyseCallParamList();
                    System.out.println("TODO analyse call param list");
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

    public static void main(String[] args) throws Exception {
//        FileInputStream fis = null;
//        InputStreamReader isr = null;
//        BufferedReader br = null; //用于包装InputStreamReader,提高处理性能。因为BufferedReader有缓冲的，而InputStreamReader没有。
//        String str = "";
//        String str1 = "";
//        fis = new FileInputStream("/Users/huangjunqin/Desktop/data.txt");// FileInputStream
//        // 从文件系统中的某个文件中获取字节
//        isr = new InputStreamReader(fis);// InputStreamReader 是字节流通向字符流的桥梁,
//        br = new BufferedReader(isr);// 从字符输入流中读取文件中的内容,封装了一个new InputStreamReader的对象
//        str = br.readLine();
//
//        System.out.println(str);
        InputStream input;
        input = new FileInputStream("/Users/huangjunqin/Desktop/data.txt");

        Scanner scanner;
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);
        var tokenizer = new Tokenizer(iter);
        testAnalyser analyser = new testAnalyser(tokenizer);
        analyser.analyseExpression(1);

        System.out.println("finish");
    }
}
