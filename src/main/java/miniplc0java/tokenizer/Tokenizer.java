package miniplc0java.tokenizer;

import miniplc0java.util.Pos;
import miniplc0java.error.TokenizeError;
import miniplc0java.error.ErrorCode;

import java.util.HashMap;

public class Tokenizer {
    private StringIter it;
    private HashMap<String, TokenType> keywordMap = new HashMap<>() {
        {
//            put("begin", TokenType.Begin);
//            put("end", TokenType.End);
//            put("var", TokenType.Var);
//            put("const", TokenType.Const);
//            put("print", TokenType.Print);
            put("fn", TokenType.FN_KW);
            put("let", TokenType.LET_KW);
            put("const", TokenType.CONST_KW);
            put("as", TokenType.AS_KW);
            put("while", TokenType.WHILE_KW);
            put("if", TokenType.IF_KW);
            put("else", TokenType.ELSE_KW);
            put("return", TokenType.RETURN_KW);

            /* type */
            put("ty", TokenType.Ty);
        }
    };

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        if (Character.isDigit(peek)) {
            return lexUInt();
        } else if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        } else if(peek == '\'') {
            return lexChar();
        } else if(peek == '\"') {
            return lexString();
        } else {
            return lexOperatorOrUnknown();
        }
    }

    // TODO done!
    private Token lexUInt() throws TokenizeError {
        // 请填空：
        String val = "";
        // 直到查看下一个字符不是数字为止:
        char peek = it.peekChar();
        // 获取当前字符串位置
        Pos begin = it.currentPos();
        while (Character.isDigit(peek)) {
            it.nextChar();
            val = val + peek;
            peek = it.peekChar();
            // -- 前进一个字符，并存储这个字符
        }
        // 获取当前字符串位置
        Pos end = it.currentPos();
        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        try { // TODO 转换为无符号整数 ?　这玩意儿怎么可能转换失败 ?
            Token token = new Token(TokenType.Uint, Integer.parseInt(val), begin, end);
            return token;
        } catch (Exception e) {
            throw new TokenizeError(ErrorCode.InvalidInput, begin);
        }
        // Token 的 Value 应填写数字的值
    }

    // Todo new 检查是否是string
    private Token lexString() throws TokenizeError {
        String val = "";
        char peek = it.peekChar();
        Pos begin = it.currentPos();
        if (peek  != '\"') {
            throw new TokenizeError(ErrorCode.InvalidInput, begin);
        }
        it.nextChar();
        peek = it.peekChar();
        while(peek != '\"') { // TODO 如何停止，'''和'\''的区别？
            it.nextChar();
            if(peek == '\\') {  // 转义字符
                val = val + peek;
                peek = it.peekChar();
                if(peek == '\\' || peek == '\"' || peek == '\'' || peek == 'n' || peek == 'r' || peek == 't') {
                    it.nextChar();
                }else {
                    throw new TokenizeError(ErrorCode.InvalidInput, begin);
                }
            }
            val = val + peek;
            peek = it.peekChar();
        }
        Pos end = it.currentPos();
        return new Token(TokenType.String, val, begin, end);
    }

    // Todo new 检查是否是Char
    private Token lexChar() throws TokenizeError {
        String val = " ";
        char peek = it.peekChar();
        Pos begin = it.currentPos();
        if (peek  != '\'') {
            throw new TokenizeError(ErrorCode.InvalidInput, begin);
        }
        it.nextChar();
        peek = it.peekChar();
        if(peek == '\\') {
            val = val + peek;
            it.nextChar();
            peek = it.peekChar();
            if(peek == '\\' || peek == '\"' || peek == '\'' || peek == 'n' || peek == 'r' || peek == 't') {
                val = val + peek;
                it.nextChar();
            } else {
                throw new TokenizeError(ErrorCode.InvalidInput, begin);
            }
        } else {
            val = val + peek;
            it.nextChar();
        }
        peek = it.peekChar();
        if(peek != '\'') {
            throw new TokenizeError(ErrorCode.InvalidInput, begin);
        }
        it.nextChar();
        Pos end = it.currentPos();
        return new Token(TokenType.Char, val, begin, end);
    }

    // TODO done!
    private Token lexIdentOrKeyword() throws TokenizeError {
        // 请填空：
        // 直到查看下一个字符不是数字或字母为止:
        String val = "";
        char peek = it.peekChar();
        Pos begin = it.currentPos();
        while (Character.isDigit(peek) || Character.isAlphabetic(peek)) {
            it.nextChar();
            val = val + peek;
            peek = it.peekChar(); // 查看下一个字符
            // -- 前进一个字符，并存储这个字符
        }
        Pos end = it.currentPos();
        if (this.keywordMap.containsKey(val)) {  // 如果是关键字
            return new Token(keywordMap.get(val), val, begin, end);
        } else {  // 是标识符
            return new Token(TokenType.Ident, val, begin, end);
        }
        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        //
        // Token 的 Value 应填写标识符或关键字的字符串
    }

    private Token lexOperatorOrUnknown() throws TokenizeError {
        Pos begin, end;
        char peek;
        switch (it.nextChar()) {
            case '+':
                return new Token(TokenType.Plus, '+', it.previousPos(), it.currentPos());
            case '-':
                begin = it.previousPos();
                peek = it.peekChar();
                if(peek == '>') {
                    it.nextChar();
                    return new Token(TokenType.Arrow, "->", begin, it.currentPos());
                }
                return new Token(TokenType.Minus, '-', it.previousPos(), it.currentPos());
            case '*':
                return new Token(TokenType.Mult, '*', it.previousPos(), it.currentPos());
            case '/':
                return new Token(TokenType.Div, '/', it.previousPos(), it.currentPos());
            case '=':
                begin = it.previousPos();
                peek = it.peekChar();
                if(peek == '=') {
                    it.nextChar();
                    return new Token(TokenType.Eq, "==", begin, it.currentPos());
                }
                return new Token(TokenType.Assign, '=', it.previousPos(), it.currentPos());
            case '!':
                begin = it.previousPos();
                peek = it.peekChar();
                if(peek == '=') {
                    it.nextChar();
                    return new Token(TokenType.Eq, "!=", begin, it.currentPos());
                } else {
                    throw new TokenizeError(ErrorCode.InvalidInput, begin);
                }
            case ';':
                return new Token(TokenType.Semicolon, ';', it.previousPos(), it.currentPos());
            case ':':
                return new Token(TokenType.Colon, ':', it.previousPos(), it.currentPos());
            case ',':
                return new Token(TokenType.Comma, ',', it.previousPos(), it.currentPos());
            case '(':
                return new Token(TokenType.LParen, '(', it.previousPos(), it.currentPos());
            case ')':
                return new Token(TokenType.RParen, ')', it.previousPos(), it.currentPos());
            case '{':
                return new Token(TokenType.LBrace, '{', it.previousPos(), it.currentPos());
            case '}':
                return new Token(TokenType.RBrace, '}', it.previousPos(), it.currentPos());
            case '<':
                begin = it.previousPos();
                peek = it.peekChar();
                if(peek == '=') {
                    it.nextChar();
                    return new Token(TokenType.Le, "<=", begin, it.currentPos());
                }
                return new Token(TokenType.Lt, '<', it.previousPos(), it.currentPos());
            case '>':
                begin = it.previousPos();
                peek = it.peekChar();
                if(peek == '=') {
                    it.nextChar();
                    return new Token(TokenType.Ge, ">=", begin, it.currentPos());
                }
                return new Token(TokenType.Gt, '>', it.previousPos(), it.currentPos());
            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
}