package miniplc0java.analyser;

import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;

public class SymbolEntry {
    boolean isConstant;
    boolean isInitialized;
    boolean isFunction;
    TokenType type;

    public SymbolEntry(boolean isConstant, boolean isDeclared, TokenType type) {
        this.isConstant = isConstant;
        this.isInitialized = isDeclared;
        this.type = type;
        this.isFunction = false;
    }

    public SymbolEntry(boolean isConstant, boolean isDeclared, TokenType type, boolean isFunction) {
        this.isConstant = isConstant;
        this.isInitialized = isDeclared;
        this.type = type;
        this.isFunction = isFunction;
    }

    /**
     * @return the isConstant
     */
    public boolean isConstant() {
        return isConstant;
    }

    /**
     * @return the isInitialized
     */
    public boolean isInitialized() {
        return isInitialized;
    }

    /**
     * @param isConstant the isConstant to set
     */
    public void setConstant(boolean isConstant) {
        this.isConstant = isConstant;
    }

    /**
     * @param isInitialized the isInitialized to set
     */
    public void setInitialized(boolean isInitialized) {
        this.isInitialized = isInitialized;
    }
}
