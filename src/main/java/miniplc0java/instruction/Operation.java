package miniplc0java.instruction;

public enum Operation {
    ILL,
    LIT,
    LOD,
    STO,
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    NEQ,
    LT, // <
    GT, // >
    LE, // <=
    GE, // >=
    CALL,
    STACKALLOC,
    JUMP, // 条件跳转
    NOCONJUMP, // 无条件跳转
    WRT
}
