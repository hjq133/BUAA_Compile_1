package miniplc0java.instruction;

import java.util.Objects;

public class Instruction {
    private Operation opt;
    Integer x;
    String y;

    public Instruction(Operation opt) {
        this.opt = opt;
        this.x = 0;
        this.y = null;
    }

    public Instruction(Operation opt, Integer x) {
        this.opt = opt;
        this.x = x;
        this.y = null;
    }

    public Instruction(Operation opt, String y) {
        this.opt = opt;
        this.y = y;
    }

    public Instruction() {
        this.opt = Operation.LIT;
        this.x = 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Instruction that = (Instruction) o;
        return opt == that.opt && Objects.equals(x, that.x);
    }

    @Override
    public int hashCode() {
        return Objects.hash(opt, x);
    }

    public Operation getOpt() {
        return opt;
    }

    public void setOpt(Operation opt) {
        this.opt = opt;
    }

    public Integer getX() {
        return x;
    }

    public void setX(Integer x) {
        this.x = x;
    }

    @Override
    public String toString() {
        switch (this.opt) {
            case ADD:
            case DIV:
            case ILL:
            case MUL:
            case SUB:
            case WRT:
                return String.format("%s", this.opt);
            case LIT:
            case LOD:
            case STO:
            case STACKALLOC:
                if(this.y != null) return String.format("%s %s", this.opt, this.y);
                return String.format("%s %s", this.opt, this.x);
            default:
                return "ILL";
        }
    }
}
