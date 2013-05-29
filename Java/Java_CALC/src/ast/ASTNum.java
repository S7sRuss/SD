package ast;

public class ASTNum implements ASTNode {
	public final int num;

	public ASTNum(int num) {
		this.num = num;
	}

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visit(this);
	}
}
