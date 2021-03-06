package ast;

public class ASTAdd implements ASTNode {

	public final ASTNode left;
	public final ASTNode right;

	public ASTAdd(ASTNode left, ASTNode right) {
		this.left = left;
		this.right = right;
	}

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visit(this);
	}

}
