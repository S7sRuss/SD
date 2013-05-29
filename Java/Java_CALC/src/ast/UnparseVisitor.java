package ast;

public class UnparseVisitor implements Visitor<String> {

	@Override
	public String visit(ASTNum number) {
		return "" + number.num;
	}

	@Override
	public String visit(ASTAdd plus) {
		return "Plus(" + plus.left.accept(this) + "," + plus.right.accept(this)
				+ ")";
	}

	@Override
	public String visit(ASTMul mult) {
		return "Mult(" + mult.left.accept(this) + "," + mult.right.accept(this)
				+ ")";
	}

	@Override
	public String visit(ASTSub minus) {
		return "Sub(" + minus.left.accept(this) + "," + minus.right.accept(this)
				+ ")";
	}

	@Override
	public String visit(ASTDiv div) {
		return "Div(" + div.left.accept(this) + "," + div.right.accept(this)
				+ ")";
	}

}
