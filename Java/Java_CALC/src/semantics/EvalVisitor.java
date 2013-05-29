package semantics;

import ast.ASTAdd;
import ast.ASTDiv;
import ast.ASTMul;
import ast.ASTNum;
import ast.ASTSub;
import ast.Visitor;

public class EvalVisitor implements Visitor<Integer> {

	@Override
	public Integer visit(ASTNum number) {
		return number.num;
	}

	@Override
	public Integer visit(ASTAdd plus) {
		return plus.left.accept(this)+plus.right.accept(this);
	}

	@Override
	public Integer visit(ASTMul mult) {
		return mult.left.accept(this)*mult.right.accept(this);
	}

	@Override
	public Integer visit(ASTSub minus) {
		return minus.left.accept(this)-minus.right.accept(this);
	}

	@Override
	public Integer visit(ASTDiv div) {
		return div.left.accept(this)/div.right.accept(this);
	}
}
