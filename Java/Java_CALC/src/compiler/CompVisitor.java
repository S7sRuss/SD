package compiler;

import ast.ASTAdd;
import ast.ASTDiv;
import ast.ASTMul;
import ast.ASTNum;
import ast.ASTSub;
import ast.Visitor;

public class CompVisitor implements Visitor<String>{

	@Override
	public String visit(ASTNum number) {
		return "ldc.i4 "+number.num;
	}

	@Override
	public String visit(ASTAdd plus) {
		return plus.left.accept(this)+"\n"+plus.right.accept(this)+"\n"+"add";
	}

	@Override
	public String visit(ASTSub minus) {
		return minus.left.accept(this)+"\n"+minus.right.accept(this)+"\n"+"sub";
	}

	@Override
	public String visit(ASTMul mult) {
		return mult.left.accept(this)+"\n"+mult.right.accept(this)+"\n"+"mul";
	}

	@Override
	public String visit(ASTDiv div) {
		return div.left.accept(this)+"\n"+div.right.accept(this)+"\n"+"div";
	}

	

}
