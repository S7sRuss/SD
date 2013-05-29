package ast;

public interface Visitor<T> {
	T visit(ASTNum number);

	T visit(ASTAdd plus);
	T visit(ASTSub minus);
	T visit(ASTMul mult);
	T visit(ASTDiv div);
}
