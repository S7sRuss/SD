import compiler.CompVisitor;

import parser.ParseException;
import parser.Parser;
import semantics.EvalVisitor;
import ast.ASTNode;
import ast.UnparseVisitor;

public class Main {
	public static void main(String args[]) throws ParseException {
		Parser parser = new Parser(System.in);
		while (true) {
			System.out.print("> ");
			try {
				ASTNode exp = parser.main();
				System.out.println("Ok:  "+exp.accept(new UnparseVisitor()));
				System.out.println(exp.accept(new CompVisitor()));
				System.out.println("Val: "+exp.accept(new EvalVisitor()));
			} catch (Error e) {
				System.out.println("Parsing error");
				System.out.println(e.getMessage());
				break;
			} catch (Exception e) {
				System.out.println("NOK.");
				e.printStackTrace();
				break;
			}
		}
	}
}
