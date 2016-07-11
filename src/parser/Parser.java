package parser;
import java.io.*; import lexer.*; import symbols.*; import inter.*;

public class Parser {

   private Lexer lex;    // 词法分析器
   private Token look;   // 向前看词法单元
   Env top = null;       // 当前或者顶层符号表
   int used = 0;         // 变量声明的存储位置

   public Parser(Lexer l) throws IOException { lex = l; move(); }

   void move() throws IOException { look = lex.scan(); }

   void error(String s) { throw new Error("near line "+lex.line+": "+s); }

   void match(int t) throws IOException {
      if( look.tag == t ) move(); //tag做出语法分析决定
      else error("syntax error");
   }

   public void program() throws IOException {  // program -> block  调用program 和block
      Stmt s = block();
      int begin = s.newlabel();  int after = s.newlabel();
      s.emitlabel(begin);  s.gen(begin, after);  s.emitlabel(after); //中间代码生成
   }

   Stmt block() throws IOException {  // block -> { decls stmts }
      match('{');  Env savedEnv = top;  top = new Env(top); //top存放最顶层符号表 savedEnv指向前面符号表的连接
      decls(); Stmt s = stmts();
      match('}');  top = savedEnv;
      return s;
   }

   void decls() throws IOException {

      while( look.tag == Tag.BASIC ) {   // D -> type ID ;
         Type p = type(); Token tok = look; match(Tag.ID); match(';');
         Id id = new Id((Word)tok, p, used);
         top.put( tok, id );
         used = used + p.width;
      }
   }

   Type type() throws IOException {

      Type p = (Type)look;            // expect look.tag == Tag.BASIC 
      match(Tag.BASIC);
      if( look.tag != '[' ) return p; // T -> basic
      else return dims(p);            // return array type
   }

   Type dims(Type p) throws IOException {
      match('[');  Token tok = look;  match(Tag.NUM);  match(']');
      if( look.tag == '[' )
      p = dims(p);
      return new Array(((Num)tok).value, p);
   }

   Stmt stmts() throws IOException {
      if ( look.tag == '}' ) return Stmt.Null;
      else return new Seq(stmt(), stmts());
   }

   Stmt stmt() throws IOException {
      Expr x;  Stmt s, s1, s2;
      Stmt savedStmt;         // 用于为break语句保存外层的循环语句

      switch( look.tag ) {

      case ';':         //各个case对应于非终结符stmt的各个产生式 
         move();
         return Stmt.Null;

      case Tag.IF:     //每个case分支通过构造函数建立构造对应的结点  语句处理
         match(Tag.IF); match('('); x = bool(); match(')');
         s1 = stmt();
         if( look.tag != Tag.ELSE ) return new If(x, s1);
         match(Tag.ELSE);
         s2 = stmt();
         return new Else(x, s1, s2);

      case Tag.WHILE:
         While whilenode = new While();
         savedStmt = Stmt.Enclosing; Stmt.Enclosing = whilenode;
         match(Tag.WHILE); match('('); x = bool(); match(')');
         s1 = stmt();
         whilenode.init(x, s1);
         Stmt.Enclosing = savedStmt;  // reset Stmt.Enclosing
         return whilenode;

      case Tag.DO:
         Do donode = new Do();
         savedStmt = Stmt.Enclosing; Stmt.Enclosing = donode;
         match(Tag.DO);
         s1 = stmt();
         match(Tag.WHILE); match('('); x = bool(); match(')'); match(';');
         donode.init(s1, x);
         Stmt.Enclosing = savedStmt;  // reset Stmt.Enclosing
         return donode;

      case Tag.BREAK:
         match(Tag.BREAK); match(';');
         return new Break();

      case '{':
         return block();

      default:
         return assign();
      }
   }

   Stmt assign() throws IOException {         //赋值语句的处理出现在辅助过程assign中
      Stmt stmt;  Token t = look;
      match(Tag.ID);
      Id id = top.get(t);
      if( id == null ) error(t.toString() + " undeclared");

      if( look.tag == '=' ) {       // S -> id = E ;
         move();  stmt = new Set(id, bool());
      }
      else {                        // S -> L = E ;
         Access x = offset(id);
         match('=');  stmt = new SetElem(x, bool());
      }
      match(';');
      return stmt;
   }

   Expr bool() throws IOException {       //创建抽象语法树结点
      Expr x = join();
      while( look.tag == Tag.OR ) {
         Token tok = look;  move();  x = new Or(tok, x, join());
      }
      return x;
   }

   Expr join() throws IOException {
      Expr x = equality();
      while( look.tag == Tag.AND ) {
         Token tok = look;  move();  x = new And(tok, x, equality());
      }
      return x;
   }

   Expr equality() throws IOException {
      Expr x = rel();
      while( look.tag == Tag.EQ || look.tag == Tag.NE ) {
         Token tok = look;  move();  x = new Rel(tok, x, rel());
      }
      return x;
   }

   Expr rel() throws IOException {       //实现>,<
      Expr x = expr();
      switch( look.tag ) {
      case '<': case Tag.LE: case Tag.GE: case '>':
         Token tok = look;  move();  return new Rel(tok, x, expr());
      default:
         return x;
      }
   }

   Expr expr() throws IOException {
      Expr x = term();
      while( look.tag == '+' || look.tag == '-' ) {
         Token tok = look;  move();  x = new Arith(tok, x, term());
      }
      return x;
   }

   Expr term() throws IOException {
      Expr x = unary();
      while(look.tag == '*' || look.tag == '/' ) {
         Token tok = look;  move();   x = new Arith(tok, x, unary());
      }
      return x;
   }

   Expr unary() throws IOException {      //表达式处理
      if( look.tag == '-' ) {
         move();  return new Unary(Word.minus, unary());
      }
      else if( look.tag == '!' ) {
         Token tok = look;  move();  return new Not(tok, unary());
      }
      else return factor();
   }

   Expr factor() throws IOException {                //因子
      Expr x = null;
      switch( look.tag ) {
      case '(':
         move(); x = bool(); match(')');
         return x;
      case Tag.NUM:
         x = new Constant(look, Type.Int);    move(); return x;
      case Tag.REAL:
         x = new Constant(look, Type.Float);  move(); return x;
      case Tag.TRUE:
         x = Constant.True;                   move(); return x;
      case Tag.FALSE:
         x = Constant.False;                  move(); return x;
      default:
         error("syntax error");
         return x;
      case Tag.ID:
         String s = look.toString();
         Id id = top.get(look);
         if( id == null ) error(look.toString() + " undeclared");
         move();
         if( look.tag != '[' ) return id;
         else return offset(id);
      }
   }

   Access offset(Id a) throws IOException {   // I -> [E] | [E] I  为数组地址计算生成代码
      Expr i; Expr w; Expr t1, t2; Expr loc;  // inherit id

      Type type = a.type;
      match('['); i = bool(); match(']');     // first index, I -> [ E ]
      type = ((Array)type).of;
      w = new Constant(type.width);
      t1 = new Arith(new Token('*'), i, w);
      loc = t1;
      while( look.tag == '[' ) {      // multi-dimensional I -> [ E ] I
         match('['); i = bool(); match(']');
         type = ((Array)type).of;
         w = new Constant(type.width);
         t1 = new Arith(new Token('*'), i, w);
         t2 = new Arith(new Token('+'), loc, t1);
         loc = t2;
      }

      return new Access(a, loc, type);
   }
}
