package lexer;
import symbols.Type;

import java.io.*;
import java.util.Hashtable;

public class Lexer {
	public static int line = 1;
	char peek = ' ';
	char buffer = ' ';
	Hashtable words = new Hashtable();
	File file = new File("file.txt");
	Reader reader = null;

	void reserve(Word w) {
		words.put(w.lexeme, w);
	}

	public Lexer() {
		try {
			reader = new InputStreamReader(new FileInputStream(file));
		} catch (Exception e) {
		}
		reserve(new Word("if", Tag.IF));
		reserve(new Word("else", Tag.ELSE));
		reserve(new Word("while", Tag.WHILE));
		reserve(new Word("do", Tag.DO));
		reserve(new Word("break", Tag.BREAK));
		reserve(Word.True);
		reserve(Type.Bool);
		reserve(Word.False);
		reserve(Type.Char);
		reserve(Type.Int);
		reserve(Type.Float);
	}

	void readch() throws IOException {
		try {
			int temp;
			if ((temp = reader.read()) == -1)
				peek = '#';
			peek = (char) temp;
		} catch (Exception e1) {
		}
	}

	boolean readch(char c) throws IOException {
		readch();
		if (peek != c)
			return false;
		peek = ' ';
		return true;
	}

	public Token scan() throws IOException {

		while (true) {
			if (buffer == ' ') {
				readch();
			} else {
				peek = buffer;
				buffer = ' ';
			}
			if (peek == ' ' || peek == '\t')
				continue;
			else if (peek == '\r') {
				readch();
				line++;
			} else
				break;

		}

		switch (peek) {

		case '|':
			if (readch('|')) {
				buffer = ' ';
				return Word.or;
			} else {
				buffer = peek;
				return new Token('|');
			}
		case '=':
			if (readch('=')) {
				buffer = ' ';
				return Word.eq;
			} else {
				buffer = peek;
				return new Token('=');
			}
		case '<': {
			readch();
			if (peek == '=') {
				buffer = ' ';
				return Word.le;
			} else if (peek == '>') {
				buffer = ' ';
				return Word.ne;
			} else {
				buffer = peek;
				return new Token('<');
			}
		}
		case '>':
			if (readch('=')) {
				buffer = ' ';
				return Word.ge;
			} else {
				buffer = peek;
				return new Token('>');
			}
		case '&':
			if (readch('&')) {
				buffer = ' ';
				return Word.and;
			} else {
				buffer = peek;
				return new Token(peek);
			}
		}

		if (peek == '/') {
			if (readch('/')) {
				while (peek != '\r') {
					readch();
				}
				readch();
				line++;
				return Word.zs;
			} else {
				buffer = peek;
				return new Token((peek));
			}

		}
		if (Character.isDigit(peek)) {
			int v = 0;
			do {
				v = 10 * v + Character.digit(peek, 10);
				readch();
			} while (Character.isDigit(peek));
			if (peek != '.') {
				buffer = peek;
				return new Num(v);
			}
			float x = v;
			float d = 10;
			while (true) {
				readch();
				if (!Character.isDigit(peek))
					break;

				x = x + Character.digit(peek, 10) / d;
				d = d * 10;
			}
			buffer = peek;
			return new Real(x);
		}
		if (Character.isLetter(peek)) {
			StringBuffer b = new StringBuffer();
			do {
				b.append(peek);
				readch();
			} while (Character.isLetterOrDigit(peek));
			buffer = peek;
			String s = b.toString();
			Word w = (Word) words.get(s);
			if (w != null) {
				return w;
			}
			w = new Word(s, Tag.ID);
			words.put(s, w);
			return w;
		}
		Token tok = new Token(peek);
		peek = ' ';

		return tok;
	}

	/*
	 * public static void main(String[] args) {
	 * 
	 * 
	 * Lexer lexer = new Lexer(); try{while(true){
	 * 
	 * System.out.println(lexer.scan()); } }catch (Exception e2){}
	 * 
	 * }
	 */

}
