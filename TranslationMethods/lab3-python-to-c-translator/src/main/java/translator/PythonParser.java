// Generated from java-escape by ANTLR 4.11.1
package translator;

    import java.util.*;
    import java.lang.*;
    import java.util.stream.Collectors;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class PythonParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, TRUE=22, FALSE=23, INT=24, ID=25, 
		NEWLINE=26, INDENT=27, DEDENT=28, WS=29;
	public static final int
		RULE_file = 0, RULE_stat = 1, RULE_stats = 2, RULE_block = 3, RULE_assign = 4, 
		RULE_expr = 5;
	private static String[] makeRuleNames() {
		return new String[] {
			"file", "stat", "stats", "block", "assign", "expr"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'='", "'int'", "'('", "'input'", "')'", "'print'", "'if'", "':'", 
			"'elif'", "'else'", "'while'", "'//'", "'*'", "'+'", "'-'", "'not'", 
			"'and'", "'or'", "'<'", "'>'", "'=='", "'True'", "'False'", null, null, 
			null, "'{'", "'}'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, "TRUE", "FALSE", 
			"INT", "ID", "NEWLINE", "INDENT", "DEDENT", "WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "java-escape"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }


	    public StringBuilder translated = new StringBuilder();
	    Map<String, String> memory = new HashMap<String, String>();

	public PythonParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FileContext extends ParserRuleContext {
		public String res;
		public StatsContext stats;
		public StatsContext stats() {
			return getRuleContext(StatsContext.class,0);
		}
		public TerminalNode EOF() { return getToken(PythonParser.EOF, 0); }
		public FileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_file; }
	}

	public final FileContext file() throws RecognitionException {
		FileContext _localctx = new FileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_file);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(12);
			((FileContext)_localctx).stats = stats();
			setState(13);
			match(EOF);

			String varDecls = memory.entrySet().stream().map(it -> it.getValue() + " " + it.getKey() + ";\n").collect(Collectors.joining());
			translated.append(String.format("""
			#include <stdio.h>
			#include <stdbool.h>
			%s
			int main() {
			%s
			    return 0;
			}
			""", varDecls, ((FileContext)_localctx).stats.res));

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StatContext extends ParserRuleContext {
		public String res;
		public Token ID;
		public ExprContext expr;
		public BlockContext block;
		public AssignContext assign;
		public TerminalNode ID() { return getToken(PythonParser.ID, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(PythonParser.NEWLINE, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public AssignContext assign() {
			return getRuleContext(AssignContext.class,0);
		}
		public StatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stat; }
	}

	public final StatContext stat() throws RecognitionException {
		StatContext _localctx = new StatContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_stat);
		try {
			setState(67);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,0,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(16);
				((StatContext)_localctx).ID = match(ID);
				setState(17);
				match(T__0);
				setState(18);
				match(T__1);
				setState(19);
				match(T__2);
				setState(20);
				match(T__3);
				setState(21);
				match(T__2);
				setState(22);
				match(T__4);
				setState(23);
				match(T__4);

				((StatContext)_localctx).res =  "scanf(\"%d\", &" + (((StatContext)_localctx).ID!=null?((StatContext)_localctx).ID.getText():null) + ");\n";
				memory.put((((StatContext)_localctx).ID!=null?((StatContext)_localctx).ID.getText():null), "int");

				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(25);
				match(T__5);
				setState(26);
				match(T__2);
				setState(27);
				((StatContext)_localctx).expr = expr(0);
				setState(28);
				match(T__4);
				 ((StatContext)_localctx).res =  "printf(\"%d\\n\", " + ((StatContext)_localctx).expr.res + ");\n"; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(31);
				match(T__6);
				setState(32);
				((StatContext)_localctx).expr = expr(0);
				setState(33);
				match(T__7);
				setState(34);
				match(NEWLINE);
				setState(35);
				((StatContext)_localctx).block = block();
				 ((StatContext)_localctx).res =  "if (" + ((StatContext)_localctx).expr.res + ")" + ((StatContext)_localctx).block.res; 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(38);
				match(T__8);
				setState(39);
				((StatContext)_localctx).expr = expr(0);
				setState(40);
				match(T__7);
				setState(41);
				match(NEWLINE);
				setState(42);
				((StatContext)_localctx).block = block();
				 ((StatContext)_localctx).res =  "else if (" + ((StatContext)_localctx).expr.res + ")" + ((StatContext)_localctx).block.res; 
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(45);
				match(T__9);
				setState(46);
				match(T__7);
				setState(47);
				match(NEWLINE);
				setState(48);
				((StatContext)_localctx).block = block();
				 ((StatContext)_localctx).res =  "else" + ((StatContext)_localctx).block.res; 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(51);
				match(T__10);
				setState(52);
				((StatContext)_localctx).expr = expr(0);
				setState(53);
				match(T__7);
				setState(54);
				match(NEWLINE);
				setState(55);
				((StatContext)_localctx).block = block();
				 ((StatContext)_localctx).res =  "while (" + ((StatContext)_localctx).expr.res + ")" + ((StatContext)_localctx).block.res; 
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(58);
				((StatContext)_localctx).assign = assign();
				setState(59);
				match(NEWLINE);
				 ((StatContext)_localctx).res =  ((StatContext)_localctx).assign.res + ";\n"; 
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(62);
				((StatContext)_localctx).expr = expr(0);
				setState(63);
				match(NEWLINE);
				 ((StatContext)_localctx).res =  ((StatContext)_localctx).expr.res + ";\n"; 
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(66);
				match(NEWLINE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StatsContext extends ParserRuleContext {
		public String res;
		public StatContext stat;
		public List<StatContext> sts = new ArrayList<StatContext>();
		public List<StatContext> stat() {
			return getRuleContexts(StatContext.class);
		}
		public StatContext stat(int i) {
			return getRuleContext(StatContext.class,i);
		}
		public StatsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stats; }
	}

	public final StatsContext stats() throws RecognitionException {
		StatsContext _localctx = new StatsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_stats);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(72);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 130092744L) != 0) {
				{
				{
				setState(69);
				((StatsContext)_localctx).stat = stat();
				((StatsContext)_localctx).sts.add(((StatsContext)_localctx).stat);
				}
				}
				setState(74);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}

			((StatsContext)_localctx).res =  ((StatsContext)_localctx).sts.stream().filter(it -> it.res != null)
			    .map(it -> it.res).collect(Collectors.joining());

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockContext extends ParserRuleContext {
		public String res;
		public StatsContext stats;
		public TerminalNode INDENT() { return getToken(PythonParser.INDENT, 0); }
		public StatsContext stats() {
			return getRuleContext(StatsContext.class,0);
		}
		public TerminalNode DEDENT() { return getToken(PythonParser.DEDENT, 0); }
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_block);
		try {
			setState(83);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case EOF:
			case T__2:
			case T__5:
			case T__6:
			case T__8:
			case T__9:
			case T__10:
			case T__15:
			case TRUE:
			case FALSE:
			case INT:
			case ID:
			case NEWLINE:
			case DEDENT:
				enterOuterAlt(_localctx, 1);
				{
				}
				break;
			case INDENT:
				enterOuterAlt(_localctx, 2);
				{
				setState(78);
				match(INDENT);
				setState(79);
				((BlockContext)_localctx).stats = stats();
				setState(80);
				match(DEDENT);
				 ((BlockContext)_localctx).res =  "{\n    " + ((BlockContext)_localctx).stats.res.replace(";\n", ";\n    ") + "}\n"; 
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AssignContext extends ParserRuleContext {
		public String res;
		public Token ID;
		public ExprContext expr;
		public TerminalNode ID() { return getToken(PythonParser.ID, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public AssignContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assign; }
	}

	public final AssignContext assign() throws RecognitionException {
		AssignContext _localctx = new AssignContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_assign);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(85);
			((AssignContext)_localctx).ID = match(ID);
			setState(86);
			match(T__0);
			setState(87);
			((AssignContext)_localctx).expr = expr(0);

			((AssignContext)_localctx).res =  (((AssignContext)_localctx).ID!=null?((AssignContext)_localctx).ID.getText():null) + "=" + ((AssignContext)_localctx).expr.res;
			memory.put((((AssignContext)_localctx).ID!=null?((AssignContext)_localctx).ID.getText():null), ((AssignContext)_localctx).expr.type);

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public String res;
		public String type;
		public ExprContext a;
		public ExprContext expr;
		public Token ID;
		public Token INT;
		public ExprContext b;
		public Token op;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode TRUE() { return getToken(PythonParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(PythonParser.FALSE, 0); }
		public TerminalNode ID() { return getToken(PythonParser.ID, 0); }
		public TerminalNode INT() { return getToken(PythonParser.INT, 0); }
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	}

	public final ExprContext expr() throws RecognitionException {
		return expr(0);
	}

	private ExprContext expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprContext _localctx = new ExprContext(_ctx, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 10;
		enterRecursionRule(_localctx, 10, RULE_expr, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(108);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__15:
				{
				setState(91);
				match(T__15);
				setState(92);
				((ExprContext)_localctx).expr = expr(9);
				 ((ExprContext)_localctx).res =  "!" + ((ExprContext)_localctx).expr.res; ((ExprContext)_localctx).type =  "bool"; 
				}
				break;
			case T__2:
				{
				setState(95);
				match(T__2);
				setState(96);
				((ExprContext)_localctx).expr = expr(0);
				setState(97);
				match(T__4);
				 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).expr.res; ((ExprContext)_localctx).type =  ((ExprContext)_localctx).expr.type; 
				}
				break;
			case TRUE:
				{
				setState(100);
				match(TRUE);
				 ((ExprContext)_localctx).res =  "true"; ((ExprContext)_localctx).type =  "bool"; 
				}
				break;
			case FALSE:
				{
				setState(102);
				match(FALSE);
				 ((ExprContext)_localctx).res =  "false"; ((ExprContext)_localctx).type =  "bool"; 
				}
				break;
			case ID:
				{
				setState(104);
				((ExprContext)_localctx).ID = match(ID);
				 ((ExprContext)_localctx).res =  (((ExprContext)_localctx).ID!=null?((ExprContext)_localctx).ID.getText():null); ((ExprContext)_localctx).type =  memory.get((((ExprContext)_localctx).ID!=null?((ExprContext)_localctx).ID.getText():null)); 
				}
				break;
			case INT:
				{
				setState(106);
				((ExprContext)_localctx).INT = match(INT);
				 ((ExprContext)_localctx).res =  (((ExprContext)_localctx).INT!=null?((ExprContext)_localctx).INT.getText():null); ((ExprContext)_localctx).type =  "int"; 
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(142);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(140);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
					case 1:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(110);
						if (!(precpred(_ctx, 12))) throw new FailedPredicateException(this, "precpred(_ctx, 12)");
						setState(111);
						match(T__11);
						setState(112);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(13);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + "/" + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "int"; 
						}
						break;
					case 2:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(115);
						if (!(precpred(_ctx, 11))) throw new FailedPredicateException(this, "precpred(_ctx, 11)");
						setState(116);
						match(T__12);
						setState(117);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(12);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + "*" + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "int"; 
						}
						break;
					case 3:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(120);
						if (!(precpred(_ctx, 10))) throw new FailedPredicateException(this, "precpred(_ctx, 10)");
						setState(121);
						((ExprContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !(_la==T__13 || _la==T__14) ) {
							((ExprContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(122);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(11);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + (((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null) + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "int"; 
						}
						break;
					case 4:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(125);
						if (!(precpred(_ctx, 8))) throw new FailedPredicateException(this, "precpred(_ctx, 8)");
						setState(126);
						match(T__16);
						setState(127);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(9);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + "&&" + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "bool"; 
						}
						break;
					case 5:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(130);
						if (!(precpred(_ctx, 7))) throw new FailedPredicateException(this, "precpred(_ctx, 7)");
						setState(131);
						match(T__17);
						setState(132);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(8);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + "||" + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "bool"; 
						}
						break;
					case 6:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.a = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(135);
						if (!(precpred(_ctx, 6))) throw new FailedPredicateException(this, "precpred(_ctx, 6)");
						setState(136);
						((ExprContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !(((_la) & ~0x3f) == 0 && ((1L << _la) & 3670016L) != 0) ) {
							((ExprContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(137);
						((ExprContext)_localctx).b = ((ExprContext)_localctx).expr = expr(7);
						 ((ExprContext)_localctx).res =  ((ExprContext)_localctx).a.res + (((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null) + ((ExprContext)_localctx).b.res; ((ExprContext)_localctx).type =  "bool"; 
						}
						break;
					}
					} 
				}
				setState(144);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 5:
			return expr_sempred((ExprContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 12);
		case 1:
			return precpred(_ctx, 11);
		case 2:
			return precpred(_ctx, 10);
		case 3:
			return precpred(_ctx, 8);
		case 4:
			return precpred(_ctx, 7);
		case 5:
			return precpred(_ctx, 6);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u001d\u0092\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0003\u0001D\b\u0001\u0001\u0002"+
		"\u0005\u0002G\b\u0002\n\u0002\f\u0002J\t\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0003\u0003T\b\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004"+
		"\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0003\u0005m\b\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0005\u0005\u008d\b\u0005\n\u0005"+
		"\f\u0005\u0090\t\u0005\u0001\u0005\u0000\u0001\n\u0006\u0000\u0002\u0004"+
		"\u0006\b\n\u0000\u0002\u0001\u0000\u000e\u000f\u0001\u0000\u0013\u0015"+
		"\u00a0\u0000\f\u0001\u0000\u0000\u0000\u0002C\u0001\u0000\u0000\u0000"+
		"\u0004H\u0001\u0000\u0000\u0000\u0006S\u0001\u0000\u0000\u0000\bU\u0001"+
		"\u0000\u0000\u0000\nl\u0001\u0000\u0000\u0000\f\r\u0003\u0004\u0002\u0000"+
		"\r\u000e\u0005\u0000\u0000\u0001\u000e\u000f\u0006\u0000\uffff\uffff\u0000"+
		"\u000f\u0001\u0001\u0000\u0000\u0000\u0010\u0011\u0005\u0019\u0000\u0000"+
		"\u0011\u0012\u0005\u0001\u0000\u0000\u0012\u0013\u0005\u0002\u0000\u0000"+
		"\u0013\u0014\u0005\u0003\u0000\u0000\u0014\u0015\u0005\u0004\u0000\u0000"+
		"\u0015\u0016\u0005\u0003\u0000\u0000\u0016\u0017\u0005\u0005\u0000\u0000"+
		"\u0017\u0018\u0005\u0005\u0000\u0000\u0018D\u0006\u0001\uffff\uffff\u0000"+
		"\u0019\u001a\u0005\u0006\u0000\u0000\u001a\u001b\u0005\u0003\u0000\u0000"+
		"\u001b\u001c\u0003\n\u0005\u0000\u001c\u001d\u0005\u0005\u0000\u0000\u001d"+
		"\u001e\u0006\u0001\uffff\uffff\u0000\u001eD\u0001\u0000\u0000\u0000\u001f"+
		" \u0005\u0007\u0000\u0000 !\u0003\n\u0005\u0000!\"\u0005\b\u0000\u0000"+
		"\"#\u0005\u001a\u0000\u0000#$\u0003\u0006\u0003\u0000$%\u0006\u0001\uffff"+
		"\uffff\u0000%D\u0001\u0000\u0000\u0000&\'\u0005\t\u0000\u0000\'(\u0003"+
		"\n\u0005\u0000()\u0005\b\u0000\u0000)*\u0005\u001a\u0000\u0000*+\u0003"+
		"\u0006\u0003\u0000+,\u0006\u0001\uffff\uffff\u0000,D\u0001\u0000\u0000"+
		"\u0000-.\u0005\n\u0000\u0000./\u0005\b\u0000\u0000/0\u0005\u001a\u0000"+
		"\u000001\u0003\u0006\u0003\u000012\u0006\u0001\uffff\uffff\u00002D\u0001"+
		"\u0000\u0000\u000034\u0005\u000b\u0000\u000045\u0003\n\u0005\u000056\u0005"+
		"\b\u0000\u000067\u0005\u001a\u0000\u000078\u0003\u0006\u0003\u000089\u0006"+
		"\u0001\uffff\uffff\u00009D\u0001\u0000\u0000\u0000:;\u0003\b\u0004\u0000"+
		";<\u0005\u001a\u0000\u0000<=\u0006\u0001\uffff\uffff\u0000=D\u0001\u0000"+
		"\u0000\u0000>?\u0003\n\u0005\u0000?@\u0005\u001a\u0000\u0000@A\u0006\u0001"+
		"\uffff\uffff\u0000AD\u0001\u0000\u0000\u0000BD\u0005\u001a\u0000\u0000"+
		"C\u0010\u0001\u0000\u0000\u0000C\u0019\u0001\u0000\u0000\u0000C\u001f"+
		"\u0001\u0000\u0000\u0000C&\u0001\u0000\u0000\u0000C-\u0001\u0000\u0000"+
		"\u0000C3\u0001\u0000\u0000\u0000C:\u0001\u0000\u0000\u0000C>\u0001\u0000"+
		"\u0000\u0000CB\u0001\u0000\u0000\u0000D\u0003\u0001\u0000\u0000\u0000"+
		"EG\u0003\u0002\u0001\u0000FE\u0001\u0000\u0000\u0000GJ\u0001\u0000\u0000"+
		"\u0000HF\u0001\u0000\u0000\u0000HI\u0001\u0000\u0000\u0000IK\u0001\u0000"+
		"\u0000\u0000JH\u0001\u0000\u0000\u0000KL\u0006\u0002\uffff\uffff\u0000"+
		"L\u0005\u0001\u0000\u0000\u0000MT\u0001\u0000\u0000\u0000NO\u0005\u001b"+
		"\u0000\u0000OP\u0003\u0004\u0002\u0000PQ\u0005\u001c\u0000\u0000QR\u0006"+
		"\u0003\uffff\uffff\u0000RT\u0001\u0000\u0000\u0000SM\u0001\u0000\u0000"+
		"\u0000SN\u0001\u0000\u0000\u0000T\u0007\u0001\u0000\u0000\u0000UV\u0005"+
		"\u0019\u0000\u0000VW\u0005\u0001\u0000\u0000WX\u0003\n\u0005\u0000XY\u0006"+
		"\u0004\uffff\uffff\u0000Y\t\u0001\u0000\u0000\u0000Z[\u0006\u0005\uffff"+
		"\uffff\u0000[\\\u0005\u0010\u0000\u0000\\]\u0003\n\u0005\t]^\u0006\u0005"+
		"\uffff\uffff\u0000^m\u0001\u0000\u0000\u0000_`\u0005\u0003\u0000\u0000"+
		"`a\u0003\n\u0005\u0000ab\u0005\u0005\u0000\u0000bc\u0006\u0005\uffff\uffff"+
		"\u0000cm\u0001\u0000\u0000\u0000de\u0005\u0016\u0000\u0000em\u0006\u0005"+
		"\uffff\uffff\u0000fg\u0005\u0017\u0000\u0000gm\u0006\u0005\uffff\uffff"+
		"\u0000hi\u0005\u0019\u0000\u0000im\u0006\u0005\uffff\uffff\u0000jk\u0005"+
		"\u0018\u0000\u0000km\u0006\u0005\uffff\uffff\u0000lZ\u0001\u0000\u0000"+
		"\u0000l_\u0001\u0000\u0000\u0000ld\u0001\u0000\u0000\u0000lf\u0001\u0000"+
		"\u0000\u0000lh\u0001\u0000\u0000\u0000lj\u0001\u0000\u0000\u0000m\u008e"+
		"\u0001\u0000\u0000\u0000no\n\f\u0000\u0000op\u0005\f\u0000\u0000pq\u0003"+
		"\n\u0005\rqr\u0006\u0005\uffff\uffff\u0000r\u008d\u0001\u0000\u0000\u0000"+
		"st\n\u000b\u0000\u0000tu\u0005\r\u0000\u0000uv\u0003\n\u0005\fvw\u0006"+
		"\u0005\uffff\uffff\u0000w\u008d\u0001\u0000\u0000\u0000xy\n\n\u0000\u0000"+
		"yz\u0007\u0000\u0000\u0000z{\u0003\n\u0005\u000b{|\u0006\u0005\uffff\uffff"+
		"\u0000|\u008d\u0001\u0000\u0000\u0000}~\n\b\u0000\u0000~\u007f\u0005\u0011"+
		"\u0000\u0000\u007f\u0080\u0003\n\u0005\t\u0080\u0081\u0006\u0005\uffff"+
		"\uffff\u0000\u0081\u008d\u0001\u0000\u0000\u0000\u0082\u0083\n\u0007\u0000"+
		"\u0000\u0083\u0084\u0005\u0012\u0000\u0000\u0084\u0085\u0003\n\u0005\b"+
		"\u0085\u0086\u0006\u0005\uffff\uffff\u0000\u0086\u008d\u0001\u0000\u0000"+
		"\u0000\u0087\u0088\n\u0006\u0000\u0000\u0088\u0089\u0007\u0001\u0000\u0000"+
		"\u0089\u008a\u0003\n\u0005\u0007\u008a\u008b\u0006\u0005\uffff\uffff\u0000"+
		"\u008b\u008d\u0001\u0000\u0000\u0000\u008cn\u0001\u0000\u0000\u0000\u008c"+
		"s\u0001\u0000\u0000\u0000\u008cx\u0001\u0000\u0000\u0000\u008c}\u0001"+
		"\u0000\u0000\u0000\u008c\u0082\u0001\u0000\u0000\u0000\u008c\u0087\u0001"+
		"\u0000\u0000\u0000\u008d\u0090\u0001\u0000\u0000\u0000\u008e\u008c\u0001"+
		"\u0000\u0000\u0000\u008e\u008f\u0001\u0000\u0000\u0000\u008f\u000b\u0001"+
		"\u0000\u0000\u0000\u0090\u008e\u0001\u0000\u0000\u0000\u0006CHSl\u008c"+
		"\u008e";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}