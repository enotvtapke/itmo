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
public class PythonPreprocessorParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ANY=1, NEWLINE=2, IDENT=3;
	public static final int
		RULE_file = 0, RULE_stat = 1, RULE_idents = 2;
	private static String[] makeRuleNames() {
		return new String[] {
			"file", "stat", "idents"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ANY", "NEWLINE", "IDENT"
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
	    Set<String> memory = new HashSet<String>();
	    Integer ident = 0;

	public PythonPreprocessorParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FileContext extends ParserRuleContext {
		public String res;
		public StatContext stat;
		public List<StatContext> stats = new ArrayList<StatContext>();
		public TerminalNode EOF() { return getToken(PythonPreprocessorParser.EOF, 0); }
		public List<StatContext> stat() {
			return getRuleContexts(StatContext.class);
		}
		public StatContext stat(int i) {
			return getRuleContext(StatContext.class,i);
		}
		public FileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_file; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).enterFile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).exitFile(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof PythonPreprocessorVisitor ) return ((PythonPreprocessorVisitor<? extends T>)visitor).visitFile(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FileContext file() throws RecognitionException {
		FileContext _localctx = new FileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_file);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(9);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ANY || _la==IDENT) {
				{
				{
				setState(6);
				((FileContext)_localctx).stat = stat();
				((FileContext)_localctx).stats.add(((FileContext)_localctx).stat);
				}
				}
				setState(11);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(12);
			match(EOF);

			String stats = ((FileContext)_localctx).stats.stream().map(it -> it.res).collect(Collectors.joining());
			translated.append(stats);

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
		public IdentsContext idents;
		public Token ANY;
		public Token NEWLINE;
		public IdentsContext idents() {
			return getRuleContext(IdentsContext.class,0);
		}
		public TerminalNode ANY() { return getToken(PythonPreprocessorParser.ANY, 0); }
		public TerminalNode NEWLINE() { return getToken(PythonPreprocessorParser.NEWLINE, 0); }
		public StatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stat; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).enterStat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).exitStat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof PythonPreprocessorVisitor ) return ((PythonPreprocessorVisitor<? extends T>)visitor).visitStat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatContext stat() throws RecognitionException {
		StatContext _localctx = new StatContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_stat);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(15);
			((StatContext)_localctx).idents = idents();
			setState(16);
			((StatContext)_localctx).ANY = match(ANY);
			setState(17);
			((StatContext)_localctx).NEWLINE = match(NEWLINE);

			int curIdent = ((StatContext)_localctx).idents.count;
			if (curIdent > ident) {
			    ((StatContext)_localctx).res =  "{" + (((StatContext)_localctx).ANY!=null?((StatContext)_localctx).ANY.getText():null) + (((StatContext)_localctx).NEWLINE!=null?((StatContext)_localctx).NEWLINE.getText():null);
			} else if (curIdent < ident) {
			    ((StatContext)_localctx).res =  "}".repeat(ident - curIdent) + (((StatContext)_localctx).ANY!=null?((StatContext)_localctx).ANY.getText():null) + (((StatContext)_localctx).NEWLINE!=null?((StatContext)_localctx).NEWLINE.getText():null);
			} else {
			    ((StatContext)_localctx).res =  (((StatContext)_localctx).ANY!=null?((StatContext)_localctx).ANY.getText():null) + (((StatContext)_localctx).NEWLINE!=null?((StatContext)_localctx).NEWLINE.getText():null);
			}
			ident = curIdent;

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
	public static class IdentsContext extends ParserRuleContext {
		public Integer count;
		public int i = 0;
		public List<TerminalNode> IDENT() { return getTokens(PythonPreprocessorParser.IDENT); }
		public TerminalNode IDENT(int i) {
			return getToken(PythonPreprocessorParser.IDENT, i);
		}
		public IdentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_idents; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).enterIdents(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof PythonPreprocessorListener ) ((PythonPreprocessorListener)listener).exitIdents(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof PythonPreprocessorVisitor ) return ((PythonPreprocessorVisitor<? extends T>)visitor).visitIdents(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentsContext idents() throws RecognitionException {
		IdentsContext _localctx = new IdentsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_idents);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(24);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==IDENT) {
				{
				{
				setState(20);
				match(IDENT);
				_localctx.i += 1;
				}
				}
				setState(26);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			 ((IdentsContext)_localctx).count =  _localctx.i; 
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

	public static final String _serializedATN =
		"\u0004\u0001\u0003\u001e\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0001\u0000\u0005\u0000\b\b\u0000\n\u0000\f\u0000"+
		"\u000b\t\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0005\u0002"+
		"\u0017\b\u0002\n\u0002\f\u0002\u001a\t\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0000\u0000\u0003\u0000\u0002\u0004\u0000\u0000\u001c\u0000\t\u0001"+
		"\u0000\u0000\u0000\u0002\u000f\u0001\u0000\u0000\u0000\u0004\u0018\u0001"+
		"\u0000\u0000\u0000\u0006\b\u0003\u0002\u0001\u0000\u0007\u0006\u0001\u0000"+
		"\u0000\u0000\b\u000b\u0001\u0000\u0000\u0000\t\u0007\u0001\u0000\u0000"+
		"\u0000\t\n\u0001\u0000\u0000\u0000\n\f\u0001\u0000\u0000\u0000\u000b\t"+
		"\u0001\u0000\u0000\u0000\f\r\u0005\u0000\u0000\u0001\r\u000e\u0006\u0000"+
		"\uffff\uffff\u0000\u000e\u0001\u0001\u0000\u0000\u0000\u000f\u0010\u0003"+
		"\u0004\u0002\u0000\u0010\u0011\u0005\u0001\u0000\u0000\u0011\u0012\u0005"+
		"\u0002\u0000\u0000\u0012\u0013\u0006\u0001\uffff\uffff\u0000\u0013\u0003"+
		"\u0001\u0000\u0000\u0000\u0014\u0015\u0005\u0003\u0000\u0000\u0015\u0017"+
		"\u0006\u0002\uffff\uffff\u0000\u0016\u0014\u0001\u0000\u0000\u0000\u0017"+
		"\u001a\u0001\u0000\u0000\u0000\u0018\u0016\u0001\u0000\u0000\u0000\u0018"+
		"\u0019\u0001\u0000\u0000\u0000\u0019\u001b\u0001\u0000\u0000\u0000\u001a"+
		"\u0018\u0001\u0000\u0000\u0000\u001b\u001c\u0006\u0002\uffff\uffff\u0000"+
		"\u001c\u0005\u0001\u0000\u0000\u0000\u0002\t\u0018";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}