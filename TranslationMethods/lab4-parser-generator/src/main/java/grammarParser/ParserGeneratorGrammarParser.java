// Generated from java-escape by ANTLR 4.11.1
package grammarParser;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class ParserGeneratorGrammarParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, TYPE_ID=11, VAR_ID=12, STRING=13, INT=14, SK=15, CODE=16, WS=17;
	public static final int
		RULE_rules = 0, RULE_prefix = 1, RULE_myParserRule = 2, RULE_return = 3, 
		RULE_alternatives = 4, RULE_nonterm = 5, RULE_args = 6, RULE_types = 7, 
		RULE_type = 8, RULE_token = 9, RULE_label = 10, RULE_terminal = 11, RULE_tokens = 12, 
		RULE_myLexerRule = 13;
	private static String[] makeRuleNames() {
		return new String[] {
			"rules", "prefix", "myParserRule", "return", "alternatives", "nonterm", 
			"args", "types", "type", "token", "label", "terminal", "tokens", "myLexerRule"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'grammar'", "';'", "'@prefix'", "':'", "'returns'", "'|'", "'['", 
			"','", "']'", "'='"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, "TYPE_ID", 
			"VAR_ID", "STRING", "INT", "SK", "CODE", "WS"
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

	public ParserGeneratorGrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RulesContext extends ParserRuleContext {
		public TerminalNode TYPE_ID() { return getToken(ParserGeneratorGrammarParser.TYPE_ID, 0); }
		public PrefixContext prefix() {
			return getRuleContext(PrefixContext.class,0);
		}
		public List<MyParserRuleContext> myParserRule() {
			return getRuleContexts(MyParserRuleContext.class);
		}
		public MyParserRuleContext myParserRule(int i) {
			return getRuleContext(MyParserRuleContext.class,i);
		}
		public List<MyLexerRuleContext> myLexerRule() {
			return getRuleContexts(MyLexerRuleContext.class);
		}
		public MyLexerRuleContext myLexerRule(int i) {
			return getRuleContext(MyLexerRuleContext.class,i);
		}
		public RulesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rules; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterRules(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitRules(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitRules(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RulesContext rules() throws RecognitionException {
		RulesContext _localctx = new RulesContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_rules);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(28);
			match(T__0);
			setState(29);
			match(TYPE_ID);
			setState(30);
			match(T__1);
			setState(32);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__2) {
				{
				setState(31);
				prefix();
				}
			}

			setState(38);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==TYPE_ID || _la==VAR_ID) {
				{
				setState(36);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case VAR_ID:
					{
					setState(34);
					myParserRule();
					}
					break;
				case TYPE_ID:
					{
					setState(35);
					myLexerRule();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				setState(40);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
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
	public static class PrefixContext extends ParserRuleContext {
		public TerminalNode CODE() { return getToken(ParserGeneratorGrammarParser.CODE, 0); }
		public PrefixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prefix; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterPrefix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitPrefix(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitPrefix(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrefixContext prefix() throws RecognitionException {
		PrefixContext _localctx = new PrefixContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_prefix);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(41);
			match(T__2);
			setState(42);
			match(CODE);
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
	public static class MyParserRuleContext extends ParserRuleContext {
		public TerminalNode VAR_ID() { return getToken(ParserGeneratorGrammarParser.VAR_ID, 0); }
		public AlternativesContext alternatives() {
			return getRuleContext(AlternativesContext.class,0);
		}
		public TypesContext types() {
			return getRuleContext(TypesContext.class,0);
		}
		public ReturnContext return_() {
			return getRuleContext(ReturnContext.class,0);
		}
		public MyParserRuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_myParserRule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterMyParserRule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitMyParserRule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitMyParserRule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MyParserRuleContext myParserRule() throws RecognitionException {
		MyParserRuleContext _localctx = new MyParserRuleContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_myParserRule);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(44);
			match(VAR_ID);
			setState(46);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(45);
				types();
				}
			}

			setState(49);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__4) {
				{
				setState(48);
				return_();
				}
			}

			setState(51);
			match(T__3);
			setState(52);
			alternatives();
			setState(53);
			match(T__1);
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
	public static class ReturnContext extends ParserRuleContext {
		public TypesContext types() {
			return getRuleContext(TypesContext.class,0);
		}
		public ReturnContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_return; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterReturn(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitReturn(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitReturn(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ReturnContext return_() throws RecognitionException {
		ReturnContext _localctx = new ReturnContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_return);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(55);
			match(T__4);
			setState(56);
			types();
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
	public static class AlternativesContext extends ParserRuleContext {
		public List<TokensContext> tokens() {
			return getRuleContexts(TokensContext.class);
		}
		public TokensContext tokens(int i) {
			return getRuleContext(TokensContext.class,i);
		}
		public AlternativesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alternatives; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterAlternatives(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitAlternatives(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitAlternatives(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AlternativesContext alternatives() throws RecognitionException {
		AlternativesContext _localctx = new AlternativesContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_alternatives);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(58);
			tokens();
			setState(63);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__5) {
				{
				{
				setState(59);
				match(T__5);
				setState(60);
				tokens();
				}
				}
				setState(65);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
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
	public static class NontermContext extends ParserRuleContext {
		public TerminalNode VAR_ID() { return getToken(ParserGeneratorGrammarParser.VAR_ID, 0); }
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public ArgsContext args() {
			return getRuleContext(ArgsContext.class,0);
		}
		public NontermContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nonterm; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterNonterm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitNonterm(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitNonterm(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NontermContext nonterm() throws RecognitionException {
		NontermContext _localctx = new NontermContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_nonterm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(67);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				{
				setState(66);
				label();
				}
				break;
			}
			setState(69);
			match(VAR_ID);
			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(70);
				args();
				}
			}

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
	public static class ArgsContext extends ParserRuleContext {
		public List<TerminalNode> VAR_ID() { return getTokens(ParserGeneratorGrammarParser.VAR_ID); }
		public TerminalNode VAR_ID(int i) {
			return getToken(ParserGeneratorGrammarParser.VAR_ID, i);
		}
		public ArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_args; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitArgs(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitArgs(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgsContext args() throws RecognitionException {
		ArgsContext _localctx = new ArgsContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_args);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(73);
			match(T__6);
			{
			setState(74);
			match(VAR_ID);
			}
			setState(79);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(75);
				match(T__7);
				{
				setState(76);
				match(VAR_ID);
				}
				}
				}
				setState(81);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(82);
			match(T__8);
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
	public static class TypesContext extends ParserRuleContext {
		public List<LabelContext> label() {
			return getRuleContexts(LabelContext.class);
		}
		public LabelContext label(int i) {
			return getRuleContext(LabelContext.class,i);
		}
		public List<TerminalNode> TYPE_ID() { return getTokens(ParserGeneratorGrammarParser.TYPE_ID); }
		public TerminalNode TYPE_ID(int i) {
			return getToken(ParserGeneratorGrammarParser.TYPE_ID, i);
		}
		public TypesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_types; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterTypes(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitTypes(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitTypes(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypesContext types() throws RecognitionException {
		TypesContext _localctx = new TypesContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_types);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(84);
			match(T__6);
			setState(85);
			label();
			setState(86);
			match(TYPE_ID);
			setState(93);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(87);
				match(T__7);
				setState(88);
				label();
				setState(89);
				match(TYPE_ID);
				}
				}
				setState(95);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(96);
			match(T__8);
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
	public static class TypeContext extends ParserRuleContext {
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public TerminalNode TYPE_ID() { return getToken(ParserGeneratorGrammarParser.TYPE_ID, 0); }
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_type);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(98);
			match(T__6);
			setState(99);
			label();
			setState(100);
			match(TYPE_ID);
			setState(101);
			match(T__8);
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
	public static class TokenContext extends ParserRuleContext {
		public NontermContext nonterm() {
			return getRuleContext(NontermContext.class,0);
		}
		public TerminalContext terminal() {
			return getRuleContext(TerminalContext.class,0);
		}
		public TerminalNode CODE() { return getToken(ParserGeneratorGrammarParser.CODE, 0); }
		public TokenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_token; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterToken(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitToken(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitToken(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenContext token() throws RecognitionException {
		TokenContext _localctx = new TokenContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_token);
		try {
			setState(106);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(103);
				nonterm();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(104);
				terminal();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(105);
				match(CODE);
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
	public static class LabelContext extends ParserRuleContext {
		public TerminalNode VAR_ID() { return getToken(ParserGeneratorGrammarParser.VAR_ID, 0); }
		public LabelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_label; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterLabel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitLabel(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitLabel(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LabelContext label() throws RecognitionException {
		LabelContext _localctx = new LabelContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_label);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(108);
			match(VAR_ID);
			setState(109);
			match(T__9);
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
	public static class TerminalContext extends ParserRuleContext {
		public TerminalNode TYPE_ID() { return getToken(ParserGeneratorGrammarParser.TYPE_ID, 0); }
		public LabelContext label() {
			return getRuleContext(LabelContext.class,0);
		}
		public TerminalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_terminal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterTerminal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitTerminal(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitTerminal(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TerminalContext terminal() throws RecognitionException {
		TerminalContext _localctx = new TerminalContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_terminal);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==VAR_ID) {
				{
				setState(111);
				label();
				}
			}

			setState(114);
			match(TYPE_ID);
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
	public static class TokensContext extends ParserRuleContext {
		public List<TokenContext> token() {
			return getRuleContexts(TokenContext.class);
		}
		public TokenContext token(int i) {
			return getRuleContext(TokenContext.class,i);
		}
		public TokensContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokens; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterTokens(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitTokens(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitTokens(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokensContext tokens() throws RecognitionException {
		TokensContext _localctx = new TokensContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_tokens);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((_la) & ~0x3f) == 0 && ((1L << _la) & 71680L) != 0) {
				{
				{
				setState(116);
				token();
				}
				}
				setState(121);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
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
	public static class MyLexerRuleContext extends ParserRuleContext {
		public TerminalNode TYPE_ID() { return getToken(ParserGeneratorGrammarParser.TYPE_ID, 0); }
		public TerminalNode STRING() { return getToken(ParserGeneratorGrammarParser.STRING, 0); }
		public TerminalNode SK() { return getToken(ParserGeneratorGrammarParser.SK, 0); }
		public MyLexerRuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_myLexerRule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).enterMyLexerRule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof ParserGeneratorGrammarListener ) ((ParserGeneratorGrammarListener)listener).exitMyLexerRule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof ParserGeneratorGrammarVisitor ) return ((ParserGeneratorGrammarVisitor<? extends T>)visitor).visitMyLexerRule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MyLexerRuleContext myLexerRule() throws RecognitionException {
		MyLexerRuleContext _localctx = new MyLexerRuleContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_myLexerRule);
		try {
			setState(131);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(122);
				match(TYPE_ID);
				setState(123);
				match(T__3);
				setState(124);
				match(STRING);
				setState(125);
				match(SK);
				setState(126);
				match(T__1);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(127);
				match(TYPE_ID);
				setState(128);
				match(T__3);
				setState(129);
				match(STRING);
				setState(130);
				match(T__1);
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

	public static final String _serializedATN =
		"\u0004\u0001\u0011\u0086\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007"+
		"\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b"+
		"\u0002\f\u0007\f\u0002\r\u0007\r\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0003\u0000!\b\u0000\u0001\u0000\u0001\u0000\u0005\u0000%\b\u0000"+
		"\n\u0000\f\u0000(\t\u0000\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002"+
		"\u0001\u0002\u0003\u0002/\b\u0002\u0001\u0002\u0003\u00022\b\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0005\u0004>\b\u0004\n\u0004"+
		"\f\u0004A\t\u0004\u0001\u0005\u0003\u0005D\b\u0005\u0001\u0005\u0001\u0005"+
		"\u0003\u0005H\b\u0005\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0005\u0006N\b\u0006\n\u0006\f\u0006Q\t\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0005\u0007\\\b\u0007\n\u0007\f\u0007_\t\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\t\u0001\t\u0001"+
		"\t\u0003\tk\b\t\u0001\n\u0001\n\u0001\n\u0001\u000b\u0003\u000bq\b\u000b"+
		"\u0001\u000b\u0001\u000b\u0001\f\u0005\fv\b\f\n\f\f\fy\t\f\u0001\r\u0001"+
		"\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0003\r\u0084"+
		"\b\r\u0001\r\u0000\u0000\u000e\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010"+
		"\u0012\u0014\u0016\u0018\u001a\u0000\u0000\u0086\u0000\u001c\u0001\u0000"+
		"\u0000\u0000\u0002)\u0001\u0000\u0000\u0000\u0004,\u0001\u0000\u0000\u0000"+
		"\u00067\u0001\u0000\u0000\u0000\b:\u0001\u0000\u0000\u0000\nC\u0001\u0000"+
		"\u0000\u0000\fI\u0001\u0000\u0000\u0000\u000eT\u0001\u0000\u0000\u0000"+
		"\u0010b\u0001\u0000\u0000\u0000\u0012j\u0001\u0000\u0000\u0000\u0014l"+
		"\u0001\u0000\u0000\u0000\u0016p\u0001\u0000\u0000\u0000\u0018w\u0001\u0000"+
		"\u0000\u0000\u001a\u0083\u0001\u0000\u0000\u0000\u001c\u001d\u0005\u0001"+
		"\u0000\u0000\u001d\u001e\u0005\u000b\u0000\u0000\u001e \u0005\u0002\u0000"+
		"\u0000\u001f!\u0003\u0002\u0001\u0000 \u001f\u0001\u0000\u0000\u0000 "+
		"!\u0001\u0000\u0000\u0000!&\u0001\u0000\u0000\u0000\"%\u0003\u0004\u0002"+
		"\u0000#%\u0003\u001a\r\u0000$\"\u0001\u0000\u0000\u0000$#\u0001\u0000"+
		"\u0000\u0000%(\u0001\u0000\u0000\u0000&$\u0001\u0000\u0000\u0000&\'\u0001"+
		"\u0000\u0000\u0000\'\u0001\u0001\u0000\u0000\u0000(&\u0001\u0000\u0000"+
		"\u0000)*\u0005\u0003\u0000\u0000*+\u0005\u0010\u0000\u0000+\u0003\u0001"+
		"\u0000\u0000\u0000,.\u0005\f\u0000\u0000-/\u0003\u000e\u0007\u0000.-\u0001"+
		"\u0000\u0000\u0000./\u0001\u0000\u0000\u0000/1\u0001\u0000\u0000\u0000"+
		"02\u0003\u0006\u0003\u000010\u0001\u0000\u0000\u000012\u0001\u0000\u0000"+
		"\u000023\u0001\u0000\u0000\u000034\u0005\u0004\u0000\u000045\u0003\b\u0004"+
		"\u000056\u0005\u0002\u0000\u00006\u0005\u0001\u0000\u0000\u000078\u0005"+
		"\u0005\u0000\u000089\u0003\u000e\u0007\u00009\u0007\u0001\u0000\u0000"+
		"\u0000:?\u0003\u0018\f\u0000;<\u0005\u0006\u0000\u0000<>\u0003\u0018\f"+
		"\u0000=;\u0001\u0000\u0000\u0000>A\u0001\u0000\u0000\u0000?=\u0001\u0000"+
		"\u0000\u0000?@\u0001\u0000\u0000\u0000@\t\u0001\u0000\u0000\u0000A?\u0001"+
		"\u0000\u0000\u0000BD\u0003\u0014\n\u0000CB\u0001\u0000\u0000\u0000CD\u0001"+
		"\u0000\u0000\u0000DE\u0001\u0000\u0000\u0000EG\u0005\f\u0000\u0000FH\u0003"+
		"\f\u0006\u0000GF\u0001\u0000\u0000\u0000GH\u0001\u0000\u0000\u0000H\u000b"+
		"\u0001\u0000\u0000\u0000IJ\u0005\u0007\u0000\u0000JO\u0005\f\u0000\u0000"+
		"KL\u0005\b\u0000\u0000LN\u0005\f\u0000\u0000MK\u0001\u0000\u0000\u0000"+
		"NQ\u0001\u0000\u0000\u0000OM\u0001\u0000\u0000\u0000OP\u0001\u0000\u0000"+
		"\u0000PR\u0001\u0000\u0000\u0000QO\u0001\u0000\u0000\u0000RS\u0005\t\u0000"+
		"\u0000S\r\u0001\u0000\u0000\u0000TU\u0005\u0007\u0000\u0000UV\u0003\u0014"+
		"\n\u0000V]\u0005\u000b\u0000\u0000WX\u0005\b\u0000\u0000XY\u0003\u0014"+
		"\n\u0000YZ\u0005\u000b\u0000\u0000Z\\\u0001\u0000\u0000\u0000[W\u0001"+
		"\u0000\u0000\u0000\\_\u0001\u0000\u0000\u0000][\u0001\u0000\u0000\u0000"+
		"]^\u0001\u0000\u0000\u0000^`\u0001\u0000\u0000\u0000_]\u0001\u0000\u0000"+
		"\u0000`a\u0005\t\u0000\u0000a\u000f\u0001\u0000\u0000\u0000bc\u0005\u0007"+
		"\u0000\u0000cd\u0003\u0014\n\u0000de\u0005\u000b\u0000\u0000ef\u0005\t"+
		"\u0000\u0000f\u0011\u0001\u0000\u0000\u0000gk\u0003\n\u0005\u0000hk\u0003"+
		"\u0016\u000b\u0000ik\u0005\u0010\u0000\u0000jg\u0001\u0000\u0000\u0000"+
		"jh\u0001\u0000\u0000\u0000ji\u0001\u0000\u0000\u0000k\u0013\u0001\u0000"+
		"\u0000\u0000lm\u0005\f\u0000\u0000mn\u0005\n\u0000\u0000n\u0015\u0001"+
		"\u0000\u0000\u0000oq\u0003\u0014\n\u0000po\u0001\u0000\u0000\u0000pq\u0001"+
		"\u0000\u0000\u0000qr\u0001\u0000\u0000\u0000rs\u0005\u000b\u0000\u0000"+
		"s\u0017\u0001\u0000\u0000\u0000tv\u0003\u0012\t\u0000ut\u0001\u0000\u0000"+
		"\u0000vy\u0001\u0000\u0000\u0000wu\u0001\u0000\u0000\u0000wx\u0001\u0000"+
		"\u0000\u0000x\u0019\u0001\u0000\u0000\u0000yw\u0001\u0000\u0000\u0000"+
		"z{\u0005\u000b\u0000\u0000{|\u0005\u0004\u0000\u0000|}\u0005\r\u0000\u0000"+
		"}~\u0005\u000f\u0000\u0000~\u0084\u0005\u0002\u0000\u0000\u007f\u0080"+
		"\u0005\u000b\u0000\u0000\u0080\u0081\u0005\u0004\u0000\u0000\u0081\u0082"+
		"\u0005\r\u0000\u0000\u0082\u0084\u0005\u0002\u0000\u0000\u0083z\u0001"+
		"\u0000\u0000\u0000\u0083\u007f\u0001\u0000\u0000\u0000\u0084\u001b\u0001"+
		"\u0000\u0000\u0000\u000e $&.1?CGO]jpw\u0083";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}