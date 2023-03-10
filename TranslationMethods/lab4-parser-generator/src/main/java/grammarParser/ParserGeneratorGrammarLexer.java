// Generated from java-escape by ANTLR 4.11.1
package grammarParser;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class ParserGeneratorGrammarLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.11.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, TYPE_ID=11, VAR_ID=12, STRING=13, INT=14, SK=15, CODE=16, WS=17;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
			"T__9", "TYPE_ID", "VAR_ID", "STRING", "INT", "SK", "CODE", "WS"
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


	public ParserGeneratorGrammarLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ParserGeneratorGrammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\u0011\u0090\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002"+
		"\u0001\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002"+
		"\u0004\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002"+
		"\u0007\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002"+
		"\u000b\u0007\u000b\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e"+
		"\u0002\u000f\u0007\u000f\u0002\u0010\u0007\u0010\u0001\u0000\u0001\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000"+
		"\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003"+
		"\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004"+
		"\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006"+
		"\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001\t\u0001\t\u0001\n\u0001"+
		"\n\u0005\nL\b\n\n\n\f\nO\t\n\u0001\u000b\u0001\u000b\u0005\u000bS\b\u000b"+
		"\n\u000b\f\u000bV\t\u000b\u0001\f\u0001\f\u0001\f\u0001\f\u0005\f\\\b"+
		"\f\n\f\f\f_\t\f\u0001\f\u0001\f\u0001\r\u0001\r\u0003\re\b\r\u0001\r\u0004"+
		"\rh\b\r\u000b\r\f\ri\u0001\u000e\u0001\u000e\u0001\u000e\u0001\u000e\u0005"+
		"\u000ep\b\u000e\n\u000e\f\u000es\t\u000e\u0001\u000e\u0001\u000e\u0001"+
		"\u000e\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0004\u000f|\b"+
		"\u000f\u000b\u000f\f\u000f}\u0001\u000f\u0003\u000f\u0081\b\u000f\u0005"+
		"\u000f\u0083\b\u000f\n\u000f\f\u000f\u0086\t\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u0010\u0004\u0010\u008b\b\u0010\u000b\u0010\f\u0010\u008c\u0001"+
		"\u0010\u0001\u0010\u0000\u0000\u0011\u0001\u0001\u0003\u0002\u0005\u0003"+
		"\u0007\u0004\t\u0005\u000b\u0006\r\u0007\u000f\b\u0011\t\u0013\n\u0015"+
		"\u000b\u0017\f\u0019\r\u001b\u000e\u001d\u000f\u001f\u0010!\u0011\u0001"+
		"\u0000\b\u0001\u0000AZ\u0004\u000009AZ__az\u0001\u0000az\u0003\u0000\n"+
		"\n\r\r\'\'\u0002\u0000++--\u0001\u000009\u0002\u0000{{}}\u0003\u0000\t"+
		"\n\r\r  \u009a\u0000\u0001\u0001\u0000\u0000\u0000\u0000\u0003\u0001\u0000"+
		"\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000\u0007\u0001\u0000"+
		"\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u000b\u0001\u0000\u0000"+
		"\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f\u0001\u0000\u0000\u0000"+
		"\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013\u0001\u0000\u0000\u0000"+
		"\u0000\u0015\u0001\u0000\u0000\u0000\u0000\u0017\u0001\u0000\u0000\u0000"+
		"\u0000\u0019\u0001\u0000\u0000\u0000\u0000\u001b\u0001\u0000\u0000\u0000"+
		"\u0000\u001d\u0001\u0000\u0000\u0000\u0000\u001f\u0001\u0000\u0000\u0000"+
		"\u0000!\u0001\u0000\u0000\u0000\u0001#\u0001\u0000\u0000\u0000\u0003+"+
		"\u0001\u0000\u0000\u0000\u0005-\u0001\u0000\u0000\u0000\u00075\u0001\u0000"+
		"\u0000\u0000\t7\u0001\u0000\u0000\u0000\u000b?\u0001\u0000\u0000\u0000"+
		"\rA\u0001\u0000\u0000\u0000\u000fC\u0001\u0000\u0000\u0000\u0011E\u0001"+
		"\u0000\u0000\u0000\u0013G\u0001\u0000\u0000\u0000\u0015I\u0001\u0000\u0000"+
		"\u0000\u0017P\u0001\u0000\u0000\u0000\u0019W\u0001\u0000\u0000\u0000\u001b"+
		"d\u0001\u0000\u0000\u0000\u001dk\u0001\u0000\u0000\u0000\u001fy\u0001"+
		"\u0000\u0000\u0000!\u008a\u0001\u0000\u0000\u0000#$\u0005g\u0000\u0000"+
		"$%\u0005r\u0000\u0000%&\u0005a\u0000\u0000&\'\u0005m\u0000\u0000\'(\u0005"+
		"m\u0000\u0000()\u0005a\u0000\u0000)*\u0005r\u0000\u0000*\u0002\u0001\u0000"+
		"\u0000\u0000+,\u0005;\u0000\u0000,\u0004\u0001\u0000\u0000\u0000-.\u0005"+
		"@\u0000\u0000./\u0005p\u0000\u0000/0\u0005r\u0000\u000001\u0005e\u0000"+
		"\u000012\u0005f\u0000\u000023\u0005i\u0000\u000034\u0005x\u0000\u0000"+
		"4\u0006\u0001\u0000\u0000\u000056\u0005:\u0000\u00006\b\u0001\u0000\u0000"+
		"\u000078\u0005r\u0000\u000089\u0005e\u0000\u00009:\u0005t\u0000\u0000"+
		":;\u0005u\u0000\u0000;<\u0005r\u0000\u0000<=\u0005n\u0000\u0000=>\u0005"+
		"s\u0000\u0000>\n\u0001\u0000\u0000\u0000?@\u0005|\u0000\u0000@\f\u0001"+
		"\u0000\u0000\u0000AB\u0005[\u0000\u0000B\u000e\u0001\u0000\u0000\u0000"+
		"CD\u0005,\u0000\u0000D\u0010\u0001\u0000\u0000\u0000EF\u0005]\u0000\u0000"+
		"F\u0012\u0001\u0000\u0000\u0000GH\u0005=\u0000\u0000H\u0014\u0001\u0000"+
		"\u0000\u0000IM\u0007\u0000\u0000\u0000JL\u0007\u0001\u0000\u0000KJ\u0001"+
		"\u0000\u0000\u0000LO\u0001\u0000\u0000\u0000MK\u0001\u0000\u0000\u0000"+
		"MN\u0001\u0000\u0000\u0000N\u0016\u0001\u0000\u0000\u0000OM\u0001\u0000"+
		"\u0000\u0000PT\u0007\u0002\u0000\u0000QS\u0007\u0001\u0000\u0000RQ\u0001"+
		"\u0000\u0000\u0000SV\u0001\u0000\u0000\u0000TR\u0001\u0000\u0000\u0000"+
		"TU\u0001\u0000\u0000\u0000U\u0018\u0001\u0000\u0000\u0000VT\u0001\u0000"+
		"\u0000\u0000W]\u0005\'\u0000\u0000X\\\b\u0003\u0000\u0000YZ\u0005\\\u0000"+
		"\u0000Z\\\u0005\'\u0000\u0000[X\u0001\u0000\u0000\u0000[Y\u0001\u0000"+
		"\u0000\u0000\\_\u0001\u0000\u0000\u0000][\u0001\u0000\u0000\u0000]^\u0001"+
		"\u0000\u0000\u0000^`\u0001\u0000\u0000\u0000_]\u0001\u0000\u0000\u0000"+
		"`a\u0005\'\u0000\u0000a\u001a\u0001\u0000\u0000\u0000be\u0007\u0004\u0000"+
		"\u0000ce\u0001\u0000\u0000\u0000db\u0001\u0000\u0000\u0000dc\u0001\u0000"+
		"\u0000\u0000eg\u0001\u0000\u0000\u0000fh\u0007\u0005\u0000\u0000gf\u0001"+
		"\u0000\u0000\u0000hi\u0001\u0000\u0000\u0000ig\u0001\u0000\u0000\u0000"+
		"ij\u0001\u0000\u0000\u0000j\u001c\u0001\u0000\u0000\u0000kl\u0005-\u0000"+
		"\u0000lm\u0005>\u0000\u0000mq\u0001\u0000\u0000\u0000np\u0003!\u0010\u0000"+
		"on\u0001\u0000\u0000\u0000ps\u0001\u0000\u0000\u0000qo\u0001\u0000\u0000"+
		"\u0000qr\u0001\u0000\u0000\u0000rt\u0001\u0000\u0000\u0000sq\u0001\u0000"+
		"\u0000\u0000tu\u0005s\u0000\u0000uv\u0005k\u0000\u0000vw\u0005i\u0000"+
		"\u0000wx\u0005p\u0000\u0000x\u001e\u0001\u0000\u0000\u0000y\u0084\u0005"+
		"{\u0000\u0000z|\b\u0006\u0000\u0000{z\u0001\u0000\u0000\u0000|}\u0001"+
		"\u0000\u0000\u0000}{\u0001\u0000\u0000\u0000}~\u0001\u0000\u0000\u0000"+
		"~\u0080\u0001\u0000\u0000\u0000\u007f\u0081\u0003\u001f\u000f\u0000\u0080"+
		"\u007f\u0001\u0000\u0000\u0000\u0080\u0081\u0001\u0000\u0000\u0000\u0081"+
		"\u0083\u0001\u0000\u0000\u0000\u0082{\u0001\u0000\u0000\u0000\u0083\u0086"+
		"\u0001\u0000\u0000\u0000\u0084\u0082\u0001\u0000\u0000\u0000\u0084\u0085"+
		"\u0001\u0000\u0000\u0000\u0085\u0087\u0001\u0000\u0000\u0000\u0086\u0084"+
		"\u0001\u0000\u0000\u0000\u0087\u0088\u0005}\u0000\u0000\u0088 \u0001\u0000"+
		"\u0000\u0000\u0089\u008b\u0007\u0007\u0000\u0000\u008a\u0089\u0001\u0000"+
		"\u0000\u0000\u008b\u008c\u0001\u0000\u0000\u0000\u008c\u008a\u0001\u0000"+
		"\u0000\u0000\u008c\u008d\u0001\u0000\u0000\u0000\u008d\u008e\u0001\u0000"+
		"\u0000\u0000\u008e\u008f\u0006\u0010\u0000\u0000\u008f\"\u0001\u0000\u0000"+
		"\u0000\f\u0000MT[]diq}\u0080\u0084\u008c\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}