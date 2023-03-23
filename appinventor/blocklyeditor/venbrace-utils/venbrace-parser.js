'use strict';

goog.provide('Blockly.VenbraceParser');

goog.require('Blockly.parserCombinator');

Blockly.VenbraceParser = {};

/**
 * Parses the contents of a code block. Returns an object with 
 *  
 *    parses: The parse trees
 *  
 *    success: True if parsing was successful
 *  
 *    string: The string that was parsed (the contents of the code block)
 *  
 *    errorAt: The index of the error. -1 if it was successful.
 */
Blockly.VenbraceParser.parseCodeBlock = function(codeBlock) {
    var str = codeBlock.getFieldValue('CODE');
    var parses = Blockly.VenbraceParser.parse(str,codeBlock.type).map(function(result) {
        return result[0]; //result = [parseTree,""], we want to discard the ""
    });
    var success = parses.length > 0;
    var errorAt = success ? -1 : (str.length + ("<EOF>".length)) - Blockly.parserCombinator.errorAt.length // Blockly.parserCombinator.errorAt is the string left over when the error happens
    var parseObj = {
        "parses":parses,
        "success":success,
        "string":str,
        "errorAt":errorAt
    };
    return parseObj;
}

/**
 * Parses a string based on whether it comes from a block of type code_expr, code_stmt, or code_decl.
 */
Blockly.VenbraceParser.parse = function(str,type) {

    var pc = Blockly.parserCombinator;

    // ****** BASIC COMBINATORS ****** //
    var Parser = pc.Parser,
        character = pc.character,
        item = pc.item,
        sat = pc.sat,
        manyStar = pc.manyStar,
        manyPlus = pc.manyPlus,
        keyword = pc.keyword,
        digit = pc.digit,
        letter = pc.letter,
        alphanum = pc.alphanum,
        spaces = pc.spaces,
        space = pc.space,
        result = pc.result,
        zero = pc.zero,
        comprehension = pc.comprehension,
        reShift = pc.reShift
    ;

/** 
 * A Parser that consumes nothing.
 */
var identity = result([]);

function opt(p) {
    return p.or(identity);
};

var optspaces = spaces.or(identity);

var separator = sat(function(x) {
    // the / was previously before the ^ which broke everything for some reason
    return "(){}[]+-/*^'\".<".indexOf(x) >= 0; // all available App Inventor symbols
}).bind(function(x) {
    return reShift(x);
});

// ******************************************************************************** //
// ****************************** KEYWORDS & SYMBOLS ****************************** //
// ******************************************************************************** //

/**
 * Takes a list of strings, turns it into a parser for a sequence 
 * of keywords separated by spaces.
 */
var multiWordKeyword = function(args) {
    return args.length === 1 ? keyword(args[0]) : keyword(args[0]).seq(spaces).seq(multiWordKeyword(args.slice(1)))
}

/**
 * Creates a token from a string or list of strings (in the case of multi word keywords) 
 * which are followed by spaces. The presence of at least one space is required if spOpt is false.
 */
var tok = function(t,spOpt) {
    var kw = typeof(t) === "string" ? keyword(t) : multiWordKeyword(t);
    return spOpt ? kw.seq(optspaces) : kw.seq(spaces.or(separator));
}


var sym = function(t) { return tok(t,true); }

/* ~~ Keywords ~~ */

var KEYWORD_STR = {
    MIN: "min",
    MAX: "max",
    NEG: "neg",
    SQRT: ["square","root"],
    SET: "set",
    TO: "to",
    GET: "get",
    LIST: "list",
    MAKE_A_LIST: ["make","a","list"],
    CREATE_EMPTY_LIST: ["create","empty","list"],
    IF: "if",
    THEN: "then",
    ELSE: "else",
    ELSE_IF: ["else","if"],
    TRUE: "true",
    FALSE: "false",
    NOT: "not",
    WHEN: "when",
    DO: "do",
    IS_LIST_EMPTY: ["is","list","empty?"],
    CALL: "call",
    CLICK: ".Click",  // will probably want to separate the dot
    CHANGED: ".Changed",
    VISIBLE: ".Visible",
    RADIUS: ".Radius",
    SPEED: ".Speed",
    ON: ".On",
    ENABLED: ".Enabled",
    CHECKED: ".Checked",
    TEXT: ".Text"
};

// The tokenized version of the keywords - i.e. keyword parsers
var KEY = Object.fromEntries(
    Object.keys(KEYWORD_STR).map(function(k) {
        return [k,tok(KEYWORD_STR[k])]
    })
)

function isNotReserved(s) {
    var reserved = Object.values(KEYWORD_STR);
    return reserved.indexOf(s) >= 0 ? false : true;
}

/* ~~ Braces ~~ */

var LBRACE = {};
LBRACE["("] = sym("(");
LBRACE["{"] = sym("{");
LBRACE["["] = sym("[");

var RBRACE = {};
RBRACE["("] = sym(")");
RBRACE["{"] = sym("}");
RBRACE["["] = sym("]");

var emptySlot = LBRACE["("].or(LBRACE["{"]).or(LBRACE["["]).bind(function(lbrace) {
    return RBRACE[lbrace].bind(function(rbrace) {
        return result(["emptySlot"]);
    })
})

/**
 * A node surrounded by one or more pairs of matching braces of any kind.
 */
var bracedNode = function(nodeParser) {
    return LBRACE["("].or(LBRACE["{"]).or(LBRACE["["]).bind(function(lbrace) {
        return nodeParser.or(bracedNode(nodeParser)).bind(function(node) {
            return RBRACE[lbrace].bind(function(rbrace) {
                return rbrace.length != 0 ? result(node) : zero;
            });
        });
    });
};

/**
 * A node surrounded by zero or more braces.
 */
var lenientNode = function(nodeParser) {
    return nodeParser.or(bracedNode(nodeParser));
}

/* ~~ Symbols ~~ */

var QUOTE = {};
QUOTE['"'] = sym('"');
QUOTE["'"] = sym("'");

var OP = {
    PLUS: sym("+"),
    MINUS: sym("-"),
    TIMES: sym("*"),
    DIVIDE: sym("/"),
    POWER: sym("^")
};

var EOF = keyword("<EOF>");


/* ~~ Literals ~~ */

var lit = function(p) {
    return p.seq(spaces.or(separator))
};

var LIT = {
    VARNAME: lit(
        letter.seq(manyStar(alphanum,true))).first().bind(function(v) {
            return isNotReserved(v) ? result(v) : zero;
        }
    ), // also works for components and function calls

    NUMBER: lit(manyPlus(digit,true).first()),
};

// ************************************************************************* //
// ****************************** EXPRESSIONS ****************************** //
// ************************************************************************* //

/* ~~ Variables, Components, & Procedure Calls ~~ */

var getVar = comprehension(
    opt(KEY.GET),
    LIT.VARNAME,
    function(_,varName) {
        return ['variable get', varName];
    }
);

// can be expr or stmt // TODO - separate expr and stmt!!
var procedureCallExpr = KEY.CALL.bind(function(_) {
    return comprehension(LIT.VARNAME, manyStar(expr), function(fname, args) {
        return ["call expr",fname,args];
    })
}); 

var logicProperty = KEY.CHECKED
    .or(KEY.ENABLED)
    .or(KEY.ON)
    .or(KEY.VISIBLE);

var mathProperty = KEY.RADIUS
    .or(KEY.SPEED)
    .or(KEY.TEXT);

var stringProperty = KEY.TEXT;

function getComponent(properties) {
    return comprehension(
        LIT.VARNAME,
        properties,
        function(component,property) {
            return ["component get",component,property];
        }
    );
}

var logicPropertyGetter = getComponent(logicProperty);
var mathPropertyGetter = getComponent(mathProperty);
var stringPropertyGetter = getComponent(stringProperty);


// vars and procedure calls can be any type, so should be allowed in any type of expression
function includeVars(p) {
    return p.or(procedureCallExpr).or(getVar).or(emptySlot);
}

/* ~~ Strings ~~ */

var string = QUOTE["'"].or(QUOTE['"']).bind(function(quoteMark) {
    return manyStar(sat(function(s) {return s != quoteMark}),true).bind(function(str) {
        return QUOTE[quoteMark].bind(function(_) {
            return result(["str",str]);
        });
    });
});

var stringExprTop = string.or(stringPropertyGetter);

var stringExpr = lenientNode(includeVars(stringExprTop));


/* ~~ Lists ~~ */

var createListExpr = KEY.LIST.or(KEY.CREATE_EMPTY_LIST).or(KEY.MAKE_A_LIST).bind(function(prefix) {
    var argparser;
    if (prefix === "list") {    // "list" can have any number of elements
        argparser = optExprs;
    } else if (prefix === "makealist") {  // "make a list" must have at least one element
        argparser = exprs;
    } else {                    // "create empty list" has no elements
        argparser = identity;
    }
    return argparser.bind(function(elts) {
        return result(["list"].concat(elts));
    });
});


var isListEmpty = KEY.IS_LIST_EMPTY.bind(function() {
    return listExpr.bind(function(ls) {
        return result(["isListEmpty", ls]);
    });
});

var listExprTop = createListExpr;

var listExpr = lenientNode(includeVars(listExprTop));

/* ~~ Logic ~~ */

var bool = KEY.TRUE.or(KEY.FALSE);

var notExpr = KEY.NOT.bind(function(_) {
    return logicExpr.bind(function(x) {
        return result(["not",x]);
    });
});

var logicExprTop = bool
    .or(notExpr)
    .or(isListEmpty)
    .or(logicPropertyGetter);

var logicExpr = lenientNode(includeVars(logicExprTop));

/* ~~ Math ~~ */

var negExpr = KEY.NEG.bind(function(_) {
    return mathExpr.bind(function(e) {
        return result(["neg",e])
    })
})

var sqrtExpr = KEY.SQRT.bind(function(_) {
    return mathExpr.bind(function(e) {
        return result(["sqrt",e]);
    })
})

var minMaxExpr = (KEY.MIN.or(KEY.MAX)).bind(function(op) {
    // having two optional expressions before manyStar forces it to take at least 2 arguments if it can
    return comprehension(
        opt(mathExpr), 
        opt(mathExpr), 
        manyStar(mathExpr),
        function(x1,x2,xs) {
            var args = x1.length > 0 ? [].concat([x1]) : [];
            args = x2.length > 0 ? args.concat([x2]) : args;
            args = xs.length > 0 ? args.concat(xs) : args;
            return args 
        }
    ).bind(function(args) {
        return result([op].concat(args));
    });
});

// leftAssoc(4,[['*',3],['/',2]]) => ['/',['*',4,3],2]
function leftAssoc(first,rest) {
    if (rest.length == 0) {
        return first;
    } 
    else {
        var op = rest[0][0];
        var r2 = rest[0][1];
        return leftAssoc([op, first, r2], rest.slice(1));
    }
}

// right associative
var expo = identity.bind(function(_) {
    return comprehension(
        prefixMathExpr.or(bracedNode(mathExpr)),
        OP.POWER,
        expo,
        function(r1,op,r2) {return [op,r1,r2]}
    ).or(prefixMathExpr.or(bracedNode(mathExpr)));
})

// Roughly: 
// T := A * A | A / A | A
// A := Expo | (E)
var term = identity.bind(function(_) {
    return comprehension(
        expo, // first
        manyStar(OP.TIMES.or(OP.DIVIDE).bind(function(op) { // rest
            return expo.bind(function(r2) {
                return result([op, r2])
            })
        })),
        leftAssoc
    )
})

// Roughly: 
// E := T + T | T - T | T | (E) | E
var mathExpr = identity.bind(function(_) {
    return comprehension(
            term,
            manyStar(OP.PLUS.or(OP.MINUS).bind(function(op) {
                return term.bind(function(r2) {
                    return result([op, r2])
                })
            })),
            leftAssoc
        ).or(bracedNode(mathExpr))
})

var prefixMathExpr = lenientNode(includeVars(
    LIT.NUMBER
    .or(negExpr)
    .or(sqrtExpr)
    .or(minMaxExpr)
    .or(mathPropertyGetter)
));

var expr = lenientNode(logicExprTop
    .or(stringExprTop)
    .or(listExprTop)
    .or(mathExpr) // var and procedure calls are captured here, which means mathExpr needs to be last
);

var optExprs = manyStar(expr);
var exprs = manyPlus(expr);

// ************************************************************************ //
// ****************************** STATEMENTS ****************************** //
// ************************************************************************ //

/* ~~ Variables & Components ~~ */

var varSetter = identity.bind(function(_) {
    return comprehension(
        KEY.SET,
        LIT.VARNAME,
        opt(KEY.TO),
        expr,
        function(_,varName,_,ex) {
            return ["variable set",varName,ex];
        }
    );
});

function setComponent(properties, typedExpr) {
    return comprehension(
        KEY.SET,
        LIT.VARNAME,
        properties,
        opt(KEY.TO),
        typedExpr,
        function(_,varName,property,_,ex) {
            return ["component set",varName, property, ex];
        }
    );
}

var logicPropertySetter = setComponent(logicProperty, logicExpr);
var mathPropertySetter = setComponent(mathProperty, mathExpr);
var stringPropertySetter = setComponent(stringProperty,expr); // .Text is very type lenient

var setter = stringPropertySetter
    .or(logicPropertySetter)
    .or(mathPropertySetter)
    .or(varSetter);

var procedureCallStmt = KEY.CALL.bind(function(_) {
    return comprehension(LIT.VARNAME, manyStar(expr), function(fname, args) {
        return ["call stmt",fname,args];
    })
}); 

/* ~~ If Statements ~~ */

// comprehension is wrapped inside an identity.bind to delay evaluation of stmtSuiteLenient defined lower down
var ifDo = identity.bind(function(_) {
    return comprehension(
        KEY.IF,
        logicExpr,
        stmtSuiteLenient(KEY.THEN),
        function(_,cond,ifBody) {
            return [cond,ifBody];
        }
    );
})

var elseIfDo = KEY.ELSE.bind(function(el) {
    return ifDo.bind(function(elseIfBody){
        return result(elseIfBody)
    })
});

// elseDo has the restriction that the first statement in its body cannot be
// an unbraced if statement because this should be treated as an else if
var elseDo = KEY.ELSE.bind(function(_) {
    var stmtBracedIf = bracedNode(ifStmt).or(stmtNoIf);
    var elseBody = stmtBracedIf.bind(function(stmt1) {
        return optStmts.bind(function(restStmts) {
            return result([[stmt1].concat(restStmts)])
        })
    });
    return elseBody.or(bracedNode(elseBody))
});

var ifStmt = comprehension(
    ifDo,
    manyStar(elseIfDo),
    elseDo.plus(identity),  // elseDo.plus(identity) makes dangling else ambiguous (unlike or() which is deterministic)
    function(ifBody,elifDos,optElse) {
        elifDos = elifDos.flat(1);  // manyStar nests the list so we flatten
        return ["ifStmt"].concat(ifBody).concat(elifDos).concat(optElse);
    }
)

// all statements except if - necessary to distinguish "else if" from "else {if...}"
var stmtNoIf = lenientNode(
    setter
    .or(emptySlot)
    .or(procedureCallStmt)
);
var stmt = stmtNoIf.or(lenientNode(ifStmt));

var optStmts = manyStar(stmt);
var stmts = manyPlus(stmt);

var stmtSuiteLenient = function(leadIn) {
    return lenientNode(opt(leadIn).bind(function(_) {
        return lenientNode(stmts);
    }));
}

// ************************************************************************** //
// ****************************** DECLARATIONS ****************************** //
// ************************************************************************** //

var event = KEY.CLICK
    .or(KEY.CHANGED);

var eventHandler = comprehension(
    KEY.WHEN,
    LIT.VARNAME,
    event,
    stmtSuiteLenient(KEY.DO),
    function(_,component,event,stmts) {
        return ["when",component,event,stmts];
    }
)

var decl = lenientNode(eventHandler);


// ************ Parsing Happens Here ************ //

var topParser = function(p) {
    return comprehension(
        optspaces,
        p,
        EOF,
        function(_,parsetree,_) {
            return parsetree;
        }
    );
}

var stringToParse = (str + "<EOF>");
Blockly.parserCombinator.errorAt = null; // this is a pretty hacky way of tracking the error

if (type === "code_expr") {
    return topParser(expr).parse(stringToParse);
} else if (type === "code_stmt") {
    return topParser(stmt).parse(stringToParse);
} else if (type === "code_decl") {
    return topParser(decl).parse(stringToParse);
} else {
    console.log("That's not a code block");
}

}