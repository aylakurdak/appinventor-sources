'use strict';

goog.provide('Blockly.VenbraceParser');

goog.require('Blockly.parserCombinator');

Blockly.VenbraceParser = {};

Blockly.VenbraceParser.parseToEnd = function(codeblock) {
    var str = codeblock.getFieldValue('CODE');
    
    var parses = Blockly.VenbraceParser.parse(str,codeblock.type);
    
    console.log(parses)
    var fullParses = [];
    var partialParses = [];
    for (var i = 0; i < parses.length; i++) {
        var parse = parses[i];
        if (parse[1] === "") {
            fullParses.push(parse[0]);
        } else {
            partialParses.push(parse); // collecting this but not using it yet
                                       // if can't parse to end, need this to give feedback on where the parse failed
        }
    }
    return fullParses;
}

Blockly.VenbraceParser.firstParse = function(str) {
    var parseTree = Blockly.VenbraceParser.parse(str);
    return parseTree.length > 0 ? parseTree[0][0] : parseTree
}

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
var procedureCall = KEY.CALL.bind(function(_) {
    return comprehension(LIT.VARNAME, manyStar(expr), function(fname, args) {
        return ["call",fname,args];
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
            return ["get component",component,property];
        }
    );
}

var logicPropertyGetter = getComponent(logicProperty);
var mathPropertyGetter = getComponent(mathProperty);
var stringPropertyGetter = getComponent(stringProperty);


// vars and procedure calls can be any type, so should be allowed in any type of expression
function includeVars(p) {
    return p.or(procedureCall).or(getVar).or(emptySlot);
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
})


var isListEmpty = KEY.IS_LIST_EMPTY.bind(function() {
    return listExpr.bind(function(ls) {
        return result(["isListEmpty", ls])
    })
})

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
    return prefixMathExpr.or(bracedNode(mathExpr)).bind(function(r1) {
        return OP.POWER.bind(function(op) {
            return expo.bind(function(r2) {
                return result([op,r1,r2])
            })
        })
    }).or(prefixMathExpr.or(bracedNode(mathExpr)))
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


/* ~~ If Statements ~~ */

var ifDo = KEY.IF.bind(function(_) {
    return logicExpr.bind(function(cond) {
        // lenientNode to accept "{then ___}' as well as "then {___}"
        return lenientNode(opt(KEY.THEN).bind(function(_) { // "then" is optional since we anticipate this to be a common error
            return stmtSuiteLenient.bind(function(ifBody) {
                return result([cond,ifBody])
            })
        }))
    })
});

// this comprehension is identical to the commented out ifDo definition
// it's wrapped inside an identity.bind to delay evaluation of stmtSuiteLenient defined lower down
// var ifDo = identity.bind(function(_) {
//     return comprehension(
//         KEY.IF,
//         logicExpr,
//         opt(KEY.THEN),
//         stmtSuiteLenient,
//         function(_,cond,_,ifBody) {
//             return [cond,ifBody];
//         }
//     );
// })

var elseIfDo = KEY.ELSE.bind(function(el) {
    return ifDo.bind(function(elseIfBody){
        return result(elseIfBody)
    })
});

// var elseIfDo = comprehension(KEY.ELSE,ifDo,function(_,elseIfBody) {
//     return elseIfBody;
// })

// elseDo has the restriction that the first statement in its body cannot be
// and unbraced if statement because this should be treated as an else if
var elseDo = KEY.ELSE.bind(function(_) {
    var stmtBracedIf = bracedNode(ifStmt).or(stmtNoIf);
    var elseBody = stmtBracedIf.bind(function(stmt1) {
        return optStmts.bind(function(restStmts) {
            return result([[stmt1].concat(restStmts)])
        })
    });
    return elseBody.or(bracedNode(elseBody))
});

var ifStmt = ifDo.bind(function(ifBody) {
    return manyStar(elseIfDo).bind(function(elifDos){
        return elseDo.plus(identity).bind(function(optElse) {       // elseDo.plus(identity) makes the else optional
            elifDos = elifDos.length === 0 ? [] : elifDos[0];       // manyStar nests the list so we flatten with elifDos[0]
            return result(["ifStmt"].concat(ifBody).concat(elifDos).concat(optElse));
        });
    })
});

// all statements except if - necessary to distinguish "else if" from "else {if...}"
var stmtNoIf = lenientNode(
    setter
    .or(emptySlot)
    .or(procedureCall)
);
var stmt = stmtNoIf.or(lenientNode(ifStmt));

var optStmts = manyStar(stmt);
var stmts = manyPlus(stmt);

var stmtSuiteLenient = stmts.or(bracedNode(stmts))  // allows braces around a sequence of statements


// ************************************************************************** //
// ****************************** DECLARATIONS ****************************** //
// ************************************************************************** //

var event = KEY.CLICK
    .or(KEY.CHANGED);

var eventHandler = comprehension(
    KEY.WHEN,
    LIT.VARNAME,
    event,
    opt(KEY.DO), // should wrap this and stmts in a lenient node like if then
    stmts,
    function(_,component,event,_,stmts) {
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