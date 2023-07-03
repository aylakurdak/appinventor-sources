'use strict';

goog.provide('Blockly.VenbraceParser');

goog.require('Blockly.parserCombinator');

Blockly.VenbraceParser = {};

Blockly.VenbraceParser.eofSymbol = "#EOF#";

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
    var errorAt = success ? -1 : (str.length + (Blockly.VenbraceParser.eofSymbol.length)) - Blockly.parserCombinator.errorAt.length // Blockly.parserCombinator.errorAt is the string left over when the error happens
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
    return "(){}[]+-/*^'\".<=>!#".indexOf(x) >= 0; // all available App Inventor symbols
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
    SQUARE_ROOT: ["square","root"],
    SQRT: "sqrt",
    ABSOLUTE: "absolute",
    ABS: "abs",
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
    LENGTH_OF_LIST: ["length","of","list"],
    CALL: "call",
    CLICK: ".Click",  // will probably want to separate the dot
    CHANGED: ".Changed",
    VISIBLE: ".Visible",
    RADIUS: ".Radius",
    SPEED: ".Speed",
    HEIGHT: ".Height",
    WIDTH: ".Width",
    ON: ".On",
    ENABLED: ".Enabled",
    CHECKED: ".Checked",
    TEXT: ".Text",
    OR: "or",
    AND: "and",
    LENGTH: "length",
    WHILE: "while",
    TEST: "test",
    FOREACH: ["for", "each"],
    IN: "in"
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
    POWER: sym("^"),
    GT: sym(">"),
    GTE: sym(">="),
    LT: sym("<"),
    LTE: sym("<="),
    EQ: sym("="),
    NEQ: sym("!=")
};

var EOF = keyword(Blockly.VenbraceParser.eofSymbol);


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

/* ~~ Helper Functions ~~ */

// helper for prefixed expressions with one operand
function prefixSingleExpr(prefixParser, exprParser, nodeName) {
    return comprehension(
        prefixParser,
        exprParser,
        function(_, ex) { return [nodeName, ex]; }
    );
}

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

function arithLeftAssoc(first,rest) {
    if (rest.length == 0) {
        return first;
    } 
    else {
        // var op = rest[0][0];
        // var r2 = rest[0][1];
        
        // var l = [op, first, r2];
        var l = first;
        var collectingPlusTimes = false;
        for (var i = 0; i < rest.length; i++) {
            var op = rest[i][0];
            var r2 = rest[i][1];
            if ("+*".indexOf(op) != -1) {
                if (collectingPlusTimes) {
                    l.push(r2);
                } else {
                    collectingPlusTimes = true;
                    l = [op, l, r2];
                }
            } else {
                collectingPlusTimes = false;
                l = [op, l, r2];
            }
        }
        return l;
        //return leftAssoc([op, first, r2], rest.slice(1));
    }
}


/* ~~ Variables, Components, & Procedure Calls ~~ */

var getVar = comprehension(
    opt(KEY.GET),
    lenientNode(LIT.VARNAME),
    function(_,varName) {
        return ['variable get', varName];
    }
);

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
    .or(KEY.TEXT)
    .or(KEY.HEIGHT)
    .or(KEY.WIDTH);

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
function includeUntyped(p) {
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

var strLen = identity.bind(function(_) {
    return prefixSingleExpr(KEY.LENGTH, stringExpr, "strLen");
})

// stringPropertyGetter only includes .Text at the moment
// .Text can also be math, which causes conflict
var stringExprTop = string//.or(stringPropertyGetter);

var stringExpr = lenientNode(includeUntyped(stringExprTop.or(stringPropertyGetter)));

/* ~~ Lists ~~ */

var createListExpr = KEY.LIST.or(KEY.CREATE_EMPTY_LIST).or(KEY.MAKE_A_LIST).bind(function(prefix) {
    var argparser;
    if (prefix === "list") {    // "list" can have any number of elements
        argparser = optExprs;
    } else if (prefix === "makealist") {  // "make a list" must have at least one element
        argparser = comprehension(
            opt(expr),
            optExprs,
            function(expr1,rest) {
                return expr1.length === 0 ? [] : [expr1].concat(rest);
            }
        );
    } else {                    // "create empty list" has no elements
        argparser = identity;
    }
    return argparser.bind(function(elts) {
        return result(["list"].concat(elts));
    });
});


var isListEmpty = identity.bind(function(_) {
    return prefixSingleExpr(KEY.IS_LIST_EMPTY.seq(KEY.LIST.plus(identity)), listExpr, "isListEmpty");
})

var lengthOfList = identity.bind(function(_) {
    return prefixSingleExpr(KEY.LENGTH_OF_LIST.seq(KEY.LIST.plus(identity)), listExpr, "lengthOfList");
})

var listExprTop = createListExpr;

var listExpr = lenientNode(includeUntyped(listExprTop));

/* ~~ Logic ~~ */

var bool = KEY.TRUE.or(KEY.FALSE);

// BROKEN
// var logicCompare = identity.bind(function(_) {
//     var operandParser = stringExprTop
//         .or(listExprTop)
//         .or(logicExpr)
//         .or(mathExpr);
//     var opParser = OP.EQ.or(OP.NEQ);
//     return comprehension(
//         expr,
//         opParser,
//         expr,
//         function(r1,op,r2) {
//             return ["logic" + op, r1, r2];
//         }
//     );
// })

// logicSimple := all logic operators except OR and AND
var logicSimple = identity.bind(function(_) {
    return includeUntyped(bool
        //.or(notExpr)
        .or(isListEmpty)
        .or(logicPropertyGetter)
        .or(mathCompare)
        //.or(logicCompare)
    ).or(bracedNode(logicExpr));
})

var notExpr = identity.bind(function(_) {
    return comprehension(
        manyStar(KEY.NOT),
        logicSimple,
        function(nots, operand) {
            var r = operand;
            for (var i = 0; i < nots.length; i++) {
                r = ["not", r];
            }
            return r;
        }
    )
});

// logicTerm := logicSimple AND logicSimple
var logicTerm = identity.bind(function(_) {
    return comprehension(
        notExpr,
        manyStar(KEY.AND.bind(function(_) {
            return notExpr.bind(function(r2) {
                return result(["and", r2])
            })
        })),
        leftAssoc
    );
});

// logicExpr := logicTerm OR logicTerm
var logicExpr = identity.bind(function(_) {
    return comprehension(
        logicTerm,
        manyStar(KEY.OR.bind(function(_) {
            return logicTerm.bind(function(r2) {
                return result(["or", r2])
            })
        })),
        leftAssoc
    )
});

// excludes untyped
var logicExprTop = logicExpr.bind(function(ex) {
    var untyped = ["variable get", "emptySlot", "call expr"]
    if (ex.length > 0 && untyped.indexOf(ex[0]) >= 0) {
        return zero; // if an untyped expr is the only thing that was parsed, fail
    }
    else {
        return result(ex);
    }
}) 

/* ~~ Math ~~ */

var comparisonOps = OP.EQ.or(OP.NEQ)
    .or(OP.GTE).or(OP.GT)
    .or(OP.LTE).or(OP.LT);

var mathCompare = identity.bind(function(_) {
    return comprehension(
        mathExpr,
        comparisonOps,
        mathExpr,
        function(ex1,op,ex2) {return [op,ex1,ex2];}
    )
});


var sqrtExpr = identity.bind(function(_) {
    return prefixSingleExpr(KEY.SQUARE_ROOT.or(KEY.SQRT), mathExpr, "sqrt");
});

var absoluteExpr = identity.bind(function(_) {
    return prefixSingleExpr(KEY.ABSOLUTE.or(KEY.ABS), mathExpr, "absolute");
});

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

var prefixMathExpr = identity.bind(function(_) {
    return includeUntyped(
        LIT.NUMBER
        //.or(negExpr)
        //.or(unaryMinusExpr)
        .or(sqrtExpr)
        .or(absoluteExpr)
        .or(minMaxExpr)
        .or(mathPropertyGetter)
        .or(lengthOfList)
        .or(strLen)
    ).or(bracedNode(mathExpr));
})

var negExpr = identity.bind(function(_) {
    return comprehension(
        manyStar(KEY.NEG.or(OP.MINUS)),
        prefixMathExpr,
        function(negs, operand) {
            var r = operand;
            var negationCount = negs.length;
            if (negs.length > 0 && negs[negs.length-1] === "-" && ! Array.isArray(operand)) { // if - and number => -num
                r = "-" + r;
                negationCount--;
            } 
            for (var i = 0; i < negationCount; i++) {
                r = ["neg", r];
            }
            return r;
        }
    )
});

// right associative
var expo = identity.bind(function(_) {
    return comprehension(
        negExpr,
        OP.POWER,
        expo,
        function(r1,op,r2) {return [op,r1,r2]}
    ).plus(negExpr); // plus not or
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
        arithLeftAssoc
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
        arithLeftAssoc
    )
})

/* ~~ All Expressions ~~ */

var expr = lenientNode(
    stringExprTop
    .or(listExprTop)
    .or(logicExprTop)  
    .or(mathExpr) // untyped exprs (var refs, proc calls, empty slots) included in mathExpr, so it must come last
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
        if (stmt1[0]==="emptySlot") {
            return result([[stmt1]]); //double nested because tree-to-xml expects stmt suites
        } else {
            return optStmts.bind(function(restStmts) {
                return result([[stmt1].concat(restStmts)])
            });
        }
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

/* ~~ Loops ~~ */

var whileLoop = identity.bind(function(_) {
    return comprehension(
        KEY.WHILE,
        opt(KEY.TEST),
        logicExpr,
        stmtSuiteLenient(KEY.DO),
        function(_,_,test,stmts) {
            return ["while",test,stmts];
        }
    )
})

var forEachInList = identity.bind(function(_) {
    return comprehension(
        KEY.FOREACH,
        LIT.VARNAME,
        KEY.IN,
        KEY.LIST,
        listExpr,
        stmtSuiteLenient(KEY.DO),
        function(_,itemVar,_,_,list,stmts) {
            return ["forEachInList", itemVar, list, stmts];
        }
    )
})

/* ~~ All Statements ~~ */

// all statements that don't need to be handled separately
// excludes empty slots since a statement suite should not have empty slots
// excludes ifStmt which needs special handling in else clauses to avoid else if collision
var stmtBase = lenientNode(
    setter
    .or(whileLoop)
    .or(forEachInList)
    .or(procedureCallStmt)
);

var stmtNoIf = stmtBase.or(lenientNode(emptySlot));
var stmtNoEmpty = stmtBase.or(lenientNode(ifStmt));

var stmt = stmtBase.or(lenientNode(
    ifStmt
    .or(emptySlot)
));

var stmts = manyPlus(stmtNoEmpty).or(lenientNode(emptySlot).bind(function(empty) {
    return result([empty]);
}));
var optStmts = stmts.plus(identity);

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

// ************************************************************************** //

// ************ Parsing Happens Here ************ //

var DEBUG = 1;

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

var stringToParse = (str + Blockly.VenbraceParser.eofSymbol);
Blockly.parserCombinator.errorAt = null; // this is a pretty hacky way of tracking the error

var parseTrees;
if (type === "code_expr") {
    parseTrees = topParser(expr).parse(stringToParse);
} else if (type === "code_stmt") {
    //parseTrees = topParser(stmt).parse(stringToParse);
    var multiStmtParser = stmts.bind(function(ss) {
        return result(["stmtSuite"].concat(ss));
    })
    parseTrees = topParser(multiStmtParser).parse(stringToParse);
} else if (type === "code_decl") {
    parseTrees = topParser(decl).parse(stringToParse);
} else {
    console.log("That's not a code block");
    return;
}

if (DEBUG == 1) {
    console.log("In venbrace-parser.js:")

    pretty = ""
    for (var i = 0; i < parseTrees.length; i++) {
        pretty += "\t" + JSON.stringify(parseTrees[i]) + "\n";
    }
    if (pretty.length == 0) {
        pretty = "\tPARSE FAILED\n"
        console.log("Error at:", Blockly.parserCombinator.errorAt);
    }
    console.log("Input:\t" + stringToParse)
    console.log("Output:" + pretty)
}

return parseTrees;

}