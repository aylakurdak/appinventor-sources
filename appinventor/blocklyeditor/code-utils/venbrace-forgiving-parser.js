'use strict';

goog.provide('Blockly.VenbraceParser');

goog.require('Blockly.parserCombinator');

Blockly.VenbraceParser = {};

Blockly.VenbraceParser.fullParses = function(str) {
    var parses = Blockly.VenbraceParser.parse(str);
    var fullParses = []
    for (var i = 0; i < parses.length; i++) {
        var parse = parses[i];
        if (parse[1] === "") {
            fullParses.push(parse[0]);
        } 
    }
    return fullParses;
}

Blockly.VenbraceParser.firstParse = function(str) {
    var parseTree = Blockly.VenbraceParser.parse(str);
    return parseTree.length > 0 ? parseTree[0][0] : parseTree
}

Blockly.VenbraceParser.parse = function(str) {

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
        spaces = pc.spaces,
        space = pc.space,
        result = pc.result,
        zero = pc.zero,
        seq = pc.seq,
        or = pc.or,
        first = pc.first,
        comprehension = pc.comprehension,
        identity = pc.identity
    ;

    // /** 
    //  * A Parser that consumes nothing.
    //  * @type {Parser}
    //  */
    // var identity = parserCombinator.identity = new Parser(function(string) {
    //     return [["",string]]
    // })

    // spaces
    var optSpaces = manyStar(space);

    var precedingSpaces = function(p) {
        return spaces.bind(function(_) {
            return p;
        });
    };

    var optPrecedingSpaces = function(p) {
        return optSpaces.bind(function(_) {
            return p;
        });
    };

    // braces
    var openBrace = sat(function(s) {return "({[".indexOf(s) > -1}),
        closeBrace = sat(function(s) {return ")}]".indexOf(s) > -1});

    function matchingBrace(left, right) {
        return ((left == "(" && right == ")") ||
                (left == "{" && right == "}") ||
                (left == "[" && right == "]"));
    };

    var bracedNode = function(p) {
        return openBrace.bind(function(lBrace) {
            return p.or(bracedNode(p)).bind(function(content) {
                return optSpaces.bind(function(_) {     // preceding spaces are handled in expr, but not succeeding spaces
                    return closeBrace.bind(function(rBrace) {
                        return matchingBrace(lBrace,rBrace) ? result(content) : result([]);
                    });
                });
            });
        });
    };

    // ****** KEYWORDS & SYMBOLS ****** //

    var MIN = keyword("min"),
        MAX = keyword("max"),
        LIST = keyword("list"),
        NEG = keyword("neg"),
        PLUS = optPrecedingSpaces(character("+")),
        MINUS = optPrecedingSpaces(character("-")),
        TIMES = optPrecedingSpaces(character("*")),
        DIVIDE = optPrecedingSpaces(character("/"))
    ;

    // ****** EXPRESSIONS ****** //

    var number = manyPlus(digit,true).first();

    var atom = number;

    var minMaxExpr = (MIN.or(MAX)).bind(function(op) {
        return exprs.bind(function(args) {
            return result([op].concat(args));
        });
    });

    var negExpr = NEG.bind(function(_) {
        return expr.bind(function(e) {
            return result(["neg",e]);
        });
    });

    var listExpr = LIST.bind(function(_) {
        return exprs.bind(function(args) {
            return result(["list"].concat(args));
        });
    });

    var prefixExpr = identity.bind(function(_) {
        return atom
            .or(minMaxExpr)
            .or(negExpr)
            .or(listExpr)
            .or(precedingSpaces(prefixExpr))
            .or(bracedNode(prefixExpr));
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

    // Roughly: 
    // T := A * A | A / A | A
    // A := PrefixExpr | (E)
    var term = identity.bind(function(_) {
        return comprehension(
            prefixExpr.or(bracedNode(expr)).or(precedingSpaces(expr)), // first
            manyStar(TIMES.or(DIVIDE).bind(function(op) { // rest
                return prefixExpr.or(bracedNode(expr)).or(precedingSpaces(expr)).bind(function(r2) {
                    return result([op, r2]);
                });
            })),
            leftAssoc
        );
    });

    // Roughly: 
    // E := T + T | T - T | T | (E) | _E
    var expr = identity.bind(function(_) {
        return comprehension(
                term,
                manyStar(PLUS.or(MINUS).bind(function(op) {
                    return term.bind(function(r2) {
                        return result([op, r2]);
                    });
                })),
                leftAssoc
            ).or(bracedNode(expr)).or(precedingSpaces(expr));
    });
    // This does not map properly to App Inventor's infix operators which can take any number of args
    // Here +* must take 2 operands

    var exprs = manyStar(expr);

    // right now, expr is our top level
    return expr.parse(str);

};