"use strict";

goog.provide('Blockly.ParseTreeToXml');

Blockly.ParseTreeToXml = {};

/***
 * STILL TO DO
 * var KEYWORD_STR = {
    SET: "set",
    TO: "to",
    GET: "get",
    WHEN: "when",
    CALL: "call",
    CLICK: ".Click",  // will probably want to separate the dot
    CHANGED: ".Changed",
    VISIBLE: ".Visible",
    RADIUS: ".Radius",
    SPEED: ".Speed",
    ON: ".On",
    ENABLED: ".Enabled",
    CHECKED: ".Checked",
    TEXT: ".Text"};
 * 
 * component get
 * component set
 * component event handler
 * proc call (expr and stmt)
 */





/**
 * Creates an XML string representation of a block given a parse tree.
 * @param {Array} parseTree A single parse tree result from the forgiving parser (ex. ["max", ["-", "1", "2"], "3"])
 */
Blockly.ParseTreeToXml.makeXmlString = function(parseTree) {

  /**
   * Creates the content of an XML string (without the <xml>...</xml> tags) from a parse tree.
   * 
   * @param {Array} parseTree The output of the forgiving parser (or a branch of it)
   * @param {Array} nextStmts Optional, for statements, the list of following statements (all the nextConnections)
   */
  var treeToXmlString = function(parseTree, nextStmts) {
    
    // Atom only includes nums and bools because those are the only things that don't
    // get nested inside a list.
    var atom = atomicToXmlString(parseTree); 
    if (atom) {
      return atom; 
    } 
    else {
      var first = parseTree[0];
      var rest = parseTree.slice(1,parseTree.length);
      try{
        if (nextStmts) {
            return operatorToXml[first](rest,nextStmts);
          } else {
            return operatorToXml[first](rest);
          }
      } catch(error) {
        console.log("first:",first)
        throw(error);
      }
      
    }   
  };
  
  /**
   * Returns an XML string if the parse tree is an atom. Otherwise, returns false.
   */
  var atomicToXmlString = function(parseTree) {
    //var first = parseTree[0];
    
    if (! isNaN(Number(parseTree))) {
      return makeBlockXmlString("math_number","",null,[["NUM",Number(parseTree)]]);
    } 
    else if (parseTree === "true") {
        return makeBlockXmlString("logic_boolean","",null,[["BOOL","TRUE"]]);
    } 
    else if (parseTree === "false") {
        return makeBlockXmlString("logic_boolean","",null,[["BOOL","FALSE"]]);
    }
    else {
      return false;
    }
  };
  
  /**
   * Creates the XML string for a block (and its children).
   * @param {String} type The name of the block type (ex. "math_on_list").
   * @param {Array<Array>} childrenXml The xml string of the children. For atoms, should be an empty string.
   * @param {Object} mutations Optional. A map of mutation and values (attributes) to be
   *      added to to a <mutation/> tag.
   * @param {Array<Array>} fields Optional. An array of fields where each field is a pair of form [name, value]. 
   *      Each field will be wrapped with a <field name=...> tag.
   * @param {Array} nextStmts Optional. For statements, the list of statements that follow.
   */
  var makeBlockXmlString = function(type,childrenXml,mutations,fields,nextStmts) {
    var xml = '<block type="' + type + '">'
        + mutationsXmlString(mutations)
        + fieldXmlString(fields)
        + childrenXml //+ childrenToXmlString(namedChildren),
        + makeNextStmts(nextStmts)
        + "</block>"
    return xml;
  };
  
  var mutationsXmlString = function(mutations) {
    var xml = "";
    if (mutations) {
      xml = "<mutation "
      for (var mutationName in mutations) {
        xml += mutationName + '="' + mutations[mutationName] + '" ';
      }
      xml += "></mutation>";
    }
    return xml;
  };
  
  var fieldXmlString = function(fields) {
    var xml = "";
    if (fields) {
      for (var i = 0; i < fields.length; i++) {
        var name = fields[i][0];
        var value = fields[i][1];
        xml += "<field name=" + '"' + name + '">' + value + "</field>";
      }
    }
    return xml;
  };
  
  
  // TODO: deal with empty sockets
  
  /**
   * Creates an xml string of a list of child blocks where each is wrapped in 
   * <value name=NAME>...CHILD BLOCK...</value>
   * @param {Array} namedChildren A list of pairs of form [value name, tree].
   */
  var childrenToXmlString = function(namedChildren) {
    var xml = "";
    if (namedChildren) {
      for (var i = 0; i < namedChildren.length; i++) {
        var valOrStmt = 'value' // will need to add in val vs stmt to child names
        var name = namedChildren[i][0];
        var childXml = treeToXmlString(namedChildren[i][1]);
        //xml += `<${valOrStmt} name="${name}>${childXml}</${valOrStmt}>`
        xml += '<value name="' + name + '">' + childXml + "</value>";
      }
    }
    return xml;
  };


  function valChildXml(name,child) {
    var childXml = treeToXmlString(child);
    var xml = '<value name="' + name + '">' + childXml + "</value>";
    //var xml = `<value name="${name}">${childXml}</value>` 
    return xml;
  }

  function stmtChildXml(name,stmts) {
    var firstStmt = stmts[0];

    var childXml = treeToXmlString(firstStmt, stmts.slice(1));
    //var xml = `<statement name="${name}">${childXml}</statement>` 
    var xml = '<statement name="' + name + '">' + childXml + "</statement>";
    return xml;
  }
  
  var makeNamedChildrenList = function(nameRoot, children) {
    var namedChildren = [];
    for (var i = 0; i < children.length; i++) {
        var name = nameRoot+i;
        var child = children[i];
        namedChildren.push([name,child]);
    }
    return namedChildren;
  };

  function makeArgListXml(nameRoot, children) {
    var xml = "";
    for (var i = 0; i < children.length; i++) {
        var name = nameRoot+i;
        var child = children[i];
        xml += valChildXml(name,child);
    }
    return xml;
  };

  function makeNextStmts(nextStmts) {
    var xml = "";
    if (nextStmts && nextStmts.length != 0) {
        var next = nextStmts[0];
        xml = "<next>" + treeToXmlString(next,nextStmts.slice(1)) + "</next>";
    }
    return xml;
  }
  
  
  /* ~~~~~~~~~ Block Types ~~~~~~~~~~ */
  
  // really should share these strings with forgiving parser
  var MAX = 'max',
      MIN = 'min',
      NEG = 'neg',
      PLUS = '+',
      MINUS = '-',
      TIMES = '*',
      DIVIDE = '/',
      LIST = 'list',
      IF = 'ifStmt',
      STR = 'str',
      SLOT = 'emptySlot',
      VAR_GET = 'variable get',
      VAR_SET = 'variable set',
      NOT = 'not',
      IS_LIST_EMPTY = "isListEmpty";
  
  var operatorToXml = {};
  
//   operatorToXml[MAX]= function(children) {
//     return makeBlockXmlString(
//         "math_on_list",
//         makeNamedChildrenList("NUM",children),
//         {"items":children.length},
//         [["OP","MAX"]]);
//   };

operatorToXml[MAX]= function(children) {
    return makeBlockXmlString(
        "math_on_list",
        makeArgListXml("NUM",children),
        {"items":children.length},
        [["OP","MAX"]]);
};
  
  operatorToXml[MIN]= function(children) {
    return makeBlockXmlString(
        "math_on_list",
        makeArgListXml("NUM",children),
        {"items":children.length},
        [["OP","MIN"]]);
  };
  
  operatorToXml[NEG] = function(children) {
    return makeBlockXmlString(
        "math_neg",
        valChildXml("NUM",children[0]),
        null,
        [["OP","NEG"]]);
  };
  
  operatorToXml[PLUS] = function(children) {
    return makeBlockXmlString(
        "math_add",
        makeArgListXml("NUM",children),
        {"items":children.length},
        null);
  };
  
  operatorToXml[TIMES] = function(children) {
    return makeBlockXmlString(
        "math_multiply",
        makeArgListXml("NUM",children),
        {"items":children.length},
        null);
  }
  
  operatorToXml[MINUS] = function(children) {
    var childrenXml = valChildXml("A",children[0]) 
        + valChildXml("B",children[1]);

    return makeBlockXmlString(
        "math_subtract",
        childrenXml);
  };
  
  operatorToXml[DIVIDE] = function(children) {
    var childrenXml = valChildXml("A",children[0]) 
        + valChildXml("B",children[1]);

    return makeBlockXmlString(
        "math_division",
        childrenXml);
  };
  
  operatorToXml[LIST] = function(children) {
    return makeBlockXmlString(
        "lists_create_with",
        makeArgListXml("ADD",children),
        {"items":children.length});
  };

  operatorToXml[IF] = function(children,nextStmts) {
    var childrenXml = "";

    var numElse = 0;
    
    for (var i=0; (i+1)*2 <= children.length; i+=1) {
        var cond = children[i*2];
        var stmts = children[i*2+1];
        childrenXml += valChildXml('IF'+i,cond);
        childrenXml += stmtChildXml('DO'+i,stmts);
    }
    if (children.length % 2 == 1) { // else
        childrenXml += stmtChildXml('ELSE',children.slice(-1)[0]);
        numElse = 1;
    }
    var numElif = Math.floor(children.length/2) - 1;

    return makeBlockXmlString(
        "controls_if",
        childrenXml,
        {"elseif":numElif,"else":numElse},
        null,
        nextStmts
    );
  };

  operatorToXml[SLOT] = function() {
    return "";
  };

  // could cause problems if someone enters a crazy string (what if the string is xml?)
  operatorToXml[STR] = function(text) {
    return makeBlockXmlString(
        "text",
        "",
        null,
        [["TEXT",text]]
    );
  };

  operatorToXml[VAR_GET] = function(varName) {
    return makeBlockXmlString(
        "lexical_variable_get",  // global variables not currently supported
        "",
        null,
        [["VAR",varName]]
    );
  };

  operatorToXml[VAR_SET] = function(nameAndChild, nextStmts) {
    var varName = nameAndChild[0];
    var child = valChildXml("VALUE", nameAndChild[1])
    return makeBlockXmlString(
        "lexical_variable_set",  // global variables not currently supported
        child,
        null,
        [["VAR",varName]],
        nextStmts
    );
  };

  operatorToXml[NOT] = function(children) {
    return makeBlockXmlString(
        "logic_negate",
        valChildXml("BOOL",children[0])
    );
  };

  operatorToXml[IS_LIST_EMPTY] = function(children) {
    return makeBlockXmlString(
        "lists_is_empty",
        valChildXml("LIST",children[0])
    );
  };
  
  return '<xml xmlns="http://www.w3.org/1999/xhtml">' + treeToXmlString(parseTree) + "</xml>"; // might need to add xmlns="http://www.w3.org/1999/xhtml"

};