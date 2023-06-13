"use strict";

goog.provide('Blockly.ParseTreeToXml');

Blockly.ParseTreeToXml = {};

/**
 * Converts a parse tree into and XML string.
 * 
 * Returns an object which contains a boolean "aborted" which is false if the conversion was successful, and
 * a string "str" which is the XML string if the conversion was successful or an error message otherwise.
 * @param {Array} parseTree A single parse tree result from the forgiving parser (ex. ["max", ["-", "1", "2"], "3"])
 * @param {Block} codeBlock The code block being converted. 
 * 
 */
Blockly.ParseTreeToXml.makeXmlString = function(parseTree, codeBlock) {

    // If we can't make valid App Inventor blocks from this parse tree, abort and return false.
    // This might happen if we call a function or component that doesn't exist,
    // or call a function with the wrong number of args. This is definitely not ideal behavior,
    // but it's necessary for now, especially with ambiguous parsing (need to filter unreasonable cases)
    var aborted = false;
    var abortMessage = "";

    function abort(message) {
        aborted = true;
        abortMessage = message;
        console.log("Aborting:", message);
      }

  /**
   * Creates the content of an XML string (without the <xml>...</xml> tags) from a parse tree.
   * 
   * @param {Array} parseTree The output of the forgiving parser (or a branch of it)
   * @param {Array} nextStmts Optional, for statements, the list of following statements (all the nextConnections)
   */
  var treeToXmlString = function(parseTree, nextStmts) {

    if (aborted) {
        return "";
    }
    
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
        console.log("In tree-to-xml.js:")
        console.log("failed on node:",first)
        throw(error);
      }
      
    }   
  };
  
  /**
   * Returns an XML string if the parse tree is an atom. Otherwise, returns false.
   */
  var atomicToXmlString = function(parseTree) {
    //var first = parseTree[0];
    
    if (! isNaN(Number(parseTree)) && parseTree.length != 0) {
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
        + childrenXml
        + makeNextStmts(nextStmts)
        + "</block>"
    return xml;
  };

  // identical to above but with mutation xml already handled instead of an object to be handled
  var makeProcXmlString = function(type,childrenXml,mutationXml,fields,nextStmts) {
    var xml = '<block type="' + type + '">'
        + mutationXml
        + fieldXmlString(fields)
        + childrenXml
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
      SQRT = 'sqrt',
      ABSOLUTE = 'absolute',
      PLUS = '+',
      MINUS = '-',
      TIMES = '*',
      DIVIDE = '/',
      POWER = '^',
      EQ = '=',
      NEQ = '!=',
      LT = '<',
      LTE = '<=',
      GT = '>',
      GTE = '>=',
      LIST = 'list',
      IF = 'ifStmt',
      STR = 'str',
      SLOT = 'emptySlot',
      VAR_GET = 'variable get',
      VAR_SET = 'variable set',
      NOT = 'not',
      IS_LIST_EMPTY = "isListEmpty",
      LENGTH_OF_LIST = "lengthOfList",
      CALL_RETURN = 'call expr',
      CALL_NORETURN = 'call stmt',
      COMPONENT_GET = 'component get',
      COMPONENT_SET = 'component set',
      WHEN = 'when',
      STRLEN = 'strLen',
      AND = 'and',
      OR = 'or',
      WHILE = 'while',
      FOR_EACH_IN_LIST = 'forEachInList';
  
  var operatorToXml = {};

  operatorToXml[SLOT] = function() {
    return "";
  };

  /* ~~~~~~~~~ Math ~~~~~~~~~~ */

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

  operatorToXml[SQRT] = function(children) {
    return makeBlockXmlString(
        "math_single",
        valChildXml("NUM",children[0]),
        null,
        [["OP","ROOT"]]);
  };

  operatorToXml[ABSOLUTE] = function(children) {
    return makeBlockXmlString(
        "math_single",
        valChildXml("NUM",children[0]),
        null,
        [["OP","ABS"]]);
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

  operatorToXml[POWER] = function(children) {
    var childrenXml = valChildXml("A",children[0]) 
        + valChildXml("B",children[1]);

    return makeBlockXmlString(
        "math_power",
        childrenXml);
  };

  function mathCompare(op, children) {
    var childrenXml = valChildXml("A",children[0]) 
        + valChildXml("B",children[1]);
    
    return makeBlockXmlString(
      "math_compare",
      childrenXml,
      null,
      [['OP',op]]
    );
  }

  operatorToXml[EQ] = function(children) {
    return mathCompare('EQ', children);
  }

  operatorToXml[NEQ] = function(children) {
    return mathCompare('NEQ', children);
  }

  operatorToXml[LT] = function(children) {
    return mathCompare('LT', children);
  }

  operatorToXml[LTE] = function(children) {
    return mathCompare('LTE', children);
  }

  operatorToXml[GT] = function(children) {
    return mathCompare('GT', children);
  }

  operatorToXml[GTE] = function(children) {
    return mathCompare('GTE', children);
  }

  /* ~~~~~~~~~ Lists ~~~~~~~~~~ */
  
  operatorToXml[LIST] = function(children) {
    return makeBlockXmlString(
        "lists_create_with",
        makeArgListXml("ADD",children),
        {"items":children.length});
  };

  operatorToXml[IS_LIST_EMPTY] = function(children) {
    return makeBlockXmlString(
        "lists_is_empty",
        valChildXml("LIST",children[0])
    );
  };

  operatorToXml[LENGTH_OF_LIST] = function(children) {
    return makeBlockXmlString(
        "lists_length",
        valChildXml("LIST",children[0])
    );
  };

  /* ~~~~~~~~~ Strings ~~~~~~~~~~ */

  // could cause problems if someone enters a crazy string (what if the string is xml?)
  operatorToXml[STR] = function(text) {
    return makeBlockXmlString(
        "text",
        "",
        null,
        [["TEXT",text]]
    );
  };

  operatorToXml[STRLEN] = function(children) {
    return makeBlockXmlString(
      "text_length",
      valChildXml("VALUE", children[0])
    );
  };
  
  /* ~~~~~~~~~ Logic ~~~~~~~~~~ */

  operatorToXml[NOT] = function(children) {
    return makeBlockXmlString(
        "logic_negate",
        valChildXml("BOOL",children[0])
    );
  };

  // OR and AND
  // currently only allows 2 arguments since the parser
  // will always parse as two args
  function logicOperator(op, children) {
    var childrenXml = valChildXml("A",children[0]) 
      + valChildXml("B",children[1]);

    return makeBlockXmlString(
        "logic_operation",
        childrenXml,
        {"items":2}, // always 2 args
        [["OP", op]]);
  }

  operatorToXml[AND] = function(children) {
    return logicOperator("AND", children);
  };

  operatorToXml[OR] = function(children) {
    return logicOperator("OR", children);
  };

  /* ~~~~~~~~~ Variables ~~~~~~~~~~ */

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

  /* ~~~~~~~~~ Controls ~~~~~~~~~~ */

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

  operatorToXml[WHILE] = function(children, nextStmts) {
    var childrenXml = valChildXml('TEST',children[0])
      +  stmtChildXml('DO', children[1]);

    return makeBlockXmlString(
      "controls_while",
      childrenXml,
      null,
      null,
      nextStmts
    );
  }

  operatorToXml[FOR_EACH_IN_LIST] = function(children, nextStmts) {
    var varname = children[0];
    var listExpr = children[1];
    var stmts = children[2];

    var childrenXml = valChildXml('LIST', listExpr)
      + stmtChildXml('DO', stmts);
    
    return makeBlockXmlString(
      "controls_forEach",
      childrenXml,
      null,
      [["VAR", varname]],
      nextStmts
    )
  }

  /* ~~~~~~~~~ Procedure Calls ~~~~~~~~~~ */

  function procCall(children,type,nextStmts) {
    var workspace = codeBlock.workspace;
    var fname = children[0];
    var args = children[1];
    var procedure = workspace.getProcedureDatabase().getProcedureByName(fname);
    if (procedure) {
        if (procedure.type == "procedures_def"+type) {
            var procArgs = procedure.arguments_;
            if (args.length == procArgs.length) {
                var childrenXml = "";
                for (var i=0;i<args.length;i++) {
                    childrenXml += valChildXml("ARG"+i,args[i]);
                }

                var mutation = Blockly.Xml.domToText(procedure.mutationToDom()); // this might cause problems - it seems to be working
                
                return makeProcXmlString(
                    "procedures_call"+type,
                    childrenXml,
                    mutation,
                    [["PROCNAME",fname]],
                    nextStmts
                );

            } else {
                abort("Wrong number of args for procedure " + fname + ".\n Expected " + procArgs.length + ". Got " + args.length);
            }
        } else {
            abort("Procedure " + fname + " is not of type procedures_call"+type);
        }
    } else {
        abort(fname + " is not a procedure.");
    }
  }
  operatorToXml[CALL_RETURN] = function(children) {
    return procCall(children,"return");
  }

  operatorToXml[CALL_NORETURN] = function(children,nextStmts) {
    return procCall(children,"noreturn",nextStmts);
  }

  /* ~~~~~~~~~ Components ~~~~~~~~~~ */

  function componentSetGet(children,setGet,nextStmts) {
    var workspace = codeBlock.workspace;
    var db = workspace.getComponentDatabase();

    var componentName = children[0];
    var propertyName = children[1].slice(1); // .Text to Text - really, the parser should handle this, but it doesn't
    var componentType = db.instanceNameToTypeName(componentName);

    if (!componentType) {
        abort("No component with name " + componentName);
        return;
    }

    var hasProperty = db.getPropertyForType(componentType,propertyName) ? true : false;
    if (!hasProperty) {
        abort("No property " + propertyName + " for components of type " + componentType);
        return;
    }

    var mutation = {
        "component_type": componentType,
        "set_or_get": setGet,
        "property_name":propertyName,
        "is_generic":false,
        "instance_name": componentName
    };

    var fields = [["COMPONENT_SELECTOR",componentName],["PROP",propertyName]]

    if (setGet === "set") {
        var childrenXml = valChildXml("VALUE",children[2]);
    } else {
        var childrenXml = ""
    }

    return makeBlockXmlString(
        "component_set_get",
        childrenXml,
        mutation,
        fields,
        nextStmts // only present for set
    );
  }

  operatorToXml[COMPONENT_GET] = function(children) {
    return componentSetGet(children,"get")
  }

  operatorToXml[COMPONENT_SET] = function(children,nextStmts) {
    return componentSetGet(children,"set",nextStmts)
  }

  operatorToXml[WHEN] = function(children) {
    var workspace = codeBlock.workspace;
    var db = workspace.getComponentDatabase();

    var componentName = children[0];
    var eventName = children[1].slice(1); // .Text to Text - really, the parser should handle this, but it doesn't
    var componentType = db.instanceNameToTypeName(componentName);

    if (!componentType) {
        abort("No component with name " + componentName);
        return;
    }

    var hasEvent = db.getEventForType(componentType,eventName) ? true : false;
    if (!hasEvent) {
        abort("No event " + eventName + " for components of type " + componentType);
        return;
    }

    var mutation = {
        "component_type": componentType,
        "event_name":eventName,
        "is_generic":false,
        "instance_name": componentName
    };
    var field = [["COMPONENT_SELECTOR",componentName]];

    return makeBlockXmlString(
        "component_event",
        stmtChildXml("DO",children[2]),
        mutation,
        field
    );

  }


  /* ~~~~~~~~~ Top level treeToXmlString call ~~~~~~~~~~ */
  
  var xmlstr = '<xml xmlns="http://www.w3.org/1999/xhtml">' + treeToXmlString(parseTree) + "</xml>"; // might need to add xmlns="http://www.w3.org/1999/xhtml"

  if (aborted) {
    console.log("Aborted:", abortMessage);
    console.log("Aborted Xml string:", xmlstr);
    return {"aborted":true,"str":abortMessage};
  } 
  else {
    return {"aborted":false,"str":xmlstr};
  }
};