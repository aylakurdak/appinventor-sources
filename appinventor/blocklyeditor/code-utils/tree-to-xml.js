"use strict";

goog.provide('Blockly.ParseTreeToXml');

Blockly.ParseTreeToXml = {};

/**
 * Creates an XML string representation of a block given a parse tree.
 * @param {Array} parseTree A single parse tree result from the forgiving parser (ex. ["max", ["-", "1", "2"], "3"])
 */
Blockly.ParseTreeToXml.makeXmlString = function(parseTree) {

  /**
   * Creates the content of an XML string (without the <xml>...</xml> tags) from a parse tree.
   */
  var treeToXmlString = function(parseTree) {
    var atom = atomicToXmlString(parseTree);
    if (atom) {
      return atom; 
    } 
    else {
      var first = parseTree[0];
      var rest = parseTree.slice(1,parseTree.length);
      return operatorToXml[first](rest);
    }   
  };
  
  /**
   * Returns an XML string if the parse tree is an atom. Otherwise, returns false.
   */
  var atomicToXmlString = function(parseTree) {
    if (Number(parseTree)) { // right now our only atoms are numbers
      return makeBlockXmlString("math_number",null,null,[["NUM",Number(parseTree)]]);
    } 
    else {
      return false;
    }
  };
  
  /**
   * Creates the XML string for a block (and its children)
   * @param {String} type The name of the block type (ex. "math_on_list")
   * @param {Array<Array>} namedChildren An array of children where each child is a pair of form [value name, tree]. 
   *      Each child will be wrapped with a <value name=...> tag.
   * @param {Object} mutations A map of mutation and values (attributes) to be
   *      added to to a <mutation/> tag.
   * @param {Array<Array>} fields An array of fields where each field is a pair of form [name, value]. 
   *      Each field will be wrapped with a <field name=...> tag.
   */
  var makeBlockXmlString = function(type,namedChildren,mutations,fields) {
    var xml = '<block type="' + type + '">'
        + mutationsXmlString(mutations)
        + fieldXmlString(fields)
        + childrenToXmlString(namedChildren)
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
        var name = namedChildren[i][0];
        var childXml = treeToXmlString(namedChildren[i][1]);
        xml += '<value name="' + name + '">' + childXml + "</value>";
      }
    }
    return xml;
  };
  
  var makeNamedChildrenList = function(nameRoot, children) {
    var namedChildren = [];
    for (var i = 0; i < children.length; i++) {
        var name = nameRoot+i;
        var child = children[i];
        namedChildren.push([name,child]);
    }
    return namedChildren;
  };
  
  
  /* ~~~~~~~~~ Block Types ~~~~~~~~~~ */
  
  var MAX = 'max',
      MIN = 'min',
      NEG = 'neg',
      PLUS = '+',
      MINUS = '-',
      TIMES = '*',
      DIVIDE = '/',
      LIST = 'list';
  
  var operatorToXml = {};
  
  operatorToXml[MAX]= function(children) {
    return makeBlockXmlString("math_on_list",
                              makeNamedChildrenList("NUM",children),
                              {"items":children.length},
                              [["OP","MAX"]]);
  };
  
  operatorToXml[MIN]= function(children) {
    return makeBlockXmlString("math_on_list",
                              makeNamedChildrenList("NUM",children),
                              {"items":children.length},
                              [["OP","MIN"]]);
  };
  
  operatorToXml[NEG] = function(children) {
    return makeBlockXmlString("math_neg",
                              [["NUM",children[0]]],
                              null,
                              [["OP","NEG"]]);
  };
  
  operatorToXml[PLUS] = function(children) {
    return makeBlockXmlString("math_add",
                              makeNamedChildrenList("NUM",children),
                              {"items":children.length},
                              null);
  };
  
  operatorToXml[TIMES] = function(children) {
    return makeBlockXmlString("math_multiply",
                              makeNamedChildrenList("NUM",children),
                              {"items":children.length},
                              null);
  }
  
  operatorToXml[MINUS] = function(children) {
    return makeBlockXmlString("math_subtract",
                              [["A",children[0]],["B",children[1]]]);
  };
  
  operatorToXml[DIVIDE] = function(children) {
    return makeBlockXmlString("math_division",
                              [["A",children[0]],["B",children[1]]]);
  };
  
  operatorToXml[LIST] = function(children) {
    return makeBlockXmlString("lists_create_with",
                              makeNamedChildrenList("ADD",children),
                              {"items":children.length});
  };
  
  return '<xml xmlns="http://www.w3.org/1999/xhtml">' + treeToXmlString(parseTree) + "</xml>"; // might need to add xmlns="http://www.w3.org/1999/xhtml"

};