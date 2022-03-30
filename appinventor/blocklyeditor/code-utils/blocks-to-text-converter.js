Blockly.BlocksToTextConverter = {};

//is there a way to check if it is an expr, stmt, or decl by checking the connectors?
Blockly.BlocksToTextConverter.expressionBlocks = [
    "code_expr",
    "color_black", "color_blue", "color_white", "color_magenta", "color_red", "color_light_gray", "color_pink",
    "color_gray", "color_orange", "color_dark_gray", "color_yellow", "color_green", "color_cyan", "color_make_color",
    "component_set_get",
    "controls_choose",
    "lexical_variable_get",
    "lists_create_with",
    "local_declaration_expression",
    "logic_operation", "logic_negate", "logic_compare", "logic_boolean", "logic_false",
    "math_number",
    "math_compare",
    "math_add", "math_multiply", "math_subtract", "math_division", "math_power", "math_divide",
    "math_single", "math_abs", "math_neg", "math_round", "math_ceiling", "math_floor", "math_trig", "math_cos", "math_tan",
    "procedures_callreturn",
    "text,"
   ];
  
  Blockly.BlocksToTextConverter.statementBlocks = [
    "code_stmt",
    "component_set_get",
    "component_method",
    "controls_if", 
    "lexical_variable_set",
    "local_declaration_statement",
    "procedures_callnoreturn",
  ];
  
  Blockly.BlocksToTextConverter.declarationBlocks = [
    "component_event",
    "global_declaration",
    "procedures_defnoreturn", "procedures_defreturn", ];

Blockly.BlocksToTextConverter.venbraceText;

Blockly.BlocksToTextConverter.getImmediateChildrenByTagName = function(element, tagName){
	var elementsWithTag = [];
	var current = element.firstElementChild;
	while(!!current){
		if(current.nodeName.toLowerCase() === tagName.toLowerCase()){
			elementsWithTag.push(current);
		}
		current = current.nextElementSibling;
	}
	return elementsWithTag;
};

Blockly.BlocksToTextConverter.checkEmptySockets = function(block){
	if(Blockly.WarningHandler.checkEmptySockets.call(block)){
		return true;
	} else{ //check decendants of block
		var children = block.getChildren();
		for (var i = 0; i<children.length; i++){
			if(Blockly.WarningHandler.checkEmptySockets.call(children[i])){
				return true;
			}
		}
		return false;
	}
};


Blockly.BlocksToTextConverter.blockToText = function(block) {
    var root = Blockly.Xml.blockToDom(block);
    
    Blockly.BlocksToTextConverter.venbraceText = "";
    Blockly.BlocksToTextConverter.translateBlock(root);

    var venbraceBlockDom = document.createElement("block");
    venbraceBlockDom.setAttribute("type", Blockly.BlocksToTextConverter.type(root));
    var title = document.createElement("title");
    title.setAttribute("name", "CODE");
    title.innerHTML = Blockly.BlocksToTextConverter.venbraceText;
    venbraceBlockDom.appendChild(title);

    var codeBlock = Blockly.Xml.domToBlock(venbraceBlockDom, block.workspace);
    return codeBlock;
};


Blockly.BlocksToTextConverter.translateBlock = function(element) {
    var tagName = element.nodeName.toLowerCase();
    if(tagName === "block"){
        var type = element.getAttribute("type");
        if(type === "component_set_get"){ //what is this???
            var mutations = Blockly.BlocksToTextConverter.getImmediateChildrenByTagName(element, "mutation");
            var mutation;
            if(mutations.length !== 0){
                mutation = mutations[0];
            }
            if(mutation.getAttribute("set_or_get") === "get"){
                Blockly.BlocksToTextConverter.translateExpressionBlock(element);
            } else{
                Blockly.BlocksToTextConverter.translateStatementBlock(element);
            }
        }
        else{
            if(Blockly.BlocksToTextConverter.expressionBlocks.indexOf(type) !== -1){
              Blockly.BlocksToTextConverter.translateExpressionBlock(element);
            } 
            else if(Blockly.BlocksToTextConverter.statementBlocks.indexOf(type) !== -1){
              Blockly.BlocksToTextConverter.translateStatementBlock(element);
            } 
            else if(Blockly.BlocksToTextConverter.declarationBlocks.indexOf(type) !== -1){
              Blockly.BlocksToTextConverter.translateDeclarationBlock(element);
            } 
            else{
              console.log("Blockly.BlocksToTextConverter.translateBlock: I'm getting not getting an expression, statement, or top level block!! D=")
            }
        }
    }
};


Blockly.BlocksToTextConverter.type = function(element){
    var tagName = element.nodeName.toLowerCase();
    if(tagName === "block"){
      var type = element.getAttribute("type");
      if(type === "component_set_get"){
        var mutations = Blockly.BlocksToTextConverter.getImmediateChildrenByTagName(element, "mutation");
        var mutation;
        if(mutations.length !== 0){
          mutation = mutations[0];
        }
        if(mutation.getAttribute("set_or_get") === "get"){
          return "code_expr";
        } else {
          return "code_stmt";
        }
  
      }
      else{
        if(Blockly.BlocksToTextConverter.expressionBlocks.indexOf(type) !== -1){
          return "code_expr";
        } 
        else if(Blockly.BlocksToTextConverter.statementBlocks.indexOf(type) !== -1){
          return "code_stmt";
        } 
        else if(Blockly.BlocksToTextConverter.declarationBlocks.indexOf(type) !== -1){
          return "code_decl";
        } else{
          //do nothing
          console.log("Blockly.BlocksToTextConverter.type: I'm getting not getting an expression, statement, or top level block!! D=")
        }
      }
    }
  };



Blockly.BlocksToTextConverter.translateExpressionBlock = function(element){
	Blockly.BlocksToTextConverter.venbraceText += '{}';
};

Blockly.BlocksToTextConverter.translateStatementBlock = function(element){
	Blockly.BlocksToTextConverter.venbraceText += '[]';
};

Blockly.BlocksToTextConverter.translateDeclarationBlock = function(element){
	Blockly.BlocksToTextConverter.venbraceText += '()';
};