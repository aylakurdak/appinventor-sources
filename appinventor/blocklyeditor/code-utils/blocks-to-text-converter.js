Blockly.BlocksToTextConverter = {};

// remember to add blocks to these lists!
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
    "math_add", "math_multiply", "math_subtract", "math_division", "math_power", "math_divide", "math_on_list",
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
    var elementType = element.getAttribute("type");
    Blockly.BlocksToTextConverter.venbraceText += '(';
    Blockly.BlocksToTextConverter["translate_" + elementType].call(this, element);
	Blockly.BlocksToTextConverter.venbraceText += ')';
};

// handles potentially empty sockets
Blockly.BlocksToTextConverter.translateChildExpression = function(item) {
    if (! item) {
      Blockly.BlocksToTextConverter.venbraceText += '()';
    } else {
      Blockly.BlocksToTextConverter.translateExpressionBlock(item.firstElementChild);
    }
};

Blockly.BlocksToTextConverter.translateStatementBlock = function(element){
	Blockly.BlocksToTextConverter.venbraceText += '{}';
};

Blockly.BlocksToTextConverter.translateDeclarationBlock = function(element){
	Blockly.BlocksToTextConverter.venbraceText += '[]';
};


/************************************* EXPRESSIONS ****************************************/

/**** MATH EXPRESIONS ****/

Blockly.BlocksToTextConverter.translate_math_number = function(element){
	Blockly.BlocksToTextConverter.venbraceText += element.firstElementChild.innerHTML;
};

Blockly.BlocksToTextConverter.translate_math_compare = function(element){
	var children = element.children;
	var aItem = children.namedItem("A");
	var bItem = children.namedItem("B");
	var op = children.namedItem("OP").innerHTML;
	
	if(op === "EQ"){
		op = '=';
	} else if(op === "NEQ"){
		op = '!=';
	} else if(op === "LT"){
		op = '<';
	} else if(op === "GT"){
		op = '>';
	} else if(op === "LTE"){
		op = '<=';
	} else {
		op = '>=';
	}

	Blockly.BlocksToTextConverter.translateChildExpression(aItem);
	Blockly.BlocksToTextConverter.venbraceText += ' ' + op + ' ';
	Blockly.BlocksToTextConverter.translateChildExpression(bItem);
};

Blockly.BlocksToTextConverter.translate_math_add = function(element){
	var children = element.children;
	
	var mutationBlock;
	for(var i=0; i<children.length; i++){
		var child = children.item(i);
		if(child.nodeName.toLowerCase() === "mutation"){
			mutationBlock = child;
		}
	}

	var numItems = (!!mutationBlock) ? parseInt(mutationBlock.getAttribute("items")) : 0;
	
	var valueItem = children.namedItem("NUM0");
	Blockly.BlocksToTextConverter.translateChildExpression(valueItem);

	for (var i = 1; i<numItems; i++){
		valueItem = children.namedItem("NUM"+i);

		Blockly.BlocksToTextConverter.venbraceText += ' + ';
		Blockly.BlocksToTextConverter.translateChildExpression(valueItem);
	}
};


Blockly.BlocksToTextConverter.translate_math_multiply = function (element) {
	var children = element.children;
	
	var mutationBlock;
	for(var i=0; i<children.length; i++){
		var child = children.item(i);
		if(child.nodeName.toLowerCase() === "mutation"){
			mutationBlock = child;
		}
	}

	var numItems = (!!mutationBlock) ? parseInt(mutationBlock.getAttribute("items")) : 0;
	
	var valueItem = children.namedItem("NUM0");
	Blockly.BlocksToTextConverter.translateChildExpression(valueItem);

	for (var i = 1; i<numItems; i++){
		valueItem = children.namedItem("NUM"+i);

		Blockly.BlocksToTextConverter.venbraceText += ' * ';
		Blockly.BlocksToTextConverter.translateChildExpression(valueItem);
	}
};

Blockly.BlocksToTextConverter.translate_math_subtract = function(element) {
	var children = element.children;
	var aItem = children.namedItem("A");
	var bItem = children.namedItem("B");

	Blockly.BlocksToTextConverter.translateChildExpression(aItem);
	Blockly.BlocksToTextConverter.venbraceText += ' - ';
	Blockly.BlocksToTextConverter.translateChildExpression(bItem);
};


Blockly.BlocksToTextConverter.translate_math_division = function (element) {
	var children = element.children;
	var aItem = children.namedItem("A");
	var bItem = children.namedItem("B");

	Blockly.BlocksToTextConverter.translateChildExpression(aItem);
	Blockly.BlocksToTextConverter.venbraceText += ' / ';
	Blockly.BlocksToTextConverter.translateChildExpression(bItem);
};

Blockly.BlocksToTextConverter.translate_math_power = function (element) {
	var children = element.children;
	var aItem = children.namedItem("A");
	var bItem = children.namedItem("B");

	Blockly.BlocksToTextConverter.translateChildExpression(aItem);
	Blockly.BlocksToTextConverter.venbraceText += ' ^ ';
	Blockly.BlocksToTextConverter.translateChildExpression(bItem);
};

Blockly.BlocksToTextConverter.translate_math_divide = function(element){
	var children = element.children;
	var dividendItem = children.namedItem("DIVIDEND");
	var divisorItem = children.namedItem("DIVISOR");
	
    //mod, remainder, quotient - is this the best syntax
	var op = children.namedItem("OP").innerHTML.toLowerCase() + '_of';

	Blockly.BlocksToTextConverter.venbraceText += op + ' ';
	Blockly.BlocksToTextConverter.translateChildExpression(dividendItem);
	Blockly.BlocksToTextConverter.venbraceText += ' / ';
	Blockly.BlocksToTextConverter.translateChildExpression(divisorItem);
};

Blockly.BlocksToTextConverter.translate_math_on_list = function(element){
    var children = element.children;

    var op = children.namedItem("OP").innerHTML.toLowerCase();
    Blockly.BlocksToTextConverter.venbraceText += op + " (";

    var mutationBlock;
	for(var i = 0; i < children.length; i++){
		var child = children.item(i);
		if(child.nodeName.toLowerCase() === "mutation"){
			mutationBlock = child;
		}
	}

	var numItems = (!!mutationBlock) ? parseInt(mutationBlock.getAttribute("items")) : 0;
	
	var valueItem = children.namedItem("NUM0");
	Blockly.BlocksToTextConverter.translateChildExpression(valueItem);

	for (var i = 1; i<numItems; i++){
		valueItem = children.namedItem("NUM"+i);

		Blockly.BlocksToTextConverter.venbraceText += ', ';
		Blockly.BlocksToTextConverter.translateChildExpression(valueItem);
	}

    Blockly.BlocksToTextConverter.venbraceText += ")"; //closing the list, is this bad form?
    
};

Blockly.BlocksToTextConverter.translate_math_single = function(element){
	var children = element.children;
	var exprItem = children.namedItem("NUM");
	
	var op = children.namedItem("OP").innerHTML.toLowerCase();
	if (op === "root") {
		op = 'sqrt';
	} else if(op === "ln"){
		op = 'log';
	} else if(op === "exp"){
		op = 'e^';
	} //no else case, op should remain the same for abs, round, ceiling, and floor

	Blockly.BlocksToTextConverter.venbraceText += op + ' ';
	Blockly.BlocksToTextConverter.translateChildExpression(exprItem);
};

Blockly.BlocksToTextConverter.translate_math_abs = function(element){
	Blockly.BlocksToTextConverter.translate_math_single(element);
};

Blockly.BlocksToTextConverter.translate_math_neg = function(element){
	Blockly.BlocksToTextConverter.translate_math_single(element);
};

Blockly.BlocksToTextConverter.translate_math_round = function(element){
	Blockly.BlocksToTextConverter.translate_math_single(element);
};

Blockly.BlocksToTextConverter.translate_math_ceiling = function(element){
	Blockly.BlocksToTextConverter.translate_math_single(element);
};

Blockly.BlocksToTextConverter.translate_math_floor = function(element){
	Blockly.BlocksToTextConverter.translate_math_single(element);
};