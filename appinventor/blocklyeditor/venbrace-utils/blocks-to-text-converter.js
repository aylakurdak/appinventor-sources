goog.provide('Blockly.Blocks.blocksToText');

goog.require('Blockly.Blocks.Utilities');

Blockly.BlocksToTextConverter = {};

// remember to add blocks to these lists!
Blockly.BlocksToTextConverter.expressionBlocks = [
    "component_set_get",
    "lexical_variable_get",
    "lists_create_with", "lists_is_empty",
    "logic_negate", "logic_boolean", "logic_false",
    "math_number",
    "math_add", "math_multiply", "math_subtract", "math_division", "math_power", "math_divide", "math_on_list",
    "math_single", "math_abs", "math_neg", "math_round", "math_ceiling", "math_floor",
    "procedures_callreturn",
    "text"
   ];
  
Blockly.BlocksToTextConverter.statementBlocks = [
    "component_set_get",
    "controls_if", 
    "lexical_variable_set",
    "procedures_callnoreturn",
  ];
  
Blockly.BlocksToTextConverter.declarationBlocks = [
    "component_event",
    "global_declaration",
    "procedures_defnoreturn", "procedures_defreturn", ];

Blockly.BlocksToTextConverter.atomicBlocks = [
	"math_number",
	"logic_boolean",
    "logic_false",
    "text",
    "lexical_variable_get"
];

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



Blockly.BlocksToTextConverter.translateExpressionBlock = function(element) {
    var elementType = element.getAttribute("type");

	if(Blockly.BlocksToTextConverter.atomicBlocks.indexOf(elementType) !== -1) {
		Blockly.BlocksToTextConverter["translate_" + elementType].call(this, element);
	} else {
		Blockly.BlocksToTextConverter.venbraceText += '(';
		Blockly.BlocksToTextConverter["translate_" + elementType].call(this, element);
		Blockly.BlocksToTextConverter.venbraceText += ')';
	}
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
    var elementType = element.getAttribute("type");

	Blockly.BlocksToTextConverter.venbraceText += '{';
	Blockly.BlocksToTextConverter["translate_" + elementType].call(this, element);
	Blockly.BlocksToTextConverter.venbraceText += '}';

};

Blockly.BlocksToTextConverter.translateChildStatement = function(element){
	if (!element) {
        Blockly.BlocksToTextConverter.venbraceText += '{}';
    } else { //if element is not null (make sure it's not the empty statement)
        var curBlock = element.firstElementChild; //this should be the first statement block
        Blockly.BlocksToTextConverter.translateStatementBlock(curBlock);
        var next = curBlock.lastElementChild.nodeName === "NEXT" ? curBlock.lastElementChild : false;
        if(next){
            Blockly.BlocksToTextConverter.translateChildStatement(next);
        }
    }
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
	
	var op = children.namedItem("OP").innerHTML.toLowerCase() + ' of';

	Blockly.BlocksToTextConverter.venbraceText += op + ' ';
	Blockly.BlocksToTextConverter.translateChildExpression(dividendItem);
	Blockly.BlocksToTextConverter.venbraceText += ' / ';
	Blockly.BlocksToTextConverter.translateChildExpression(divisorItem);
};

Blockly.BlocksToTextConverter.translate_math_on_list = function(element){
    var children = element.children;

    var op = children.namedItem("OP").innerHTML.toLowerCase();
    Blockly.BlocksToTextConverter.venbraceText += op + ' ';

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

		Blockly.BlocksToTextConverter.venbraceText += ' ';
		Blockly.BlocksToTextConverter.translateChildExpression(valueItem);
	};
    
};

Blockly.BlocksToTextConverter.translate_math_single = function(element){
	var children = element.children;
	var exprItem = children.namedItem("NUM");
	
	var op = children.namedItem("OP").innerHTML.toLowerCase();
	if (op === "root") {
		op = 'square root';
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


Blockly.BlocksToTextConverter.translate_text = function(element) {
	Blockly.BlocksToTextConverter.venbraceText += '"' + element.firstElementChild.innerHTML + '"';
}

Blockly.BlocksToTextConverter.translate_logic_boolean = function(element) {
	Blockly.BlocksToTextConverter.venbraceText += element.firstElementChild.innerHTML.toLowerCase();
}

Blockly.BlocksToTextConverter.translate_logic_false = function(element) {
    Blockly.BlocksToTextConverter.translate_logic_boolean(element);
}

Blockly.BlocksToTextConverter.translate_logic_negate = function(element){
	var children = element.children;
	var item = children.namedItem("BOOL");

	Blockly.BlocksToTextConverter.venbraceText += "not ";

	Blockly.BlocksToTextConverter.translateChildExpression(item);
}

Blockly.BlocksToTextConverter.translate_lists_create_with = function(element){
	Blockly.BlocksToTextConverter.venbraceText += 'list';

	var children = element.children;
	var mutation = parseInt(element.firstElementChild.getAttribute("items"));

	for(var i=0; i<mutation; i++){
		var valBlock= children.namedItem("ADD" + i);
		if(valBlock){
			Blockly.BlocksToTextConverter.venbraceText += ' ';
			Blockly.BlocksToTextConverter.translateChildExpression(valBlock);
		}
	}
};

Blockly.BlocksToTextConverter.translate_lists_is_empty = function(element) {
	var children = element.children;
	var item = children.namedItem("LIST");

	Blockly.BlocksToTextConverter.venbraceText += "is list empty? ";

	Blockly.BlocksToTextConverter.translateChildExpression(item);
}

Blockly.BlocksToTextConverter.translate_lexical_variable_get = function(element) {
    Blockly.BlocksToTextConverter.venbraceText += element.firstElementChild.innerHTML;
}

Blockly.BlocksToTextConverter.translate_lexical_variable_set = function(element) {
	var children = element.children;
	var varName = children.namedItem("VAR").innerHTML;
	var value = children.namedItem("VALUE");

    Blockly.BlocksToTextConverter.venbraceText += "set " + varName + " to ";
    Blockly.BlocksToTextConverter.translateChildExpression(value);
}

Blockly.BlocksToTextConverter.translate_controls_if = function(element) {
	var numElse = 0;
    var numElseIf = 0;
    if (element.firstElementChild.nodeName === "MUTATION") {
        var mutation = element.firstElementChild;
        numElse = mutation.getAttribute("else");
		numElse == null ? 0 : numElse;
        numElseIf = mutation.getAttribute("elseif");
		numElseIf == null ? 0 : numElseIf;
    }

	var children = element.children;

	var if0 = children.namedItem("IF0");
	var do0 = children.namedItem("DO0");


    Blockly.BlocksToTextConverter.venbraceText += "if ";
    Blockly.BlocksToTextConverter.translateChildExpression(if0);
    Blockly.BlocksToTextConverter.venbraceText += " then ";
    Blockly.BlocksToTextConverter.translateChildStatement(do0);

    for (var i = 1; i <= numElseIf; i++){
        Blockly.BlocksToTextConverter.venbraceText += " else if ";
        Blockly.BlocksToTextConverter.translateChildExpression(children.namedItem("IF"+i));
        Blockly.BlocksToTextConverter.venbraceText += " then ";
        Blockly.BlocksToTextConverter.translateChildStatement(children.namedItem("DO"+i));
	}

    if (numElse == 1) {
        Blockly.BlocksToTextConverter.venbraceText += " else ";
        Blockly.BlocksToTextConverter.translateChildStatement(children.namedItem("ELSE"));
    } 
    
}