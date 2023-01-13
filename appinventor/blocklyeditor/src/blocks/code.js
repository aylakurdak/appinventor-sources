'use strict';

goog.provide('Blockly.Blocks.code');

goog.require('Blockly.Blocks.Utilities');
goog.require('Blockly.VenbraceParser');
goog.require('Blockly.ParseTreeToXml');
goog.require('Blockly.Xml');
goog.require('Blockly.ExportBlocksImage');

Blockly.Blocks["code_decl"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code decl [")
            .appendField(new Blockly.FieldTextInput(''), 'CODE')
            .appendField("]");
    },
    typeblock: [{translatedName: "code declaration"}],
    codeCustomContextMenu: function(options) {
        var myBlock = this;
        //var workspace = myBlock.workspace;
        var convertToBlocksOption = {text: "Convert to Blocks"};
        convertToBlocksOption.enabled = (!this.errorIcon && !this.disabled);
        convertToBlocksOption.callback = function() {
            // Blockly.Tail.generateAITopLevelBlock(myBlock, true);
            //Blockly.VenbraceParser.printParse('(max 1 2)');

            // var testDom = document.createElement("block");
            // testDom.setAttribute("type", "math_number");
            // var field = document.createElement("field");
            // field.setAttribute("name", "NUM");
            // field.innerHTML = "4";
            // testDom.appendChild(field);

            // console.log(Blockly.Xml.domToPrettyText(testDom));
            // newBlock = Blockly.Xml.domToBlock(testDom, workspace);
            // console.log(newBlock);
            // Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(myBlock,newBlock);
            


            // var testXmlStr = '<xml><block xmlns="http://www.w3.org/1999/xhtml" type="math_on_list"><mutation items="2"></mutation><field name="OP">MIN</field><value name="NUM0"><block type="math_number"><field name="NUM">4</field></block></value><value name="NUM1"><block type="math_number"><field name="NUM">5</field></block></value></block></xml>';
            // var testXmlDom = Blockly.Xml.textToDom(testXmlStr);
            // var children = testXmlDom.children;
            // console.log(children);
            // var testXmlBlock = Blockly.Xml.domToBlock(children[0], workspace);
            // var testXmlDom = Blockly.Xml.textToDom(testXmlStr);
            // console.log("Test Xml Dom:");
            // console.log(Blockly.Xml.domToPrettyText(testXmlDom));
            // var testXmlBlock = Blockly.Xml.domToBlock(testXmlDom, workspace);
            // console.log(testXmlBlock);

            // var pngrefdiv = document.createElement("div");


            // var pngSrc = Blockly.getBlockAsPng(newBlock, pngrefdiv);
            // if (pngrefdiv.firstChild) {
            //     console.log(pngrefdiv.firstChild.href)
            // } else {
            //     console.log("No child");
            // }

            // console.log("PNG SRC");
            // console.log(pngSrc);
            // var uri = "http://" + String(pngSrc[0]);
            // console.log(uri);
            // var url = document.getElementById("pngRefDiv")//.firstChild.href;
            // console.log(url);
            // var content1 = goog.html.SafeHtml.create('img', {'src':uri});
            // var content2 = goog.html.SafeHtml.create('img', {'src':url});
            //Blockly.exportBlockAsPng(testXmlBlock);

            var workspace = myBlock.workspace;

            var trees = Blockly.VenbraceParser.fullParses(myBlock.getFieldValue('CODE'));
            //console.log(tree);
            
            var dialog = new goog.ui.Dialog(null, true, new goog.dom.DomHelper(top.document));
            dialog.setTitle("Text to Blocks");
            //var content1 = goog.html.SafeHtml.create('img', {'src':"static/media/backpack-closed.png"});
            //var content2 = goog.html.SafeHtml.create('p',{}, xmlstr);
            var content = goog.html.SafeHtml.create('p',{},"Parse trees");
            for (var i = 0; i < trees.length; i++) {
                var treeP = goog.html.SafeHtml.create('p',{},JSON.stringify(trees[i]));
                content = goog.html.SafeHtml.concat(content, treeP);
            }
            dialog.setSafeHtmlContent(content);
            dialog.setButtonSet(new goog.ui.Dialog.ButtonSet()
                .addButton(goog.ui.Dialog.ButtonSet.DefaultButtons.OK,false, true));
            
            if (trees.length > 0) {
                xmlstr = Blockly.ParseTreeToXml.makeXmlString(trees[0]);
                var blockDom = Blockly.Xml.textToDom(xmlstr);
                Blockly.Xml.domToWorkspace(blockDom, workspace); //domToBlock doesn't work with hand written xml (bug seemingly), appendDomToWorkspace is not implemented
                myBlock.dispose(true, false);
                if (trees.length > 1) {
                    dialog.setVisible(true);
                }
            }
            else {
                dialog.setSafeHtmlContent(goog.html.SafeHtml.create('p',{},"Cannot translate text to blocks."));
                dialog.setVisible(true);
            }
            //console.log(Blockly.Xml.domToPrettyText(blockDom)); 
        };
        options.push(convertToBlocksOption);
      }
}

Blockly.Blocks["code_stmt"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code stmt {")
            .appendField(new Blockly.FieldTextInput(''), 'CODE')
            .appendField("}");
        this.setPreviousStatement(true);
        this.setNextStatement(true);
    },
    typeblock: [{translatedName: "code statement"}],
    // codeCustomContextMenu: function(options){
    //     var myBlock = this;
    //     var convertToBlocksOption = {text:  "Convert to Blocks"};
    //     convertToBlocksOption.enabled = (!this.errorIcon && !this.disabled);
    //     convertToBlocksOption.callback = function(){
    //         Blockly.Blocks.code_decl.convertToBlocksOption.callback;
    //       // Blockly.Tail.generateAIStatementBlock(myBlock, true);
    //       // myBlock.dispose(false, false); //we don't want to use the healstack option here
    //     };
    //     options.push(convertToBlocksOption);
    //   }
    codeCustomContextMenu: Blockly.Blocks.code_decl.codeCustomContextMenu
}

Blockly.Blocks["code_expr"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code expr (")
            .appendField(new Blockly.FieldTextInput(''), 'CODE')
            .appendField(")");
        this.setOutput(true, null);
    },
    typeblock: [{translatedName: "code expression"}],
    // codeCustomContextMenu: function(options){
    // 	var myBlock = this;
    // 	var convertToBlocksOption = {text:  "Convert to Blocks"};
    //     convertToBlocksOption.enabled = (!this.errorIcon && !this.disabled);
    // 	convertToBlocksOption.callback = function(){
    // 		// Blockly.Tail.generateAIExpressionBlock(myBlock, true);
    //         Blockly.Blocks.code_decl.convertToBlocksOption.callback;
    // 		//myBlock.dispose(false, false); //we don't want to use the healstack option here
    // 	};
    // 	options.push(convertToBlocksOption);
    // }
    codeCustomContextMenu: Blockly.Blocks.code_decl.codeCustomContextMenu
}