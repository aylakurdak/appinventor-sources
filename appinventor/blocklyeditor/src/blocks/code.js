'use strict';

goog.provide('Blockly.Blocks.code');

goog.require('Blockly.Blocks.Utilities');
goog.require('Blockly.VenbraceParser');
goog.require('Blockly.ParseTreeToXml');
goog.require('Blockly.Xml');
goog.require('Blockly.ExportBlocksImage');
goog.require('Blockly.ParsePopup');

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
        var convertToBlocksOption = {text: "Convert to Blocks"};
        convertToBlocksOption.enabled = (!this.errorIcon && !this.disabled);
        convertToBlocksOption.callback = function() {
            var workspace = myBlock.workspace;

            var trees = Blockly.VenbraceParser.fullParses(myBlock.getFieldValue('CODE'));
            
            if (trees.length == 1) {
                var xmlstr = Blockly.ParseTreeToXml.makeXmlString(trees[0]);
                var blockDom = Blockly.Xml.textToDom(xmlstr);
                var xml = blockDom.firstElementChild; // extra step translating from xml->block due to Blockly bug (probably fixed in the multiline version)
                var newBlock = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, workspace);
                Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(myBlock,newBlock);
                myBlock.dispose(true, false);
            }
            else if (trees.length > 1) {
                // call out to parser popup
                Blockly.ParsePopup.handleAmbiguity(trees, myBlock, workspace);

            }
            else {
                console.log("Cannot translate text to blocks :(")
                // create pop up with this message
            }            
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