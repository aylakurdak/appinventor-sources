'use strict';

goog.provide('Blockly.Blocks.code');

goog.require('Blockly.Blocks.Utilities');

Blockly.Blocks["code_decl"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code decl")
            .appendField(new Blockly.FieldTextInput(''), 'CODE');
        this.errors = [{name:"checkParseError"}];
        console.log("")
    },
    typeblock: [{translatedName: "code declaration"}],
    parseErrorMessage: false
}

Blockly.Blocks["code_stmt"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code stmt")
            .appendField(new Blockly.FieldTextInput(''), 'CODE');
        this.setPreviousStatement(true);
        this.setNextStatement(true);
        this.errors = [{name:"checkParseError"}];
    },
    typeblock: [{translatedName: "code statement"}],
    parseErrorMessage: false
}

Blockly.Blocks["code_expr"] = {
    category: "Code",
    init: function() {
        this.setColour(Blockly.CODE_CATEGORY_HUE);
        this.appendDummyInput()
            .appendField("code expr")
            .appendField(new Blockly.FieldTextInput(''), 'CODE');
        this.setOutput(true, null);
        this.errors = [{name:"checkParseError"}];
    },
    typeblock: [{translatedName: "code expression"}],
    parseErrorMessage: false
}