'use strict';

goog.provide("Blockly.Venbrace");

/**
 * Helper functions for Venbrace to block conversion
 */

Blockly.Venbrace = {};

Blockly.Venbrace.convertToBlocks = function(codeBlock) {
    var parseTrees = Blockly.VenbraceParser.parseToEnd(codeBlock);

    if (parseTrees.length == 1) {
        var xmlstr = Blockly.ParseTreeToXml.makeXmlString(parseTrees[0]);
        var blockDom = Blockly.Xml.textToDom(xmlstr);
        var xml = blockDom.firstElementChild; // extra step translating from xml->block due to Blockly bug (probably fixed in the multiline version)
        var newBlock = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, codeBlock.workspace);
        Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(codeBlock,newBlock);
        codeBlock.dispose(true, false);
    }
    else if (parseTrees.length > 1) {
        Blockly.Venbrace.handleAmbiguity(parseTrees, codeBlock);
    }
    else {
        Blockly.Venbrace.handleParsingError()
        console.log("Cannot translate text to blocks :(")
        // create pop up with this message
    }  
}

Blockly.Venbrace.handleAmbiguity = function(parseTrees, codeBlock) {

    function populateDialog(dialog) {
        var uris = [];
        for (var i = 0; i < parseTrees.length; i++) {
            var xmlstr = Blockly.ParseTreeToXml.makeXmlString(parseTrees[i]);
            var blockDom = Blockly.Xml.textToDom(xmlstr);
            var xml = blockDom.firstElementChild;
            var block = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, codeBlock.workspace); // need to create the block in the workspace to generate the uri, removed at the end of this process
            block.moveBy(5, 5); // random amount, this is necessary for generating the uri due to some weirdness in svgAsDataUri (needs translate attribute which only exists if the block is not at the origin)
            var uri = Blockly.getDataUri(block);
            uris.push(uri);
            block.dispose(false);

            var img = new Image();
            img.className = "parse-option-img"
            img.src = uri;
            img.alt = xmlstr;
            
            var btn = document.createElement("button");
            btn.className = "parse-option-button";
            btn.appendChild(img);
            
            btn.addEventListener("click", function() {
                var xmlstr = this.firstElementChild.alt;
                console.log(xmlstr);
                var blockDom = Blockly.Xml.textToDom(xmlstr);
                var xml = blockDom.firstElementChild;
                var newBlock = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, codeBlock.workspace);
                Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(codeBlock,newBlock);
                codeBlock.dispose(false);
                dialog.close();

            })
            dialog.appendChild(btn);
        }
    }

    var dialog = document.createElement("dialog");
    dialog.className = "parse-dialog";
    populateDialog(dialog);

    var cancelButton = document.createElement("button");
    cancelButton.textContent = "Cancel"
    dialog.appendChild(cancelButton);
    cancelButton.addEventListener("click", function() {
        dialog.close();
    })

    document.body.appendChild(dialog);
    dialog.showModal(); 
}

Blockly.Venbrace.handleParsingError = function() {
    var dialog = document.createElement("dialog");
    dialog.className = "parse-dialog";
    dialog.textContent = "Cannot translate text to blocks.";

    // TODO, give helpful error message

    var okButton = document.createElement("button");
    okButton.textContent = "OK"
    dialog.appendChild(okButton);
    okButton.addEventListener("click", function() {
        dialog.close();
    })

    document.body.appendChild(dialog);
    dialog.showModal(); 

}