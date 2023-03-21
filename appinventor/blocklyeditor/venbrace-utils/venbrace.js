'use strict';

goog.provide("Blockly.Venbrace");

/**
 * Helper functions for Venbrace to block conversion.
 * 
 */

Blockly.Venbrace = {};

/**
 * Converts a code block to regular App Inventor blocks. 
 * 
 * This is the top level for all Venbrace->Block conversion.
 * @param {Block} codeBlock 
 * @returns 
 */
Blockly.Venbrace.convertToBlocks = function(codeBlock) {
    var parseTrees = Blockly.VenbraceParser.parseToEnd(codeBlock);

    if (parseTrees.length == 1) {
        var xmlstrObj = Blockly.ParseTreeToXml.makeXmlString(parseTrees[0],codeBlock);
        if (xmlstrObj.aborted) {
            Blockly.Venbrace.handleParsingError(xmlstrObj.str);
            return;
        }
        
        var xmlstr = xmlstrObj.str;
        var newBlock = Blockly.Venbrace.xmlstrToBlock(xmlstr,codeBlock);
        if (newBlock) {
            Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(codeBlock,newBlock);
            codeBlock.dispose(true, false);
        }
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

Blockly.Venbrace.xmlstrToBlock = function(xmlstr, codeBlock) {
    try {
        var blockDom = Blockly.Xml.textToDom(xmlstr);
        var xml = blockDom.firstElementChild; // extra step translating from xml->block due to Blockly bug (probably fixed in the multiline version)
        var newBlock = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, codeBlock.workspace);
        return newBlock;

    }
    catch (error) {
        console.log("Error in the XML string.");
        console.log("Parse tree:", parseTree);
        console.log("Xmlstr:", xmlstr);
        console.log(error);
    } 
}

Blockly.Venbrace.handleAmbiguity = function(parseTrees, codeBlock) {

    //var numOptions = 0;
    var errorMessage = "";

    function populateDialog(dialog) {
        var uris = [];
        for (var i = 0; i < parseTrees.length; i++) {
            var xmlstrObj = Blockly.ParseTreeToXml.makeXmlString(parseTrees[i],codeBlock);
            if (xmlstrObj.aborted) {
                errorMessage = xmlstrObj.str;
                continue;
            } 

            var xmlstr = xmlstrObj.str;
            var block = Blockly.Venbrace.xmlstrToBlock(xmlstr, codeBlock);

            if (!block) {
                continue;
            }
            //numOptions++;

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
                var newBlock = Blockly.Venbrace.xmlstrToBlock(xmlstr, codeBlock);
                Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(codeBlock,newBlock);
                codeBlock.dispose(false);
                dialog.close();
                dialog.remove();

            })
            dialog.appendChild(btn);
        }
    }

    var dialog = document.createElement("dialog");
    dialog.className = "parse-dialog";
    populateDialog(dialog);

    if (dialog.children.length == 0) { // all parse trees were invalid
        Blockly.Venbrace.handleParsingError(errorMessage);
    }
    else if (dialog.children.length == 1) { // unambiguous, only 1 parse tree was valid
        dialog.firstElementChild.click();
    }
    else {
        var cancelButton = document.createElement("button");
        cancelButton.textContent = "Cancel"
        dialog.appendChild(cancelButton);
        cancelButton.addEventListener("click", function() {
            dialog.close();
            dialog.remove();
        })

        document.body.appendChild(dialog);
        dialog.showModal(); 
    }
}

Blockly.Venbrace.handleParsingError = function(message) {
    var dialog = document.createElement("dialog");
    dialog.className = "parse-dialog";
    dialog.textContent = "Cannot translate text to blocks.";
    if (message) {
        dialog.textContent += message;
    }

    // TODO, give helpful error message

    var okButton = document.createElement("button");
    okButton.textContent = "OK"
    dialog.appendChild(okButton);
    okButton.addEventListener("click", function() {
        dialog.close();
        dialog.remove();
    })

    document.body.appendChild(dialog);
    dialog.showModal(); 

}