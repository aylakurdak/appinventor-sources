'use-strict';

goog.provide('Blockly.ParsePopup');

goog.require('Blockly.ParseTreeToXml');
goog.require('Blockly.Xml');
goog.require('Blockly.ExportBlocksImage');

Blockly.ParsePopup = {};

Blockly.ParsePopup.handleAmbiguity = function(parseTrees, codeBlock, workspace) {

    function populateDialog(dialog, trees, codeBlock, workspace) {
        var uris = [];
        for (var i = 0; i < trees.length; i++) {
            var xmlstr = Blockly.ParseTreeToXml.makeXmlString(trees[i]);
            var blockDom = Blockly.Xml.textToDom(xmlstr);
            var xml = blockDom.firstElementChild;
            var block = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, workspace); // need to create the block in the workspace to generate the uri, removed at the end of this process
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
                console.log(this.firstElementChild.src);
                var xmlstr = this.firstElementChild.alt;
                console.log(xmlstr);
                var blockDom = Blockly.Xml.textToDom(xmlstr);
                var xml = blockDom.firstElementChild;
                var newBlock = /** @type {Blockly.BlockSvg} */ Blockly.Xml.domToBlock(xml, workspace);
                Blockly.BlocklyEditor.repositionNewlyGeneratedBlock(codeBlock,newBlock);
                codeBlock.dispose(false);
                dialog.close();

            })
            dialog.appendChild(btn);
        }
    }

    var dialog = document.createElement("dialog");
    dialog.className = "parse-dialog";
    populateDialog(dialog, parseTrees, codeBlock, workspace);

    var cancelButton = document.createElement("button");
    cancelButton.textContent = "Cancel"
    dialog.appendChild(cancelButton);
    cancelButton.addEventListener("click", function() {
        dialog.close();
    })

    document.body.appendChild(dialog);
    dialog.showModal();
        
    
}