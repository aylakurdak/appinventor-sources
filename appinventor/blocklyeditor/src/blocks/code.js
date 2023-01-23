'use strict';

goog.provide('Blockly.Blocks.code');

goog.require('Blockly.Blocks.Utilities');
goog.require('Blockly.VenbraceParser');
goog.require('Blockly.ParseTreeToXml');
goog.require('Blockly.Xml');
goog.require('Blockly.ExportBlocksImage');
goog.require('goog.graphics.ext')

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

            

            var uri = Blockly.getDataUri(myBlock);

            var xml = document.createElement('xml');
            xml.appendChild(Blockly.Xml.blockToDom(myBlock, true));
            //console.log(xml);
            var code = Blockly.Xml.domToText(xml);

            var img = new Image();
            img.src = uri;
            img.onload = function() {
                var canvas = document.createElement('canvas');
                canvas.width = 2 * img.width;
                canvas.height = 2 * img.height;
                var context = canvas.getContext('2d');
                context.drawImage(img, 0, 0, img.width, img.height, 0, 0, canvas.width, canvas.height);

                function returnPng(png) {
                    png.setCodeChunk(code);
                    for (var i = 0; i < png.chunks.length; i++) {
                        var phy = [112, 72, 89, 115];
                        if (png.chunks[i].type == 'pHYs') {
                            png.chunks.splice(i, 1, new PNG.Chunk(9, 'pHYs', pHY_data, crc32(phy.concat(pHY_data)))); //replacing existing pHYs chunk
                            break;
                        } else if (png.chunks[i].type == 'IDAT') {
                            png.chunks.splice(i, 0, new PNG.Chunk(9, 'pHYs', pHY_data, crc32(phy.concat(pHY_data)))); // adding new pHYs chunk
                            break;
                        }
                    }
                    var blob = png.toBlob();

                    var url = URL.createObjectURL(blob);
                    console.log(url);

                    function test1() {
                        var dialog = new goog.ui.Dialog(null, true, new goog.dom.DomHelper(top.document));
                        dialog.setTitle("Text to Blocks (test 1)");
                        var content = goog.html.SafeHtml.create('img', {'src':url});
                        dialog.setSafeHtmlContent(content);
                        dialog.setVisible(true);

                        // Problems: blob url causes ERR_UNKNOWN_URL_SCHEME
                    }

                    function test2() {
                        var dialog = new goog.ui.Dialog(null, true, new goog.dom.DomHelper(top.document));
                        dialog.setTitle("Text to Blocks test 2");
                        console.log(dialog.isInDocument());
                        dialog.render();
                        console.log(dialog.isInDocument());
                        //randomThing = new goog.ui.DatePicker();
                        //randomThing.render(dialog);
                        //dialog.addChild(randomThing);
                        dialog.setVisible(true);
                        var graphic = new goog.graphics.ext.Graphics(100, 100);
                        var group = new goog.graphics.ext.Group(graphic);
                        var image = new goog.graphics.ext.Image(group, url);
                        graphic.render(document.body);
                        //dialog.addChild(graphic, true);
                        //graphic.render(dialog)
                        //var image = goog.graphics.CanvasImage.drawImage

                        // Problems: can't addChild to the dialog because graphic is not a goog.ui.Component
                    }

                    test1();
                    //test2();
                    
                }

                if (canvas.toBlob === undefined) {
                    var src = canvas.toDataURL('image/png');
                    var base64img = src.split(',')[1];
                    var decoded = window.atob(base64img);
                    var rawLength = decoded.length;
                    var buffer = new Uint8Array(new ArrayBuffer(rawLength));
                    for (var i = 0; i < rawLength; i++) {
                        buffer[i] = decoded.charCodeAt(i);
                    }
                    var blob = new Blob([buffer], {'type': 'image/png'});

                    var url = URL.createObjectURL(blob);
                    console.log(url);

                    // var dialog1 = new goog.ui.Dialog(null, true, new goog.dom.DomHelper(top.document));
                    // dialog1.setTitle("Text to Blocks");
                    // var content = goog.html.SafeHtml.create('img', {'src':"static/media/backpack-closed.png"});
                    // dialog1.setSafeHtmlContent(content);
                    // dialog1.setVisible(true);

                    //new PNG().readFromBlob(blob, returnPng);
                    
                } else {
                    canvas.toBlob(function (blob) {

                    // var url1 = URL.createObjectURL(blob);
                    // console.log(url1);
                    // var dialog2 = new goog.ui.Dialog(null, true, new goog.dom.DomHelper(top.document));
                    // dialog2.setTitle("Text to Blocks 1");
                    // var content = goog.html.SafeHtml.create('img', {'src':"static/media/backpack-closed.png"});
                    // dialog2.setSafeHtmlContent(content);
                    // dialog2.setVisible(true);
                    new PNG().readFromBlob(blob, returnPng);

                    // testThing = document.createElement("img");
                    // testThing.src = url1;
                    // document.body.appendChild(testThing);
                    });
                }
            }

            var workspace = myBlock.workspace;

            var trees = Blockly.VenbraceParser.fullParses(myBlock.getFieldValue('CODE'));
            //console.log(tree);
            /*
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
            */
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