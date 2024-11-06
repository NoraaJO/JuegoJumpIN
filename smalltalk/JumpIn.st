| ventana 
colorBlanco1 
colorAzulClaro 
colorAzulOscuro 
cont_Menu cont_Game cont_Create cont_Help cont_About cont_Title 
text_Title text_About1text_About2text_About3text_About4text_About5text_About6text_About7text_About8btn_Play btn_Create btn_About btn_Help btn_BackGame btn_BackCreate btn_BackHelp btn_BackAbout input validNumber|

ventana := SystemWindow new.
ventana setLabel: 'JumpIn'.

"
COLORES RGB

Dividar los datos RGB entre 255 cada uno
como sale a continuacion:
"
colorBlanco1    := Color r: (216/255) g: (233/255) b: (242/255).
colorAzulClaro  := Color r: (43/255)  g: (81/255)  b: (140/255).
colorAzulOscuro := Color r: (24/255)  g: (44/255)  b: (77/255).

cont_Title := AlignmentMorph newRow.

cont_Title color: colorAzulClaro.
cont_Title extent: 730@50.
cont_Title position: 10@35.
cont_Title listDirection: #leftTopRight.
cont_Title borderWidth: 4; borderColor: Color black.

text_Title := StringMorph new.
text_Title contents: '   JumpIn Menu'; color: colorBlanco1.
text_Title font: (TextStyle default fontOfSize: 50).
text_Title extent: 730@40.
text_Title position: 500@30.

cont_Title addMorph: text_Title."---------------------------> Crear el contenedor de Juego <---------------------------"cont_Game := Morph new.cont_Game color: colorBlanco1.cont_Game extent: 730@570.cont_Game position: 10@80.cont_Game hResizing: #spaceFill; vResizing: #spaceFill.cont_Game borderWidth: 4; borderColor: Color black.btn_BackGame := SimpleButtonMorph new label: '<--' font: (TextStyle default fontOfSize: 25).btn_BackGame position: 690@46.btn_BackGame extent: 35@25.btn_BackGame color: colorAzulClaro.cont_Game  addMorph: btn_BackGame."---------------------------> Crear el contenedor de Crear <---------------------------"cont_Create := Morph new.cont_Create color: colorBlanco1.cont_Create extent: 730@570.cont_Create position: 10@80.cont_Create hResizing: #spaceFill; vResizing: #spaceFill.cont_Create borderWidth: 4; borderColor: Color black.btn_BackCreate := SimpleButtonMorph new label: '<--' font: (TextStyle default fontOfSize: 25).btn_BackCreate position: 690@46.btn_BackCreate extent: 35@25.btn_BackCreate color: colorAzulClaro.cont_Create  addMorph: btn_BackCreate."---------------------------> Crear el contenedor de Help <---------------------------"cont_Help := Morph new.cont_Help color: colorBlanco1.cont_Help extent: 730@570.cont_Help position: 10@80.cont_Help hResizing: #spaceFill; vResizing: #spaceFill.cont_Help borderWidth: 4; borderColor: Color black.btn_BackHelp := SimpleButtonMorph new label: '<--' font: (TextStyle default fontOfSize: 25).btn_BackHelp position: 690@46.btn_BackHelp extent: 35@25.btn_BackHelp color: colorAzulClaro.cont_Help  addMorph: btn_BackHelp.
"---------------------------> Crear el contenedor de About <---------------------------"cont_About := Morph new.cont_About color: colorBlanco1.cont_About extent: 730@570.cont_About position: 10@80.cont_About hResizing: #spaceFill; vResizing: #spaceFill.cont_About borderWidth: 4; borderColor: Color black.text_About1 := StringMorph new.text_About1 contents: ' Instituto Tecnológico de Costa Rica'; color: colorAzulOscuro.text_About1 font: (TextStyle default fontOfSize: 20).text_About1 extent: 730@40.text_About1 position: 200@150.text_About2 := StringMorph new.text_About2 contents: 'Ingeniería en Computación'; color: colorAzulOscuro.text_About2 font: (TextStyle default fontOfSize: 20).text_About2 extent: 730@40.text_About2 position: 205@175.text_About3 := StringMorph new.text_About3 contents: 'Lenguajes de Programación'; color: colorAzulOscuro.text_About3 font: (TextStyle default fontOfSize: 20).text_About3 extent: 730@40.text_About3 position: 205@200.text_About4 := StringMorph new.text_About4 contents: 'Joseph Quirós Calvo     c.2022040595'; color: colorAzulOscuro.text_About4 font: (TextStyle default fontOfSize: 20).text_About4 extent: 730@40.text_About4 position: 205@300.text_About5 := StringMorph new.text_About5 contents: 'Aarón Ortiz Jimenez     c:2022437529'; color: colorAzulOscuro.text_About5 font: (TextStyle default fontOfSize: 20).text_About5 extent: 730@40.text_About5 position: 205@325.text_About6 := StringMorph new.text_About6 contents: '"Si no te conoces a ti mismo ni a tu oponente, '; color: colorAzulOscuro.text_About6 font: (TextStyle default fontOfSize: 20).text_About6 extent: 730@40.text_About6 position: 205@425.text_About7 := StringMorph new.text_About7 contents: 'en cada batalla serás derrotado..."'; color: colorAzulOscuro.text_About7 font: (TextStyle default fontOfSize: 20).text_About7 extent: 730@40.text_About7 position: 205@450.text_About8 := StringMorph new.text_About8 contents: 'Sun Tzu'; color: colorAzulOscuro.text_About8 font: (TextStyle default fontOfSize: 20).text_About8 extent: 730@40.text_About8 position: 475@475.btn_BackAbout := SimpleButtonMorph new label: '<--' font: (TextStyle default fontOfSize: 25).btn_BackAbout position: 690@46.btn_BackAbout extent: 35@25.btn_BackAbout color: colorAzulClaro.cont_About  addMorph: text_About1.cont_About  addMorph: text_About2.cont_About  addMorph: text_About3.cont_About  addMorph: text_About4.cont_About  addMorph: text_About5.cont_About  addMorph: text_About6.cont_About  addMorph: text_About7.cont_About  addMorph: text_About8.cont_About  addMorph: btn_BackAbout.

"---------------------------> Crear el contenedor de Menu <---------------------------"
cont_Menu := Morph new.
cont_Menu color: colorBlanco1.
cont_Menu extent: 730@570.
cont_Menu position: 10@80.
cont_Menu hResizing: #spaceFill; vResizing: #spaceFill.
cont_Menu borderWidth: 4; borderColor: Color black.

btn_Play := SimpleButtonMorph new label: 'PLAY' font: (TextStyle default fontOfSize: 25).btn_Play position: 240@200.btn_Play extent: 250@30.btn_Play color: colorAzulClaro.
btn_Create := SimpleButtonMorph new label: 'CREAR' font: (TextStyle default fontOfSize: 25).btn_Create position: 240@275.btn_Create extent: 250@30.btn_Create color: colorAzulClaro.btn_Help := SimpleButtonMorph new label: 'AYUDA' font: (TextStyle default fontOfSize: 25).btn_Help position: 240@350.btn_Help extent: 250@30.btn_Help color: colorAzulClaro.
btn_About := SimpleButtonMorph new label: 'ACERCA DE' font: (TextStyle default fontOfSize: 25).btn_About position: 240@425.btn_About extent: 250@30.btn_About color: colorAzulClaro.cont_Menu addMorph: btn_Play.cont_Menu addMorph: btn_Create.cont_Menu addMorph: btn_Help.cont_Menu addMorph: btn_About."---------------------------> Acciones de Botones <---------------------------"btn_Play on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Jugar'; color: colorBlanco1.		validNumber := false.	[validNumber not] whileTrue: [	    input := UIManager default 	        request: 'Ingrese un número entre 1 y 60' 	        initialAnswer: ''.	    "Si el usuario presiona cancelar, termina el ciclo"	    input isNil ifTrue: [^Transcript show: 'Operación cancelada.'; cr].	    "Verifica si la entrada es un número dentro del rango"	    (input isAllDigits and: [input asInteger between: 1 and: 60])	        ifTrue: [	            validNumber := true.	            Transcript show: 'Nivel seleccionado: ', input; cr.	        ]	        ifFalse: [	            UIManager default inform: 'Por favor ingrese un número válido entre 1 y 60.'.	        ].	].		ventana removeMorph: cont_Menu. 	ventana addMorph: cont_Game.].btn_Create  on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Crear'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_Create.].btn_Help on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Ayuda'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_Help.].btn_About on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Acerca De'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_About.].btn_BackGame on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_Game. 	ventana addMorph: cont_Menu.].btn_BackCreate on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_Create. 	ventana addMorph: cont_Menu.].btn_BackAbout on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_About. 	ventana addMorph: cont_Menu.].btn_BackHelp on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_Help. 	ventana addMorph: cont_Menu.].
ventana addMorph: cont_Create.
ventana addMorph: cont_Game.
ventana addMorph: cont_About.
ventana addMorph: cont_Help.
ventana addMorph: cont_Title.ventana addMorph: cont_Menu.

ventana openInWorld.
ventana extent: 750@660.ventana position: 475@200.
ventana color: colorAzulOscuro.
