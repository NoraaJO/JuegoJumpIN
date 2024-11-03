| ventana 
colorBlanco1 
colorAzulClaro 
colorAzulOscuro
cont_Menu cont_Gamecont_Createcont_Helpcont_Aboutcont_Title
text_Title btn_Playbtn_Createbtn_Aboutbtn_Helpbtn_BackGamebtn_BackCreatebtn_BackHelpbtn_BackAbout|

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

cont_Title addMorph: text_Title."---------------------------> Crear el contenedor de Juego <---------------------------"cont_Game := Morph new.cont_Game color: colorBlanco1.cont_Game extent: 730@570.cont_Game position: 163@105.cont_Game hResizing: #spaceFill; vResizing: #spaceFill.cont_Game borderWidth: 4; borderColor: Color black.btn_BackGame := SimpleButtonMorph new label: 'ATRAS' font: (TextStyle default fontOfSize: 25).btn_BackGame position: 600@600.btn_BackGame extent: 250@30.btn_BackGame color: colorAzulClaro.cont_Game  addMorph: btn_BackGame."---------------------------> Crear el contenedor de Crear <---------------------------"cont_Create := Morph new.cont_Create color: colorBlanco1.cont_Create extent: 730@570.cont_Create position: 163@105.cont_Create hResizing: #spaceFill; vResizing: #spaceFill.cont_Create borderWidth: 4; borderColor: Color black.btn_BackCreate := SimpleButtonMorph new label: 'ATRAS' font: (TextStyle default fontOfSize: 25).btn_BackCreate position: 600@600.btn_BackCreate extent: 250@30.btn_BackCreate color: colorAzulClaro.cont_Create  addMorph: btn_BackCreate."---------------------------> Crear el contenedor de Help <---------------------------"cont_Help := Morph new.cont_Help color: colorBlanco1.cont_Help extent: 730@570.cont_Help position: 163@105.cont_Help hResizing: #spaceFill; vResizing: #spaceFill.cont_Help borderWidth: 4; borderColor: Color black.btn_BackHelp := SimpleButtonMorph new label: 'ATRAS' font: (TextStyle default fontOfSize: 25).btn_BackHelp position: 600@600.btn_BackHelp extent: 250@30.btn_BackHelp color: colorAzulClaro.cont_Help  addMorph: btn_BackHelp.
"---------------------------> Crear el contenedor de About <---------------------------"cont_About := Morph new.cont_About color: colorBlanco1.cont_About extent: 730@570.cont_About position: 163@105.cont_About hResizing: #spaceFill; vResizing: #spaceFill.cont_About borderWidth: 4; borderColor: Color black.btn_BackAbout := SimpleButtonMorph new label: 'ATRAS' font: (TextStyle default fontOfSize: 25).btn_BackAbout position: 600@600.btn_BackAbout extent: 250@30.btn_BackAbout color: colorAzulClaro.cont_About  addMorph: btn_BackAbout.

"---------------------------> Crear el contenedor de Menu <---------------------------"
cont_Menu := Morph new.
cont_Menu color: colorBlanco1.
cont_Menu extent: 730@570.
cont_Menu position: 10@80.
cont_Menu hResizing: #spaceFill; vResizing: #spaceFill.
cont_Menu borderWidth: 4; borderColor: Color black.

btn_Play := SimpleButtonMorph new label: 'PLAY' font: (TextStyle default fontOfSize: 25).btn_Play position: 240@200.btn_Play extent: 250@30.btn_Play color: colorAzulClaro.
btn_Create := SimpleButtonMorph new label: 'CREAR' font: (TextStyle default fontOfSize: 25).btn_Create position: 240@275.btn_Create extent: 250@30.btn_Create color: colorAzulClaro.btn_Help := SimpleButtonMorph new label: 'AYUDA' font: (TextStyle default fontOfSize: 25).btn_Help position: 240@350.btn_Help extent: 250@30.btn_Help color: colorAzulClaro.
btn_About := SimpleButtonMorph new label: 'ACERCA DE' font: (TextStyle default fontOfSize: 25).btn_About position: 240@425.btn_About extent: 250@30.btn_About color: colorAzulClaro.btn_Play on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Jugar'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_Game.].btn_Create  on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Crear'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_Create.].btn_Help on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Ayuda'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_About.].btn_About on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Acerca De'; color: colorBlanco1.	ventana removeMorph: cont_Menu. 	ventana addMorph: cont_About.].btn_BackGame on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_Game. 	ventana addMorph: cont_Menu.].btn_BackCreate on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_Create. 	ventana addMorph: cont_Menu.].btn_BackAbout on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_About. 	ventana addMorph: cont_Menu.].btn_BackHelp on: #mouseUp send: #value to: [ 	text_Title contents: '   JumpIn: Menu'; color: colorBlanco1.	ventana removeMorph: cont_About. 	ventana addMorph: cont_Menu.].

cont_Menu addMorph: btn_Play.
cont_Menu addMorph: btn_Create.cont_Menu addMorph: btn_Help.
cont_Menu addMorph: btn_About.


ventana addMorph: cont_Menu.
ventana addMorph: cont_Title.

ventana openInWorld.
ventana extent: 750@660.
ventana color: colorAzulOscuro.
