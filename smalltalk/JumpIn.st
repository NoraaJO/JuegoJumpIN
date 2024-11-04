| ventana 
colorBlanco1 
colorAzulClaro 
colorAzulOscuro 
cont_Menu 
text_Title 

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

cont_Title addMorph: text_Title.


"---------------------------> Crear el contenedor de Menu <---------------------------"
cont_Menu := Morph new.
cont_Menu color: colorBlanco1.
cont_Menu extent: 730@570.
cont_Menu position: 10@80.
cont_Menu hResizing: #spaceFill; vResizing: #spaceFill.
cont_Menu borderWidth: 4; borderColor: Color black.

btn_Play := SimpleButtonMorph new label: 'PLAY' font: (TextStyle default fontOfSize: 25).
btn_Create := SimpleButtonMorph new label: 'CREAR' font: (TextStyle default fontOfSize: 25).
btn_About := SimpleButtonMorph new label: 'ACERCA DE' font: (TextStyle default fontOfSize: 25).



ventana addMorph: cont_Menu.
ventana addMorph: cont_Title.

ventana openInWorld.
ventana extent: 750@660.
ventana color: colorAzulOscuro.