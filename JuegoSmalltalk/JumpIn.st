| ventana colorBlanco1 colorAzulClaro colorAzulOscuro cont_Base text_Title icon_AcercaDe |

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

"Crear el contenedor base"
cont_Base := AlignmentMorph newRow.

cont_Base color: colorBlanco1.
cont_Base extent: 730@590.
cont_Base position: 10@50.
cont_Base hResizing: #spaceFill; vResizing: #spaceFill.
cont_Base borderWidth: 4; borderColor: Color black.

cont_Title := AlignmentMorph newRow.

cont_Title color: colorAzulClaro.
cont_Title extent: 730@50.
cont_Title position: 10@25.
cont_Title listDirection: #leftTopRight.
cont_Title borderWidth: 4; borderColor: Color black.

text_Title := StringMorph new.
text_Title contents: '   JumpIn Menu'; color: colorBlanco1.
text_Title font: (TextStyle default fontOfSize: 50).
text_Title extent: 730@60.
text_Title position: 500@30.

"icon_AcercaDe := (Form fromFileNamed: 'JuegoSmalltalk//AcercaDe.png') ifNil: [Form dotOfSize: 24]."

"btn_AcercaDe := IconicButtonMorph new.
btn_AcercaDe icon: icon_AcercaDe."

cont_Title addMorph: text_Title.
"cont_Title addMorph: btn_AcercaDe."

ventana addMorph: cont_Base.
ventana addMorph: cont_Title.

ventana openInWorld.
ventana extent: 750@650.
ventana color: colorAzulOscuro.
