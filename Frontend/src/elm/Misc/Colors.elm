{- Material Design 2014 colors -}


module Misc.Colors exposing (amber100, amber200, amber300, amber400, amber50, amber500, amber600, amber700, amber800, amber900, amberA100, amberA200, amberA400, amberA700, black, blue100, blue200, blue300, blue400, blue50, blue500, blue600, blue700, blue800, blue900, blueA100, blueA200, blueA400, blueA700, blueGray100, blueGray200, blueGray300, blueGray400, blueGray50, blueGray500, blueGray600, blueGray700, blueGray800, blueGray900, brown100, brown200, brown300, brown400, brown50, brown500, brown600, brown700, brown800, brown900, cyan100, cyan200, cyan300, cyan400, cyan50, cyan500, cyan600, cyan700, cyan800, cyan900, cyanA100, cyanA200, cyanA400, cyanA700, deepOrange100, deepOrange200, deepOrange300, deepOrange400, deepOrange50, deepOrange500, deepOrange600, deepOrange700, deepOrange800, deepOrange900, deepOrangeA100, deepOrangeA200, deepOrangeA400, deepOrangeA700, deeppurple100, deeppurple200, deeppurple300, deeppurple400, deeppurple50, deeppurple500, deeppurple600, deeppurple700, deeppurple800, deeppurple900, deeppurpleA100, deeppurpleA200, deeppurpleA400, deeppurpleA700, gray100, gray200, gray300, gray400, gray50, gray500, gray600, gray700, gray800, gray900, grayed, green100, green200, green300, green400, green500, green600, green700, green800, green900, greenA100, greenA200, greenA400, greenA700, greengreen50, indigo100, indigo200, indigo300, indigo400, indigo50, indigo500, indigo600, indigo700, indigo800, indigo900, indigoA100, indigoA200, indigoA400, indigoA700, lightBlue100, lightBlue200, lightBlue300, lightBlue400, lightBlue50, lightBlue500, lightBlue600, lightBlue700, lightBlue800, lightBlue900, lightBlueA100, lightBlueA200, lightBlueA400, lightBlueA700, lightGreen100, lightGreen200, lightGreen300, lightGreen400, lightGreen50, lightGreen500, lightGreen600, lightGreen700, lightGreen800, lightGreen900, lightGreenA100, lightGreenA200, lightGreenA400, lightGreenA700, lime100, lime200, lime300, lime400, lime50, lime500, lime600, lime700, lime800, lime900, limeA100, limeA200, limeA400, limeA700, orange100, orange200, orange300, orange400, orange50, orange500, orange600, orange700, orange800, orange900, orangeA100, orangeA200, orangeA400, orangeA700, pink100, pink200, pink300, pink400, pink50, pink500, pink600, pink700, pink800, pink900, pinkA100, pinkA200, pinkA400, pinkA700, purple100, purple200, purple300, purple400, purple50, purple500, purple600, purple700, purple800, purple900, purpleA100, purpleA200, purpleA400, purpleA700, red100, red200, red300, red400, red50, red500, red600, red700, red800, red900, redA100, redA200, redA400, redA700, rgbHex, rgbaHex, teal100, teal200, teal300, teal400, teal50, teal500, teal600, teal700, teal800, teal900, tealA100, tealA200, tealA400, tealA700, white, yellow100, yellow200, yellow300, yellow400, yellow50, yellow500, yellow600, yellow700, yellow800, yellow900, yellowA100, yellowA200, yellowA400, yellowA700)

import Bitwise exposing (and, shiftRightBy)
import Element exposing (rgb255, rgba255)



-- HELPERS


rgbHex hex =
    rgb255
        (hex |> shiftRightBy 16 |> and 0xFF)
        (hex |> shiftRightBy 8 |> and 0xFF)
        (hex |> and 0xFF)


rgbaHex hex alpha =
    rgba255
        (hex |> shiftRightBy 16 |> and 0xFF)
        (hex |> shiftRightBy 8 |> and 0xFF)
        (hex |> and 0xFF)
        alpha



-- CUSTOM COLORS


grayed =
    rgbHex 0x00B2DEDA



--- MATERIAL COLORS


red50 =
    rgbHex 0x00FFEBEE


red100 =
    rgbHex 0x00FFCDD2


red200 =
    rgbHex 0x00EF9A9A


red300 =
    rgbHex 0x00E57373


red400 =
    rgbHex 0x00EF5350


red500 =
    rgbHex 0x00F44336


red600 =
    rgbHex 0x00E53935


red700 =
    rgbHex 0x00D32F2F


red800 =
    rgbHex 0x00C62828


red900 =
    rgbHex 0x00B71C1C


redA100 =
    rgbHex 0x00FF8A80


redA200 =
    rgbHex 0x00FF5252


redA400 =
    rgbHex 0x00FF1744


redA700 =
    rgbHex 0x00D50000


pink50 =
    rgbHex 0x00FCE4EC


pink100 =
    rgbHex 0x00F8BBD0


pink200 =
    rgbHex 0x00F48FB1


pink300 =
    rgbHex 0x00F06292


pink400 =
    rgbHex 0x00EC407A


pink500 =
    rgbHex 0x00E91E63


pink600 =
    rgbHex 0x00D81B60


pink700 =
    rgbHex 0x00C2185B


pink800 =
    rgbHex 0x00AD1457


pink900 =
    rgbHex 0x00880E4F


pinkA100 =
    rgbHex 0x00FF80AB


pinkA200 =
    rgbHex 0x00FF4081


pinkA400 =
    rgbHex 0x00F50057


pinkA700 =
    rgbHex 0x00C51162


purple50 =
    rgbHex 0x00F3E5F5


purple100 =
    rgbHex 0x00E1BEE7


purple200 =
    rgbHex 0x00CE93D8


purple300 =
    rgbHex 0x00BA68C8


purple400 =
    rgbHex 0x00AB47BC


purple500 =
    rgbHex 0x009C27B0


purple600 =
    rgbHex 0x008E24AA


purple700 =
    rgbHex 0x007B1FA2


purple800 =
    rgbHex 0x006A1B9A


purple900 =
    rgbHex 0x004A148C


purpleA100 =
    rgbHex 0x00EA80FC


purpleA200 =
    rgbHex 0x00E040FB


purpleA400 =
    rgbHex 0x00D500F9


purpleA700 =
    rgbHex 0x00AA00FF


deeppurple50 =
    rgbHex 0x00EDE7F6


deeppurple100 =
    rgbHex 0x00D1C4E9


deeppurple200 =
    rgbHex 0x00B39DDB


deeppurple300 =
    rgbHex 0x009575CD


deeppurple400 =
    rgbHex 0x007E57C2


deeppurple500 =
    rgbHex 0x00673AB7


deeppurple600 =
    rgbHex 0x005E35B1


deeppurple700 =
    rgbHex 0x00512DA8


deeppurple800 =
    rgbHex 0x004527A0


deeppurple900 =
    rgbHex 0x00311B92


deeppurpleA100 =
    rgbHex 0x00B388FF


deeppurpleA200 =
    rgbHex 0x007C4DFF


deeppurpleA400 =
    rgbHex 0x00651FFF


deeppurpleA700 =
    rgbHex 0x006200EA


indigo50 =
    rgbHex 0x00E8EAF6


indigo100 =
    rgbHex 0x00C5CAE9


indigo200 =
    rgbHex 0x009FA8DA


indigo300 =
    rgbHex 0x007986CB


indigo400 =
    rgbHex 0x005C6BC0


indigo500 =
    rgbHex 0x003F51B5


indigo600 =
    rgbHex 0x003949AB


indigo700 =
    rgbHex 0x00303F9F


indigo800 =
    rgbHex 0x00283593


indigo900 =
    rgbHex 0x001A237E


indigoA100 =
    rgbHex 0x008C9EFF


indigoA200 =
    rgbHex 0x00536DFE


indigoA400 =
    rgbHex 0x003D5AFE


indigoA700 =
    rgbHex 0x00304FFE


blue50 =
    rgbHex 0x00E3F2FD


blue100 =
    rgbHex 0x00BBDEFB


blue200 =
    rgbHex 0x0090CAF9


blue300 =
    rgbHex 0x0064B5F6


blue400 =
    rgbHex 0x0042A5F5


blue500 =
    rgbHex 0x002196F3


blue600 =
    rgbHex 0x001E88E5


blue700 =
    rgbHex 0x001976D2


blue800 =
    rgbHex 0x001565C0


blue900 =
    rgbHex 0x000D47A1


blueA100 =
    rgbHex 0x0082B1FF


blueA200 =
    rgbHex 0x00448AFF


blueA400 =
    rgbHex 0x002979FF


blueA700 =
    rgbHex 0x002962FF


lightBlue50 =
    rgbHex 0x00E1F5FE


lightBlue100 =
    rgbHex 0x00B3E5FC


lightBlue200 =
    rgbHex 0x0081D4FA


lightBlue300 =
    rgbHex 0x004FC3F7


lightBlue400 =
    rgbHex 0x0029B6F6


lightBlue500 =
    rgbHex 0x0003A9F4


lightBlue600 =
    rgbHex 0x00039BE5


lightBlue700 =
    rgbHex 0x000288D1


lightBlue800 =
    rgbHex 0x000277BD


lightBlue900 =
    rgbHex 0x0001579B


lightBlueA100 =
    rgbHex 0x0080D8FF


lightBlueA200 =
    rgbHex 0x0040C4FF


lightBlueA400 =
    rgbHex 0xB0FF


lightBlueA700 =
    rgbHex 0x91EA


cyan50 =
    rgbHex 0x00E0F7FA


cyan100 =
    rgbHex 0x00B2EBF2


cyan200 =
    rgbHex 0x0080DEEA


cyan300 =
    rgbHex 0x004DD0E1


cyan400 =
    rgbHex 0x0026C6DA


cyan500 =
    rgbHex 0xBCD4


cyan600 =
    rgbHex 0xACC1


cyan700 =
    rgbHex 0x97A7


cyan800 =
    rgbHex 0x838F


cyan900 =
    rgbHex 0x6064


cyanA100 =
    rgbHex 0x0084FFFF


cyanA200 =
    rgbHex 0x0018FFFF


cyanA400 =
    rgbHex 0xE5FF


cyanA700 =
    rgbHex 0xB8D4


teal50 =
    rgbHex 0x00E0F2F1


teal100 =
    rgbHex 0x00B2DFDB


teal200 =
    rgbHex 0x0080CBC4


teal300 =
    rgbHex 0x004DB6AC


teal400 =
    rgbHex 0x0026A69A


teal500 =
    rgbHex 0x9688


teal600 =
    rgbHex 0x897B


teal700 =
    rgbHex 0x796B


teal800 =
    rgbHex 0x695C


teal900 =
    rgbHex 0x4D40


tealA100 =
    rgbHex 0x00A7FFEB


tealA200 =
    rgbHex 0x0064FFDA


tealA400 =
    rgbHex 0x001DE9B6


tealA700 =
    rgbHex 0xBFA5


greengreen50 =
    rgbHex 0x00E8F5E9


green100 =
    rgbHex 0x00C8E6C9


green200 =
    rgbHex 0x00A5D6A7


green300 =
    rgbHex 0x0081C784


green400 =
    rgbHex 0x0066BB6A


green500 =
    rgbHex 0x004CAF50


green600 =
    rgbHex 0x0043A047


green700 =
    rgbHex 0x00388E3C


green800 =
    rgbHex 0x002E7D32


green900 =
    rgbHex 0x001B5E20


greenA100 =
    rgbHex 0x00B9F6CA


greenA200 =
    rgbHex 0x0069F0AE


greenA400 =
    rgbHex 0xE676


greenA700 =
    rgbHex 0xC853


lightGreen50 =
    rgbHex 0x00F1F8E9


lightGreen100 =
    rgbHex 0x00DCEDC8


lightGreen200 =
    rgbHex 0x00C5E1A5


lightGreen300 =
    rgbHex 0x00AED581


lightGreen400 =
    rgbHex 0x009CCC65


lightGreen500 =
    rgbHex 0x008BC34A


lightGreen600 =
    rgbHex 0x007CB342


lightGreen700 =
    rgbHex 0x00689F38


lightGreen800 =
    rgbHex 0x00558B2F


lightGreen900 =
    rgbHex 0x0033691E


lightGreenA100 =
    rgbHex 0x00CCFF90


lightGreenA200 =
    rgbHex 0x00B2FF59


lightGreenA400 =
    rgbHex 0x0076FF03


lightGreenA700 =
    rgbHex 0x0064DD17


lime50 =
    rgbHex 0x00F9FBE7


lime100 =
    rgbHex 0x00F0F4C3


lime200 =
    rgbHex 0x00E6EE9C


lime300 =
    rgbHex 0x00DCE775


lime400 =
    rgbHex 0x00D4E157


lime500 =
    rgbHex 0x00CDDC39


lime600 =
    rgbHex 0x00C0CA33


lime700 =
    rgbHex 0x00AFB42B


lime800 =
    rgbHex 0x009E9D24


lime900 =
    rgbHex 0x00827717


limeA100 =
    rgbHex 0x00F4FF81


limeA200 =
    rgbHex 0x00EEFF41


limeA400 =
    rgbHex 0x00C6FF00


limeA700 =
    rgbHex 0x00AEEA00


yellow50 =
    rgbHex 0x00FFFDE7


yellow100 =
    rgbHex 0x00FFF9C4


yellow200 =
    rgbHex 0x00FFF59D


yellow300 =
    rgbHex 0x00FFF176


yellow400 =
    rgbHex 0x00FFEE58


yellow500 =
    rgbHex 0x00FFEB3B


yellow600 =
    rgbHex 0x00FDD835


yellow700 =
    rgbHex 0x00FBC02D


yellow800 =
    rgbHex 0x00F9A825


yellow900 =
    rgbHex 0x00F57F17


yellowA100 =
    rgbHex 0x00FFFF8D


yellowA200 =
    rgbHex 0x00FFFF00


yellowA400 =
    rgbHex 0x00FFEA00


yellowA700 =
    rgbHex 0x00FFD600


amber50 =
    rgbHex 0x00FFF8E1


amber100 =
    rgbHex 0x00FFECB3


amber200 =
    rgbHex 0x00FFE082


amber300 =
    rgbHex 0x00FFD54F


amber400 =
    rgbHex 0x00FFCA28


amber500 =
    rgbHex 0x00FFC107


amber600 =
    rgbHex 0x00FFB300


amber700 =
    rgbHex 0x00FFA000


amber800 =
    rgbHex 0x00FF8F00


amber900 =
    rgbHex 0x00FF6F00


amberA100 =
    rgbHex 0x00FFE57F


amberA200 =
    rgbHex 0x00FFD740


amberA400 =
    rgbHex 0x00FFC400


amberA700 =
    rgbHex 0x00FFAB00


orange50 =
    rgbHex 0x00FFF3E0


orange100 =
    rgbHex 0x00FFE0B2


orange200 =
    rgbHex 0x00FFCC80


orange300 =
    rgbHex 0x00FFB74D


orange400 =
    rgbHex 0x00FFA726


orange500 =
    rgbHex 0x00FF9800


orange600 =
    rgbHex 0x00FB8C00


orange700 =
    rgbHex 0x00F57C00


orange800 =
    rgbHex 0x00EF6C00


orange900 =
    rgbHex 0x00E65100


orangeA100 =
    rgbHex 0x00FFD180


orangeA200 =
    rgbHex 0x00FFAB40


orangeA400 =
    rgbHex 0x00FF9100


orangeA700 =
    rgbHex 0x00FF6D00


deepOrange50 =
    rgbHex 0x00FBE9E7


deepOrange100 =
    rgbHex 0x00FFCCBC


deepOrange200 =
    rgbHex 0x00FFAB91


deepOrange300 =
    rgbHex 0x00FF8A65


deepOrange400 =
    rgbHex 0x00FF7043


deepOrange500 =
    rgbHex 0x00FF5722


deepOrange600 =
    rgbHex 0x00F4511E


deepOrange700 =
    rgbHex 0x00E64A19


deepOrange800 =
    rgbHex 0x00D84315


deepOrange900 =
    rgbHex 0x00BF360C


deepOrangeA100 =
    rgbHex 0x00FF9E80


deepOrangeA200 =
    rgbHex 0x00FF6E40


deepOrangeA400 =
    rgbHex 0x00FF3D00


deepOrangeA700 =
    rgbHex 0x00DD2C00


brown50 =
    rgbHex 0x00EFEBE9


brown100 =
    rgbHex 0x00D7CCC8


brown200 =
    rgbHex 0x00BCAAA4


brown300 =
    rgbHex 0x00A1887F


brown400 =
    rgbHex 0x008D6E63


brown500 =
    rgbHex 0x00795548


brown600 =
    rgbHex 0x006D4C41


brown700 =
    rgbHex 0x005D4037


brown800 =
    rgbHex 0x004E342E


brown900 =
    rgbHex 0x003E2723


gray50 =
    rgbHex 0x00FAFAFA


gray100 =
    rgbHex 0x00F5F5F5


gray200 =
    rgbHex 0x00EEEEEE


gray300 =
    rgbHex 0x00E0E0E0


gray400 =
    rgbHex 0x00BDBDBD


gray500 =
    rgbHex 0x009E9E9E


gray600 =
    rgbHex 0x00757575


gray700 =
    rgbHex 0x00616161


gray800 =
    rgbHex 0x00424242


gray900 =
    rgbHex 0x00212121


blueGray50 =
    rgbHex 0x00ECEFF1


blueGray100 =
    rgbHex 0x00CFD8DC


blueGray200 =
    rgbHex 0x00B0BEC5


blueGray300 =
    rgbHex 0x0090A4AE


blueGray400 =
    rgbHex 0x0078909C


blueGray500 =
    rgbHex 0x00607D8B


blueGray600 =
    rgbHex 0x00546E7A


blueGray700 =
    rgbHex 0x00455A64


blueGray800 =
    rgbHex 0x0037474F


blueGray900 =
    rgbHex 0x00263238


black =
    rgbHex 0x00


white =
    rgbHex 0x00FFFFFF
