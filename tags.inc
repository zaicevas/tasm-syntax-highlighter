; START/END TAGS
    base_tags db "<!DOCTYPE html>", 13, 10, "<html>", 13, 10, "<head>", 13, 10, "<style>", 13, 10
    base_tags_length EQU ($ - base_tags)

    dark_theme db  "g {color:rgba(152, 251, 152, 0.822)}", 13, 10, "r {color:Tomato}", 13, 10, "b {color:rgb(0, 132, 255)}", 13, 10, \
    "pu {color:rgb(150, 150, 255)}", 13, 10, "o {color:rgb(255, 230, 0)}", 13, 10, "t {color:rgb(200, 130, 125)}", 13, 10, "</style>", 13, 10, "<pre>"
    dark_theme_length EQU ($ - dark_theme)

    white_theme db "g {color:green}", 13, 10, "r {color:red}", 13, 10, "b {color:blue}", 13, 10, "pu {color:purple}", 13, 10, "o {color:orange}", \
    13, 10, "t {color:rgb(128, 88, 238)}", 13, 10, "</style>", 13, 10, "<pre>"
    white_theme_length EQU ($ - white_theme)

    dark_background db "<body bgcolor=", '"', "#1b1e23", '"',  " text=", '"', "#F0F8FF", '"', '>'
    dark_background_length EQU ($ - dark_background)

    ending_tags2 db "</h1><r>register</r> <b>instruction</b> <g>comment</g> ", \
    "<pu>immediate operand</pu> <t>data type</t> <o>label</o>", 13, 10, 13, 10
    ending_tags2_length EQU ($ - ending_tags2)

    h1_tag db "<h1>"
    ending db 13, 10, "</pre>", 13, 10, "</body>", 13, 10, "</html>"
    ending_length EQU ($ - ending)

; TAGS FOR HIGHLIGHTING:
    gtag db "<g>", 0
    gtagoff db "</g>", 0
    rtag db "<r>", 0
    rtagoff db "</r>", 0
    btag db "<b>", 0
    btagoff db "</b>", 0
    putag db "<pu>", 0
    putagoff db "</pu>", 0
    otag db "<o>", 0
    otagoff db "</o>", 0
    ttag db "<t>", 0
    ttagoff db "</t>", 0
