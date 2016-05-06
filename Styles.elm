module Styles where


lightGrey = "#eee"
darkGrey = "#333"
white = "white"
primary = "#ff4593"
secondary = "#45ff4d"

containerStyle : List (String, String)
containerStyle =
  widthConstrained
    ++ [ ("display", "flex")
       , ("flexDirection", "column")
       , ("justifyContent", "space-between") ]


widthConstrained : List (String, String)
widthConstrained =
  [ ("maxWidth", "900px")
  , ("width", "100%")
  ]


mediumPadding : List (String, String)
mediumPadding =
  [ ("padding", "1em") ]


mediumMargin : List (String, String)
mediumMargin =
  [ ("margin", "1em") ]


pageStyle : List (String, String)
pageStyle =
  [ ("backgroundColor", lightGrey)
  , ("display", "flex")
  , ("justifyContent", "center")
  , ("height", "100%")
  ]
