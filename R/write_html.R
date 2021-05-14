write_css <- function() {
  as.character(shiny::tags$style(shiny::HTML('
    .zoomDiv {
      opacity: 0;
      position: fixed;
      top: 50%;
      left: 50%;
      z-index: 50;
      transform: translate(-50%, -50%);
      box-shadow: 0px 0px 3000px 800px #888888;
      max-height: 100%;
      overflow: scroll;
    }
    .zoomImg {width: 100%}
    a {font-style: italic}
    figure {
      text-align: center;
      margin: 2%;
      display: inline-block;
      width: 96%;
    }
    .figimg{
      margin: 0;
      display: inline-block;
    }
    figcaption{
      max-width: 90%;
      overflow: hidden;
      margin-left: 5%;
    }
    div{
      vertical-align:middle;
    }
    .imgContainer {
      text-align: center;
      text-indent: 0;
      display: inline-block;
      border: thin silver solid;
    }
    .small {max-width: 18%}
    .medium {max-width: 23%}
    .large {max-width: 32%}
    .imgsmall {width: 45%}
    .imgmedium {width: 30%}
    .imglarge {width: 22%}
    img {width: 100%}
    img:hover {
      transition: .2s;
      -moz-box-shadow: 0 0 20px #ccc;
      -webkit-box-shadow: 0 0 20px #ccc;
      box-shadow: 0 12 12px #ccc;
    }
  ')))
}


write_jscript <- function(){
  as.character(paste0(
    shiny::tags$script(type="text/javascript", 
                src="http://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"
    ), "\n",
    shiny::tags$script(type="text/javascript", shiny::HTML('
      $(document).ready(function() {
        $("body").prepend("<div class=\\\"zoomDiv\\\"><img src=\\\"\\\" class=\\\"zoomImg\\\"></div>");
        $("img:not(.zoomImg)").click(function() {
          $(".zoomImg").attr("src", $(this).attr("src"));
          $(".zoomDiv").css({opacity: "1", width: "70%"});
        });
        $("img.zoomImg").click(function() {
          $(".zoomDiv").css({opacity: "0", width: "0%"});
        });
      });
    ')), "\n"
  ))
}
