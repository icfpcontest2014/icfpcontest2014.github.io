
// Grid dimensions in pixels
var width = 20
var height = 20

function prettymaps () {
  $( ".gamemap" ).each(prettymap)
}

function prettymap (index, div) {
  var content = div.textContent
  var dimX = width * mapsizeX(content)
  var dimY = height * mapsizeY(content)

  var canvas = document.createElement("canvas")
  canvas.width = dimX
  canvas.height = dimY
  div.innerHTML = ""
  div.appendChild(canvas)

  var ctx = canvas.getContext("2d")
  var img = document.getElementById("alltiles")

  var i = 0
  var x = 0
  var y = 0
  var tileno = null
  var ghostno = 0
  for (i = 0; i < content.length; i++) {
    switch (content[i]) {
      case '\n':
        tileno = null
        x = 0
        y++
        break

      case '#':
        tileno = 0
        x++
        break

      case ' ':
        tileno = 1
        x++
        break

      case '.':
        tileno = 2
        x++
        break

      case 'o':
        tileno = 3
        x++
        break

      case '%':
        tileno = 4
        x++
        break

      case '\\':
        tileno = 5
        x++
        break

      case '=':
        tileno = 7 + ghostno
        x++
        ghostno++
        if (ghostno == 4) { ghostno = 0 }
        break

      default:
        tileno = null
        x++
        break
    }
    if (tileno != null) {
      ctx.drawImage(img, tileno * width, 0, width, height, (x-1) * width, y * height, width, height)
    }
  }
}

function mapsizeX (content) {
  var i;
  for (i = 0; i < content.length; i++) {
    if (content[i] == '\n') { break }
  }
  return i
}

function mapsizeY (content) {
  var i;
  var n = 0;
  for (i = 0; i < content.length; i++) {
    if (content[i] == '\n') { n++ }
  }
  return n
}

$(document).ready(function(){
  prettymaps ()
})

