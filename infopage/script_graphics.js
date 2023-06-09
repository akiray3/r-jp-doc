const librarylist = [
  { id: "abline", href: "/detail/base/abline.html", texttag: "abline" },
  { id: "arrows", href: "/detail/base/arrows.html", texttag: "arrows" },
  {
    id: "assocplot",
    href: "/detail/base/assocplot.html",
    texttag: "assocplot",
  },
  { id: "axis", href: "/detail/base/axis.html", texttag: "axis" },
  { id: "Axis", href: "/detail/base/Axis.html", texttag: "Axis" },
  { id: "axis.Date", href: "/detail/base/axisDate.html", texttag: "axis.Date" },
  {
    id: "axis.POSIXct",
    href: "/detail/base/axisPOSIXct.html",
    texttag: "axis.POSIXct",
  },
  { id: "axTicks", href: "/detail/base/axTicks.html", texttag: "axTicks" },
  { id: "barplot", href: "/detail/base/barplot.html", texttag: "barplot" },
  {
    id: "barplot.default",
    href: "/detail/base/barplotdefault.html",
    texttag: "barplot.default",
  },
  { id: "box", href: "/detail/base/box.html", texttag: "box" },
  { id: "boxplot", href: "/detail/base/boxplot.html", texttag: "boxplot" },
  {
    id: "boxplot.default",
    href: "/detail/base/boxplotdefault.html",
    texttag: "boxplot.default",
  },
  {
    id: "boxplot.matrix",
    href: "/detail/base/boxplotmatrix.html",
    texttag: "boxplot.matrix",
  },
  { id: "bxp", href: "/detail/base/bxp.html", texttag: "bxp" },
  { id: "cdplot", href: "/detail/base/cdplot.html", texttag: "cdplot" },
  { id: "clip", href: "/detail/base/clip.html", texttag: "clip" },
  {
    id: "close.screen",
    href: "/detail/base/closescreen.html",
    texttag: "close.screen",
  },
  {
    id: "co.intervals",
    href: "/detail/base/cointervals.html",
    texttag: "co.intervals",
  },
  { id: "contour", href: "/detail/base/contour.html", texttag: "contour" },
  {
    id: "contour.default",
    href: "/detail/base/contourdefault.html",
    texttag: "contour.default",
  },
  { id: "coplot", href: "/detail/base/coplot.html", texttag: "coplot" },
  { id: "curve", href: "/detail/base/curve.html", texttag: "curve" },
  { id: "dotchart", href: "/detail/base/dotchart.html", texttag: "dotchart" },
  {
    id: "erase.screen",
    href: "/detail/base/erasescreen.html",
    texttag: "erase.screen",
  },
  {
    id: "filled.contour",
    href: "/detail/base/filledcontour.html",
    texttag: "filled.contour",
  },
  {
    id: "fourfoldplot",
    href: "/detail/base/fourfoldplot.html",
    texttag: "fourfoldplot",
  },
  { id: "frame", href: "/detail/base/frame.html", texttag: "frame" },
  {
    id: "grconvertX",
    href: "/detail/base/grconvertX.html",
    texttag: "grconvertX",
  },
  {
    id: "grconvertY",
    href: "/detail/base/grconvertY.html",
    texttag: "grconvertY",
  },
  { id: "grid", href: "/detail/base/grid.html", texttag: "grid" },
  { id: "hist", href: "/detail/base/hist.html", texttag: "hist" },
  {
    id: "hist.default",
    href: "/detail/base/histdefault.html",
    texttag: "hist.default",
  },
  { id: "identify", href: "/detail/base/identify.html", texttag: "identify" },
  { id: "image", href: "/detail/base/image.html", texttag: "image" },
  {
    id: "image.default",
    href: "/detail/base/imagedefault.html",
    texttag: "image.default",
  },
  { id: "layout", href: "/detail/base/layout.html", texttag: "layout" },
  {
    id: "layout.show",
    href: "/detail/base/layoutshow.html",
    texttag: "layout.show",
  },
  { id: "lcm", href: "/detail/base/lcm.html", texttag: "lcm" },
  { id: "legend", href: "/detail/base/legend.html", texttag: "legend" },
  { id: "lines", href: "/detail/base/lines.html", texttag: "lines" },
  {
    id: "lines.default",
    href: "/detail/base/linesdefault.html",
    texttag: "lines.default",
  },
  { id: "locator", href: "/detail/base/locator.html", texttag: "locator" },
  { id: "matlines", href: "/detail/base/matlines.html", texttag: "matlines" },
  { id: "matplot", href: "/detail/base/matplot.html", texttag: "matplot" },
  {
    id: "matpoints",
    href: "/detail/base/matpoints.html",
    texttag: "matpoints",
  },
  {
    id: "mosaicplot",
    href: "/detail/base/mosaicplot.html",
    texttag: "mosaicplot",
  },
  { id: "mtext", href: "/detail/base/mtext.html", texttag: "mtext" },
  { id: "pairs", href: "/detail/base/pairs.html", texttag: "pairs" },
  {
    id: "pairs.default",
    href: "/detail/base/pairsdefault.html",
    texttag: "pairs.default",
  },
  {
    id: "panel.smooth",
    href: "/detail/base/panelsmooth.html",
    texttag: "panel.smooth",
  },
  { id: "par", href: "/detail/base/par.html", texttag: "par" },
  { id: "persp", href: "/detail/base/persp.html", texttag: "persp" },
  { id: "pie", href: "/detail/base/pie.html", texttag: "pie" },
  { id: "plot", href: "/detail/base/plot.html", texttag: "plot" },
  {
    id: "plot.default",
    href: "/detail/base/plotdefault.html",
    texttag: "plot.default",
  },
  {
    id: "plot.design",
    href: "/detail/base/plotdesign.html",
    texttag: "plot.design",
  },
  {
    id: "plot.function",
    href: "/detail/base/plotfunction.html",
    texttag: "plot.function",
  },
  { id: "plot.new", href: "/detail/base/plotnew.html", texttag: "plot.new" },
  {
    id: "plot.window",
    href: "/detail/base/plotwindow.html",
    texttag: "plot.window",
  },
  { id: "plot.xy", href: "/detail/base/plotxy.html", texttag: "plot.xy" },
  { id: "points", href: "/detail/base/points.html", texttag: "points" },
  {
    id: "points.default",
    href: "/detail/base/pointsdefault.html",
    texttag: "points.default",
  },
  { id: "polygon", href: "/detail/base/polygon.html", texttag: "polygon" },
  { id: "polypath", href: "/detail/base/polypath.html", texttag: "polypath" },
  {
    id: "rasterImage",
    href: "/detail/base/rasterImage.html",
    texttag: "rasterImage",
  },
  { id: "rect", href: "/detail/base/rect.html", texttag: "rect" },
  { id: "rug", href: "/detail/base/rug.html", texttag: "rug" },
  { id: "screen", href: "/detail/base/screen.html", texttag: "screen" },
  { id: "segments", href: "/detail/base/segments.html", texttag: "segments" },
  {
    id: "smoothScatter",
    href: "/detail/base/smoothScatter.html",
    texttag: "smoothScatter",
  },
  {
    id: "spineplot",
    href: "/detail/base/spineplot.html",
    texttag: "spineplot",
  },
  {
    id: "split.screen",
    href: "/detail/base/splitscreen.html",
    texttag: "split.screen",
  },
  { id: "stars", href: "/detail/base/stars.html", texttag: "stars" },
  { id: "stem", href: "/detail/base/stem.html", texttag: "stem" },
  {
    id: "strheight",
    href: "/detail/base/strheight.html",
    texttag: "strheight",
  },
  {
    id: "stripchart",
    href: "/detail/base/stripchart.html",
    texttag: "stripchart",
  },
  { id: "strwidth", href: "/detail/base/strwidth.html", texttag: "strwidth" },
  {
    id: "sunflowerplot",
    href: "/detail/base/sunflowerplot.html",
    texttag: "sunflowerplot",
  },
  { id: "symbols", href: "/detail/base/symbols.html", texttag: "symbols" },
  { id: "text", href: "/detail/base/text.html", texttag: "text" },
  {
    id: "text.default",
    href: "/detail/base/textdefault.html",
    texttag: "text.default",
  },
  { id: "title", href: "/detail/base/title.html", texttag: "title" },
  { id: "xinch", href: "/detail/base/xinch.html", texttag: "xinch" },
  { id: "xspline", href: "/detail/base/xspline.html", texttag: "xspline" },
  { id: "xyinch", href: "/detail/base/xyinch.html", texttag: "xyinch" },
  { id: "yinch", href: "/detail/base/yinch.html", texttag: "yinch" },
];

var infocontent = document.getElementById("baseinfo");
for (let i = 0; i < librarylist.length; i++) {
  const basename = librarylist[i];
  const div = document.createElement("div");
  div.className = "lineitem";
  div.id = basename.id;
  div.style.display = "inline-block";
  div.style.width = "289px";
  div.style.height = "125px";
  div.style.fill = "none";

  const div2 = document.createElement("div");
  div2.className = "basenamebox";
  div2.style.width = "274.045px";
  div2.style.height = "100.124px";
  div2.style.borderRadius = "9.194px";
  div2.style.stroke = "#243D25";
  div2.style.strokeMiterlimit = "10";
  div2.style.strokeWidth = "1";
  div2.style.fontSize = "27px";
  div2.style.border = "1px solid #000";
  div2.style.transform = "translate(21.529px ,57.832px)";

  const atag = document.createElement("a");
  atag.href = basename.href;

  const txttag = document.createElement("text");
  const txtnode = document.createTextNode(basename.texttag);
  txttag.style.fontFamily = "ArialRoundedMTBold";

  txttag.appendChild(txtnode);
  atag.appendChild(txttag);
  div2.appendChild(atag);
  div.appendChild(div2);
  infocontent.appendChild(div);
}
