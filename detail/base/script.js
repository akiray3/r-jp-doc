const content = document.querySelector(".block");
//全体関数ブロック
const div = document.createElement("div");
div.style.whiteSpace = "nowrap";
div.style.transform = "translate(47px ,57px)";

//関数名
const funcframe = document.createElement("div");
funcframe.style.display = "inline-block";
funcframe.style.width = "50%";
div.appendChild(funcframe);

const funcname = document.createElement("div");
funcname.style.display = "inline";
funcname.style.fontSize = "48px";
funcname.style.fontFamily = "ArialRoundedMTBold";
funcname.style.display = "flex: 0 0 auto; margin-right: 10px";
const funcnametxt = document.createTextNode("function");
funcname.appendChild(funcnametxt);
funcframe.appendChild(funcname);

//(
const abc = document.createElement("text");
abc.style.fontSize = "48px";
abc.style.fontFamily = "ArialRoundedMTBold";
abc.style.display = "flex: 0 0 auto; margin-right: 10px";
const abctxt = document.createTextNode("(");
abc.appendChild(abctxt);
funcframe.appendChild(abc);
//引数１
const txt1 = document.createElement("text");
txt1.className = "txt1";
txt1.style.fontSize = "48px";
txt1.style.fontFamily = "ArialRoundedMTBold";
txt1.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt1node = document.createTextNode("x");
txt1.appendChild(txt1node);
funcframe.appendChild(txt1);
//period
const period = document.createElement("text");
period.style.fontSize = "48px";
period.style.fontFamily = "ArialRoundedMTBold";
period.style.display = "flex: 0 0 auto; margin-right: 10px";
const periodtxt = document.createTextNode(",");
period.appendChild(periodtxt);
funcframe.appendChild(period);
//引数２
const txt2 = document.createElement("text");
txt2.className = "txt2";
txt2.style.fontSize = "48px";
txt2.style.fontFamily = "ArialRoundedMTBold";
txt2.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt2node = document.createTextNode("string");
txt2.appendChild(txt2node);
funcframe.appendChild(txt2);

//)
const xyz = document.createElement("text");
xyz.style.fontSize = "48px";
xyz.style.fontFamily = "ArialRoundedMTBold";
xyz.style.display = "flex: 0 0 auto; margin-right: 10px";
const xyztxt = document.createTextNode(")");
xyz.appendChild(xyztxt);
funcframe.appendChild(xyz);

//hoverframe
const argframe = document.createElement("div");
argframe.style.width = "50%";
argframe.style.display = "inline-block";
argframe.style.marginRight = "10px";
div.appendChild(argframe);

///hover
const hovinfo1 = document.createElement("div");
hovinfo1.style.display = "none";
hovinfo1.style.width = "200px";
hovinfo1.style.height = "120px";
hovinfo1.style.backgroundColor = "#BDBDD7";
hovinfo1.style.fontSize = "20px";
const hov1txt = document.createTextNode("引数１");
hovinfo1.appendChild(hov1txt);
argframe.appendChild(hovinfo1);

//hover
const hovinfo2 = document.createElement("div");
hovinfo2.style.display = "none";
hovinfo2.style.width = "200px";
hovinfo2.style.height = "120px";
hovinfo2.style.backgroundColor = "#BDBDD7";
hovinfo2.style.fontSize = "20px";
const hov2txt = document.createTextNode("引数2");
hovinfo2.appendChild(hov2txt);
argframe.appendChild(hovinfo2);

txt1.addEventListener("mouseover", function () {
  hovinfo1.style.display = "block";
});
txt1.addEventListener("mouseout", function () {
  hovinfo1.style.display = "none";
});
txt2.addEventListener("mouseover", function () {
  hovinfo2.style.display = "block";
});
txt2.addEventListener("mouseout", function () {
  hovinfo2.style.display = "none";
});

content.appendChild(div);
