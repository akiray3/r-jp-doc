const content = document.querySelector(".block");
//全体関数ブロック
const div = document.createElement("div");
div.style.whiteSpace = "flex";
div.style.width = "100%";
div.style.transform = "translate(47px ,57px)";

//関数名
const funcframe = document.createElement("div");
funcframe.style.display = "inline-block";
funcframe.style.flexGrow = "1";
funcframe.style.height = "100%";
div.appendChild(funcframe);

const funcname = document.createElement("div");
funcname.style.fontSize = "48px";
funcname.style.fontFamily = "ArialRoundedMTBold";
funcname.style.display = "flex: 0 0 auto; margin-right: 10px";
const funcnametxt = document.createTextNode("function(");
funcname.appendChild(funcnametxt);
funcframe.appendChild(funcname);

//引数ブロック
const argblock = document.createElement("div");
argblock.style.display = "inline-block";
div.appendChild(argblock);

const argcol = document.createElement("div");
argcol.style.display = "flex";
argcol.style.flexDirection = "column";
argblock.appendChild(argcol);

//引数１
const txt1 = document.createElement("text");
txt1.className = "txt1";
txt1.style.flexGrow = "1";
txt1.style.fontSize = "48px";
txt1.style.fontFamily = "ArialRoundedMTBold";
txt1.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt1node = document.createTextNode("x" + ",");
txt1.appendChild(txt1node);
argcol.appendChild(txt1);

//引数２
const txt2 = document.createElement("text");
txt2.className = "txt2";
txt2.style.flexGrow = "2";
txt2.style.fontSize = "48px";
txt2.style.fontFamily = "ArialRoundedMTBold";
txt2.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt2node = document.createTextNode("y" + ",");
txt2.appendChild(txt2node);
argcol.appendChild(txt2);

//引数３
const txt3 = document.createElement("text");
txt3.className = "txt3";
txt3.style.flexGrow = "3";
txt3.style.fontSize = "48px";
txt3.style.fontFamily = "ArialRoundedMTBold";
txt3.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt3node = document.createTextNode("z");
txt3.appendChild(txt3node);
argcol.appendChild(txt3);

//)
const xyz = document.createElement("text");
xyz.style.fontSize = "48px";
xyz.style.fontFamily = "ArialRoundedMTBold";
xyz.style.display = "flex: 0 0 auto; margin-right: 10px";
const xyztxt = document.createTextNode(")");
xyz.appendChild(xyztxt);
argcol.appendChild(xyz);

//hoverframe
const argframe = document.createElement("div");
argframe.style.width = "50%";
argframe.style.display = "inline-block";
argframe.style.marginRight = "10px";
div.appendChild(argframe);

///hover
const hovinfo1 = document.createElement("div");
hovinfo1.classList.add("hovanime");
hovinfo1.style.display = "none";
hovinfo1.style.width = "50%";
hovinfo1.style.height = "120px";
hovinfo1.style.backgroundColor = "#BDBDD7";
hovinfo1.style.fontSize = "20px";
const hov1txt = document.createTextNode("引数１");
hovinfo1.appendChild(hov1txt);
argframe.appendChild(hovinfo1);

//hover
const hovinfo2 = document.createElement("div");
hovinfo2.classList.add("hovanime");
hovinfo2.style.display = "none";
hovinfo2.style.width = "50%";
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
