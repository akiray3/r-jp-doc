const content = document.querySelector(".block");
//全体関数ブロック
const div = document.createElement("div");
div.style.display = "flex";
div.style.transform = "translate(47px ,57px)";

//関数名
const funcname = document.createElement("div");
funcname.style.display = "flex";
funcname.style.fontSize = "27px";
funcname.style.fontFamily = "ArialRoundedMTBold";
funcname.style.display = "flex: 0 0 auto; margin-right: 10px";
const funcnametxt = document.createTextNode("function");
funcname.appendChild(funcnametxt);
div.appendChild(funcname);

//(
const abc = document.createElement("text");
abc.style.fontSize = "27px";
abc.style.fontFamily = "ArialRoundedMTBold";
abc.style.display = "flex: 0 0 auto; margin-right: 10px";
const abctxt = document.createTextNode("(");
abc.appendChild(abctxt);
div.appendChild(abc);

//引数１
const txt1 = document.createElement("text");
txt1.className = "txt1";
txt1.style.fontSize = "27px";
txt1.style.fontFamily = "ArialRoundedMTBold";
txt1.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt1node = document.createTextNode("x");
txt1.appendChild(txt1node);
div.appendChild(txt1);
///hover
const hovinfo1 = document.createElement("div");
hovinfo1.style.display = "none";
hovinfo1.style.width = "200px";
hovinfo1.style.height = "120px";
hovinfo1.style.backgroundColor = "#BDBDD7";
hovinfo1.style.fontSize = "20px";
const hov1txt = document.createTextNode("引数１");
hovinfo1.appendChild(hov1txt);
txt1.appendChild(hovinfo1);

//period
const period = document.createElement("text");
period.style.fontSize = "27px";
period.style.fontFamily = "ArialRoundedMTBold";
period.style.display = "flex: 0 0 auto; margin-right: 10px";
const periodtxt = document.createTextNode(",");
period.appendChild(periodtxt);
div.appendChild(period);

//引数２
const txt2 = document.createElement("text");
txt2.className = "txt2";
txt2.style.fontSize = "27px";
txt2.style.fontFamily = "ArialRoundedMTBold";
txt2.style.display = "flex: 0 0 auto; margin-right: 10px";
const txt2node = document.createTextNode("string");
txt2.appendChild(txt2node);
div.appendChild(txt2);
//hover
const hovinfo2 = document.createElement("div");
hovinfo2.style.display = "none";
hovinfo2.style.width = "200px";
hovinfo2.style.height = "120px";
hovinfo2.style.backgroundColor = "#BDBDD7";
hovinfo2.style.fontSize = "20px";
const hov2txt = document.createTextNode("引数2");
hovinfo2.appendChild(hov2txt);
txt2.appendChild(hovinfo2);

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

//)
const xyz = document.createElement("text");
xyz.style.fontSize = "27px";
xyz.style.fontFamily = "ArialRoundedMTBold";
xyz.style.display = "flex: 0 0 auto; margin-right: 10px";
const xyztxt = document.createTextNode(")");
xyz.appendChild(xyztxt);
div.appendChild(xyz);

content.appendChild(div);