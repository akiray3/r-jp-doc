//関数部分
//jsonファイル定義
const path = "../../help_db_raw.json";
const request = new XMLHttpRequest();
request.open("GET", path, true);
request.send();

request.onload = function () {
  const content = document.querySelector(".block");
  //全体関数ブロック
  const div = document.createElement("div");
  div.style.whiteSpace = "flex";
  div.style.flexWrap = "nowrap";
  div.style.width = "100%";
  div.style.overflow = "auto";
  div.style.transform = "translate(167px ,57px)";
  div.style.position = "relative";
  content.appendChild(div);

  //関数名
  const funcframe = document.createElement("div");
  funcframe.style.display = "inline-block";
  funcframe.style.flexGrow = "1";
  funcframe.style.height = "100%";
  div.appendChild(funcframe);

  const funcname = document.createElement("div");
  funcname.style.fontSize = "48px";
  funcname.style.fontFamily = "Hiragino Maru Gothic ProN,YuGoshic,sans-serif";
  funcname.style.display = "flex: 0 0 auto; margin-right: 10px";

  //引数ブロック
  const argblock = document.createElement("div");
  argblock.style.display = "inline-block";
  div.appendChild(argblock);

  const argcol = document.createElement("div");
  argcol.style.display = "flex";
  argcol.style.flexDirection = "column";
  argblock.appendChild(argcol);

  const jsondata = JSON.parse(request.responseText);
  const title = document.head.querySelector("title").textContent;
  const findfunc = jsondata.find((obj) => obj.func === title);

  //関数名
  const funcnametxt = document.createTextNode(findfunc.func + "(");
  funcname.appendChild(funcnametxt);
  funcframe.appendChild(funcname);

  //引数１
  const txt1 = document.createElement("text");
  txt1.className = "txt";
  txt1.style.flexGrow = "1";
  txt1.style.display = "flex: 0 0 auto; margin-right: 10px";
  const txt1node = document.createTextNode("x" + ",");
  txt1.appendChild(txt1node);
  argcol.appendChild(txt1);

  //引数２
  const txt2 = document.createElement("text");
  txt2.className = "txt";
  txt2.style.flexGrow = "2";
  txt2.style.display = "flex: 0 0 auto; margin-right: 10px";
  const txt2node = document.createTextNode("y" + ",");
  txt2.appendChild(txt2node);
  argcol.appendChild(txt2);

  //引数３
  const txt3 = document.createElement("text");
  txt3.className = "txt";
  txt3.style.flexGrow = "3";
  txt3.style.display = "flex: 0 0 auto; margin-right: 10px";
  const txt3node = document.createTextNode("z");
  txt3.appendChild(txt3node);
  argcol.appendChild(txt3);

  //)
  const xyz = document.createElement("text");
  xyz.style.fontSize = "48px";
  xyz.style.fontFamily = "Hiragino Maru Gothic ProN,YuGoshic,sans-serif";
  xyz.style.display = "flex: 0 0 auto; margin-right: 10px";
  const xyztxt = document.createTextNode(")");
  xyz.appendChild(xyztxt);
  txt3.appendChild(xyz);

  const heightblock = argblock.offsetHeight;
  content.style.height = `${heightblock + 300}px`;

  //hoverframe
  const argframe = document.createElement("div");
  argframe.style.display = "inline-block";
  argframe.height = "100%";
  argframe.style.position = "absolute";
  argframe.style.transform = "translate(120px,10px)";
  div.appendChild(argframe);

  ///hover
  const hovinfo1 = document.createElement("div");
  hovinfo1.classList.add("hovanime");
  hovinfo1.style.display = "none";
  hovinfo1.style.height = "120px";
  hovinfo1.style.backgroundColor = "#DAE1E7";
  hovinfo1.style.fontSize = "20px";
  hovinfo1.style.flexGrow = "1";
  const hov1txt = document.createTextNode("引数１");
  hovinfo1.appendChild(hov1txt);
  argframe.appendChild(hovinfo1);

  //hover
  const hovinfo2 = document.createElement("div");
  hovinfo2.classList.add("hovanime");
  hovinfo2.style.display = "none";
  hovinfo2.style.height = "120px";
  hovinfo2.style.backgroundColor = "#DAE1E7";
  hovinfo2.style.fontSize = "20px";
  const hov2txt = document.createTextNode("引数2");
  hovinfo2.appendChild(hov2txt);
  argframe.appendChild(hovinfo2);

  const hovinfo3 = document.createElement("div");
  hovinfo3.classList.add("hovanime");
  hovinfo3.style.display = "none";
  hovinfo3.style.height = "120px";
  hovinfo3.style.width = "120px";
  hovinfo3.style.backgroundColor = "#DAE1E7";
  hovinfo3.style.fontSize = "20px";
  const hov3txt = document.createTextNode("引数3");
  hovinfo3.appendChild(hov3txt);
  argframe.appendChild(hovinfo3);

  const imgframe = document.createElement("div");
  const image = document.createElement("img");
  image.classList.add("hovanime");
  image.setAttribute("src", "../../www/math-operations.svg");
  image.style.width = "220px";
  imgframe.appendChild(image);
  argframe.appendChild(imgframe);

  txt1.addEventListener("mouseover", function () {
    hovinfo1.style.display = "block";
    image.style.display = "none";
  });
  txt1.addEventListener("mouseout", function () {
    hovinfo1.style.display = "none";
    image.style.display = "block";
  });
  txt2.addEventListener("mouseover", function () {
    hovinfo2.style.display = "block";
    image.style.display = "none";
  });
  txt2.addEventListener("mouseout", function () {
    hovinfo2.style.display = "none";
    image.style.display = "block";
  });
  txt3.addEventListener("mouseover", function () {
    hovinfo3.style.display = "block";
    image.style.display = "none";
  });
  txt3.addEventListener("mouseout", function () {
    hovinfo3.style.display = "none";
    image.style.display = "block";
  });

  content.appendChild(div);
};

///////////////////////////////////////////////////////
//tabmenu
const tabcontent = document.querySelector(".tabs");

const Descriptionbox = document.createElement("div");
Descriptionbox.classList.add("content");
Descriptionbox.setAttribute("id", "Description");

const Argumentsbox = document.createElement("div");
Argumentsbox.classList.add("content");
Argumentsbox.setAttribute("id", "Arguments");

const Valuebox = document.createElement("div");
Valuebox.classList.add("content");
Valuebox.setAttribute("id", "Value");

const Detailsbox = document.createElement("div");
Detailsbox.classList.add("content");
Detailsbox.setAttribute("id", "Details");

const Examplesbox = document.createElement("div");
Examplesbox.classList.add("content");
Examplesbox.setAttribute("id", "Examples");

const Referencesbox = document.createElement("div");
Referencesbox.classList.add("content");
Referencesbox.setAttribute("id", "References");

const See_Alsobox = document.createElement("div");
See_Alsobox.classList.add("content");
See_Alsobox.setAttribute("id", "See_Also");

const load = new XMLHttpRequest();
load.open("GET", path, true);
load.send();

function openTab(event, tabId) {
  const tabs = document.querySelectorAll(".tab");

  const contents = document.querySelectorAll(".content");
  tabs.forEach((tab) => tab.classList.remove("active"));
  contents.forEach((cont) => (cont.style.display = "none"));

  const clicktab = event.currentTarget;
  console.log(clicktab);
  const clickedcontent = document.querySelector("#" + tabId);
  clicktab.classList.add("active");

  clickedcontent.style.display = "block";
}

load.onload = function () {
  const jsondata = JSON.parse(load.responseText);
  const title = document.head.querySelector("title").textContent;
  const findfunc = jsondata.find((obj) => obj.func === title);

  const Descriptiontxt = document.createTextNode(findfunc.Description);
  Descriptionbox.appendChild(Descriptiontxt);
  tabcontent.appendChild(Descriptionbox);

  const Argumentstxt = document.createTextNode(findfunc.Arguments);
  Argumentsbox.appendChild(Argumentstxt);
  tabcontent.appendChild(Argumentsbox);

  const Valuetxt = document.createTextNode(findfunc.Value);
  Valuebox.appendChild(Valuetxt);
  tabcontent.appendChild(Valuebox);

  const Detailstxt = document.createTextNode(findfunc.Details);
  Detailsbox.appendChild(Detailstxt);
  tabcontent.appendChild(Detailsbox);

  const Examplestxt = document.createTextNode(findfunc.Examples);
  Examplesbox.appendChild(Examplestxt);
  tabcontent.appendChild(Examplesbox);

  const Referencestxt = document.createTextNode(findfunc.References);
  Referencesbox.appendChild(Referencestxt);
  tabcontent.appendChild(Referencesbox);

  const See_Alsotxt = document.createTextNode(findfunc.See_Also);
  See_Alsobox.appendChild(See_Alsotxt);
  tabcontent.appendChild(See_Alsobox);

  document.querySelector("#default").click();
};
