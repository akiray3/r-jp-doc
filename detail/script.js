const path = "../../help_db_raw.json";
const request = new XMLHttpRequest();
request.open("GET", path, true);
request.send();

request.onload = function () {
  const jsondata = JSON.parse(request.responseText);
  const title = document.head.querySelector("title").textContent;
  const findfunc = jsondata.find((obj) => obj.func === title);

  const content = document.querySelector(".block");
  //全体関数ブロック
  const div = document.createElement("div");
  div.style.whiteSpace = "flex";
  div.style.flexWrap = "nowrap";
  div.style.width = "100%";
  div.style.overflow = "auto";
  div.style.transform = "translate(167px ,57px)";
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
  const funcnametxt = document.createTextNode(findfunc.func + "(");
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
  content.style.height = `${heightblock + 197}px`;

  //hoverframe
  const argframe = document.createElement("div");
  argframe.style.width = "40%";
  argframe.style.display = "inline-block";
  argframe.style.marginRight = "10px";
  argframe.style.transform = "translate(50px,10px)";
  div.appendChild(argframe);

  ///hover
  const hovinfo1 = document.createElement("div");
  hovinfo1.classList.add("hovanime");
  hovinfo1.style.display = "none";
  hovinfo1.style.height = "120px";
  hovinfo1.style.backgroundColor = "#DAE1E7";
  hovinfo1.style.fontSize = "20px";
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
  hovinfo3.style.backgroundColor = "#DAE1E7";
  hovinfo3.style.fontSize = "20px";
  const hov3txt = document.createTextNode("引数3");
  hovinfo3.appendChild(hov3txt);
  argframe.appendChild(hovinfo3);

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
  txt3.addEventListener("mouseover", function () {
    hovinfo3.style.display = "block";
  });
  txt3.addEventListener("mouseout", function () {
    hovinfo3.style.display = "none";
  });

  content.appendChild(div);
};

//tabmenu
const tabcontent = document.querySelector(".tabcontents");

const Descriptionbox = document.createElement("div");
Descriptionbox.classList.add("content");
Descriptionbox.setAttribute("id", "Description");
const Descriptiontxt = document.createTextNode("description内容");
Descriptionbox.appendChild(Descriptiontxt);
tabcontent.appendChild(Descriptionbox);

const Argumentsbox = document.createElement("div");
Argumentsbox.classList.add("content");
Argumentsbox.setAttribute("id", "Arguments");
const Argumentstxt = document.createTextNode("Arguments内容");
Argumentsbox.appendChild(Argumentstxt);
tabcontent.appendChild(Argumentsbox);

const Valuebox = document.createElement("div");
Valuebox.classList.add("content");
Valuebox.setAttribute("id", "Value");
const Valuetxt = document.createTextNode("Value内容");
Valuebox.appendChild(Valuetxt);
tabcontent.appendChild(Valuebox);

const Detailsbox = document.createElement("div");
Detailsbox.classList.add("content");
Detailsbox.setAttribute("id", "Details");
const Detailstxt = document.createTextNode("Details内容");
Detailsbox.appendChild(Detailstxt);
tabcontent.appendChild(Detailsbox);

const Examplesbox = document.createElement("div");
Examplesbox.classList.add("content");
Examplesbox.setAttribute("id", "Examples");
const Examplestxt = document.createTextNode("Examples内容");
Examplesbox.appendChild(Examplestxt);
tabcontent.appendChild(Examplesbox);

const Referencesbox = document.createElement("div");
Referencesbox.classList.add("content");
Referencesbox.setAttribute("id", "References");
const Referencestxt = document.createTextNode("References内容");
Referencesbox.appendChild(Referencestxt);
tabcontent.appendChild(Referencesbox);

const See_Alsobox = document.createElement("div");
See_Alsobox.classList.add("content");
See_Alsobox.setAttribute("id", "See_Also");
const See_Alsotxt = document.createTextNode("See_Also内容");
See_Alsobox.appendChild(See_Alsotxt);
tabcontent.appendChild(See_Alsobox);

function openTab(event, tabId) {
  const tabs = document.querySelectorAll(".tab");
  const contents = document.querySelectorAll(".content");
  tabs.forEach((tab) => tab.classList.remove("active"));
  contents.forEach((content) => (content.style.display = "none"));

  const clicktab = event.currentTarget;
  const clickedcontent = document.getElementById(tabId);
  clicktab.classList.add("active");
  clickedcontent.style.display = "block";
}

const defaulttab = document.querySelector(".tab");
const defaulttabid = defaulttab.getAttribute("Description");
openTab({ currentTarget: defaulttab }, defaulttabid);
