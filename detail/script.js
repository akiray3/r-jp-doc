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
  div.style.overflow = "no";
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
  argblock.className = "argblock";
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

  //引数
  const path_arg = "../argument.json";
  const req = new XMLHttpRequest();
  req.open("GET", path_arg, true);
  req.send();

  //hoverframe
  const argframe = document.createElement("div");
  argframe.style.display = "inline-block";

  argframe.style.position = "absolute";
  argframe.style.transform = "translate(90px,10px)";
  div.appendChild(argframe);

  req.onload = function () {
    const json = JSON.parse(req.responseText);
    const arg = json.find((obj) => obj.func === title);
    for (let b = 0; b <= 50; b++) {
      if (arg["arguments" + b]) {
        const txt = document.createElement("text");
        txt.className = "txt";
        txt.style.flexGrow = "1";
        txt.style.display = "flex: 0 0 auto; margin-right: 10px";
        const txtnode = document.createTextNode(arg["arguments" + b] + ",");
        txt.appendChild(txtnode);
        argcol.appendChild(txt);

        ///hover
        const hovinfo = document.createElement("div");
        hovinfo.classList.add("hover");
        hovinfo.style.display = "none";
        hovinfo.style.backgroundColor = "#00000000";
        hovinfo.style.fontSize = "20px";
        hovinfo.style.flexGrow = "1";

        const hovtxt = document.createTextNode("引数１");
        hovinfo.appendChild(hovtxt);
        argframe.appendChild(hovinfo);

        const pack = findfunc.pack;
        const svgpath = "../../infopage/" + pack + ".json";
        const svgjson = new XMLHttpRequest();
        svgjson.open("GET", svgpath, true);
        svgjson.send();
        svgjson.onload = function () {
          const loaddata = JSON.parse(svgjson.responseText);
          const svgdata = loaddata.find((obj) => obj.funcname === title);
          if (svgdata.svg) {
            const imgframe = document.createElement("div");
            const image = document.createElement("img");
            image.classList.add("image");
            image.classList.add("funcimage");
            image.setAttribute("src", "../../www/" + svgdata.svg);
            imgframe.appendChild(image);
            argframe.appendChild(imgframe);
          } else {
          }
        };

        //ホバー有効無効の判定
        //ホバー有効
        txt.addEventListener("mouseover", function () {
          hovinfo.style.display = "block";
          if (document.querySelector(".funcimage")) {
            const image = document.querySelector(".funcimage");
            image.style.display = "none";
          } else {
          }
        });
        txt.addEventListener("mouseout", function () {
          hovinfo.style.display = "none";
          if (document.querySelector(".funcimage")) {
            const image = document.querySelector(".funcimage");
            image.style.display = "block";
          } else {
          }
        });
        //ホバー無効
        function hoverjudge() {
          return window.matchMedia("(hover:none)").matches;
        }
        txt.addEventListener("touchstart", function () {
          if (hoverjudge()) {
            hovinfo.style.display = "block";
            if (document.querySelector(".funcimage")) {
              const image = document.querySelector(".funcimage");
              image.style.display = "none";
            } else {
            }
          }
        });
      }
    }
    //hover size
    heightblock = argblock.offsetHeight;
    const hovinfo = document.querySelectorAll(".hover");
    for (let i = 0; i < hovinfo.length; i++) {
      hovinfo[i].style.height = `${heightblock + 20}px`;
    }

    if (arg) {
      const alltxt = document.querySelectorAll(".txt");
      const lasttxt = alltxt[alltxt.length - 1];
      for (let i = 0; i < alltxt.length; i++) {
        if (alltxt[i] == lasttxt) {
          //)
          const xyz = document.createElement("text");
          xyz.style.fontSize = "48px";
          xyz.style.fontFamily =
            "Hiragino Maru Gothic ProN,YuGoshic,sans-serif";
          xyz.style.display = "flex: 0 0 auto; margin-right: 10px";
          const xyztxt = document.createTextNode(")");
          xyz.appendChild(xyztxt);
          alltxt[i].appendChild(xyz);
        } else if (alltxt[1] !== lasttxt) {
        }
      }
    } else {
    }
    //関数名ボックスのサイズ
    heightblock = argblock.offsetHeight;
    content.style.height = `${heightblock + 19}px`;
  };

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
