/*__ githubサーバー上と、ローカル上で必要なパスが異なる。githubサーバー上でのみ必要になるパスをここに定義。ローカルでテストするときには空にしておくこととする _______________________*/

/*"../r-jp-doc"*/
const path_for_github = "../r-jp-doc";

const WindowSize = window.innerWidth;
console.log(WindowSize);

document.addEventListener("DOMContentLoaded", function () {
  const currentURL = window.location.search;
  const urlParams = new URLSearchParams(currentURL);
  const keyword = urlParams.get("keyword");

  const path = "../help_db_jpn_main.json";
  const jsonfile = new XMLHttpRequest();
  jsonfile.open("GET", path, true);
  jsonfile.send();
  jsonfile.onload = function () {
    const jsondata = JSON.parse(jsonfile.responseText);
    const returnfuncpage = document.querySelector(".rturnfunc");
    const returnhref = document.getElementById("funcListBack");
    const packname = jsondata.filter((item) => item.func === keyword);
    returnhref.href = `/r-jp-doc/infopage/${packname.pack}.html`;
    returnhref.textContent = `${packname[0].pack}一覧へ`;
    const content = document.querySelector(".block");
    //全体関数ブロック
    const div = document.createElement("div");
    div.className = "funcblockall";
    content.appendChild(div);

    //関数名
    const funcframe = document.createElement("div");
    funcframe.className = "funcFrame";
    div.appendChild(funcframe);

    const funcname = document.createElement("div");
    funcname.className = "funcName";

    //引数ブロック
    const argblock = document.createElement("div");
    argblock.style.display = "inline-block";
    argblock.className = "argblock";
    div.appendChild(argblock);

    const argcol = document.createElement("div");
    argcol.style.display = "flex";
    argcol.style.flexDirection = "column";
    argblock.appendChild(argcol);

    //引数/////この辺に入れる
    const path_arg = "../help_db_jpn_arguments.json";
    const req = new XMLHttpRequest();
    req.open("GET", path_arg, true);
    req.send();

    //hoverframe
    const argframe = document.createElement("div");
    argframe.style.display = "inline-block";
    //svg
    const svgpath =
      `../${path_for_github}infopage/` + packname[0].pack + ".json";
    const svgjson = new XMLHttpRequest();
    svgjson.open("GET", svgpath, true);
    svgjson.send();
    svgjson.onload = function () {
      const loaddata = JSON.parse(svgjson.responseText);
      const svgdata = loaddata.find((obj) => obj.funcname === keyword);
      if (svgdata.svg) {
        const imgframe = document.createElement("div");
        const image = document.createElement("img");
        image.classList.add("image");
        image.classList.add("funcimage");
        image.setAttribute("src", "../www/" + svgdata.svg);
        imgframe.appendChild(image);
        argframe.appendChild(imgframe);
      } else {
      }
    };

    argframe.style.position = "absolute";
    argframe.style.transform = "translate(90px,10px)";
    div.appendChild(argframe);

    req.onload = function () {
      const json = JSON.parse(req.responseText);
      console.log(keyword);
      const arg = json.filter((item) => item.func === keyword);

      //関数名
      if (WindowSize <= 395) {
        const funcnametxt = document.createTextNode(keyword);
        const funcstart = document.createElement("text");
        funcstart.className = "txt";
        const funcstarttxt = document.createTextNode("(");
        funcstart.style.fontSize = "40px";
        funcstart.style.fontFamily =
          "Hiragino Maru Gothic ProN, YuGoshic,sans-serif";
        funcstart.style.display = "flex";
        funcstart.style.marginRight = "10px";
        funcname.appendChild(funcnametxt);
        funcstart.appendChild(funcstarttxt);
        argcol.appendChild(funcstart);
      } else if (WindowSize > 395) {
        const funcnametxt = document.createTextNode(`${keyword}(`);
        funcname.appendChild(funcnametxt);
      }
      arg.forEach((element, index) => {
        const txt = document.createElement("text");
        txt.className = "txt";
        txt.style.flexGrow = "1";
        txt.style.flex = "0 0 auto";
        txt.style.marginRight = "3px";
        const LastArg = index === arg.length - 1;
        const argLast = document.createElement("text");
        const LastArgText = document.createTextNode(")");
        argLast.appendChild(LastArgText);
        argLast.classList.add("funclast");
        const elementArg = LastArg ? `${element.code}` : `${element.code},`;
        const txtnode = document.createTextNode(elementArg);
        txt.appendChild(txtnode);
        argcol.appendChild(txt);
        if (LastArg) {
          txt.appendChild(argLast);
        } else {
        }

        ///hover
        const hovinfo = document.createElement("div");
        hovinfo.classList.add("hover");
        hovinfo.style.display = "none";
        hovinfo.style.backgroundColor = "#00000000";
        hovinfo.style.fontSize = "20px";
        hovinfo.style.flexGrow = "1";

        const hovtxt = document.createTextNode(element.desc);
        hovinfo.appendChild(hovtxt);
        argframe.appendChild(hovinfo);

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
      });

      funcframe.appendChild(funcname);

      for (let b = 0; b <= 50; b++) {
        if (arg["arguments" + b]) {
        }
      }
      //hover size
      const hovinfo = document.querySelectorAll(".hover");
      for (let i = 0; i < hovinfo.length; i++) {
        hovinfo[i].style.height = `${heightblock + 20}px`;
      }

      if (arg["arguments1"]) {
        const alltxt = document.querySelectorAll(".txt");

        if (WindowSize <= 395) {
          const firstarg = alltxt[1];
          const abc = alltxt[0];
          abc.appendChild(firstarg);
          alltxt.forEach((element) => {
            if (element !== alltxt[0] && element !== alltxt[1]) {
              element.style.transform = "translate(40px,0px)";
            }
          });
        }

        const lasttxt = alltxt[alltxt.length - 1];
        for (let i = 0; i < alltxt.length; i++) {
          if (alltxt[i] == lasttxt) {
            //)
            const xyz = document.createElement("text");
            xyz.classList = "funclast";
            const xyztxt = document.createTextNode(")");
            xyz.appendChild(xyztxt);
            alltxt[i].appendChild(xyz);
          } else if (alltxt[i] !== lasttxt) {
          }
        }
      } else {
      }
    };
    content.appendChild(div);
  };
  //関数名ボックスのサイズ
  const argblock = document.querySelector(".argblock");
  const heightblock = argblock.offsetHeight;
  console.log(heightblock);
  const FuncBoxAll = document.querySelector(".block");
  FuncBoxAll.style.height = `${heightblock + 19}px`;
});

///////////////////////////////////////////////////////
//tabmenu
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

document.addEventListener("DOMContentLoaded", function () {
  if (window.innerWidth > 420) {
    const currentURL = window.location.search;
    const urlParams = new URLSearchParams(currentURL);
    const keyword = urlParams.get("keyword");
    const tabcontent = document.querySelector("#forPcTabContents");

    const Descriptionbox = document.createElement("div");
    Descriptionbox.classList.add("content");
    Descriptionbox.setAttribute("id", "Description_forpc_cont");

    const Valuebox = document.createElement("div");
    Valuebox.classList.add("content");
    Valuebox.setAttribute("id", "Value_forpc_cont");

    const Detailsbox = document.createElement("div");
    Detailsbox.classList.add("content");
    Detailsbox.setAttribute("id", "Details_forpc_cont");

    const Examplesbox = document.createElement("div");
    Examplesbox.classList.add("content");
    Examplesbox.setAttribute("id", "Examples_forpc_cont");

    const Referencesbox = document.createElement("div");
    Referencesbox.classList.add("content");
    Referencesbox.setAttribute("id", "Referrences_forpc_cont");

    const See_Alsobox = document.createElement("div");
    See_Alsobox.classList.add("content");
    See_Alsobox.setAttribute("id", "See_Also_forpc_cont");

    const path = "../help_db_jpn_main.json";
    const load = new XMLHttpRequest();
    load.open("GET", path, true);
    load.send();
    load.onload = function () {
      const jsondata = JSON.parse(load.responseText);
      const findfunc = jsondata.find((obj) => obj.func === keyword);
      const Descriptiontxt = document.createTextNode(
        findfunc.Description.value
      );
      Descriptionbox.appendChild(Descriptiontxt);
      tabcontent.appendChild(Descriptionbox);

      Valuebox.innerHTML = findfunc.Value;
      tabcontent.appendChild(Valuebox);

      const Detailstxt = document.createTextNode(findfunc.Details);
      Detailsbox.appendChild(Detailstxt);
      tabcontent.appendChild(Detailsbox);

      Examplesbox.innerHTML = findfunc.Examples;
      tabcontent.appendChild(Examplesbox);

      Referencesbox.innerHTML = findfunc.References;
      tabcontent.appendChild(Referencesbox);

      See_Alsobox.innerHTML = findfunc.See_Also;
      tabcontent.appendChild(See_Alsobox);

      //デフォルトでDescriptionを開く
      document.querySelector("#Description_forpc").click();
    };
  } else if (window.innerWidth <= 420) {
    const currentURL = window.location.search;
    const urlParams = new URLSearchParams(currentURL);
    const keyword = urlParams.get("keyword");
    console.log(keyword);

    //Mobile
    const DesContent = document.querySelector("#Description_contents");
    const ValContent = document.querySelector("#Value_contents");
    const DetContent = document.querySelector("#Details_contents");
    const ExaContent = document.querySelector("#Examples_contents");
    const RefContent = document.querySelector("#Referrences_contents");
    const SeeContent = document.querySelector("#See_Also_contents");

    const path = "../help_db_jpn_main.json";
    const load = new XMLHttpRequest();
    load.open("GET", path, true);
    load.send();

    load.onload = function () {
      const jsondata = JSON.parse(load.responseText);
      console.log(keyword);
      const findfunc = jsondata.find((obj) => obj.func === keyword);
      if (findfunc.Description.value) {
        const Descriptiontxt = document.createTextNode(
          findfunc.Description.value
        );
        DesContent.appendChild(Descriptiontxt);
      } else {
      }

      if (findfunc.Value) {
        ValContent.innerHTML = findfunc.Value;
      } else {
      }
      if (findfunc.Details.value) {
        const Detailstxt = document.createTextNode(findfunc.Details.value);
        DetContent.appendChild(Detailstxt);
      } else {
      }
      if (findfunc.Examples) {
        const Examplestxt = document.createTextNode(findfunc.Examples);
        ExaContent.appendChild(Examplestxt);
      } else {
      }
      if (findfunc.References) {
        RefContent.innerHTML = findfunc.References;
      } else {
      }
      if (findfunc.See_Also) {
        SeeContent.innerHTML = findfunc.See_Also;
      } else {
      }
    };
  }
  $(document).ready(function () {
    $(".slick-slider").slick({
      dots: true,
      infinite: true,
      speed: 300,
      slidesToShow: 1,
    });
  });
});
