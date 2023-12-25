const loading = document.querySelector("#loading");
window.addEventListener("load", () => {
  loading.classList.add("loaded");
});

/*__ githubサーバー上と、ローカル上で必要なパスが異なる。githubサーバー上でのみ必要になるパスをここに定義。ローカルでテストするときには空にしておくこととする _______________________*/

/*"../r-jp-doc"*/
const path_for_github = "../r-jp-doc";

const WindowSize = window.innerWidth;
const path = "../help_db_jpn_main.json";

console.log(WindowSize);

document.addEventListener("DOMContentLoaded", function () {
  const currentURL = window.location.search;
  const urlParams = new URLSearchParams(currentURL);
  const keyword = urlParams.get("keyword");

  const jsonfile = new XMLHttpRequest();
  jsonfile.open("GET", path, true);
  jsonfile.send();
  jsonfile.onload = function () {
    const jsondata = JSON.parse(jsonfile.responseText);
    const returnfuncpage = document.querySelector(".rturnfunc");
    const returnhref = document.getElementById("funcListBack");
    const packname = jsondata.filter((item) => item.func === keyword);
    console.log(keyword);
    returnhref.href = `/r-jp-doc/infopage/${packname[0].pack}.html`;
    returnhref.textContent = `${packname[0].pack}一覧へ`;
    const content = document.querySelector(".block");
    //全体関数ブロック
    const div = document.querySelector(".funcblockall");

    //関数名
    const funcframe = document.querySelector(".funcFrame");
    const funcname = document.querySelector(".funcName");
    const funcnameStart = document.querySelector(".funcstart");
    funcnameStart.classList.add(keyword);
    if (keyword === "-" || "-.Date") {
      funcnameStart.classList.add("hidden");
    }

    //引数ブロック
    const argblock = document.querySelector(".argblock");

    const argcol = document.querySelector(".argcol");

    //引数/////この辺に入れる
    const path_arg = "../help_db_jpn_arguments.json";
    const req = new XMLHttpRequest();
    req.open("GET", path_arg, true);
    req.send();

    //hoverframe
    const hoverFrame = document.querySelector(".hoverFrame");
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
        hoverFrame.appendChild(imgframe);
      } else {
      }
    };
    req.onload = function () {
      const json = JSON.parse(req.responseText);
      const arg = json.filter((item) => item.func === keyword);
      //関数名
      if (WindowSize <= 395) {
        const funcnametxt = document.createTextNode(keyword);
        const funcstart = document.createElement("text");
        funcstart.className = "FuncstartTxt";
        funcstart.classList.add(keyword);
        const funcstarttxt = document.createTextNode("(");
        funcstart.style.display = "flex";
        funcstart.style.marginRight = "10px";
        funcname.appendChild(funcnametxt);
        funcstart.appendChild(funcstarttxt);
        argcol.appendChild(funcstart);
      } else if (WindowSize > 395) {
        const funcnametxt = document.createTextNode(`${keyword}`);
        const funcstart = document.createTextNode("(");
        funcname.appendChild(funcnametxt);
        funcnameStart.appendChild(funcstart);
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
        if (keyword === "-" || "-.Date") {
          argLast.classList.add("hidden");
        }
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
        hovinfo.classList.add("hidden");
        hovinfo.style.backgroundColor = "#00000000";
        hovinfo.style.flexGrow = "1";

        const hovtxt = document.createTextNode(element.desc);
        hovinfo.appendChild(hovtxt);
        hoverFrame.appendChild(hovinfo);

        //ホバー有効無効の判定
        //ホバー有効
        txt.addEventListener("mouseover", function () {
          hovinfo.classList.remove("hidden");
          hovinfo.classList.add("visible");
          if (document.querySelector(".funcimage")) {
            const image = document.querySelector(".funcimage");
            image.style.display = "none";
          } else {
          }
        });
        txt.addEventListener("mouseout", function () {
          hovinfo.classList.remove("visible");
          hovinfo.classList.add("hidden");
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
            hovinfo.classList.add("visible");
            if (document.querySelector(".funcimage")) {
              const image = document.querySelector(".funcimage");
              image.style.display = "none";
            } else {
            }
          }
        });
      });
      funcframe.appendChild(funcname);
      funcframe.appendChild(funcnameStart);

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
  };
});

///////////////////////////////////////////////////////
//各データがないときにはタブ自体を非表示
document.addEventListener("DOMContentLoaded", async function () {
  const currentURL = window.location.search;
  const urlParams = new URLSearchParams(currentURL);
  const keyword = urlParams.get("keyword");
  fetch("../help_db_jpn_main.json")
    .then((response) => response.json())
    .then((jsondata) => {
      const findfunc = jsondata.find((obj) => obj.func === keyword);
      const des = document.querySelector("#Description_forpc");
      const use = document.querySelector("#Usage_forpc");
      const val = document.querySelector("#Value_forpc");
      const det = document.querySelector("#Details_forpc");
      const exa = document.querySelector("#Examples_forpc");
      const ref = document.querySelector("#Referrences_forpc");
      const see = document.querySelector("#See_Also_forpc");
      if (findfunc.Description) {
      } else {
        des.classList.add("Hide");
      }
      if (findfunc.Usage) {
      } else {
        use.classList.add("Hide");
      }
      if (findfunc.Value) {
      } else {
        val.classList.add("Hide");
      }
      if (findfunc.Details) {
      } else {
        det.classList.add("Hide");
      }
      if (findfunc.Examples) {
      } else {
        exa.classList.add("Hide");
      }
      if (findfunc.References) {
      } else {
        ref.classList.add("Hide");
      }
      if (findfunc.See_Also) {
      } else {
        see.classList.add("Hide");
      }
    });
});

//tabmenu
function openTab(event, tabId) {
  const tabs = document.querySelectorAll(".tab");

  const contents = document.querySelectorAll(".content");
  tabs.forEach((tab) => tab.classList.remove("active"));
  contents.forEach((cont) => (cont.style.display = "none"));

  const clicktab = event.currentTarget;
  const clickedcontent = document.querySelector("#" + tabId);
  clicktab.classList.toggle("active");
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

    const Usagebox = document.createElement("div");
    Usagebox.classList.add("content");
    Usagebox.setAttribute("id", "Usage_forpc_cont");

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
      const Descriptiontxt = document.createTextNode(findfunc.Description);
      Descriptionbox.appendChild(Descriptiontxt);
      tabcontent.appendChild(Descriptionbox);

      const Usagetxt = document.createTextNode(findfunc.Usage);
      Usagebox.appendChild(Usagetxt);
      tabcontent.appendChild(Usagebox);

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

      //残ったタブボタンのうち、最初のものを表示する
      const tabs = document.querySelectorAll(".tab");
      const visibleTab = Array.from(tabs).filter(
        (tab) => !tab.classList.contains("Hide")
      );
      visibleTab[0].click();
    };
  } else if (window.innerWidth <= 420) {
    const currentURL = window.location.search;
    const urlParams = new URLSearchParams(currentURL);
    const keyword = urlParams.get("keyword");
    console.log(keyword);

    //Mobile
    const DesContent = document.querySelector("#Description_contents");
    const UsageContent = document.querySelector("#Usage_contents");
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
      if (findfunc.Description) {
        const Descriptiontxt = document.createTextNode(findfunc.Description);
        DesContent.appendChild(Descriptiontxt);
      } else {
      }
      if (findfunc.Usage) {
        const Usagetxt = document.createTextNode(findfunc.Usage);
        UsageContent.appendChild(Usagetxt);
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

//error検索
const errorFilePath = "../errorlist.json";
fetch(errorFilePath)
  .then((response) => response.json())
  .then((jsondata) => {
    document
      .getElementById("errorCopy")
      .addEventListener("input", function (event) {
        const completeErrorList = document.getElementById("autoCompleteError");
        completeErrorList.innerHTML = "";
        const text = event.target.value;
        if (text === "") {
          if (document.querySelector(".option")) {
            const completeErrorList = document.querySelectorAll(".option");
            completeErrorList.forEach(function (item) {
              const completeList = document.getElementById("autoCompleteError");
              completeList.removeChild(item);
            });
          }
        } else {
          const resultError = getCompleteError(text);
          resultError.forEach(function (item) {
            displayMatchError(item.errorEN, item.errorJP);
          });
        }
      });
    console.log(jsondata);
    function getCompleteError(input) {
      const ErrorList = jsondata.map((item) => item.errorEN);
      const filterErrorList = ErrorList.filter(function (option) {
        return option.toLowerCase().includes(input.toLowerCase());
      });

      const ErrorlistIncludeJP = jsondata.filter(function (item) {
        return item.errorEN.toLowerCase().includes(input.toLowerCase());
      });
      return ErrorlistIncludeJP;
    }

    function displayMatchError(result, resultJP) {
      const completeErrorList = document.getElementById("autoCompleteError");
      const option = document.createElement("li");
      option.className = "option";
      option.textContent = result;
      completeErrorList.appendChild(option);

      const optionJp = document.createElement("li");
      optionJp.className = "optionJP";
      optionJp.textContent = resultJP;
      option.appendChild(optionJp);
    }
  });

const btn = document.getElementById("button");

document.getElementById("form").addEventListener("submit", function (event) {
  event.preventDefault();

  btn.value = "Sending...";

  const serviceID = "default_service";
  const templateID = "template_njv2rvq";

  emailjs.sendForm(serviceID, templateID, this).then(
    () => {
      btn.value = "Send Email";
      alert("Sent!");
    },
    (err) => {
      btn.value = "Send Email";
      alert(JSON.stringify(err));
    }
  );
});
