const titletag = document.getElementsByTagName("title")[0];
const titleid = titletag.id;

const WindowSize = window.innerWidth;
console.log(WindowSize);

const path = titleid + ".json";
const request = new XMLHttpRequest();
request.open("GET", path, true);
request.send();

request.onload = function () {
  const jsondata = JSON.parse(request.responseText);
  const funcname = jsondata.map((item) => item.funcname);
  const href = jsondata.map((item) => item.href);

  var infocontent = document.getElementById(titleid + "info");
  for (let i = 0; i < funcname.length; i++) {
    const div = document.createElement("div");
    div.className = "lineitem";
    div.id = funcname[i];
    div.style.display = "inline-block";
    div.style.fill = "none";

    const div2 = document.createElement("div");
    div2.className = "namebox";
    div2.style.borderRadius = "9.194px";
    div2.style.stroke = "#243D25";
    div2.style.strokeMiterlimit = "10";
    div2.style.strokeWidth = "1";
    div2.style.fontSize = "27px";
    div2.style.border = "1px solid #000";
    div2.style.transform = "translate(21.529px ,57.832px)";

    const atag = document.createElement("a");
    atag.href = `../detail/detailpage.html`;
    atag.addEventListener("click", (event) => {
      event.preventDefault();
      window.location.href = ` ../detail/detailpage.html?keyword=${funcname[i]}`;
    });

    const txttag = document.createElement("button");
    txttag.className = "ToDetailButton";

    txttag.style.backgroundColor = "#00000000";
    txttag.style.border = "none";

    if (WindowSize <= 395) {
      if (funcname[i].length < 15) {
        txttag.style.fontSize = "39px";
      } else if (funcname[i].length >= 15 && funcname[i].length < 20) {
        txttag.style.fontSize = "35px";
      } else if (funcname[i].length >= 20 && funcname[i].length < 35) {
        txttag.style.fontSize = "30px";
      } else {
        txttag.style.fontSize = "25px";
      }
    } else {
      if (funcname[i].length < 18) {
        txttag.style.fontSize = "26px";
      } else {
        txttag.style.fontSize = "20px";
      }
    }

    txttag.style.fontFamily = "ArialRoundedMTBold";

    const txtnode = document.createTextNode(funcname[i]);

    txttag.appendChild(txtnode);
    atag.appendChild(txttag);
    div2.appendChild(atag);
    div.appendChild(div2);
    infocontent.appendChild(div);
  }
};
