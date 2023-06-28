const titletag = document.getElementsByTagName("title")[0];
const titleid = titletag.id;
console.log(titleid);

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
    div.style.width = "289px";
    div.style.height = "125px";
    div.style.fill = "none";

    const div2 = document.createElement("div");
    div2.className = titleid + "namebox";
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
    atag.href = href[i];

    const txttag = document.createElement("button");
    txttag.style.width = "274.045px";
    txttag.style.height = "100.124px";
    txttag.style.backgroundColor = "#00000000";
    txttag.style.border = "none";

    if (funcname[i].length < 18) {
      txttag.style.fontSize = "26px";
    } else {
      txttag.style.fontSize = "20px";
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
