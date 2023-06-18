const path = "baselist.json";
const request = new XMLHttpRequest();
request.open("GET", path, true);
request.send();

request.onload = function () {
  const jsondata = JSON.parse(request.responseText);
  const libraryname = jsondata.map((item) => item.texttag);
  const href = jsondata.map((item) => item.href);

  console.log(libraryname[1]);
  var infocontent = document.getElementById("baseinfo");
  for (let i = 0; i < libraryname.length; i++) {
    const basename = libraryname[i];
    const div = document.createElement("div");
    div.className = "lineitem";
    div.id = basename;
    div.style.display = "inline-block";
    div.style.width = "289px";
    div.style.height = "125px";
    div.style.fill = "none";

    const div2 = document.createElement("div");
    div2.className = "basenamebox";
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

    const txttag = document.createElement("text");
    const txtnode = document.createTextNode(basename);
    txttag.style.fontFamily = "ArialRoundedMTBold";

    txttag.appendChild(txtnode);
    atag.appendChild(txttag);
    div2.appendChild(atag);
    div.appendChild(div2);
    infocontent.appendChild(div);
  }
};
