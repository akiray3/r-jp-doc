const list = ["base", "graphics", "psych", "stats"];

for (let b = 0; b < list.length; b++) {
  const path = "infopage/" + list[b] + ".json";
  const jsonfile = new XMLHttpRequest();
  jsonfile.open("GET", path, true);
  jsonfile.send();
  
  jsonfile.onload = function () {
    const jsondata = JSON.parse(jsonfile.responseText);
    const func_basename = jsondata.map((item) => item.funcname);
    const func_link = jsondata.map((item) => item.href);
    const slideblock = document.querySelector("#" + list[b] + "_slider");

    for (let i = 0; i < func_basename.length; i++) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = func_basename[i];
      const slide_link = document.createElement("a");
      slide_link.href = func_link[i];
      const imgbox = document.createElement("img");
      imgbox.src = "www/square.svg";
      imgbox.style.height = "200px";
      const funcnametxt = document.createElement("p");
      const fnnametxt = document.createTextNode(func_basename[i]);
      funcnametxt.appendChild(fnnametxt);
      slide_link.appendChild(imgbox);
      slide.appendChild(slide_link);
      slide.appendChild(funcnametxt);
      slideblock.appendChild(slide);
    }
  };
}
