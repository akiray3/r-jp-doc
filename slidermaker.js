const list = ["base", "graphics", "psych", "stats"];

for (let b = 0; b < list.length; b++) {
  const path = "infopage/" + list[b] + ".json";
  const jsonfile = new XMLHttpRequest();
  jsonfile.open("GET", path, true);
  jsonfile.send();

  jsonfile.onload = function loadpage() {
    const jsondata = JSON.parse(jsonfile.responseText);
    const slideblock = document.querySelector("#" + list[b] + "_slider");
    for (let i = 0; i < jsondata.length; i++) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = jsondata[i].funcname;
      const slide_link = document.createElement("a");
      slide_link.href = jsondata[i].href;
      if (jsondata[i].svg) {
        const imgbox = document.createElement("img");
        imgbox.src = "www/" + jsondata[i].svg;
        imgbox.style.height = "200px";
        const funcnametxt = document.createElement("p");
        const fnnametxt = document.createTextNode(jsondata[i].funcname);
        funcnametxt.appendChild(fnnametxt);
        slide_link.appendChild(imgbox);
        slide.appendChild(slide_link);
        slide.appendChild(funcnametxt);
        slideblock.appendChild(slide);
      } else {
        const funcnametxt = document.createElement("p");
        const fnnametxt = document.createTextNode(jsondata[i].funcname);
        slide_link.style.padding = "10px";
        funcnametxt.appendChild(fnnametxt);
        slide.appendChild(slide_link);
        slide.appendChild(funcnametxt);
        slideblock.appendChild(slide);
      }
      if(i<jsondata.length-1){
        loadpage(i+1);
      }
    }
  };

}
