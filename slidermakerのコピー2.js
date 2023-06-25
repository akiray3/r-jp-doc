const path = "infopage/base.json";
const jsonfile = new XMLHttpRequest();
jsonfile.open("GET", path, true);
jsonfile.send();

jsonfile.onload = function () {
  const jsondata = JSON.parse(jsonfile.responseText);
  const slideblock = document.querySelector("#base_slider");
  for (let i = 0; i < jsondata.length; i++) {
    if (jsondata[i].svg) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = jsondata[i].funcname;
      const slide_link = document.createElement("a");
      slide_link.href = jsondata[i].href;
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
    }
  }
  $(".slick-slider").slick({
    slidesToShow: 3,
    adaptiveHeight: true,
  });
};

const path_gr = "infopage/graphics.json";
const jsonfile_gr = new XMLHttpRequest();
jsonfile_gr.open("GET", path_gr, true);
jsonfile_gr.send();

jsonfile_gr.onload = function () {
  const jsondata = JSON.parse(jsonfile_gr.responseText);
  const slideblock = document.querySelector("#graphics_slider");
  for (let i = 0; i < jsondata.length; i++) {
    if (jsondata[i].svg) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = jsondata[i].funcname;
      const slide_link = document.createElement("a");
      slide_link.href = jsondata[i].href;
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
    }
  }
  $(".slick-slider").slick({
    slidesToShow: 3,
    adaptiveHeight: true,
  });
};

const path_py = "infopage/psych.json";
const jsonfile_py = new XMLHttpRequest();
jsonfile_py.open("GET", path_py, true);
jsonfile_py.send();

jsonfile_py.onload = function () {
  const jsondata = JSON.parse(jsonfile_py.responseText);
  const slideblock = document.querySelector("#psych_slider");
  for (let i = 0; i < jsondata.length; i++) {
    if (jsondata[i].svg) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = jsondata[i].funcname;
      const slide_link = document.createElement("a");
      slide_link.href = jsondata[i].href;
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
    }
  }
  $(".slick-slider").slick({
    slidesToShow: 3,
    adaptiveHeight: true,
  });
};

const path_st = "infopage/stats.json";
const jsonfile_st = new XMLHttpRequest();
jsonfile_st.open("GET", path_st, true);
jsonfile_st.send();

jsonfile_st.onload = function () {
  const jsondata = JSON.parse(jsonfile_st.responseText);
  const slideblock = document.querySelector("#stats_slider");
  for (let i = 0; i < jsondata.length; i++) {
    if (jsondata[i].svg) {
      //base
      const slide = document.createElement("div");
      slide.className = "slick-list";
      slide.id = jsondata[i].funcname;
      const slide_link = document.createElement("a");
      slide_link.href = jsondata[i].href;
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
    }
  }
  
};
