const allMenu = document.querySelector(".tagmenu");
allMenu.style.display = "none";

$(".openMenu").click(function () {
  $(this).toggleClass("active");
});
function openTagmenu(event, tabId) {
  if (allMenu.style.display === "block") {
    allMenu.style.display = "none";
  } else if ((allMenu.style.display = "none")) {
    allMenu.style.display = "block";
  }
}

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
  const infocontent = document.getElementById(titleid + "info");
  for (let i = 0; i < funcname.length; i++) {
    const div = document.createElement("div");
    div.className = "lineitem";
    div.id = funcname[i];
    div.style.display = "inline-block";
    div.style.fill = "none";

    const div2 = document.createElement("div");
    div2.className = "namebox";

    const atag = document.createElement("a");
    atag.href = `../detail/detailpage.html`;
    atag.addEventListener("click", (event) => {
      event.preventDefault();
      window.location.href = ` ../detail/detailpage.html?keyword=${funcname[i]}`;
    });

    const txttag = document.createElement("button");
    txttag.className = "ToDetailButton";

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
    const txtnode = document.createTextNode(funcname[i]);

    txttag.appendChild(txtnode);
    atag.appendChild(txttag);
    div2.appendChild(atag);
    div.appendChild(div2);
    infocontent.appendChild(div);
  }
};
function selectFunction(event, tabId) {
  const allFunc = document.querySelector(".slidermenu_base");
  allFunc.classList.add("none");
  request.open("GET", path, true);
  request.send();
  request.onload = function () {
    const jsondata = JSON.parse(request.responseText);
    const tagname = jsondata.filter((item) => item.tag === tabId);
    const filtertag = tagname.map((item) => item.funcname);

    const allMenu = document.querySelectorAll(".TagMenu");
    allMenu.forEach((item) => (item.style.display = "none"));
    const tagmenu = document.getElementById(`${tabId}Menu`);
    tagmenu.style.display = "block";

    filtertag.forEach((element) => {
      const div = document.createElement("div");
      div.className = "lineitem";
      div.id = element;
      div.style.display = "inline-block";
      div.style.fill = "none";

      const div2 = document.createElement("div");
      div2.className = "namebox";

      const atag = document.createElement("a");
      atag.href = `../detail/detailpage.html`;
      atag.addEventListener("click", (event) => {
        event.preventDefault();
        window.location.href = ` ../detail/detailpage.html?keyword=${element}`;
      });

      const txttag = document.createElement("button");
      txttag.className = "ToDetailButton";

      txttag.style.border = "none";

      if (WindowSize <= 395) {
        if (element.length < 15) {
          txttag.style.fontSize = "39px";
        } else if (element.length >= 15 && element.length < 20) {
          txttag.style.fontSize = "35px";
        } else if (element.length >= 20 && element.length < 35) {
          txttag.style.fontSize = "30px";
        } else {
          txttag.style.fontSize = "25px";
        }
      } else {
        if (element.length < 18) {
          txttag.style.fontSize = "26px";
        } else {
          txttag.style.fontSize = "20px";
        }
      }

      txttag.style.fontFamily = "ArialRoundedMTBold";
      const txtnode = document.createTextNode(element);
      txttag.appendChild(txtnode);
      atag.appendChild(txttag);
      div2.appendChild(atag);
      div.appendChild(div2);
      tagmenu.appendChild(div);
    });
  };
}


function pageTopAnime(){
  let scroll = $(window).scrollTop();
  if(scroll >= 300){
    $('#pageTop').removeClass('DownMove');
    $('#pageTop').addClass('UpMove');
  }else{
    if($('#pageTop').hasClass('UpMove')){
      $('#pageTop').removeClass('UpMove');
      $('#pageTop').addClass('DownMove');
    }
  }
}
$(window).scroll(function () {
pageTopAnime();
});

$(window).on('load',function(){
  pageTopAnime();
})
$('#pageTop a').click(function(){
  $('body,html').animate({
    scrollTop:0
  },500);
  return false;
  }
);