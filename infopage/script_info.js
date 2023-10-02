const loaddingGrey = document.querySelector("#loading");
window.addEventListener("load", () => {
  loaddingGrey.classList.add("loaded");
});
const allMenu = document.querySelector(".tagmenu");
if (allMenu) {
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
} else {
}

//メニューをページ上部に固定
function FixedMenu() {
  const menu = $("#TagmenuButton").outerHeight(true);
  const scroll = $(window).scrollTop();
  if (scroll >= menu) {
    $("#TagmenuButton").addClass("fixed");
  } else {
    $("#TagmenuButton").removeClass("fixed");
  }
}
$(window).scroll(function () {
  FixedMenu();
});

function fixedMenuButton() {
  const button = $(".tagmenu").outerHeight(true);
  const scroll = $(window).scrollTop();
  if (scroll >= button) {
    $(".tagmenu").addClass("fixed");
  } else {
    $(".tagmenu").removeClass("fixed");
  }
}
$(window).scroll(function () {
  fixedMenuButton();
});
const titletag = document.getElementsByTagName("title")[0];
const titleid = titletag.id;
const WindowSize = window.innerWidth;
console.log(WindowSize);

function groupBy(obj, key) {
  return obj.reduce((abc, xyz) => {
    const match = xyz[key];
    const group = abc[match] ?? [];

    return { ...abc, [match]: [...group, xyz] };
  }, {});
}

fetch("db_main_add.json")
  .then((response) => response.json())
  .then((jsondata) => {
    const packFilter = jsondata.filter((item) => item.pack === titleid);
    const groupbyDes = groupBy(packFilter, "Description");
    const keys = Object.keys(groupbyDes);

    keys.forEach((element) => {
      const keyframe = document.createElement("div");
      keyframe.className = "keyframe";

      const filterDes = packFilter.filter(
        (item) => item.Description === element
      );

      const FilterDesFunclist = filterDes.map((item) => item.func);
      FilterDesFunclist.forEach((element) => {
        const div = document.createElement("div");
        div.className = "lineitem";
        if (filterDes[0].tag) {
          div.classList.add(filterDes[0].tag);
        }

        div.style.display = "inline-block";
        div.style.fill = "none";

        const div2 = document.createElement("div");
        div2.className = "namebox";

        const atag = document.createElement("a");
        atag.href = `../detail/detailpage.html`;
        atag.addEventListener("click", (event) => {
          event.preventDefault();
          const EncodeElement =  encodeURIComponent(element);
          window.location.href = ` ../detail/detailpage.html?keyword=${EncodeElement}`;
        });

        const txttag = document.createElement("button");
        txttag.className = "ToDetailButton";
        txttag.id = filterDes[0].tag;

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
        const txtnode = document.createTextNode(element);
        fetch(`${titleid}.json`)
          .then((response) => response.json())
          .then((svgjsondata) => {
            const svgdata = svgjsondata.find(
              (item) => item.funcname === element
            );

            if (svgdata.svg) {
              const image = document.createElement("img");
              image.className = "svg";
              image.src = `../www/${svgdata.svg}`;
              txttag.appendChild(image);
            } else {
            }
          });
        txttag.appendChild(txtnode);
        atag.appendChild(txttag);
        div2.appendChild(atag);
        div.appendChild(div2);
        keyframe.appendChild(div);
        const sec_slide = document.querySelector(".sec_slide");
        sec_slide.appendChild(keyframe);
      });
      const descriptionBox = document.createElement("div");
      descriptionBox.className = "descriptionBox";

      descriptionBox.textContent = element;
      descriptionBox.classList.add(filterDes[0].tag);
      keyframe.appendChild(descriptionBox);
    });
  });

function pageTopAnime() {
  let scroll = $(window).scrollTop();
  if (scroll >= 300) {
    $("#pageTop").removeClass("DownMove");
    $("#pageTop").addClass("UpMove");
  } else {
    if ($("#pageTop").hasClass("UpMove")) {
      $("#pageTop").removeClass("UpMove");
      $("#pageTop").addClass("DownMove");
    }
  }
}
$(window).scroll(function () {
  pageTopAnime();
});

$(window).on("load", function () {
  pageTopAnime();
});
$("#pageTop a").click(function () {
  $("body,html").animate(
    {
      scrollTop: 0,
    },
    500
  );
  return false;
});

//絞り込み機能
function selectFunction(event, tagid) {
  const notTag = document.querySelectorAll(`.lineitem:not(.${tagid})`);
  notTag.forEach((element) => {
    element.style.display = "none";
  });
  const Tag = document.querySelectorAll(`.lineitem.${tagid}`);
  Tag.forEach((element) => {
    element.style.display = "inline-block";
  });
  const notDescription = document.querySelectorAll(
    `.descriptionBox:not(.${tagid})`
  );
  notDescription.forEach((element) => {
    element.style.display = "none";
  });
  const Description = document.querySelectorAll(`.descriptionBox.${tagid}`);
  Description.forEach((element) => {
    element.style.display = "block";
  });
}
