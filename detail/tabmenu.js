//tabmenu
const tabcontent = document.querySelector(".tabs");

const Descriptionbox = document.createElement("div");
Descriptionbox.classList.add("content");
Descriptionbox.setAttribute("id", "Description");

const Argumentsbox = document.createElement("div");
Argumentsbox.classList.add("content");
Argumentsbox.setAttribute("id", "Arguments");

const Valuebox = document.createElement("div");
Valuebox.classList.add("content");
Valuebox.setAttribute("id", "Value");

const Detailsbox = document.createElement("div");
Detailsbox.classList.add("content");
Detailsbox.setAttribute("id", "Details");

const Examplesbox = document.createElement("div");
Examplesbox.classList.add("content");
Examplesbox.setAttribute("id", "Examples");

const Referencesbox = document.createElement("div");
Referencesbox.classList.add("content");
Referencesbox.setAttribute("id", "References");

const See_Alsobox = document.createElement("div");
See_Alsobox.classList.add("content");
See_Alsobox.setAttribute("id", "See_Also");

const path = "../../help_db_raw.json";
const load = new XMLHttpRequest();
load.open("GET", path, true);
load.send();

function openTab(event, tabId) {
  const tabs = document.querySelectorAll(".tab");
  const contents = document.querySelectorAll(".content");
  tabs.forEach((tab) => tab.classList.remove("active"));
  contents.forEach((cont) => (cont.style.display = "none"));

  const clicktab = event.currentTarget;
  const clickedcontent = document.querySelector("#" + tabId);
  console.log(clickedcontent);
  clicktab.classList.add("active");
  clickedcontent.style.display = "block";
}

load.onload = function () {
  const defaulttab = document.querySelector(".tab");
  const defaulttabid = defaulttab.getAttribute("Description");
  openTab({ currentTarget: defaulttab }, defaulttabid);
};

load.onload = function () {
  const jsondata = JSON.parse(load.responseText);
  const title = document.head.querySelector("title").textContent;
  const findfunc = jsondata.find((obj) => obj.func === title);

  const Descriptiontxt = document.createTextNode(findfunc.Description);
  Descriptionbox.appendChild(Descriptiontxt);
  tabcontent.appendChild(Descriptionbox);

  const Argumentstxt = document.createTextNode(findfunc.Arguments);
  Argumentsbox.appendChild(Argumentstxt);
  tabcontent.appendChild(Argumentsbox);

  const Valuetxt = document.createTextNode("Value内容");
  Valuebox.appendChild(Valuetxt);
  tabcontent.appendChild(Valuebox);

  const Detailstxt = document.createTextNode(findfunc.Details);
  Detailsbox.appendChild(Detailstxt);
  tabcontent.appendChild(Detailsbox);

  const Examplestxt = document.createTextNode(findfunc.Examples);
  Examplesbox.appendChild(Examplestxt);
  tabcontent.appendChild(Examplesbox);

  const Referencestxt = document.createTextNode(findfunc.References);
  Referencesbox.appendChild(Referencestxt);
  tabcontent.appendChild(Referencesbox);

  const See_Alsotxt = document.createTextNode(findfunc.See_Also);
  See_Alsobox.appendChild(See_Alsotxt);
  tabcontent.appendChild(See_Alsobox);
};
