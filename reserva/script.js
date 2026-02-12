const buttons = document.querySelectorAll(".nav-btn");
const sections = document.querySelectorAll(".section");

const map = {
  "btn-fichas": "fichas-section",
  "btn-galeria": "galeria-section",
  "btn-info": "info-section",
  "btn-diagram": "diagram-section",
  "btn-send": "send-section",
};

buttons.forEach(btn => {
  btn.addEventListener("click", () => {
    buttons.forEach(b => b.classList.remove("active"));
    btn.classList.add("active");

    sections.forEach(sec => sec.classList.remove("active"));

    const targetId = map[btn.id];
    if (targetId) document.getElementById(targetId).classList.add("active");
  });
});

