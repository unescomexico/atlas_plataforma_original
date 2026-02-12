const buttons = document.querySelectorAll(".nav-btn");
const sections = document.querySelectorAll(".section");

const map = {
  "btn-home": "home-section",
  "btn-fichas": "fichas-section",
  "btn-galeria": "galeria-section",
  "btn-info": "info-section",
  "btn-diagram": "diagram-section",
  "btn-send": "send-section",
};

function activate(buttonId) {
  const targetId = map[buttonId];
  if (!targetId) return;

  buttons.forEach(b => b.classList.remove("active"));
  const btn = document.getElementById(buttonId);
  if (btn) btn.classList.add("active");

  sections.forEach(sec => sec.classList.remove("active"));
  const target = document.getElementById(targetId);
  if (target) target.classList.add("active");
}

// Sidebar buttons
buttons.forEach(btn => {
  btn.addEventListener("click", () => activate(btn.id));
});

// Buttons/links inside content (e.g., welcome CTAs)
document.addEventListener("click", (e) => {
  const el = e.target.closest("[data-nav]");
  if (!el) return;
  const buttonId = el.getAttribute("data-nav");
  activate(buttonId);
});

// Default view
activate("btn-home");
