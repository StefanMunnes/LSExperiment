const deckSelect = document.getElementById("deck-select");
const langSelect = document.getElementById("lang-select");
const container = document.getElementById("vignettes-container");
const highlightButton = document.getElementById("toggle-highlights");
const labelsButton = document.getElementById("toggle-labels");
const printButton = document.getElementById("print-preview");
const widthRange = document.getElementById("width-range");
const widthModeLabel = document.getElementById("width-mode-label");

const widthModes = {
  1: { label: "Overview", width: "260px" },
  2: { label: "Balanced", width: "360px" },
  3: { label: "Reading", width: "520px" },
};

function escapeHtml(value) {
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function currentLanguage() {
  if (hasMultipleLanguages && langSelect) {
    return langSelect.value;
  }

  return defaultLanguage || "default";
}

function updateButtonLabels() {
  highlightButton.textContent = document.body.classList.contains("no-variable-highlights")
    ? "Show highlights"
    : "Hide highlights";

  labelsButton.textContent = document.body.classList.contains("no-variable-labels")
    ? "Show factor labels"
    : "Hide factor labels";
}

function applyWidthMode() {
  const mode = widthModes[widthRange.value] || widthModes[2];
  document.documentElement.style.setProperty("--vignette-min-width", mode.width);
  widthModeLabel.textContent = mode.label;
}

function renderDeck() {
  const deck = deckSelect.value;
  const lang = currentLanguage();
  const deckData = vignetteData[deck];

  if (!deckData) {
    container.innerHTML = "<p>No data available for the selected deck.</p>";
    return;
  }

  const html = Object.keys(deckData)
    .sort((left, right) => Number(left) - Number(right))
    .map((vigNumber) => {
      const variant = deckData[vigNumber];
      const entry = variant[lang] || variant.default || variant[Object.keys(variant)[0]];

      if (!entry) {
        return "";
      }

      const variableChips = Object.entries(entry.variables || {})
        .map(([key, value]) =>
          `<span class="variable-chip"><strong>${escapeHtml(key)}:</strong> ${escapeHtml(value)}</span>`
        )
        .join("");

      return [
        '<article class="vignette-card">',
        '  <div class="vignette-header">',
        `    <span class="vignette-number">Vignette ${escapeHtml(vigNumber)}</span>`,
        `    <span class="vignette-id">${escapeHtml(entry.id || "")}</span>`,
        "  </div>",
        `  <div class="vignette-content">${entry.rendered}</div>`,
        '  <div class="variables-section">',
        '    <p class="variables-title">Factor level labels</p>',
        `    <div class="variable-chips">${variableChips}</div>`,
        "  </div>",
        "</article>",
      ].join("\n");
    })
    .join("\n");

  container.innerHTML = html;
}

deckSelect.addEventListener("change", renderDeck);

if (langSelect) {
  langSelect.addEventListener("change", renderDeck);
}

widthRange.addEventListener("input", applyWidthMode);

highlightButton.addEventListener("click", () => {
  document.body.classList.toggle("no-variable-highlights");
  updateButtonLabels();
});

labelsButton.addEventListener("click", () => {
  document.body.classList.toggle("no-variable-labels");
  updateButtonLabels();
});

printButton.addEventListener("click", () => {
  window.print();
});

applyWidthMode();
updateButtonLabels();
renderDeck();
