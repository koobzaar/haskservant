const API_BASE = "http://localhost:8080";

async function fetchJson(url, options = {}) {
  try {
    const response = await fetch(url, {
      headers: { "content-type": "application/json" },
      ...options,
    });
    const text = await response.text();
    if (!response.ok) {
      throw new Error(text || `Erro HTTP ${response.status}`);
    }
    if (!text.trim()) return null;
    return JSON.parse(text);
  } catch (err) {
    mostrarMensagem(err.message, true);
    throw err;
  }
}

async function carregarHerois() {
  try {
    const data = await fetchJson(`${API_BASE}/herois`);
    renderHerois(data.herois || []);
  } catch (error) {
    // A mensagem de erro já é mostrada dentro do fetchJson
  }
}

function renderHerois(herois) {
  const tbody = document.querySelector("#tabela-herois tbody");
  tbody.innerHTML = "";

  if (herois.length === 0) {
    const row = document.createElement("tr");
    row.classList.add("empty-row");
    const cell = document.createElement("td");
    cell.colSpan = 5;
    cell.textContent = "Nenhum herói registrado no esquadrão.";
    row.appendChild(cell);
    tbody.appendChild(row);
    return;
  }

  herois.forEach((heroi) => {
    const row = document.createElement("tr");
    row.innerHTML = `
      <td>${heroi.idHeroi}</td>
      <td>${heroi.nome}</td>
      <td>${heroi.classe}</td>
      <td>${heroi.nivel}</td>
      <td class="actions"></td>
    `;

    const actionsCell = row.querySelector(".actions");

    const editButton = document.createElement("button");
    editButton.textContent = "Editar";
    editButton.className = "btn btn-edit";
    editButton.onclick = () => atualizarHeroi(heroi.idHeroi, heroi);

    const deleteButton = document.createElement("button");
    deleteButton.textContent = "Expulsar";
    deleteButton.className = "btn btn-delete";
    deleteButton.onclick = () => removerHeroi(heroi.idHeroi);

    actionsCell.appendChild(editButton);
    actionsCell.appendChild(deleteButton);

    tbody.appendChild(row);
  });
}

async function criarHeroi(event) {
  event.preventDefault();
  const nome = document.querySelector("#nome").value.trim();
  const classe = document.querySelector("#classe").value.trim();
  const nivel = parseInt(document.querySelector("#nivel").value, 10);

  if (!nome || !classe || Number.isNaN(nivel)) {
    mostrarMensagem("Preencha todos os campos do herói.", true);
    return;
  }

  try {
    const data = await fetchJson(`${API_BASE}/heroi`, {
      method: "POST",
      body: JSON.stringify({ nome, classe, nivel }),
    });
    mostrarMensagem(`Herói ${nome} alistado! (ID: ${data.resultado}).`);
    document.querySelector("#heroi-form").reset();
    carregarHerois();
  } catch (error) {
    // A mensagem de erro já é mostrada dentro do fetchJson
  }
}

async function removerHeroi(id) {
  if (!confirm(`Expulsar o herói de ID ${id} do esquadrão?`)) return;
  try {
    await fetchJson(`${API_BASE}/heroi/${id}`, { method: "DELETE" });
    mostrarMensagem(`Herói ${id} foi removido do esquadrão.`);
    carregarHerois();
  } catch (error) {
    // A mensagem de erro já é mostrada dentro do fetchJson
  }
}

async function atualizarHeroi(id, heroiAtual) {
  const nome = prompt("Novo nome para o Herói:", heroiAtual.nome);
  if (nome === null) return;

  const classe = prompt("Nova classe para o Herói:", heroiAtual.classe);
  if (classe === null) return;

  const nivelStr = prompt("Novo nível para o Herói:", heroiAtual.nivel);
  if (nivelStr === null) return;
  
  const nivel = parseInt(nivelStr, 10);

  if (!nome.trim() || !classe.trim() || Number.isNaN(nivel)) {
    mostrarMensagem("Dados inválidos para a atualização.", true);
    return;
  }

  try {
    await fetchJson(`${API_BASE}/heroi/${id}`, {
      method: "PUT",
      body: JSON.stringify({
        idHeroi: id,
        nome,
        classe,
        nivel,
      }),
    });
    mostrarMensagem(`Herói ${id} atualizado com sucesso.`);
    carregarHerois();
  } catch (error) {
    // A mensagem de erro já é mostrada dentro do fetchJson
  }
}

function mostrarMensagem(msg, isError = false) {
  const div = document.querySelector("#mensagem");
  div.textContent = msg;
  div.style.color = isError ? "var(--cor-erro)" : "var(--cor-primaria-acao)";
}

function iniciar() {
  document
    .querySelector("#heroi-form")
    .addEventListener("submit", criarHeroi);
  carregarHerois();
}

window.onload = iniciar;
