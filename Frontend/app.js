const ws = new WebSocket("ws://localhost:8080/ws");

const joinBtn = document.getElementById("joinBtn");
const handDiv = document.getElementById("hand");
const pileDiv = document.getElementById("pile");
const statusDiv = document.getElementById("status");
const infoDiv = document.getElementById("info");

let joined = false;

joinBtn.onclick = () => {
  if (!joined && ws.readyState === WebSocket.OPEN) {
    ws.send("join");
    joinBtn.disabled = true;
    infoDiv.textContent = "Esperando a que otros jugadores se unan...";
    joined = true;
  }
};

ws.onopen = () => {
  console.log("Conectado al servidor WebSocket");
  joinBtn.disabled = false;
  infoDiv.textContent = "Conectado. Haz clic en 'Unirse al Juego'";
};

ws.onmessage = (event) => {
  console.log("Mensaje recibido:", event.data);
  try {
    const data = JSON.parse(event.data);
    if (data.type === "state") {
      renderState(data);
    } else if (data.type === "joined") {
      console.log("Te has unido al juego exitosamente");
    } else if (data.type === "error") {
      console.error("Error del juego:", data.message);
      statusDiv.textContent = "Error: " + data.message;
    }
  } catch (e) {
    console.error("Error al parsear mensaje:", e);
    console.error("Mensaje original:", event.data);
  }
};

ws.onclose = () => {
  console.log("Conexión WebSocket cerrada");
  infoDiv.textContent = "Conexión perdida. Recarga la página.";
  joinBtn.disabled = true;
};

ws.onerror = (error) => {
  console.error("Error WebSocket:", error);
  infoDiv.textContent = "Error de conexión. Verifica que el servidor esté funcionando.";
};

function renderState(data) {
  console.log("Renderizando estado:", data);
  
  // Verificar que data.hand existe y es un array
  if (!data.hand || !Array.isArray(data.hand)) {
    console.error("data.hand no es válido:", data.hand);
    handDiv.innerHTML = "<h3>Tu mano:</h3><p>Error: No se pudo cargar la mano</p>";
    return;
  }

  // Mostrar mano del jugador
  handDiv.innerHTML = "<h3>Tu mano:</h3>";
  
  if (data.hand.length === 0) {
    handDiv.innerHTML += "<p>No tienes cartas</p>";
  } else {
    data.hand.forEach((card) => {
      // Verificar que card es un array con 2 elementos
      if (!Array.isArray(card) || card.length !== 2) {
        console.error("Carta inválida:", card);
        return;
      }
      
      const [color, num] = card;
      const btn = document.createElement("button");
      btn.textContent = `${capitalize(color)} ${num}`;
      btn.disabled = !data.your_turn;
      btn.onclick = () => playCard(color, num);
      btn.style.backgroundColor = getCardColor(color);
      btn.style.margin = "5px";
      btn.style.padding = "10px";
      btn.style.fontSize = "16px";
      btn.style.border = "2px solid #333";
      btn.style.borderRadius = "5px";
      btn.style.cursor = data.your_turn ? "pointer" : "not-allowed";
      handDiv.appendChild(btn);
    });
  }

  // Mostrar carta superior de la pila
  let topCardText = "Ninguna carta";
  if (data.top_card && data.top_card !== null && data.top_card !== "none") {
    if (Array.isArray(data.top_card) && data.top_card.length === 2) {
      const [topColor, topNum] = data.top_card;
      topCardText = `<span style="color:${topColor}; font-weight: bold;">${capitalize(topColor)} ${topNum}</span>`;
    } else {
      topCardText = "Carta inválida";
    }
  }
  pileDiv.innerHTML = `<h3>Carta en la pila: ${topCardText}</h3>`;

  // Mostrar estado del turno y ganador
  if (data.winner !== null && data.winner !== undefined) {
    // Nota: En el servidor actual, winner es un PID, no un boolean
    // Necesitarías modificar el servidor para enviar información más útil sobre el ganador
    statusDiv.textContent = "¡Juego terminado!";
    disableAllButtons();
  } else {
    statusDiv.textContent = data.your_turn ? "Es tu turno." : "Esperando turno del otro jugador...";
  }

  // Actualizar info
  if (data.hand.length === 1) {
    infoDiv.textContent = "¡UNO! Te queda una carta.";
  } else if (data.hand.length === 0) {
    infoDiv.textContent = "¡Has ganado! 🎉";
  } else {
    infoDiv.textContent = `Tienes ${data.hand.length} cartas.`;
  }
}

function playCard(color, num) {
  if (ws.readyState === WebSocket.OPEN) {
    const message = JSON.stringify({ 
      type: "play", 
      card: [color.toString(), num.toString()] 
    });
    console.log("Enviando jugada:", message);
    ws.send(message);
  } else {
    console.error("WebSocket no está conectado");
    statusDiv.textContent = "Error: No hay conexión con el servidor";
  }
}

function disableAllButtons() {
  document.querySelectorAll("#hand button").forEach(btn => btn.disabled = true);
}

function capitalize(str) {
  if (!str) return "";
  return str.toString().charAt(0).toUpperCase() + str.toString().slice(1);
}

function getCardColor(color) {
  const colors = {
    'red': '#ffcccc',
    'blue': '#ccccff', 
    'green': '#ccffcc',
    'yellow': '#ffffcc'
  };
  return colors[color] || '#f0f0f0';
}