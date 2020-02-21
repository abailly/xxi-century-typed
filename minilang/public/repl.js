var websocket = (function() {
    const host = window.location.host;
    const ws = new WebSocket("ws://" + host + "/repl/1235");
    ws.binaryType = "arraybuffer";
    return ws;
})();

websocket.onopen = function(e) {
    document.getElementById("command").classList.add("connected");
};

function appendMessage(reader) {
    return () => {
        const root = document.getElementById("minilang");
        const sub = document.createElement("div");
        sub.classList.add("output");
        const txt = document.createTextNode(JSON.stringify(reader.result));
        sub.appendChild(txt);
        root.appendChild(sub);
    };
}

websocket.onmessage = function(e) {
    const reader = new FileReader();
    reader.addEventListener('loadend', appendMessage(reader));

    if(e.data instanceof ArrayBuffer) {
        reader.readAsText(new Blob([e.data]));
    } else {
        reader.readAsText(e.data);
    }
};

function makeCommand(val) {
    if(val === ":env") {
        return {"tag":"Com","contents":{"tag":"DumpEnv"}};
    } else {
        return {"tag":"In","contents":val};
    }
};

function sendCommand(e) {
    if (e.keyCode == 13) {
        const command = makeCommand(document.getElementById("command").value);
        websocket.send(JSON.stringify(command));
        return false;
    }
    return true;
};

document.getElementById("command").addEventListener("keyup", sendCommand);
