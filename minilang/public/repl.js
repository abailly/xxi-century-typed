var websocket = (function() {
    const host = window.location.host;
    const ws = new WebSocket("ws://" + host + "/repl/1235");
    ws.binaryType = "arraybuffer";
    return ws;
})();

var commandHistory = [];

websocket.onopen = function(e) {
    document.getElementById("command").classList.add("connected");
};

function appendOutput(text) {
    const root = document.getElementById("minilang");
    const sub = document.createElement("div");
    sub.classList.add("output");
    sub.appendChild(text);
    root.appendChild(sub);
};

function readMessageAndAppend(reader) {
    return () => {
        appendOutput(document.createTextNode(JSON.stringify(reader.result)));
    };
};

websocket.onmessage = function(e) {
    const reader = new FileReader();
    reader.addEventListener('loadend', readMessageAndAppend(reader));

    if(e.data instanceof ArrayBuffer) {
        reader.readAsText(new Blob([e.data]));
    } else {
        reader.readAsText(e.data);
    }
};

function makeCommand(val) {
    if(val === ":env") {
        return {"tag":"Com","contents":{"tag":"DumpEnv"}};
    } else if(val === ":clear") {
        return {"tag":"Com","contents":{"tag":"ClearEnv"}};
    } else {
        return {"tag":"In","contents":val};
    }
};

function sendCommand(e) {
    const input = document.getElementById("command");
    if (e.keyCode == 13) {
        const command = makeCommand(input.value);
        appendOutput(document.createTextNode("> "+ input.value));
        commandHistory.push(input.value);
        websocket.send(JSON.stringify(command));
        input.value = "";
        return false;
    } else if(e.keyCode == 38) {
        input.value = commandHistory.pop() || "";
        return false;
    }

    return true;
};

document.getElementById("command").addEventListener("keyup", sendCommand);
