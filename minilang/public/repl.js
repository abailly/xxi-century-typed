var websocket = undefined;

function connect() {
    websocket = (function() {
        const host = window.location.host;
        const ws = new WebSocket("ws://" + host + "/repl/1235");
        ws.binaryType = "arraybuffer";
        return ws;
    })();

    websocket.onopen = function(e) {
        document.getElementById("command").classList.remove("disconnected");
        document.getElementById("command").classList.add("connected");
        document.getElementById("connect").src = "images/connected.png";
        document.getElementById("connect").removeEventListener("click", connect);
    };

    websocket.onclose = function(e) {
        document.getElementById("command").classList.remove("connected");
        document.getElementById("command").classList.add("disconnected");
        document.getElementById("connect").src = "images/connect.png";
        document.getElementById("connect").addEventListener("click", connect);
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
}

var commandHistory = [];

function appendOutput(text) {
    const root = document.getElementById("minilang");
    const sub = document.createElement("div");
    sub.classList.add("output");
    sub.appendChild(text);
    if(root.firstChild) {
        root.insertBefore(sub, root.firstChild);
    } else {
        root.appendChild(sub);
    }
};

function formatResult(text) {
    const result = JSON.parse(text);
    if (result.tag === "Msg") {
        return result.contents;
    }
    return text;
}

function readMessageAndAppend(reader) {
    return () => {
        appendOutput(document.createTextNode(formatResult(reader.result)));
    };
};

function makeCommand(val) {
    if(val === ":env") {
        return {tag:"Com",contents:{tag:"DumpEnv"}};
    } else if(val === ":clear") {
        return {tag:"Com",contents:{tag:"ClearEnv"}};
    } else if(val === ":help") {
        return {tag:"Com",contents:{tag:"Help"}};
    } else {
        return {tag:"In",contents:val};
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

function loadText(e) {
    const text = document.getElementById("program").value;
    const command = {tag:"Com",
                     contents:{ tag:"Load",
                                contents:{ tag:"Data",
                                           contents:text}}};
    websocket.send(JSON.stringify(command));
    return false;
};

document.getElementById("command").addEventListener("keyup", sendCommand);
document.getElementById("connect").addEventListener("click", connect);
document.getElementById("send").addEventListener("click", loadText);

connect();
