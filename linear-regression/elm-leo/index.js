const { readFileSync }= require('fs');
const Elm = require('./elm.js');

const data = readFileSync('./data.cvs', 'utf8');
console.log('data -> ', data);
const worker = Elm.Main.worker();

worker.ports.result.subscribe((res) => console.log('result -> ', res));

worker.ports.incomingData.send(data);

