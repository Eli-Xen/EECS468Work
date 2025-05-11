// <!--File Server Assignment 6-->
// <!--inputs: GET/DELETE/PUT methods in the form of a request file -->
// <!--outputs: status/error codes, or the fetched file -->
// <!--creation date: 3/29/25-->

const {createServer} = require("http"); //use http module and use createServer from that module 
const methods = Object.create(null); //object called methods is creates with nothing (so far) 

const {createWriteStream} = require("fs"); //gets writestream from fs 

const {createReadStream} = require("fs"); //gets read strem from fs module 
const {stat, readdir} = require("fs").promises; //gets promises from fs so we can use them in functions 
const mime = require("mime"); //get mime to determine content type of a file 

const {parse} = require("url"); //function for url parsing 
const {resolve, sep} = require("path"); //sep is seperator / on paths 
const baseDirectory = process.cwd(); //function for finding the current working directory 

const {rmdir, unlink} = require("fs").promises; //lets us remove/unline using promises 

const {mkdir} = require("fs").promises; //gets make directory from fs module 


createServer((request, response) => //make an HTTP server 
{   response.setHeader('Access-Control-Allow-Origin', '*'); //seurity; ensures valid orgins in request, allows all 
    response.setHeader('Access-Control-Allow-Methods', '*'); //security; ensures valid methods in request, allows all 
    response.setHeader('Access-Control-Allow-Headers', '*'); //security; ensures valid headers in request, allows all 
    if (request.method === 'OPTIONS') { //if the method is OPTIONS (preflight handling)...
        return response.end(); //...close stream 
    } //closing bracket for if statment 
    let handler = methods[request.method] || notAllowed; //methods[] array of methods and function for error responses 
    handler(request) //request handler will reject (catch) or resolve(then) promise to handle request 
        .catch(error => { //request handler is rejected...
            if (error.status != null) return error; //if there is an error without statuc ode 
            return {body: "Internal Server Error", status: 500}; //return the error w status 500 in the response 
        }) //closing brackets 
        .then(({body, status = 200, type = "text/plain"}) => { //if response handler's promise is sucessful, status OK and we have text
            response.writeHead(status, {"Content-Type": type}); //set status and type in header 
            if (body && body.pipe) body.pipe(response); //if the body is readable stream forward it to writable response stream 
            else response.end(body); //if not readable stream, end stream and send  body before ending it 
        }); //closing brackets 
}).listen(8000); //listen for port 8000 e

async function notAllowed(request) { //returns 405 errors when request is recived 
    return { //return bracketed info 
        status: 405, //requested page not found 
        body: `Method ${request.method} not allowed.` //specifies what method caused 405 
    }; //closing bracket for return 
} //closing bracket 

// const {createReadStream} = require("fs"); //gets read strem from fs module 
// const {stat, readdir} = require("fs").promises; //gets promises from fs so we can use them in functions 
// const mime = require("mime"); //get mime to determine content type of a file 

// const {parse} = require("url"); //function for url parsing 
// const {resolve, sep} = require("path"); //sep is seperator / on paths 
// const baseDirectory = process.cwd(); //function for finding the current working directory 
function urlPath(url) //function that parses a given url into a path for a filesystem 
{   let {pathname} = parse(url); //parses the URL 
    let path = resolve(decodeURIComponent(pathname).slice(1)); //uses resolve() from path modeule to resolve relative paths
    if (path != baseDirectory && !path.startsWith(baseDirectory + sep)) //ensures the path we got is inside current directory; sep is seperator / on paths 
    {   throw {status: 403, body: "Forbidden"}; } //if not throw error for accessing forbiden directory 
    return path; //if error not thrown return the path 
} //closing bracket 

methods.GET = async function(request) //GET method to get file/directory contents 
{   let path = urlPath(request.url); //get the path from url using URL path function we made 
    let stats; // invoke stat object called stats
    try {stats = await stat(path);   } //wait for stat to find the file 
    catch (error) //if error happens catch it 
    {   if (error.code != "ENOENT") throw error; //if we cant determine that the error is entity not found then throw error 
        else return {status: 404, body: "File not found"}; //otherwise return error file not found 
    } //closing bracket 
    if (stats.isDirectory()) //if requested path is a directory 
    {    return {body: (await readdir(path)).join("\n")};   } //wait for readdit and join using newlines 
    else //if requested path is a file 
    {    return {body: createReadStream(path), //get the insides of the file using read stream 
        type: mime.getType(path)}; //attach file type using mime 
    } //closing bracket 
}; //closing bracket 

//const {createWriteStream} = require("fs"); //gets writestream from fs 
function pipeStream(from, to) //function that acts as a promise for pipe 
{   return new Promise((resolve, reject) => //make new promise for writing/pipe 
    {   from.on("error", reject); //something goes wrong when opening the file 
        to.on("error", reject); //stream from request fails ie network down 
        to.on("finish", resolve); //pipe is done streaming and resolve promise
        from.pipe(to); //pipes from from to to 
    }); //closing bracket 
} //closing bracket 

methods.PUT = async function(request) //method to write/edit a file 
{   let path = urlPath(request.url); //gets path from url 
    try {    await pipeStream(request, createWriteStream(path));     } //function defined above that creates a promise to succeed/fail at writing to a file 
    catch(error)
    {   return {status: 400, body: "not a directory"};     } //was not able to write file at given path, likley becuase of bad path 
    return {status: 204}; //return nothing (no content) 
};  //closing bracket 

// const {rmdir, unlink} = require("fs").promises; //lets us remove/unline using promises 
methods.DELETE = async function(request) //delete function will delete a file/directory if it exists and return appropriate status/error 
{   let path = urlPath(request.url); // translate the url into a file name
    let stats; // invoke stat object called stats
    try {   stats = await stat(path);   } // wait for stat to find the file
    
    catch (error) // handle a non-existent file name 
    {   if (error.code != "ENOENT") throw error; //if error isnt ENOENT (below, error no entity) then throw error 
        else return {status: 204}; //error for when path doesnt exist in filesystem 
    }  //closing bracket 
    
    if (stats.isDirectory()) await rmdir(path); // if the file name is a directory, remove it
    else await unlink(path); //wait for unlink/file deletion is sucessful 
    return {status: 204}; // report that the file deletion was successful
};  //closing bracket 

// const {mkdir} = require("fs").promises; //gets make directory from fs module 
methods.MKCOL = async function(request) //create a directory by calling mkdir from the fs module 
{   let path = urlPath(request.url); //gets path from url 
    try //try something but expect error 
    {   await mkdir(path); //similar to await rmdir() from DELETE 
        return {status: 201, body: "new directory created"}; //once new directory made return status of new resource completion and body of readable text 
    } //closing bracket 
    catch (error) //if error happens catch it 
    {   if (error.code=="EEXIST") //error in Node.js is a filesystem error when trying to create a file/directory that already exists: https://www.google.com/search?client=firefox-b-1-e&channel=entpr&q=JS+error+code+if+requesting+to+create+directory+that+already+exists 
        {   return {status: 409, body: "directory already exists"};     } //returns status code for trying to create duplicate resource: https://codemia.io/knowledge-hub/path/http_response_code_for_post_when_resource_already_exists
        else throw error; //if we dont know what the error is then throw some error 
    } //closing bracket 
}; //closing bracket 
