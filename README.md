# cmnode

**cmnode** is a small, Erlang/OTP based, full stack application framework that helps you design, express and implement both **server side** APIs and **client side** frontend UIs using a **declarative**, **functional** and **portable** YAML based programming language.



## Docker quickstart

The easiest and quickest way to get started is by running a node locally from Docker. 

The following command runs the hello world app from the ``examples`` folder. 

```
$ docker run -it \
       	   -v examples/hello:/opt/cmnode/etc
       	   -p 8080:8080
       	   pedrogutierrez/cmnode:latest
```

Then check everything is working fine:

```
$ curl http://localhost:8080/ -i
HTTP/1.1 200 OK
content-length: 22
content-type: application/json
date: Sat, 12 Jan 2019 00:44:42 GMT
elapsed: 88
server: Cowboy

{"msg":"Hello World"}
```



## Hello World explained

The hello world app you'll find in the examples folders is probably the simplest, smallest and less useful web service you can built with cmnode. Still, it shares the same foundation and structure of far more complex apps.

#### Ports

Ports are a simple way of opening tcp ports and defining routes. The following snippet tells Cowboy to open a listener at port `8080` and configure a HTTP handler in order to serve an app with name ``hello`` at the route  `/`.

```
type: port
name: hello
spec:
  port: 8080
  apps:
    hello:
	  http: /
```

#### Apps

Apps are a composition of modules. This provides with a great way of quickly composing apps and reusing logic between them. 

The following snippet simply indicates the app ```hello``` is composed of one module with name ```hello```. 

```
type: app
name: hello
spec:
  modules:
    - hello
```

An app can be made of 1, or many modules. During startup, all apps are compiled and the logic contained in their modules gets merged in a smart way.

#### Modules

A module defines a discrete piece of application logic. This application logic is modeled after a state machine made of:

- **Decoders** expressions: we use them to decode, validate and bind input data.
- **Update** expressions: we use them to express both state transitions of our model and interactions with the outside world as **commands** sent to **effect** managers.
- **Encoder** expressions: we use them to generate data that gets fed both into our model and effect managers.

This is a simple way of organizing application logic, heavily inspired from Elm.

The following snippet implements the core of the hello world application logic:

```
type: module
name: hello
spec:
  decoders:
  	hello:
      any: object
  update:
    hello:
      model:
        message: "Hello World"
      cmds:
        - effect: notify
          encoder: hello
  encoders:
    hello:
      status: 200
      headers:
        content-type: "application/json"
      body:
        msg:
          key: message
```

There are quite a few things going on here, so let's go through of all, that step by step:

- When data comes in (in this case, an http request) we look into our list of decoders for one that matches the request. In our example, the HTTP request is represented as an object. It will match the decoder defined at key ```hello```. Similarly to what Erlang does, we will pattern match and bind values, then we pass them on the next stage.
- Once our input data is matched, validated, and bound, we apply our state management logic. Here we select the ```hello``` update expression, simply because the decoder that matched was the ```hello``` decoder. 
- The ```hello``` update expression is quite straightfoward. First, it defines a variable in our model named ```message``` to the value ```Hello World```. It also expresses that a command (defined by an effect and encoder) should be scheduled and executed.
- Our application logic interacts with the outside world only via effects. An effect is just like a bit of Erlang code that takes parameters. Those parameters are defined by an encoder.
- In this simple example, we are using the ```notify``` effect.  This indicates the application framework to send a http response back to the client. There is a registry of well-know effects the framework is aware of, but it is also easy to write your own.
- The response is defined by the outcome of the ``hello`` encoder. I believe it is quite self-explanatory, what this encoder does. But have a look at how the content of the response body is built: It reads the value at key `message` from our model, and sets it in the field ``msg``of the final response. This is how we can produce dynamic content.
- Finally, the application framework nows, from the content-type, it should serialize it as JSON in order to produce the expected, final and super fancy result.

## Authors

* **Pedro Gutiérrez** - *Initial work* - [Pedro Gutierrez](https://github.com/pedro-gutierrez)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Inspiration

* The Elm Architecture
* Kubernetes
* Ansible
* Microservices


## Related projects

[Elementary](https://github.com/pedro-gutierrez/elementary)

