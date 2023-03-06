[![Build Status](https://travis-ci.org/Gsantomaggio/roh.svg?branch=master)](https://travis-ci.org/Gsantomaggio/roh)

# roh
ROH is a distributed Python tasks manager.

Note: This is a working in progress project!

Run:
```
make run
```


How it works:


Suppose a cluster:
```
(roh@10.0.2.2)1> [node()|nodes()].
['roh@10.0.2.2','roh@10.0.2.15']
```

you can execute your python script across the cluster, for example a RabbitMQ implementation:
```
(roh@10.0.2.2)2> impl_rabbitmq_consumers:add_cluster_consumer(5672).

```

Check the status:
```
(roh@10.0.2.2)3> roh_management:status().
[{'roh@10.0.2.2',{[<<"<0.488.0> {task,115433358,test,start,[],test,stop,[],undefined,undefined}">>,
                   <<"<0.490.0> {task,73573331,test,start,[],test,stop,[],undefined,undefined}">>],
                  [<<" {task,43014671,test,start,[],test,stop,[],undefined,undefined}">>,
                   <<" {task,43796038,test,start,[],test,stop,[],undefined,undefined}">>]}}]
```

The script is running across the cluster !!

You can check the status using the web interface:

```
http://localhost:8080/api/management
```

result:

```
[
   {
      node:"roh@10.0.2.2",
      running_tasks:[
         "<0.488.0> {task,115433358,test,start,[],test,stop,[],undefined,undefined}",
         "<0.490.0> {task,73573331,test,start,[],test,stop,[],undefined,undefined}"
      ],
      waiting_tasks:[
         " {task,43014671,test,start,[],test,stop,[],undefined,undefined}",
         " {task,43796038,test,start,[],test,stop,[],undefined,undefined}"
      ]
   }
]
```

## Full documentation

[Full documentation HTML](https://gsantomaggio.github.io/roh/)

[Full documentation PDF](https://gsantomaggio.github.io/roh/index.pdf)

#Build from source

```
gmake
```
