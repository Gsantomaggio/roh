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
Server status:
 [{'roh@10.0.2.2',[<<"Pid:<0.308.0> - Id: 80248251 Module start: [consumer]">>]},
  {'roh@10.0.2.15',[<<"Pid:<0.316.0> - Id: 80248251 Module start: [consumer]">>]}]
```

The script is running across the cluster !!

