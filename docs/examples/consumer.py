def on_message(ch, method, properties, body):
    time.sleep(0.3)
    global c
    c += 1
    print "Received on " + description + " - bd:" + body + " - received:" + str(
        c) + "\n"
    ch.basic_ack(delivery_tag=method.delivery_tag)


def threaded_rmq(channel_):
    ## tag::sample-imports[]    
    channel_.exchange_declare(exchange='test_exchange_1', durable=True,
                              exchange_type="fanout") # <1>
    result = channel_.queue_declare(queue='my_queue', durable=True)
    queue_name = result.method.queue
    channel_.queue_bind(exchange="test_exchange_1", queue=queue_name,
                        routing_key="") # <2>
    channel_.basic_qos(prefetch_count=1)
    channel_.basic_consume(on_message, queue=queue_name,
                           no_ack=False) # <3>
    channel_.start_consuming()
    ## end::sample-imports[]    
