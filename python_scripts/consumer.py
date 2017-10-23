# /bin/python
import pika
import sys
import time
from threading import Thread

c = 0
connection = None
channel = None


def on_message(ch, method, properties, body):
    time.sleep(0.3)
    global c
    c = c + 1
    print "****** Message received!********\n"
    print body + "\n"
    print "***************************** -  received:" + str(c) + "\n"
    print " "
    ch.basic_ack(delivery_tag=method.delivery_tag)


def threaded_rmq(channel):
    result = channel.queue_declare(queue='test_queue',
                                   arguments={"x-expires":20000})
    queue_name = result.method.queue
    channel.basic_qos(prefetch_count=1)
    channel.basic_consume(on_message, queue=queue_name,
                          no_ack=False)
    channel.start_consuming()


def start(rabbitmq_host, rabbitmq_port):
    credentials = pika.PlainCredentials('test', 'test')
    parameters = pika.ConnectionParameters(rabbitmq_host,
                                           rabbitmq_port,
                                           'async',
                                           credentials)
    global connection
    connection = pika.BlockingConnection(parameters)
    global channel
    channel = connection.channel()

    thread_rmq = Thread(target=threaded_rmq, args=(channel,))
    thread_rmq.start()


def stop():
    def kill():
        channel.stop_consuming()

    connection.add_timeout(0, kill)


if __name__ == "__main__":
    print 'Argument List:', str(sys.argv)
    rabbitmq_host = sys.argv[1]
    global rabbitmq_port
    rabbitmq_port = int(sys.argv[2])
    start(rabbitmq_host, rabbitmq_port)
    var = raw_input("Any key to stop")
    print "Stopping..."
    stop()
    print "Stopped"
