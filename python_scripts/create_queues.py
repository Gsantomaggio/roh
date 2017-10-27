# /bin/python
import pika
import sys
import time
import uuid

def start(rabbitmq_host, rabbitmq_port):
    credentials = pika.PlainCredentials('test', "test")
    parameters = pika.ConnectionParameters(rabbitmq_host,
                                           rabbitmq_port,
                                           'async',
                                           credentials)
    global description
    description = "host: " + rabbitmq_host + ", port:" + str(rabbitmq_port)
    global connection
    connection = pika.BlockingConnection(parameters)
    global channel
    channel = connection.channel()

    for x in xrange(1,5000):
        channel.queue_declare(queue=str(uuid.uuid4()) + "_" + str(rabbitmq_port),
                                     arguments={"x-expires": 4200000})

    print "Finished: " + description


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
