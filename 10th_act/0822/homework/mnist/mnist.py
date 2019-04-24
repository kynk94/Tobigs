import tensorflow as tf
import time
from tensorflow.examples.tutorials.mnist import input_data
mnist=input_data.read_data_sets("/tmp/data/",one_hot=True)

X=tf.placeholder(tf.float32,[None,784])
Y=tf.placeholder(tf.float32,[None,10])
phase = tf.placeholder(tf.bool, name='phase')

#레이어 만들기
def mlp(layer_n, data, output_size, act='None', batch=False):
    w = tf.get_variable(layer_n+'w',[data.get_shape().as_list()[1],output_size],initializer=tf.contrib.layers.xavier_initializer())
    b = tf.get_variable(layer_n+'b',[output_size],initializer=tf.contrib.layers.xavier_initializer())
    if act == 'relu':
        res = tf.nn.relu(tf.matmul(data,w)+b)
    elif act == 'sigmoid':
        res = tf.nn.sigmoid(tf.matmul(data,w)+b)
    else:
        res = tf.matmul(data,w)+b
    if batch == False:
        return res
    elif batch == True:
        res = tf.contrib.layers.batch_norm(res, center=True, scale=True, is_training=phase)
        return res
#dropout시 보존할 비율, 0.75 사용
keep_prob = tf.placeholder(tf.float32)

#relu와 dropout 적용, 결과는 softmax 적용하기 위해 남겨둔다.
l1=tf.nn.dropout(mlp('l1', X, 512, 'relu'), keep_prob)
l2=tf.nn.dropout(mlp('l2', l1, 512, 'relu'), keep_prob)
l3=tf.nn.dropout(mlp('l3', l2, 256, 'relu'), keep_prob)
l4=tf.nn.dropout(mlp('l4', l3, 256, 'relu'), keep_prob)
l5=tf.nn.dropout(mlp('l5', l4, 128, 'relu'), keep_prob)
l6=tf.nn.dropout(mlp('l6', l5, 128, 'relu'), keep_prob)
predict=tf.nn.dropout(mlp('l7', l6, 10), keep_prob)

#gpu를 사용하므로 epochs를 알맞게 설정 (15회 : 24초 소요)
learning_rate = 0.001
training_epochs = 50
batch_size = 100

#cost, optimizer 설정
cost=tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=predict,labels=Y))
optimizer=tf.train.AdamOptimizer(learning_rate=learning_rate).minimize(cost)

#Memory overflow 방지를 위한 config
config = tf.ConfigProto()
config.gpu_options.allow_growth=True

with tf.Session(config=config) as sess:
    start=time.time()
    tf.global_variables_initializer().run()

    # Training cycle
    for epoch in range(training_epochs):
        avg_cost = 0
        total_batch = int(mnist.train.num_examples/batch_size)

        # Fit the line.
        for step in range(total_batch):
            batch_xs, batch_ys = mnist.train.next_batch(batch_size)
            # Fit training using batch data
            sess.run(optimizer, feed_dict={X: batch_xs, Y: batch_ys, keep_prob: 0.75})
            # Compute average loss
            avg_cost += sess.run(cost, feed_dict={X: batch_xs, Y: batch_ys, keep_prob: 0.75})/total_batch
            # Display
            #print ("Epoch:", '%04d' %(epoch+1), "cost=", "{:.9f}".format(avg_cost))

        #결과 확인
        correct_prediction = tf.equal(tf.argmax(predict, 1), tf.argmax(Y, 1))
        accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float"))
        print("Epoch:", '%02d' %(epoch+1), "Accuracy:", accuracy.eval({X: mnist.test.images, Y: mnist.test.labels, keep_prob: 1.0}), "cost=", "{:.9f}".format(avg_cost))
    print ("Optimization Finished!")

    #소요시간 출력
    elapsed = time.time() - start
    print('Time:%f'%elapsed)