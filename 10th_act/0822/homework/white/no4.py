import tensorflow as tf
import numpy as np
import time
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from sklearn import cross_validation
config = tf.ConfigProto()
config.gpu_options.allow_growth=True

white = np.loadtxt('white.csv', skiprows=1, delimiter=',', dtype=np.float32)
x_data = white[:, 0:-1]; y_data = white[:, [-1]]
Y_binary = (y_data > 5)

def minmaxscaler(data):
    numerator = data - np.min(data, 0)
    denominator = np.max(data, 0) - np.min(data, 0)
    return numerator / denominator

X_mm = minmaxscaler(x_data)
X_train, X_test, Y_train, Y_test = train_test_split(X_mm,Y_binary, test_size= 0.3, random_state= 1)
X = tf.placeholder(tf.float32, [None, len(X_train[0])])
Y = tf.placeholder(tf.float32, [None, 1])

#레이어 만들기
def mlp(layer_n, data, output_size, act='None'):
    w = tf.get_variable(layer_n+'w',[data.get_shape().as_list()[1],output_size],initializer=tf.contrib.layers.xavier_initializer())
    b = tf.get_variable(layer_n+'b',[output_size],initializer=tf.contrib.layers.xavier_initializer())
    if act == 'relu':
        res = tf.nn.relu(tf.matmul(data,w)+b)
        return res
    elif act == 'sigmoid':
        res = tf.nn.sigmoid(tf.matmul(data,w)+b)
        return res
    else:
        return tf.matmul(data,w)+b

#Weight, bias 설정
keep_prob = tf.placeholder(tf.float32)
l1=tf.nn.dropout(mlp('l1', X, 22, 'sigmoid'), keep_prob)
l2=tf.nn.dropout(mlp('l2', l1, 22, 'sigmoid'), keep_prob)
l3=tf.nn.dropout(mlp('l3', l2, 44, 'sigmoid'), keep_prob)
l4=tf.nn.dropout(mlp('l4', l3, 44, 'sigmoid'), keep_prob)
H=tf.nn.dropout(mlp('l5', l4, 1), keep_prob)

#cost 설정
cost = tf.reduce_sum(tf.pow(H-Y,2))/len(X_train)
optimizer = tf.train.AdamOptimizer(learning_rate=0.001).minimize(cost)
predict = tf.cast(H > 0.5, dtype=tf.float32)
accuracy = tf.reduce_mean(tf.cast(tf.equal(predict, Y), dtype=tf.float32))

#Session 실행
sess = tf.Session(config=config)
sess.run(tf.global_variables_initializer())
for step in range(0,10000):
    cost_val, _ = sess.run([cost, optimizer], feed_dict={X: X_train, Y: Y_train, keep_prob: 0.75})
    if(step % 200 == 0):
        print(step, cost_val)
print("Accuracy : ", sess.run(accuracy, feed_dict={X: X_test, Y: Y_test, keep_prob: 1.0}))
sess.close()