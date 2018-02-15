import math
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import pandas as pd 
from sklearn.model_selection import train_test_split
from sklearn import preprocessing

# import data set 
master1 = pd.read_csv("train_cluster6.csv")
cluster6 = pd.read_csv("train_cluster6.csv")

# exploring the data set 
cluster6.head()
cluster6.describe()

# check if there is any null values
cluster6.isnull().sum()  #no null values

columns = list(cluster6)
rand_df = cluster6.sample(30)

# dropping the columns ( test dataframe)
rand_df=rand_df.drop(['Unnamed: 0', 'Location_ID','Hybrid','Yield','Check_Yield','Yield_Difference','cluster1', 'City','Country','State','cluster'],axis=1)

# dropping columns from loaded data 
cluster6 = cluster6.drop(['Unnamed: 0','X' ,'Location_ID','Hybrid','Check_Yield','Yield','Yield_Difference','cluster1', 'City','Country','State','cluster'],axis=1)

# create training and testing vars
new_columns = list(rand_df)
X = pd.DataFrame.as_matrix(rand_df,columns=new_columns)
Y = rand_df.Yield
Y = Y.reshape(Y.shape[0],1)
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.08)
X_test, X_dev, Y_test, Y_dev = train_test_split(X_test,Y_test, test_size=.5)

# Checking the shape's of the new data set as matrix 
print("No of training Examples : "+str(X_train.shape[0]))  #(230511, 78) - 92% data 
print("No of test Examples : "+str(X_test.shape[0])) 
print("No of dev Examples : "+str(X_dev.shape[0])) 
print("Shape of training data : "+str(X_train.shape))
print("Shape of test data : "+str(X_test.shape))
print("Shape of dev data : "+str(X_dev.shape))

#Flatten the data to so that all Features/X Variables 
X_train_flatten = X_train.reshape(X_train.shape[0],-1).T
Y_train_flatten = Y_train.reshape(Y_train.shape[0],-1).T
X_dev_flatten = X_dev.reshape(X_dev.shape[0],-1).T
Y_dev_flatten = Y_dev.reshape(Y_dev.shape[0],-1).T
X_test_flatten = X_test.reshape(X_test.shape[0],-1).T
Y_test_flatten = Y_test.reshape(Y_test.shape[0],-1).T

print("No of training Examples : "+str(X_train_flatten.shape))  
print("No of test Examples : "+str(Y_train_flatten.shape))  
print("No of X_dev Examples : "+str(X_dev_flatten.shape))  
print("No of Y_dev test Examples : "+str(Y_dev_flatten.shape))  
print("No of X_test Examples : "+str(X_test_flatten.shape))  
print("No of Y_test Examples : "+str(Y_test_flatten.shape))  
print("No of Sanity_test : "+str(X_train_flatten[0:5,1]))  

# Normalize features and create final Train set 
X_train_set = preprocessing.normalize(X_train_flatten)
Y_train_set = Y_train_flatten

print("No of X_train_set shape : "+str(X_train_set.shape))  
print("No of Y_train_set shape : "+str(Y_train_set.shape)) 


def create_placeholders(n_x, n_y):

    X = tf.placeholder(shape=[n_x,None],dtype = tf.float32)
    Y = tf.placeholder(shape=[n_y,None],dtype = tf.float32)
    
    return X, Y

def initialize_parameters():
    
        
    W1 = tf.get_variable("W1", [80,79], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b1 = tf.get_variable("b1", [80,1], initializer = tf.zeros_initializer())
    W2 = tf.get_variable("W2", [60,80], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b2 = tf.get_variable("b2",[60,1], initializer=tf.zeros_initializer())
    W3 = tf.get_variable("W3", [40,60], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b3 = tf.get_variable("b3",[40,1], initializer=tf.zeros_initializer())
    W4 = tf.get_variable("W4", [20,40], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b4 = tf.get_variable("b4",[20,1], initializer=tf.zeros_initializer())
    W5 = tf.get_variable("W5", [10,20], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b5 = tf.get_variable("b5",[10,1], initializer=tf.zeros_initializer())
    W6 = tf.get_variable("W6", [8,10], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b6 = tf.get_variable("b6",[8,1], initializer=tf.zeros_initializer())
    W7 = tf.get_variable("W7", [6,8], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b7 = tf.get_variable("b7",[6,1], initializer=tf.zeros_initializer())
    W8 = tf.get_variable("W8", [3,6], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b8 = tf.get_variable("b8",[3,1], initializer=tf.zeros_initializer())
    W9 = tf.get_variable("W9", [1,3], initializer = tf.contrib.layers.xavier_initializer(seed=1))
    b9 = tf.get_variable("b9",[1,1], initializer=tf.zeros_initializer())
    ### END CODE HERE ###

    parameters = {"W1": W1,
                  "b1": b1,
                  "W2": W2,
                  "b2": b2,
                  "W3": W3,
                  "b3": b3,
                  "W4": W4,
                  "b4": b4,
                  "W5": W5,
                  "b5": b5,
                  "W6": W6,
                  "b6": b6,
                  "W7": W7,
                  "b7": b7,
                  "W8": W8,
                  "b8": b8,
                  "W9": W9,
                  "b9": b9}
    
    return parameters

tf.reset_default_graph()
with tf.Session() as sess:
    parameters = initialize_parameters()
    print("W1 = " + str(parameters["W1"]))
    print("b1 = " + str(parameters["b1"]))
    print("W2 = " + str(parameters["W2"]))
    print("b2 = " + str(parameters["b2"]))


def forward_propagation(X, parameters):
    
    # Retrieve the parameters from the dictionary "parameters" 
    W1 = parameters['W1']
    b1 = parameters['b1']
    W2 = parameters['W2']
    b2 = parameters['b2']
    W3 = parameters['W3']
    b3 = parameters['b3']
    W4 = parameters['W4']
    b4 = parameters['b4']
    W5 = parameters['W5']
    b5 = parameters['b5']
    W6 = parameters['W6']
    b6 = parameters['b6']
    W7 = parameters['W7']
    b7 = parameters['b7']
    W8 = parameters['W8']
    b8 = parameters['b8']
    W9 = parameters['W9']
    b9 = parameters['b9']
    
    ### START CODE HERE ### (approx. 5 lines)                                    # Numpy Equivalents:
    Z1 = tf.add(tf.matmul(W1,X),b1)                                              # Z1 = np.dot(W1, X) + b1
    A1 = tf.nn.relu(Z1)                                                          # A1 = relu(Z1)
    Z2 = tf.add(tf.matmul(W2,A1),b2)                                             # Z2 = np.dot(W2, a1) + b2
    A2 = tf.nn.relu(Z2)                                                          # A2 = relu(Z2)
    Z3 = tf.add(tf.matmul(W3,A2),b3)    
    A3 = tf.nn.relu(Z3)                                                          # A2 = relu(Z2)
    Z4 = tf.add(tf.matmul(W4,A3),b4)  
    A4 = tf.nn.relu(Z4)                                                          # A2 = relu(Z2)
    Z5 = tf.add(tf.matmul(W5,A4),b5)                                       # Z3 = np.dot(W3,Z2) + b3
    A5 = tf.nn.relu(Z5)                                                          # A2 = relu(Z2)
    Z6 = tf.add(tf.matmul(W6,A5),b6)    
    A6 = tf.nn.relu(Z6)                                                          # A2 = relu(Z2)
    Z7 = tf.add(tf.matmul(W7,A6),b7)  
    A7 = tf.nn.relu(Z7)                                                          # A2 = relu(Z2)
    Z8 = tf.add(tf.matmul(W8,A7),b8)                                       # Z3 = np.dot(W3,Z2) + b3
    A8 = tf.nn.relu(Z8)                                                          # A2 = relu(Z2)
    Z9 = tf.add(tf.matmul(W9,A8),b9)  
    ### END CODE HERE ###
    
    return Z9

# GRADED FUNCTION: compute_cost 

def compute_cost(Z3, Y):    
   
    Z3_T = tf.transpose(Z3)
    Y_T = tf.transpose(Y)
    
    cost = tf.reduce_mean(tf.square(Z3_T - Y_T))
    
    return cost

def random_mini_batches(X, Y, mini_batch_size = 64, seed = 0):
    """
    Creates a list of random minibatches from (X, Y)
    
    Arguments:
    X -- input data, of shape (input size, number of examples)
    Y -- true "label" vector (containing 0 if cat, 1 if non-cat), of shape (1, number of examples)
    mini_batch_size - size of the mini-batches, integer
    seed -- this is only for the purpose of grading, so that you're "random minibatches are the same as ours.
    
    Returns:
    mini_batches -- list of synchronous (mini_batch_X, mini_batch_Y)
    """
    
    m = X.shape[1]                  # number of training examples
    mini_batches = []
    
    # Step 1: Shuffle (X, Y)
    permutation = list(np.random.permutation(m))
    shuffled_X = X[:, permutation]
    shuffled_Y = Y[:, permutation].reshape((Y.shape[0],m))

    # Step 2: Partition (shuffled_X, shuffled_Y). Minus the end case.
    num_complete_minibatches = math.floor(m/mini_batch_size) # number of mini batches of size mini_batch_size in your partitionning
    for k in range(0, num_complete_minibatches):
        mini_batch_X = shuffled_X[:, k * mini_batch_size : k * mini_batch_size + mini_batch_size]
        mini_batch_Y = shuffled_Y[:, k * mini_batch_size : k * mini_batch_size + mini_batch_size]
        mini_batch = (mini_batch_X, mini_batch_Y)
        mini_batches.append(mini_batch)
    
    # Handling the end case (last mini-batch < mini_batch_size)
    if m % mini_batch_size != 0:
        mini_batch_X = shuffled_X[:, num_complete_minibatches * mini_batch_size : m]
        mini_batch_Y = shuffled_Y[:, num_complete_minibatches * mini_batch_size : m]
        mini_batch = (mini_batch_X, mini_batch_Y)
        mini_batches.append(mini_batch)
    
    return mini_batches

def model(X_train, Y_train, X_test, Y_test, learning_rate = 0.005,
          num_epochs = 1500, minibatch_size = 64, print_cost = True):
 
    tf.reset_default_graph()                         # to be able to rerun the model without overwriting tf variables
    (n_x, m) = X_train.shape                          # (n_x: input size, m : number of examples in the train set)
    n_y = Y_train.shape[0]                            # n_y : output size
    costs = []                                        # To keep track of the cost
    
    # Create Placeholders of shape (n_x, n_y)
    ### START CODE HERE ### (1 line)
    X, Y = create_placeholders(n_x,n_y)
    ### END CODE HERE ###

    # Initialize parameters
    ### START CODE HERE ### (1 line)
    parameters = initialize_parameters()
    ### END CODE HERE ###
    
    # Forward propagation: Build the forward propagation in the tensorflow graph
    ### START CODE HERE ### (1 line)
    Z3 = forward_propagation(X, parameters)
    ### END CODE HERE ###
    
    # Cost function: Add cost function to tensorflow graph
    ### START CODE HERE ### (1 line)
    cost = compute_cost(Z3, Y)
    ### END CODE HERE ###
    
    # Backpropagation: Define the tensorflow optimizer. Use an AdamOptimizer.
    ### START CODE HERE ### (1 line)
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate).minimize(cost)
    ### END CODE HERE ###
    
    # Initialize all the variables
    init = tf.global_variables_initializer()

    # Start the session to compute the tensorflow graph
    with tf.Session() as sess:
        
        # Run the initialization
        sess.run(init)
        
        # Do the training loop
        for epoch in range(num_epochs):

            epoch_cost = 0.                       # Defines a cost related to an epoch
            num_minibatches = int(m / minibatch_size) # number of minibatches of size minibatch_size in the train set
            minibatches = random_mini_batches(X_train, Y_train, minibatch_size)

            for minibatch in minibatches:

                # Select a minibatch
                (minibatch_X, minibatch_Y) = minibatch
                
                # IMPORTANT: The line that runs the graph on a minibatch.
                # Run the session to execute the "optimizer" and the "cost", the feedict should contain a minibatch for (X,Y).
                ### START CODE HERE ### (1 line)
                _ , minibatch_cost = sess.run([optimizer, cost], feed_dict={X: minibatch_X, Y: minibatch_Y})
                ### END CODE HERE ###
                
                epoch_cost += minibatch_cost / num_minibatches

            # Print the cost every epoch
            if print_cost == True and epoch % 100 == 0:
                print ("Cost after epoch %i: %f" % (epoch, epoch_cost))
            if print_cost == True and epoch % 5 == 0:
                costs.append(epoch_cost)
                
        # plot the cost
        plt.plot(np.squeeze(costs))
        plt.ylabel('cost')
        plt.xlabel('iterations (per tens)')
        plt.title("Learning rate =" + str(learning_rate))
        plt.show()

        # lets save the parameters in a variable
        parameters = sess.run(parameters)
        print ("Parameters have been trained!")

        # Calculate the correct predictions
        correct_prediction = tf.equal(tf.argmax(Z3), tf.argmax(Y))

        # Calculate accuracy on the test set
        accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float"))

        print ("Train Accuracy:", accuracy.eval({X: X_train, Y: Y_train}))
        print ("Test Accuracy:", accuracy.eval({X: X_test, Y: Y_test}))
        
        return parameters
    
    
parameters = model(X_train_set, Y_train_set, X_test, Y_test)

