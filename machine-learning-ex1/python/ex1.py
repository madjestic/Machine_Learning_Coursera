import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def pause(message :str = "Program paused. Press enter to continue.\n"):
    programPause = input(message)

def identity(x :int) -> np.ndarray:
    result = np.identity(x)
    return result

def plotData(x :[float], y :[float]):
    plt.plot(x,y, 'rx')
    plt.xlabel('Population of City in 10,000s')
    plt.ylabel('Profit in $10,000s')
    plt.show()

def main():
# ==================== Part 1: Basic Function ====================
    print("Running warmUpExercise ... \n")
    print("5x5 Identity Matrix: \n")
    print (identity(5))
    pause()
    
# ======================= Part 2: Plotting =======================    
    print("Plotting Data...\n")
    data = pd.read_csv("ex1data1.txt", header = None, names = ['x','y'])
    x = data['x']
    y = data['y']    
    plotData(x,y)
    pause('Program paused. Press enter to continue.\n')

# =================== Part 3: Cost and Gradient descent ===================      
    return 0

if __name__ == "__main__":
    main()
