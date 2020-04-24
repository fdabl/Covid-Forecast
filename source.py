import numpy as np
from matplotlib import pyplot as plt

def randn_py(times):
  return [np.random.normal(0, 1) for i in range(int(times))]

def draw_py(x, width, height, dpi=100):
  plt.figure(figsize=(width / dpi, height / dpi), dpi=100)
  plt.plot(x)
  plt.savefig('Figures/Intervention-Plot.png')
  plt.close()
  
