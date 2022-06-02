import math
import sys
def main():
    args = sys.argv[1:]
    prob = float(args[0])
    print("Si P(x) = ", prob)
    print("Alors H(x) = ", prob * math.log(1/prob,2))

if __name__ == '__main__':
    main()
