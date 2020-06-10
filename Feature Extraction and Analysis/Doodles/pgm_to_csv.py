import os

# Converts PGMs in "pgm" folder to CSVs
def main(): 
    for filename in os.listdir('pgm/'):
        with open('pgm/' + filename) as f:
            for _ in range(2):
                f.readline()
            rows, columns = map(int, f.readline().split())
            f.readline()

            csv_data = [[0 for x in range(rows)] for y in range(columns)]

            for row in range(rows):
                for column in range(columns):
                    value = int(f.readline().rstrip())
                    if value >= 128:
                        csv_data[row][column] = '0'
                    elif value < 128:
                        csv_data[row][column] = '1'

        with open(os.path.splitext(filename)[0] + '.csv', 'w') as f:
            for line in csv_data:
                f.writelines('\t'.join(line) + '\n')

main()