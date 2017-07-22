import sys


class HmmSetup:
    states = ['S1', 'S2', 'S3']
    # pi:
    start_probability = {'S1': -2, 'S2': -1, 'S3': -2}
    # tau:
    trans_probability = {'S1': {'S1': -1,             'S2': -1.321928,     'S3': -3.321928},
                         'S2': {'S1': -float('Inf'),  'S2': -1.,           'S3': -1.},
                         'S3': {'S1': -1.736966,      'S2': -2.321928,     'S3': -1.}}
    # e:
    emit_probability = {'S1': {'A': -1.321928,    'C': -1.736966,  'T': -2.321928,  'G': -3.321928},
                        'S2': {'A': -2.,          'C': -2.,        'T': -2.,        'G': -2.},
                        'S3': {'A': -3.321928,    'C': -2.321928,  'T': -1.736966,  'G': -1.321928}}


class HMMProc:

    def __init__(self, fasta):
        self.para = HmmSetup()
        self.fastaList = fasta
        self.resultMatrix = [[0 for col in range(len(self.fastaList))] for row in range(len(self.para.states))]
        self.path={}

    def starthmm(self):
        emit = self.para.emit_propability
        trans = self.para.trans_probability
        pi = self.para.start_probability
        states = self.para.states
        result = self.resultMatrix
        path = self.path

        for i in range(len(states)):
            # Initialization Step: alpha1(i) = pi(i) * ei(O1)
            result[i][0] = pi[states[i]] + emit[states[i]][self.fastaList[0]]
            path[states[i]] = [states[i]]

        for j in range(1, len(self.fastaList)):
            newpath = {}

            for i in range(len(states)):
                # Iteration Step: ei(Ot+1) * MAX(alphat(j) * transij)
                (prob, state) = max([(emit[states[i]][self.fastaList[j]] + result[k][j-1] + trans[states[k]][states[i]],
                                      states[k]) for k in range(3)])
                result[i][j] = prob
                newpath[states[i]] = path[state] + [states[i]]
            path = newpath

        return result, path

    # Generate string output
    def outstring(self):
        string = '   '
        for i in range(len(self.fastaList)):
            string += str("%8s" % self.fastaList[i])
        string += '\n'

        for i in range(len(self.para.states)):
            string += str( "%.6s: " % self.para.states[i])
            for j in range(len(self.resultMatrix[0])):
                string += str( "%.7s " % ("%f" % self.resultMatrix[i][j]))
            string += '\n'
        return string

    # Generate string with path taken
    def pathstr(self):
        print(str(self.path))


class PreProc:
    # Preprocessing data and enforcing text formatting

    def __init__(self, text):
        self.text = str.upper(text)

    def preprocessdata(self):
        lst = list(self.text)
        return [x for x in lst if x == 'A' or x == 'C' or x == 'G' or x == 'T']


class FileIO:
    # Utility class for File handling

    def __init__(self, infilename, outfilename):
        self.infilename = infilename
        self.outfilename = outfilename

    def readfile(self):
        infile = open(self.infilename)
        text = infile.read()
        return text

    def writefile(self,text):
        outfile = open(self.outfilename, 'w')
        outfile.write(text)


if __name__ == '__main__':
    fileStream = None
    if len(sys.argv) < 2:
        print("arg length error")
    elif len(sys.argv) == 2:
        fileStream = FileIO(sys.argv[1], 'result_'+sys.argv[1])
    else:
        fileStream = FileIO(sys.argv[1], sys.argv[2])

    fileStream = FileIO('fastainput.txt', 'outfile-man.txt')

    DNA = fileStream.readfile()

    DNA_list = PreProc(DNA).preprocessdata()
    starthmm = HMMProc(DNA_list)
    result, path = starthmm.starthmm()
    result_txt = 'path: \n' + str(path)+'\nmatrix:\n'+starthmm.outstring()
    fileStream.writefile(result_txt)