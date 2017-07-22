import re
import urllib.request


def complement(frame):  # Returns a complement to input frame
    reverse_frame = frame[::-1]
    complement_frame = ""
    for x in range(0, len(frame)):
        if reverse_frame[x] == "a":
            complement_frame += "t"
        elif reverse_frame[x] == "t":
            complement_frame += "a"
        elif reverse_frame[x] == "c":
            complement_frame += "g"
        elif reverse_frame[x] == "g":
            complement_frame += "c"

    return complement_frame


def translate(frame, input_encoding, input_dictionary):  # Returns a translation to input frame
    start = 0
    length_of_codon = 3
    end = length_of_codon
    translated_frame = ""
    for x in range(0, len(frame)):
        translated_frame += " "+encode(frame[start:end], input_encoding, input_dictionary)
        start += length_of_codon
        end += length_of_codon
        if start+length_of_codon > len(frame):
            return translated_frame


# dictionary is an integer to identify print mode of three or one character codes
# RNA and DNA identification of input frame is done prior to encoding with required code
def encode(input_codon, input_encoding, input_dictionary):
    input_codon = input_codon.upper()

    if input_dictionary == "DNA":  # DNA translation to Protein
        if (input_codon == "TTT") or (input_codon == "TTC"):  # Phenylalanine
            if input_encoding == 1:
                return "F"
            else:
                return "Phe"
        elif ((input_codon == "TTA") or (input_codon == "TTG") or (input_codon == "CTT") or  # Leucine
                (input_codon == "CTC") or (input_codon == "CTA") or (input_codon == "CTG")):
            if input_encoding == 1:
                return "L"
            else:
                return "Leu"
        elif (input_codon == "ATT") or (input_codon == "ATC") or (input_codon == "ATA"):  # Isoleucine
            if input_encoding == 1:
                return "I"
            else:
                return "IIe"  # ???
        elif input_codon == "ATG":  # Methionine
            if input_encoding == 1:
                return "Met"  # M
            else:
                return "Met"
        elif ((input_codon == "GTT") or (input_codon == "GTC") or (input_codon == "GTG") or  # Valine
                (input_codon == "GTA")):
            if input_encoding == 1:
                return "V"
            else:
                return "Val"
        elif ((input_codon == "TCT") or (input_codon == "TCC") or (input_codon == "TCA") or  # Serine
                (input_codon == "TCG") or (input_codon == "AGT") or (input_codon == "AGC")):
            if input_encoding == 1:
                return "S"
            else:
                return "Ser"
        elif ((input_codon == "CCT") or (input_codon == "CCC") or (input_codon == "CCA") or  # Proline
                (input_codon == "CCG")):
            if input_encoding == 1:
                return "P"
            else:
                return "Pro"
        elif ((input_codon == "ACT") or (input_codon == "ACC") or (input_codon == "ACA") or  # Threonine
                (input_codon == "ACG")):
            if input_encoding == 1:
                return "T"
            else:
                return "Thr"
        elif ((input_codon == "GCT") or (input_codon == "GCC") or (input_codon == "GCA") or  # Alanine
                (input_codon == "GCG")):
            if input_encoding == 1:
                return "A"
            else:
                return "Ala"
        elif ((input_codon == "CGT") or (input_codon == "CGC") or (input_codon == "CGA") or  # Arginine
                (input_codon == "CGG") or (input_codon == "AGA") or (input_codon == "AGG")):
            if input_encoding == 1:
                return "R"
            else:
                return "Arg"
        elif ((input_codon == "GGT") or (input_codon == "GGC") or (input_codon == "GGA") or  # Glycine
                (input_codon == "GGG")):
            if input_encoding == 1:
                return "G"
            else:
                return "Gly"
        elif (input_codon == "TGT") or (input_codon == "TGC"):  # Cysteine
            if input_encoding == 1:
                return "C"
            else:
                return "Cys"
        elif (input_codon == "TAT") or (input_codon == "TAC"):  # Tyrosine
            if input_encoding == 1:
                return "Y"
            else:
                return "Tyr"
        elif (input_codon == "CAT") or (input_codon == "CAC"):  # Histedine
            if input_encoding == 1:
                return "H"
            else:
                return "His"
        elif (input_codon == "CAA") or (input_codon == "CAG"):  # Gultamine
            if input_encoding == 1:
                return "Q"
            else:
                return "Gln"
        elif (input_codon == "AAT") or (input_codon == "AAC"):  # Asparagine
            if input_encoding == 1:
                return "N"
            else:
                return "Asn"
        elif (input_codon == "AAA") or (input_codon == "AAG"):  # Lysine
            if input_encoding == 1:
                return "K"
            else:
                return "Lys"
        elif (input_codon == "GAT") or (input_codon == "GAC"):  # Aspartic acid
            if input_encoding == 1:
                return "D"
            else:
                return "Asp"
        elif (input_codon == "GAA") or (input_codon == "GAG"):  # Glutamic acid
            if input_encoding == 1:
                return "E"
            else:
                return "Glu"
        elif input_codon == "TGG":  # Tryptophan
            if input_encoding == 1:
                return "W"
            else:
                return "Trp"
        elif (input_codon == "TAA") or (input_codon == "TAG") or (input_codon == "TGA"):
            return "Stop"
        else:
            return ""
    else:  # RNA translation to Protein
        if (input_codon == "UUU") or (input_codon == "UUC"):  # Phenylalanine
            if input_encoding == 1:
                return "F"
            else:
                return "Phe"
        elif ((input_codon == "UUA") or (input_codon == "UUG") or (input_codon == "CUU") or  # Leucine
                (input_codon == "CUC") or (input_codon == "CUA") or (input_codon == "CUG")):
            if input_encoding == 1:
                return "L"
            else:
                return "Leu"
        elif (input_codon == "AUU") or (input_codon == "AUC") or (input_codon == "AUA"):  # Isoleucine
            if input_encoding == 1:
                return "I"
            else:
                return "IIe"
        elif input_codon == "AUG":  # Methionine
            if input_encoding == 1:
                return "Met"  # M
            else:
                return "Met"
        elif ((input_codon == "GUU") or (input_codon == "GUC") or (input_codon == "GUG") or  # Valine
                (input_codon == "GUA")):
            if input_encoding == 1:
                return "V"
            else:
                return "Val"
        elif ((input_codon == "UCU") or (input_codon == "UCC") or (input_codon == "UCA") or  # Serine
                (input_codon == "UCG") or (input_codon == "AGU") or (input_codon == "AGC")):
            if input_encoding == 1:
                return "S"
            else:
                return "Ser"
        elif ((input_codon == "CCU") or (input_codon == "CCC") or (input_codon == "CCA") or  # Proline
                (input_codon == "CCG")):
            if input_encoding == 1:
                return "P"
            else:
                return "Pro"
        elif ((input_codon == "ACU") or (input_codon == "ACC") or (input_codon == "ACA") or  #
                (input_codon == "ACG")):
            if input_encoding == 1:
                return "U"
            else:
                return "Uhr"
        elif ((input_codon == "GCU") or (input_codon == "GCC") or (input_codon == "GCA") or  # Alanine
                (input_codon == "GCG")):
            if input_encoding == 1:
                return "A"
            else:
                return "Ala"
        elif ((input_codon == "CGU") or (input_codon == "CGC") or (input_codon == "CGA") or  # Arginine
                (input_codon == "CGG") or (input_codon == "AGA") or (input_codon == "AGG")):
            if input_encoding == 1:
                return "R"
            else:
                return "Arg"
        elif ((input_codon == "GGU") or (input_codon == "GGC") or (input_codon == "GGA") or  # Glycine
                (input_codon == "GGG")):
            if input_encoding == 1:
                return "G"
            else:
                return "Gly"
        elif (input_codon == "UGU") or (input_codon == "UGC"):  # Cysteine
            if input_encoding == 1:
                return "C"
            else:
                return "Cys"
        elif (input_codon == "UAU") or (input_codon == "UAC"):  #
            if input_encoding == 1:
                return "Y"
            else:
                return "Uyr"
        elif (input_codon == "CAU") or (input_codon == "CAC"):  # Histedine
            if input_encoding == 1:
                return "H"
            else:
                return "His"
        elif (input_codon == "CAA") or (input_codon == "CAG"):  # Gultamine
            if input_encoding == 1:
                return "Q"
            else:
                return "Gln"
        elif (input_codon == "AAU") or (input_codon == "AAC"):  # Asparagine
            if input_encoding == 1:
                return "N"
            else:
                return "Asn"
        elif (input_codon == "AAA") or (input_codon == "AAG"):  # Lysine
            if input_encoding == 1:
                return "K"
            else:
                return "Lys"
        elif (input_codon == "GAU") or (input_codon == "GAC"):  # Aspartic acid
            if input_encoding == 1:
                return "D"
            else:
                return "Asp"
        elif (input_codon == "GAA") or (input_codon == "GAG"):  # Glutamic acid
            if input_encoding == 1:
                return "E"
            else:
                return "Glu"
        elif input_codon == "UGG":  #
            if input_encoding == 1:
                return "W"
            else:
                return "Urp"
        elif (input_codon == "UAA") or (input_codon == "UAG") or (input_codon == "UGA"):
            return "Stop"
        else:
            return ""


def shift(frame):  # Returns a frame shifted by one
    start = 0 + 1
    shifted_frame = frame[start:len(frame)] + frame[0:1]
    return shifted_frame


def start_translation(input_gene, input_encoding, input_dictionary):
    for y in range(0, 3):
        print("5'3' Frame " + str(y + 1))
        print("-------------")
        print(translate(input_gene, input_encoding, input_dictionary))
        input_gene = shift(input_gene)
        print(" ")

    for y in range(0, 3):
        print("3'5' Frame " + str(y + 1))
        print("-------------")
        complementary_frame = complement(input_gene)
        complementary_frame = complementary_frame[(y + 1) * 3:len(complementary_frame)]
        print(translate(complementary_frame, input_encoding, input_dictionary))
        input_gene = shift(input_gene)
        print(" ")


if __name__ == "__main__":  # Main

    urlregex = re.compile(r'\b(http://)')
    genepath = input("Enter path or URL to gene file:")
    encoding = int(input("Enter 1 or 3 for encoding size:"))
    dictionary = input("Enter DNA or RNA for Dictionary:")
    print(" ")

    if urlregex.search(genepath) is not None:  # Identify and read from url
        gene = urllib.request.urlopen(genepath)
        input_seq = ""
        for line in gene:
            input_seq += re.sub('[^A-Za-z]+', '', line.decode())

    else:  # Read from local file
        with open(genepath, 'r') as infile:
            gene = infile.read()
            gene = re.sub('[^A-Za-z]+', '', gene)
            input_seq = gene

    start_translation(input_seq, encoding, dictionary)
