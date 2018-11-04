# Mark: 20/20.  The code ran very well and the text was in pig latin :)
# The code was clean and easy to follow and is sound and logical.
# Very Well done!


#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 19 14:31:53 2018

@author: peteryaron
"""
#this program takes either football.txt or music_reviews.txt file remove all punctuation and numbers and translate to pig latin in both the console and to a pig_output.txt file in the same directory
#PROGRAM LIMITATIONS: 
#(1) Cannot output as a string with punctuation like the input file. 
#(2) Cannot request input file 
#(3) cannot name output file using a naming convention  
#(4) for my own personal clarity i left the variable and function names long and _ delimited so I can better follow them. I did not use CamelCase or any other more standard forms 
#(5) this program does not remove punctuation embedded or strings of punctuation only in music_reviews.txt fil. To properly do this you would need to read in the file as a string then filter out character by character each unwanted character by using import string; string.punctuation or manually defining the unwanted characters.
#eng_to_pig function drives the overall program and show the breakdown of the programs operations into functions 
def eng_2_pig():

    input_file = open('football.txt', 'r')
    pig_output = open('pig_output.txt', 'w')
#read all lines of the .txt file into a single list item
#remove punctuation and numbers (need two variables because of single and double quote issue and couldn't figure out how to get a function to work on the list in an elegant way; punctuation 3 is for the music_reviews.txt file attempt)
#the variable joins letter for letter in english_input if letter not in the punctuation variables defined (I could not figure out myself a more efficient way than this method (github: https://gist.github.com/mcescalante/7921270)
# define vowels in lower and capital case to preserve capitalization similar to input file
    english_input = input_file.readlines()
    punctuation = ":;()&%$#@[]{}0123456789.!?,-'"
    punctuation2 = '"'
    punctuation3 = "unichr(163)"
    english_input = [''.join(c for c in s if c not in punctuation) for s in english_input]
    english_input = [''.join(c for c in s if c not in punctuation2) for s in english_input]
    english_input = [''.join(c for c in s if c not in punctuation3) for s in english_input]
    vowels = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']
# this is a variable to define special cases 
    exceptions = ['th','st','qu','pl','tr']
    for string in english_input:
#this splits the list into a list where each word is split by the white space delimiter
        english_list = string.split()

#define a variable that uses the translate to pig function to translate and print to command line
    t = translate_to_pig(english_list, vowels, exceptions)
    print("Translation to Pig Latin: ", t)
#define output function variables which uses the translated file and writes to the output .txt file
    write_pig_output(t, pig_output)
#define function that translates from english to pig latin 
def translate_to_pig(english_list, vowels, exceptions):
# create a new empty list to put the translated words from the translate function
    pig_list = []
# for every word in the list perform the following operations
    for word in english_list:
        if word[0] in vowels: # if the first index of the first word is a vowel
            pig_list.append(word + "hay") #add to pig_list
        else: #if letter does not begin with vowel
            pig_list.append(word[1:] + word[0] + "ay") # add to  pig_list
        if word[0:2] in exceptions:
            pig_list.append(word[2:] + word[0:2] + "ay") #add to pig_list

    return pig_list
# function to write translated pig latin passage into a 'pig_out.txt' output file
def write_pig_output(english_list, pig_output):
    pig_output.write(" ".join(english_list))

eng_2_pig()