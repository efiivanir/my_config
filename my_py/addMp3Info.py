import sys
import os
from mp3_tagger import MP3File, VERSION_1, VERSION_2, VERSION_BOTH

mp3File = sys.argv[1]
if not os.path.isfile(mp3File):
    print("File" + mp3File + "doesn't exists")
    exit()
full_path = os.path.abspath(mp3File)
full_path_array = full_path.split("/")
song = full_path_array[-1].replace('.mp3','')

album = full_path_array[-2]
artist = full_path_array[-3]


mp3 = MP3File(full_path)
print(artist,album,song)
del mp3.album
del mp3.artist
del mp3.song


mp3.artist = artist
mp3.album = album
mp3.song = song

mp3.save()

