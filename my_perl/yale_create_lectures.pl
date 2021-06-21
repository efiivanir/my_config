#!/usr/bin/perl
use strict ;
use Data::Dumper;
use Getopt::Long;

my $path;
my $url;
my $root = '/media/ivanir/WD_3T_01/Yale/';
my $created ;
my %courses = (
# 'Early_Modern_England' => 'https://www.youtube.com/playlist?list=PL18B9F132DFD967A3',
# 'Epidemics_in_Western_Society_Since_1600' => 'https://www.youtube.com/playlist?list=PL3AE7B3B6917DE8E6',
# 'European_Civilization_1648_1945' => 'https://www.youtube.com/playlist?list=PL3A8E6CE294860A24',
# 'France_Since_1871' => 'https://www.youtube.com/playlist?list=PLE653BF062C136B62',
# 'Roman_Architecture' => 'https://www.youtube.com/playlist?list=PLBCB3059E45654BCE',
# 'The_Civil_War_and_ Reconstruction_Era_1845_1877' => 'https://www.youtube.com/playlist?list=PL5DD220D6A1282057',
# 'The_Early_Middle_Ages_284_1000' => 'https://www.youtube.com/playlist?list=PL77A337915A76F660',
               'Introduction_to_the_New_Testament_History_and_Literature' => 'https://www.youtube.com/playlist?list=PL279CFA55C51E75E0',
               'Introduction_to_the_Old_Testament_Hebrew_Bible' => 'https://www.youtube.com/playlist?list=PLh9mgdi4rNeyuvTEbD-Ei0JdMUujXfyWi',
               'Capitalism_Success_Crisis_and_Reform' => 'https://www.youtube.com/playlist?list=PL2497FD1251EED4DD', 
               'Introduction_to_Political_Philosophy' => 'https://www.youtube.com/playlist?list=PL8D95DEA9B7DFE825',
               'Introduction_to_Theory_of_Literature' => 'https://www.youtube.com/playlist?list=PLD00D35CBC75941BD',
               'Milton' => 'https://www.youtube.com/playlist?list=PL2103FD9F9D0615B7',
               'Modern_Poetry' => 'https://www.youtube.com/playlist?list=PLh9mgdi4rNewA25FVJ-lawQ-yr-alF58z',
               'The_American_Novel_Since_1945' => 'https://www.youtube.com/playlist?list=PLE33BCD966FF96F23',
               'Hemingway_Fitzgerald_Faulkner' => 'https://www.youtube.com/playlist?list=PL84C3A4DD9C263D79',
              );

foreach $path (sort keys %courses) {
  $created = $root . $path;
  mkdir($created) || die "can't create $created $!\n";
  chdir($created) || die "can't chdir $created $!\n";
  $url = $courses{$path};
  system("youtube-dl -f 18 --sub-lang en --write-sub \'$url\'");
}
