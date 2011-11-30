#!/usr/bin/perl -w

use Getopt::Std;

#parse script arguments
getopts("hl:n:", \%options);

#if all we want is help print it and exit
if($options{"h"}) {
    print "Usage: passgen.pl [-n <num>] [-h]\n";
    print "\t-n <num>\tGenerate <num> number of passwords\n";
    print "\t-l <num>\tGenerate passwords <num> characters long\n";
    print "\t-h\t\tThis screen\n";
    exit(0);
}

@chars = ("A".."Z", 0..9, "a".."z", qw(! @ $ % ^ & * - + _));

srand(time());

if (!grep(/n/, keys(%options))) {
    $options{"n"} = 1;
}

if (!grep(/l/, keys(%options))) {
    $options{"l"} = 12;
}

for( ; $options{"n"} > 0 ; $options{"n"}--) {
    print join("", @chars[map{rand(@chars)} (1 .. $options{'l'})]) . "\n";
}
