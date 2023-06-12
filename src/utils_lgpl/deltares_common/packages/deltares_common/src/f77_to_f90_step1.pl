#!/usr/bin/perl

# $URL$
# $Revision$, $Date$

# script for conversion of fortran fixed format to fortran free format
# - extension .f -> .f90  via git mv
# - comment 'c'  -> '!'
# - continuation at different position
# - add function/subroutine name to last end
#
# script also tries to join two small lines into one longer line

use strict;
use warnings;

my @f77_files = glob('*.f');

foreach my $f (@f77_files)
{
    my $basename = $f;
    $basename =~ s/\.f$//i;
    my $f90 = $f . '90';
    system "git mv $f $f90\n";

    #
    # read all lines;
    # skip EOL
    # change comment character
    # strip spaces at the of the line
    #
    open IN, "<$f90";
    my @lines = ();
    while (my $line = <IN>)
    {
        chomp($line);
        $line =~ s/^cp/! /i;
        $line =~ s/^c/!/i;
        $line =~ s/ +$//;
        push @lines, $line;
    }
    close IN;

    my $n = scalar @lines;

    #
    # handle different continuation
    # join two lines if that keeps length <= 100
    #
    my $SKIP = 'SKIP';
    for (my $i=1 ; $i < $n ; $i++)
    {
        if ($lines[$i] =~ m/^     \S/)
        {
            my $combined = can_be_joined ( \@lines, $i );
            if ($combined eq '')
            {
                $lines[$i-1] .= ' &';
                $lines[$i] =~ s/^     \S/      /;
            }
            else
            {
                $lines[$i-1] = $combined;
                $lines[$i] = $SKIP;
            }
        }
    }

    #
    # if last line is only 'end', append subroutine / function
    # last line may be '' so loop backwards till 'end' is found
    # also remove leading spaces as some editors guess style based on first line
    #
    my @types = ('subroutine', 'function', 'program');
    OUTER:
    for (my $i=$n-1 ; $i >= 0 ; $i--)
    {
        if ($lines[$i] =~ m/^ *end$/)
        {
            foreach my $type (@types)
            {
                if ($lines[0] =~ m/$type/i)
                {
                    $lines[$i] .= ' ' .  $type . ' ' . $basename;
                    $lines[0] =~ s/^      //;
                    $lines[$i] =~ s/^      //;
                    last OUTER;
                }
            }
        }
    }

    open OUT, ">$f90" or die "can't open file $f90 for writing: $!\n";
    for (my $i=0 ; $i < $n ; $i++)
    {
        print OUT $lines[$i] . "\n" if ($lines[$i] ne $SKIP);
    }
    close OUT or die "can't close file $f90 after writing: $!\n";

}

sub can_be_joined
{   # lines can be joined if:
    # - statement is split over exactly two lines;
    # - the statement is smaller than or equal to 100 characters.
    #
    # returns joined statement
    # or '' if it can't be joined
    #
    my $plines = shift;
    my $i = shift;

    my @lines = @{$plines};

    my $cur_line = $lines[$i];

    if ($i >= scalar @lines -2) {return '';}
    if ($lines[$i+1] =~ m/^     \S/) {return '';}

    $cur_line =~ s/^     \S *//;

    my $combined = $lines[$i-1] . ' ' . $cur_line;

    if (length($combined) <= 100) {return $combined;}

    return '';
}

