#!/usr/bin/perl

# $URL$
# $Revision$, $Date$

# script for conversion of fortran old-style to new style:
# - remove labels in some do-loops
# - replace <number> if () then ; goto <number> ; endif by do while ... enddo
# - replace old-style logical operators by new-style operators
#

use strict;
use warnings;

my @f90_files = glob('*.f90');

foreach my $f90 (@f90_files)
{
    #
    # read all lines;
    # skip EOL
    #
    open IN, "<$f90";
    my @lines = ();
    while (my $line = <IN>)
    {
        chomp($line);
        push @lines, $line;
    }
    close IN;

    my $n = scalar @lines;

    my $SKIP = 'SKIP';

    #
    # replace do <number> ; <number> continue by do ... enddo
    #
    for (my $i=1 ; $i < $n ; $i++)
    {
        if ($lines[$i] =~ m/^ *do (\d+)/)
        {
            my $label = $1;
            for (my $j=$i+1 ; $j < $n ; $j++)
            {
                if ($lines[$j] =~ m/^ *$label *continue/)
                {
                    $lines[$j] =~ s/continue/end do/;
                    $lines[$j] =~ s/\d/ /g;

                    $lines[$i] =~ s/\b$label //;
                    last;
                }
            }
        }
    }

    #
    # replace <number> if () then ; goto <number> ; endif by do while ... enddo
    #
    for (my $i=1 ; $i < $n ; $i++)
    {
        if ($lines[$i] =~ m/^ *(\d+) *if *\((.*)\) *then/)
        {
            my $label = $1;
            my $condition = $2;
            my $nested = 0;
            GOTO:
            for (my $j=$i+1 ; $j < $n ; $j++)
            {
                if ($lines[$j] =~ m/^ * \d* *if/) {$nested++;}
                if ($lines[$j] =~ m/^ *end *if/) {$nested--;}
                if ($lines[$j] =~ m/^ * goto *$label$/)
                {
                    if ($nested)
                    {
                        print "$f90; line $i: nested if, skipping (too hard).\n";
                        last GOTO;
                    }
                    for (my $k=$j+1 ; $k < $n ; $k++)
                    {
                        if ($lines[$k] =~ m/^ *end *if/)
                        {
                            #print $f90, "\n$label $condition $i $j $k.\n";  #debug
                            $lines[$i] =~ s/if/do while/ ;
                            $lines[$i] =~ s/then// ;
                            my $nspaces = ' ' x length($label);
                            $lines[$i] =~ s/\b$label\b/$nspaces/;
                            $lines[$i] =~ s/ +$//; # remove possible spaces between condition and 'then'

                            $lines[$j] = $SKIP;

                            $lines[$k] =~ s/end *if/end do/;
                            last GOTO;
                        }
                    }
                }
            }
        }
    }

    #
    # replace old-style logical operators by new-style operators
    #
    my @oldstyle = ('eq', 'ne', 'gt', 'lt', 'ge', 'le');
    my @newstyle = ('==', '/=', '>' , '<' , '>=', '<=');
    for (my $i=1 ; $i < $n ; $i++)
    {
        for (my $j = 0; $j < scalar @oldstyle; $j++)
        {
            my $oldstyle = $oldstyle[$j];
            my $newstyle = $newstyle[$j];
            $lines[$i] =~ s/ *\.$oldstyle\. */ $newstyle /g;
        }
    }

    #
    # replace 'include csibla.i' by 'use sibufr'
    #
    for (my $i=1 ; $i < $n ; $i++)
    {
        $lines[$i] =~ s/include ['"]csibla.i['"]/use sibufr/g;
    }

    open OUT, ">$f90" or die "can't open file $f90 for writing: $!\n";
    for (my $i=0 ; $i < $n ; $i++)
    {
        print OUT $lines[$i] . "\n" if ($lines[$i] ne $SKIP);
    }
    close OUT or die "can't close file $f90 after writing: $!\n";

}

