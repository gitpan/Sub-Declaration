use 5.006;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Sub::Declaration',
    VERSION_FROM      => 'lib/Sub/Declaration.pm',
    PREREQ_PM         => {
        'Filter::Simple'    =>  0.78,
        'Lexical::Util'     =>  0.8,
    },
    ABSTRACT_FROM     => 'lib/Sub/Declaration.pm',
    AUTHOR            => 'Kevin Michael Vail <kvail@cpan.org>',
);
